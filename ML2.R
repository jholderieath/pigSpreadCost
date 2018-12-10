source('pkgs.R')
source('edm_fx.R')
source('snow_and_pig_data.R')
source('build_and_test_h2o.R')
source('machine_learning_pig_spread.R')
# build dfs for price change calculations--------------
crops <- c("corn","soy","wheat",'rice','peanut')
markets <- c("domestic","export")
s <- Presence[,'GEOID']
suppliers <- unique(c("imports",unlist(s)))
variables <- gen_var(crops,markets,suppliers)
EB <- gen_shock(crops, place=suppliers, side = "B")
EC <- gen_shock(crops, place=markets, side = "C")
source('elast.R')
# consumption shocks-----------
ECL <- tibble(EC) %>%
  rename(name = EC) %>%
  add_column(val = 0)

rm(EC)
# production shocks -----------
#The shocks are on the production side of the equilibrium conditions. 
EBL <- tibble(EB) %>%
  rename(name = EB)%>%
  add_column(val = c(0,Presence[,'EB_corn_2020', drop = TRUE],
                     0,Presence[,'EB_soy_2020', drop = TRUE],
                     0,Presence[,'EB_wheat_2020', drop = TRUE],
                     0,Presence[,'EB_rice_2020', drop = TRUE],
                     0,Presence[,'EB_peanuts_2020', drop = TRUE]))
# assemble variables for domestic, import, and exports -------------

# domestic_wt_demand <- w_domestic * e_domestic
# export_wt_demand <- w_export * e_export
# import_wt_supply <- ss_imports * e_imports

v <-  variables%>%
  unlist()%>%
  str_subset("\\b[^EP]{0,2}_domestic|export|imports.")%>%
  as.tibble()%>%
  rename(name = value)

v$val <- val_look_up(v$name)

v <- rbind(v,ECL)%>%
  rbind(EBL)

# supply elasticities----------------

ve <- variables %>%
  unlist()%>%
  str_subset("\\b[e]_[:digit:]{5}.")%>%
  as.tibble()%>%
  rename(name = value)

ve$reg<- ve$name %>%
  str_replace_all("\\b[e]_(20|31)[:digit:]{3}.","e_Central_Plains_")%>%
  str_replace_all("\\b[e]_(17|18|19|29|39)[:digit:]{3}.","e_Corn_Belt_")%>%
  str_replace_all("\\b[e]_(05|22|28)[:digit:]{3}.","e_Delta_States_")%>%
  str_replace_all("\\b[e]_(04|06|08|16|30|32|35|41|49|53|56)[:digit:]{3}.","e_Far_West_")%>%
  str_replace_all("\\b[e]_(26|27|55)[:digit:]{3}.","e_Lake_States_")%>%
  str_replace_all("\\b[e]_(09|10|11|23|24|25|33|34|36|42|44|50|54)[:digit:]{3}.","e_Northeast_")%>%
  str_replace_all("\\b[e]_(38|46)[:digit:]{3}.","e_Northern_Plains_")%>%
  str_replace_all("\\b[e]_(01|12|13|21|37|45|47|51)[:digit:]{3}.","e_Southeast_")%>%
  str_replace_all("\\b[e]_(40|48)[:digit:]{3}.","e_Southern_Plains_")

temp <-   paste0(
  str_extract(ve$reg,'\\b[e]_'),
  str_extract(ve$reg,'(Central_Plains|Corn_Belt|Delta_States|Far_West|Lake_States|Northeast|Northern_Plains|Southeast|Southern_Plains)'),"['",
  str_extract(ve$reg,'(corn|soy|wheat|rice|peanut){1}'),"','",
  str_extract(ve$reg,'(corn|soy|wheat|rice|peanut)\\b'),"']"
)
ve$val <-  purrr::map_dbl(temp, eval_parse)
#ve$val <-  purrr::map_dbl(ve$reg, eval_parse)

v <- rbind(v,ve[,c('name','val')])
#supply weights------------
Total_Corn_Supply <- 16942000000
Total_Soy_Supply <-  4719000000 
Total_Wheat_Supply <- 3079000000
Total_Rice_Supply <- 249170000
Total_Peanut_Supply <- 8800000000

Presence$ss__corn <- Presence$p_corn_2012/Total_Corn_Supply
Presence$ss__soy <- Presence$p_soy_2012/Total_Soy_Supply
Presence$ss__wheat <- Presence$p_wheat_2012/Total_Wheat_Supply
Presence$ss__rice <- Presence$p_rice_2012/Total_Rice_Supply
Presence$ss__peanut <- Presence$p_peanuts_2012/Total_Peanut_Supply

vw <-  variables %>%
  unlist() %>%
  str_subset("\\b[ss_]") %>%
  as.tibble() %>%
  rename(name = value) 

vw <- vw %>%
  filter(str_detect(name, "imports") == FALSE)

Presence$ss__corn <- replace_na(Presence$ss__corn,0)
Presence$ss__soy <- replace_na(Presence$ss__soy,0)  
Presence$ss__wheat <- replace_na(Presence$ss__wheat,0)
Presence$ss__rice <- replace_na(Presence$ss__rice,0)
Presence$ss__peanut <- replace_na(Presence$ss__peanut,0)

tmp_Pres <- Presence %>%
  select(GEOID,ss__corn,ss__soy,ss__wheat,ss__rice,ss__peanut)

names <- vw$name
result <- list()

x <- paste0("ss__",str_extract(names,'(corn|soy|wheat|rice|peanut){1}'))
y <- str_extract(names,"[:digit:]{5}")

result <- map2_dbl(x,y,ssLookup,data=tmp_Pres)

vw <- vw %>%
  add_column(result)

vw<-  vw %>% rename(val = result, rowname = name)%>%
  mutate(rowname = substr(rowname,4,19))
rm(Total_Corn_Supply,Total_Soy_Supply,Total_Wheat_Supply,
   Total_Rice_Supply,Total_Peanut_Supply,tmp_Pres,names,x,y,result)
##==========================================================

# shock df-----------
b <- ECL %>%
  unique()%>%
  filter(str_detect(name, "\\bEC_domestic."))%>%
  rbind(ECL %>% 
            filter(str_detect(name, "\\bEC_export.")))%>%
  rbind(EBL %>%
          filter(str_detect(name, "\\bEB_import.")))%>%
  rbind(EBL %>%
          filter(str_detect(name, "\\bEB_[:digit:]{5}.")))%>%
  mutate(rowname = substr(name,4,18))

# rownames------------
rn <- b$rowname
#elasticity df-------------
A1 <- as.tibble(e_domestic) %>%
  mutate(rowname = str_subset(rn,"\\bdomestic."))
A2 <- as.tibble(e_export) %>%
  mutate(rowname = str_subset(rn,"\\bexport."))
A3 <- as.tibble(e_imports) %>%
  mutate(rowname = str_subset(rn,"\\bimport."))
vrn <- v%>%
  column_to_rownames(var = 'name')
rm(e_domestic,e_export,e_imports)
## supply elasticities----------------
A4_corn <- as.tibble(ve %>%
                       filter(str_detect(name, "corn_corn$"))%>% 
                       select(val) %>%
                       as_vector()) %>%
  add_column(soy = ve %>%
               filter(str_detect(name, "soy_corn$"))%>% 
               select(val) %>%
               as_vector())%>%
  add_column(wheat = ve %>%
               filter(str_detect(name, "wheat_corn$"))%>% 
               select(val) %>%
               as_vector())%>%
  add_column(rice = ve %>%
               filter(str_detect(name, "rice_corn$"))%>% 
               select(val) %>%
               as_vector())%>%
  add_column(peanut = ve %>%
               filter(str_detect(name, "peanut_corn$"))%>% 
               select(val) %>%
               as_vector()) %>%
  rename(corn = value)%>%
  mutate(rowname = str_subset(rn,"\\b[:digit:]{5}.corn$"))
  
A4_soy <- as.tibble(ve %>%
                       filter(str_detect(name, "corn_soy$"))%>% 
                       select(val) %>%
                       as_vector()) %>%
  add_column(soy = ve %>%
               filter(str_detect(name, "soy_soy$"))%>% 
               select(val) %>%
               as_vector())%>%
  add_column(wheat = ve %>%
               filter(str_detect(name, "wheat_soy$"))%>% 
               select(val) %>%
               as_vector())%>%
  add_column(rice = ve %>%
               filter(str_detect(name, "rice_soy$"))%>% 
               select(val) %>%
               as_vector())%>%
  add_column(peanut = ve %>%
               filter(str_detect(name, "peanut_soy$"))%>% 
               select(val) %>%
               as_vector()) %>%
  rename(corn = value)%>%
  mutate(rowname = str_subset(rn,"\\b[:digit:]{5}.soy$"))

A4_wheat <- as.tibble(ve %>%
                      filter(str_detect(name, "corn_wheat$"))%>% 
                      select(val) %>%
                      as_vector()) %>%
  add_column(soy = ve %>%
               filter(str_detect(name, "soy_wheat$"))%>% 
               select(val) %>%
               as_vector())%>%
  add_column(wheat = ve %>%
               filter(str_detect(name, "wheat_wheat$"))%>% 
               select(val) %>%
               as_vector())%>%
  add_column(rice = ve %>%
               filter(str_detect(name, "rice_wheat$"))%>% 
               select(val) %>%
               as_vector())%>%
  add_column(peanut = ve %>%
               filter(str_detect(name, "peanut_wheat$"))%>% 
               select(val) %>%
               as_vector()) %>%
  rename(corn = value)%>%
  mutate(rowname = str_subset(rn,"\\b[:digit:]{5}.wheat$"))

A4_rice <- as.tibble(ve %>%
                      filter(str_detect(name, "corn_rice$"))%>% 
                      select(val) %>%
                      as_vector()) %>%
  add_column(soy = ve %>%
               filter(str_detect(name, "soy_rice$"))%>% 
               select(val) %>%
               as_vector())%>%
  add_column(wheat = ve %>%
               filter(str_detect(name, "wheat_rice$"))%>% 
               select(val) %>%
               as_vector())%>%
  add_column(rice = ve %>%
               filter(str_detect(name, "rice_rice$"))%>% 
               select(val) %>%
               as_vector())%>%
  add_column(peanut = ve %>%
               filter(str_detect(name, "peanut_rice$"))%>% 
               select(val) %>%
               as_vector()) %>%
  rename(corn = value)%>%
  mutate(rowname = str_subset(rn,"\\b[:digit:]{5}.rice$"))

A4_peanut <- as.tibble(ve %>%
                      filter(str_detect(name, "corn_peanut$"))%>% 
                      select(val) %>%
                      as_vector()) %>%
  add_column(soy = ve %>%
               filter(str_detect(name, "soy_peanut$"))%>% 
               select(val) %>%
               as_vector())%>%
  add_column(wheat = ve %>%
               filter(str_detect(name, "wheat_peanut$"))%>% 
               select(val) %>%
               as_vector())%>%
  add_column(rice = ve %>%
               filter(str_detect(name, "rice_peanut$"))%>% 
               select(val) %>%
               as_vector())%>%
  add_column(peanut = ve %>%
               filter(str_detect(name, "peanut_peanut$"))%>% 
               select(val) %>%
               as_vector()) %>%
  rename(corn = value)%>%
  mutate(rowname = str_subset(rn,"\\b[:digit:]{5}.peanut$"))
# unweighted A df------------
#A <- bind_rows(list(A1,A2,A3,A4_corn,A4_soy,A4_wheat,A4_rice,A4_peanut))
A_dom_supply <-  bind_rows(list(A4_corn,A4_soy,A4_wheat,A4_rice,A4_peanut))
#rm(A1,A2,A3,A4_corn,A4_soy,A4_wheat,A4_rice,A4_peanut)

## weight A and b with elements from vw, ss_imports, w_domestic, w_export
## put weights in same order as in A and b
## first create weights vector in that order, then add column

  

#domestic demand---------------------------
wt_dom <- tibble(rowname = c("domestic_corn", "domestic_soy", "domestic_wheat", "domestic_rice", "domestic_peanut"),
             val = diag(w_domestic))%>%
  rename(weight = val)

dom <- left_join(wt_dom,A1, by = "rowname") 
dom <- dom[['weight']] * dom[-(1:2)]#multiply elasticities by weight 

#export demand-------------------------------
wt_exp <- tibble(rowname = c("export_corn", "export_soy", "export_wheat", "export_rice", "export_peanut"),
                 val = diag(w_export))%>%
  rename(weight = val)

exp <-  left_join(wt_exp,A2, by = "rowname")
exp <- exp[['weight']] * exp[-(1:2)]#multiply elasticities by weight 

#import supply---------------------------

wt_imp <- tibble(rowname = c("imports_corn", "imports_soy", "imports_wheat", "imports_rice", "imports_peanut"),
              val = diag(ss_imports))%>%
  rename(weight = val)

imp <- left_join(wt_imp,A3, by = "rowname")
imp$weight <- -1 * imp$weight #move import supply to left side of equal sign
imp <- imp[['weight']] * imp[-(1:2)]#multiply elasticities by weight 

#domestic supply -----------------------------------

sup_corn <- left_join(A4_corn,vw, by = "rowname")%>%
  rename(weight = val)
sup_corn$weight <- -1 * sup_corn$weight #move domestic supply to left side of equal sign
sup_corn <- sup_corn[['weight']] * sup_corn[(1:5)]#multiply elasticities by weight 
sup_corn <- summarise_all(sup_corn,sum)

sup_soy <- left_join(A4_soy,vw, by = "rowname")%>%
  rename(weight = val)
sup_soy$weight <- -1 * sup_soy$weight #move domestic supply to left side of equal sign
sup_soy <- sup_soy[['weight']] * sup_soy[(1:5)]#multiply elasticities by weight 
sup_soy <- summarise_all(sup_soy,sum)

sup_wheat <- left_join(A4_wheat,vw, by = "rowname")%>%
  rename(weight = val)
sup_wheat$weight <- -1 * sup_wheat$weight #move domestic supply to left side of equal sign
sup_wheat <- sup_wheat[['weight']] * sup_wheat[(1:5)]#multiply elasticities by weight 
sup_wheat <- summarise_all(sup_wheat,sum)


sup_rice <- left_join(A4_rice,vw, by = "rowname")%>%
  rename(weight = val)
sup_rice$weight <- -1 * sup_rice$weight #move domestic supply to left side of equal sign
sup_rice <- sup_rice[['weight']] * sup_rice[(1:5)]#multiply elasticities by weight 
sup_rice <- summarise_all(sup_rice,sum)


sup_peanut <- left_join(A4_peanut,vw, by = "rowname")%>%
  rename(weight = val)
sup_peanut$weight <- -1 * sup_peanut$weight #move domestic supply to left side of equal sign
sup_peanut <- sup_peanut[['weight']] * sup_peanut[(1:5)]#multiply elasticities by weight 
sup_peanut <- summarise_all(sup_peanut,sum)

#combine into A mat------------------
elast_corn <- dom[1,(3:7)] + exp[1,] + imp[1,] + sup_corn
elast_soy <- dom[2,(3:7)] + exp[2,] + imp[2,] + sup_soy
elast_wheat <- dom[3,(3:7)] + exp[3,] + imp[3,] + sup_wheat
elast_rice <- dom[4,(3:7)] + exp[4,] + imp[4,] + sup_rice
elast_peanut <-  dom[5,(3:7)] + exp[5,] + imp[5,] + sup_rice

A <- as.matrix(
  bind_rows(elast_corn,
            elast_soy,
            elast_wheat,
            elast_rice,
            elast_peanut
  )
)

# create the shock matrix

b_dom <- ECL %>%
  unique()%>%
  filter(str_detect(name, "\\bEC_domestic."))%>%
  mutate(rowname = substr(name,4,18))
neg_wt_dom <- wt_dom 
neg_wt_dom$weight <- -1 * neg_wt_dom$weight
b_dom <- left_join(b_dom,neg_wt_dom, by = 'rowname')
b_dom$wtedshock <- b_dom$val * b_dom$weight

b_exp <- ECL %>% 
  unique()%>%
  filter(str_detect(name, "\\bEC_export."))%>%
  mutate(rowname = substr(name,4,18))
neg_wt_exp <- wt_exp 
neg_wt_exp$weight <- -1 * neg_wt_exp$weight
b_exp <- left_join(b_exp,neg_wt_exp, by = 'rowname')
b_exp$wtedshock <- b_exp$val * b_exp$weight


b_imp <- EBL %>%
  unique()%>%
  filter(str_detect(name, "\\bEB_import."))%>%
  mutate(rowname = substr(name,4,18))
b_imp <- left_join(b_imp,wt_imp)
b_imp$wtedshock <- b_imp$val * b_imp$weight

vws <- vw%>%
  rename(weight = val)
b_sup_corn <- EBL %>%
  filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
  filter(str_detect(name, 'corn\\b'))%>%
  mutate(rowname = substr(name,4,18))
b_sup_corn <- left_join(b_sup_corn,vws, by = 'rowname')
b_sup_corn$wtedshock <- b_sup_corn$val * b_sup_corn$weight

b_sup_soy <- EBL %>%
  filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
  filter(str_detect(name, 'soy\\b'))%>%
  mutate(rowname = substr(name,4,18))
b_sup_soy <- left_join(b_sup_soy,vws, by = 'rowname')
b_sup_soy$wtedshock <- b_sup_soy$val * b_sup_soy$weight

b_sup_wheat <- EBL %>%
  filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
  filter(str_detect(name, 'wheat\\b'))%>%
  mutate(rowname = substr(name,4,18))
b_sup_wheat <- left_join(b_sup_wheat,vws, by = 'rowname')
b_sup_wheat$wtedshock <- b_sup_wheat$val * b_sup_wheat$weight

b_sup_rice <- EBL %>%
  filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
  filter(str_detect(name, 'rice\\b'))%>%
  mutate(rowname = substr(name,4,18))
b_sup_rice <- left_join(b_sup_rice,vws, by = 'rowname')
b_sup_rice$wtedshock <- b_sup_rice$val * b_sup_rice$weight

b_sup_peanut <- EBL %>%
  filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
  filter(str_detect(name, 'peanut\\b'))%>%
  mutate(rowname = substr(name,4,18))
b_sup_peanut <- left_join(b_sup_peanut,vws, by = 'rowname')
b_sup_peanut$wtedshock <- b_sup_peanut$val * b_sup_peanut$weight
#combine into b mat------------------
shock_corn <- b_dom[1,5] + b_exp[1,5] + b_imp[1,5] + sum(b_sup_corn[,5])
shock_soy <- b_dom[2,5] + b_exp[2,5] + b_imp[2,5] + sum(b_sup_soy[,5])
shock_wheat <- b_dom[3,5] + b_exp[3,5] + b_imp[3,5] + sum(b_sup_wheat[,5])
shock_rice <- b_dom[4,5] + b_exp[4,5] + b_imp[4,5] + sum(b_sup_rice[,5])
shock_peanut <-  b_dom[5,5] + b_exp[5,5] + b_imp[5,5] + sum(b_sup_peanut[,5])

shock <- 
  bind_rows(shock_corn,
                   shock_soy,
                   shock_wheat,
                   shock_rice,
                   shock_peanut)%>%
  as.matrix()

#solve for price changes------------------------------
solve(A,shock)

  
  
  




 
  



#move EB to python------------
string <- v_to_parse(name = v$name, value = v$val)
string_list <- list()

rm(s)
EP <- gen_price_chg(crops)
tmp <- paste0("var('",paste0(EP,collapse=","),"')")
EPpy <- sympy(tmp)


tmp <- paste0("var('",paste0(EC,collapse=","),"')")
ECpy <- sympy(tmp)


tmp <- paste0("var('",paste0(EB[1:(.25*length(EB))],collapse=","),"')")
EBpy <- list()
EBpy[1] <- sympy(tmp)
tmp <- paste0("var('",paste0(EB[(.25*length(EB)+1):(.5*length(EB))],collapse=","),"')")
EBpy[2] <- sympy(tmp)
tmp <- paste0("var('",paste0(EB[(.5*length(EB)+1):(.75*length(EB))],collapse=","),"')")
EBpy[3] <- sympy(tmp)
tmp <- paste0("var('",paste0(EB[(.75*length(EB)+1):(length(EB))],collapse=","),"')")
EBpy[4] <- sympy(tmp)


variablespy <- list()
tmp <- paste0("var('",paste0(variables[1:(.05*length(variables))],collapse=","),"')")
variablespy[1] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.05*length(variables)+1):(.1*length(variables))],collapse=","),"')")
variablespy[2] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.1*length(variables)+1):(.15*length(variables))],collapse=","),"')")
variablespy[3] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.15*length(variables)+1):(.2*length(variables))],collapse=","),"')")
variablespy[4] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.2*length(variables)+1):(.25*length(variables))],collapse=","),"')")
variablespy[5] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.25*length(variables)+1):(.3*length(variables))],collapse=","),"')")
variablespy[6] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.3*length(variables)+1):(.35*length(variables))],collapse=","),"')")
variablespy[7] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.35*length(variables)+1):(.4*length(variables))],collapse=","),"')")
variablespy[8] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.4*length(variables)+1):(.45*length(variables))],collapse=","),"')")
variablespy[9] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.45*length(variables)+1):(.5*length(variables))],collapse=","),"')")
variablespy[10] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.5*length(variables)+1):(.55*length(variables))],collapse=","),"')")
variablespy[11] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.55*length(variables)+1):(.6*length(variables))],collapse=","),"')")
variablespy[12] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.6*length(variables)+1):(.65*length(variables))],collapse=","),"')")
variablespy[13] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.65*length(variables)+1):(.7*length(variables))],collapse=","),"')")
variablespy[14] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.7*length(variables)+1):(.75*length(variables))],collapse=","),"')")
variablespy[15] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.75*length(variables)+1):(.8*length(variables))],collapse=","),"')")
variablespy[16] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.8*length(variables)+1):(.85*length(variables))],collapse=","),"')")
variablespy[17] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.85*length(variables)+1):(.9*length(variables))],collapse=","),"')")
variablespy[18] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.9*length(variables)+1):(.95*length(variables))],collapse=","),"')")
variablespy[19] <- sympy(tmp)
tmp <- paste0("var('",paste0(variables[(.95*length(variables)+1):(length(variables))],collapse=","),"')")
variablespy[20] <- sympy(tmp)

# library(reticulate)
# sy <- import("sympy")
# py_run_string('from sympy import *')
# 
# 
# conda_create("r-reticulate")
# repl_python()
# 
# import sympy as sy
# 
# exit
# 
# sympyStart()
# for (i in EP) {
#   tmp <- paste0(noquote(i)," <- Var('",i,"')")
#   eval(parse(text=tmp))
# }
# for (i in EB) {
#   tmp <- paste0(noquote(i)," <- Var('",i,"')")
#   eval(parse(text=tmp))
# }
# for (i in EC) {
#   tmp <- paste0(noquote(i)," <- Var('",i,"')")
#   eval(parse(text=tmp))
# }
# #.jcall("java/lang/System", method = "gc") 
# for (i in variables[1:length(variables)]) {
#   Sys.sleep(0.01)
#   tmp <- paste0(noquote(i)," <- Var('",i,"')")
#   eval(parse(text=tmp))
# }
#.jcall("java/lang/System", method = "gc") 
gc()
#eqns <- gen_eq(variables,crops,markets,suppliers, EC, EB)
# gen_eq-------------------------------------------------------------
tick <- 0
temp <- list()
demand <- list()
dem_eqns <- list()
# create portions of the demand equation
for (m in markets) {
  for (c in crops) {
    tick = tick + 1
    regex_tmp <- paste("e",m,c,sep = "_")
    pe_tmp <- stringr::str_subset(variables,regex_tmp)
    
    regex_tmp <- paste("EC",m,c,sep = "_")
    ec_tmp <- stringr::str_subset(EC,regex_tmp)
    
    eq <- paste(pe_tmp,EP,sep=' * ') %>%
      paste(collapse = " + ")
    temp[tick] <- paste0('(',eq," + ",ec_tmp,')')
    regex_tmp <- paste("w",m,c,sep = "_")
    demand[tick] <- paste(regex_tmp,temp[tick],sep = ' * ')
  }
}
# put portions of demand equations together
tick <- 0
for (c in crops) {
  tick <- tick + 1
  regex_tmp <- paste("w_(export|domestic)",c,sep = "_")
  dem_elements <- stringr::str_subset(demand,regex_tmp)
  dem_eqns[tick] <- paste(dem_elements, collapse = " + ")
}
#create portions of the supply equation
tick <- 0
temp <- list()
supply <- list()
for (s in suppliers) {
  for (c in crops) {
    tick = tick + 1
    regex_tmp <- paste("e",s,c,sep = "_")
    pe_tmp <- stringr::str_subset(variables,regex_tmp)
    
    regex_tmp <- paste("EB",s,c,sep = "_")
    eb_tmp <- stringr::str_subset(EB,regex_tmp)
    
    eq <- paste(pe_tmp,EP,sep=' * ') %>%
      paste(collapse = " + ")
    temp[tick] <- paste0('(',eq," + ",eb_tmp,')')
    regex_tmp <- paste("ss",s,c,sep = "_")
    supply[tick] <- paste(regex_tmp,temp[tick],sep = ' * ')
  }
}
#put portions of supply equations together
tick <- 0
sup_eqns <- list()
Sys.sleep(1)
for (c in crops) {
  tick <- tick + 1
  regex_tmp <- paste("ss_",suppliers,"_",c,sep = "")%>%
    paste(collapse = "|")
  
  sup_elements <- stringr::str_subset(supply,regex_tmp)
  sup_eqns[tick] <- paste(sup_elements, collapse = " + ")
}
#combine into a system of equations
eqns <- pmap_chr(list(dem_eqns,sup_eqns),paste, sep = " , ")
eqns2 <- paste0("Eq(",eqns,")")
eqns3 <- map2_chr(crops,eqns2,paste, sep = " = ")
eqns4 <- paste0("sympy('",eqns3,"')")
#return(eqns4)
eqns <- eqns4
for (i in length(eqns)) {
  eval(parse(text = eqns), envir=.GlobalEnv)
}
# end gen_eq--------------------------
#eqns_to_sympy(eqns)
sympy('eqns = [corn,soy,wheat,rice,peanut]')



#Elasticities and weights must be provided as matrices with specified dimnames.




#```
#With data entered, it is time to generate a tibble to use to send all of the variable values to `SymPy`. The tibble is then used to paste together a string that can be used to pass the values. The `eval_parse` function simply parses and then evaluates a string in the global environment (`eval(parse(text = string), envir=.GlobalEnv)`). 
#```{r pass values to sympy}


for (c in crops) {
  string_list[c] <-   eval_parse_crops(c,t=string)
}

for (c in crops) {
  eval_parse(string_list[c])
}
#```

#Most importantly, the `solve` command can be used to find the changes in prices implied by the exogenous shocks.
#```{r solve for price change}
sympy('solve(eqns, [EP_corn, EP_soy, EP_wheat, EP_rice, EP_peanut])')
#```





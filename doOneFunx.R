#load packages and data
#options(expressions = 5e5)
library(h2o)
library(tidyverse)
library(readxl)
#library(rSymPy)
library(maps)
library(reticulate)
library(choroplethr)
library(choroplethrMaps)
library(triangle)
h2o.no_progress()
#setwd("~/pigSpreadCost")
setwd("~/Documents/Github/pigSpreadCost")
source('edm_fx.R')
#data(state.fips)
# load elasticities
crops <- c("corn","soy","wheat",'rice','peanut')
source('elast.R')

# Snow et al data import
SNOW <- read_csv("SNOW.csv") 
# Recode expansion variable
SNOW$EXPAND <- recode(SNOW$EXPAND, '1' = 'present', '0' = 'present')
SNOW$EXPAND <- replace_na(SNOW$EXPAND, "unknown")
SNOW$GEOID <- str_pad(SNOW$GEOID, 5, side = "left", pad = "0")
# split Snow by time period
snow_1 <- SNOW %>% filter(TIME == 1)
snow_2 <- SNOW %>% filter(TIME == 2)
snow_3  <- SNOW %>% filter(TIME == 3)
snow_4  <- SNOW %>% filter(TIME == 4)
# import wild pig presence
PigsIn2012 <- unique(read_excel("PigsIn2012.xlsx"))
PigsIn2004 <- unique(read_excel("PigsIn2004.xlsx"))
PigsIn1988<- unique(read_excel("PigsIn1988.xlsx"))
PigsIn1982 <- unique(read_excel("PigsIn1982.xlsx"))
# import county distance data
sf12010countydistancemiles_csv <- read_csv("sf12010countydistance500miles.csv.zip")
# join distances to presence
countyPigs <- left_join(by = c('county2' = 'FIPS') , sf12010countydistancemiles_csv, PigsIn2012)
countyPigs <- left_join(by = c('county2' = 'FIPS') , countyPigs, PigsIn2004)
countyPigs <- left_join(by = c('county2' = 'FIPS') , countyPigs, PigsIn1988)
countyPigs <- left_join(by = c('county2' = 'FIPS') , countyPigs, PigsIn1982)
# recode fips codes update to match snow et al
countyPigs <- subset(countyPigs,county1 != '51515' | county2 != '51515'| county1 != '46113' | county2 != '46113')
#countyPigs <- subset(countyPigs,)
# countyPigs$county1 <- ifelse(countyPigs$county1 == '51515',c('51019'), c(countyPigs$county1))
# countyPigs$county2 <- ifelse(countyPigs$county2 == '51515',c('51019'), c(countyPigs$county2))
# countyPigs$county1 <- ifelse(countyPigs$county1 == '46113',c('46102'), c(countyPigs$county1))
# countyPigs$county2 <- ifelse(countyPigs$county2 == '46113',c('46102'), c(countyPigs$county2))

by_cty <- countyPigs %>% group_by(county1)%>% 
    filter(Id_1982 == 1) %>%
    filter(mi_to_county == min(mi_to_county))

snow_1 <- left_join(by = c('GEOID' = 'county1') , snow_1, by_cty[,c("county1","mi_to_county","county2")])
snow_1$mi_to_county <- replace_na(snow_1$mi_to_county, 500)

by_cty <- countyPigs %>% group_by(county1)%>% 
    filter(Id_1988 == 1) %>%
    filter(mi_to_county == min(mi_to_county))

snow_2 <- left_join(by = c('GEOID' = 'county1') , snow_2, by_cty[,c("county1","mi_to_county","county2")])
snow_2$mi_to_county <- replace_na(snow_2$mi_to_county, 500)


by_cty <- countyPigs %>% group_by(county1)%>% 
    filter(Id_2004 == 1) %>%
    filter(mi_to_county == min(mi_to_county))

snow_3 <- left_join(by = c('GEOID' = 'county1') , snow_3, by_cty[,c("county1","mi_to_county","county2")])
snow_3$mi_to_county <- replace_na(snow_3$mi_to_county, 500)


by_cty <- countyPigs %>% group_by(county1)%>% 
    filter(Id_2012 == 1) %>%
    filter(mi_to_county == min(mi_to_county))

snow_4 <- left_join(by = c('GEOID' = 'county1') , snow_4, by_cty[,c("county1","mi_to_county","county2")])
snow_4$mi_to_county <- replace_na(snow_4$mi_to_county, 500)

SNOW <- bind_rows(snow_1,snow_2,snow_3,snow_4)
rm(snow_1,snow_2,snow_3,snow_4)

SNOW <- left_join(by = c('GEOID' = 'FIPS') , SNOW, PigsIn1982)
SNOW <- left_join(by = c('GEOID' = 'FIPS') , SNOW, PigsIn1988)
SNOW <- left_join(by = c('GEOID' = 'FIPS') , SNOW, PigsIn2004)
SNOW <- left_join(by = c('GEOID' = 'FIPS') , SNOW, PigsIn2012)

pr_corn_2012 <- read_csv("p_corn_2012.csv", na = "(D)") %>%
    select(`State ANSI`, `County ANSI`, `Value`) %>%
    transmute(FIPS = paste0(`State ANSI`, `County ANSI`),Value) %>%
    rename(p_corn_2012 = Value)

pr_soy_2012 <- read_csv("p_soybeans_2012.csv", na = "(D)") %>%
    select(`State ANSI`, `County ANSI`, `Value`) %>%
    transmute(FIPS = paste0(`State ANSI`, `County ANSI`),Value)%>%
    rename(p_soy_2012 = Value)

pr_wheat_2012 <- read_csv("p_wheat_2012.csv", na = "(D)")%>%
    select(`State ANSI`, `County ANSI`, `Value`) %>%
    transmute(FIPS = paste0(`State ANSI`, `County ANSI`),Value)%>%
    rename(p_wheat_2012 = Value)

pr_rice_2012 <- read_csv("p_rice_2012.csv", na = "(D)")%>%
    select(`State ANSI`, `County ANSI`, `Value`) %>%
    transmute(FIPS = paste0(`State ANSI`, `County ANSI`),Value)%>%
    rename(p_rice_2012 = Value)

pr_peanuts_2012 <- read_csv("p_peanuts_2012.csv", na = "(D)")%>%
    select(`State ANSI`, `County ANSI`, `Value`) %>%
    transmute(FIPS = paste0(`State ANSI`, `County ANSI`),Value)%>%
    rename(p_peanuts_2012 = Value)

doOne <- function(SNOW,
                  pr_corn_2012,
                  pr_soy_2012,
                  pr_wheat_2012,
                  pr_rice_2012,
                  pr_peanuts_2012,
                  e_domestic,
                  e_Corn_Belt,
                  e_Central_Plains,
                  e_Delta_States,
                  e_Far_West,
                  e_Lake_States,
                  e_Northeast,
                  e_Northern_Plains,
                  e_Southeast,
                  e_Southern_Plains,
                  e_export,
                  e_imports,
                  w_domestic,
                  w_export,
                  ss_imports
                  ) {
    



train_tbl <- SNOW %>% filter(TIME == 1 | TIME == 2| TIME == 3)
#valid_tbl <- SNOW %>% filter(TIME == 2)
test_tbl  <- SNOW %>% filter(TIME == 4)

h2o.init(nthreads = -1) # Fire up h2o
factorsList <- c("GEOID", "ECO_DIVISN", "TIME", "EXPAND", 'county2', 
                 "Id_2012", "Id_2004", "Id_1988", "Id_1982")
# Convert to H2OFrame objects
train_h2o <- as.h2o(train_tbl)
train_h2o[,factorsList] <- as.factor(train_h2o[,factorsList])

#valid_h2o <- as.h2o(valid_tbl)
#valid_h2o[,factorsList] <- as.factor(valid_h2o[,factorsList])

test_h2o  <- as.h2o(test_tbl)
test_h2o[,factorsList] <- as.factor(test_h2o[,factorsList])

y <- "EXPAND"

x <-  c("AREA_KM", "ECO_DIVISN", "mammal.richness",  
        "P_Ag", "P_Dev", "P_For", "P_Oth", "P_Ran", "P_Wat", "P_Wet",
        "A_MN_Ag", "A_MN_Dev", "A_MN_For", "A_MN_Oth", "A_MN_Ran", "A_MN_Wat", "A_MN_Wet",
        "A_CV_Ag", "A_CV_Dev", "A_CV_For", "A_CV_Oth", "A_CV_Ran", "A_CV_Wat", "A_CV_Wet",
        "PD", "AREA_AM", "AREA_CV", "CWED", "CONTAG", "IJI", "DIVISION", "SIDI",
        "AI", 
        "Annual_Precip", "Summer_Precip", "Winter_Precip", "maxPrecip", "minPrecip",
        "STREAM_KM", "ROAD_KM", "HumanPop", 
        "precipsum", "precipwin", "popdes.slope", "mi_to_county")

#"Annual_Temp", "Summer_Temp", "Winter_Temp", "maxTemp", "minTemp","tmaxmean", "tmaxmax", "tminmean", "tminmin",


m <- h2o.randomForest(x,y,train_h2o, nfolds = 50, model_id = "RF_defaults",keep_cross_validation_predictions = FALSE)
#summary(m)
m
mt <- h2o.performance(m, test_h2o)
mt

# incorporate predictions to snow data--------------
SIM <- as.data.frame(h2o.predict(m,test_h2o))
SIM <- bind_cols(filter(SNOW, TIME == 4),SIM)

#generate random numbers to compare against prediction
SIM <- SIM %>%
    add_column(random_2020 = runif(nrow(SIM),min = 0, max = 1))%>%
    add_column(random_2028 = runif(nrow(SIM),min = 0, max = 1))%>%
    add_column(random_2036 = runif(nrow(SIM),min = 0, max = 1))


#Time Step 1: 2020
SIM$Id_2020 <- with(SIM, ifelse(is.na(Id_2012),
                                ifelse(random_2020 < present, 1, NA),
                                1))

#update machine learning dependent var
SIM$EXPAND <- with(SIM, ifelse(is.na(Id_2020),
                               "unknown",
                               "present"))
#update distance to nearest
countyPigs <- left_join(by = c('county2' = 'GEOID') , countyPigs, SIM[,c('GEOID','Id_2020')])# join distances to presence
by_cty <- countyPigs %>% group_by(county1)%>% 
    filter(Id_2020 == 1) %>%
    filter(mi_to_county == min(mi_to_county))

SIM <- left_join(by = c('GEOID' = 'county1') , SIM, by_cty[,c('county1',"mi_to_county",
                                                              "county2")])
SIM$county2.x <- SIM$mi_to_county.x <- NULL
SIM <- rename(SIM,mi_to_county = mi_to_county.y)
SIM <- rename(SIM,county2 = county2.y)
SIM$mi_to_county <- replace_na(SIM$mi_to_county, 500)

#Time Step 2: 2028
SIMh <- as.h2o(SIM)
REPEAT <- as.data.frame(h2o.predict(m,SIMh))
SIM <- bind_cols(SIM, REPEAT)

SIM$Id_2028 <- with(SIM, ifelse(is.na(Id_2020),
                                ifelse(random_2028 < present1, 1, NA),
                                1))
#update machine learning dependent var
SIM$EXPAND <- with(SIM, ifelse(is.na(Id_2028),
                               "unknown",
                               "present"))
#update distance to nearest
countyPigs <- left_join(by = c('county2' = 'GEOID') , countyPigs, SIM[,c('GEOID','Id_2028')])# join distances to presence
by_cty <- countyPigs %>% group_by(county1)%>% 
    filter(Id_2028 == 1) %>%
    filter(mi_to_county == min(mi_to_county))
SIM <- left_join(by = c('GEOID' = 'county1') , SIM, by_cty[,c('county1',"mi_to_county",
                                                              "county2")])
SIM$county2.x <- SIM$mi_to_county.x <- NULL
SIM <- rename(SIM,mi_to_county = mi_to_county.y)
SIM <- rename(SIM,county2 = county2.y)
SIM$mi_to_county <- replace_na(SIM$mi_to_county, 500)




#Time Step 3: 2036
SIMh <- as.h2o(SIM)
REPEAT <- as.data.frame(h2o.predict(m,SIMh))
SIM <- bind_cols(SIM, REPEAT)

SIM$Id_2036 <- with(SIM, ifelse(is.na(Id_2028),
                                ifelse(random_2036 < present2, 1, NA),
                                1))
#update machine learning dependent var
SIM$EXPAND <- with(SIM, ifelse(is.na(Id_2036),
                               "unknown",
                               "present"))
#update distance to nearest
countyPigs <- left_join(by = c('county2' = 'GEOID') , countyPigs, SIM[,c('GEOID','Id_2036')])# join distances to presence
by_cty <- countyPigs %>% group_by(county1)%>% 
    filter(Id_2036 == 1) %>%
    filter(mi_to_county == min(mi_to_county))
SIM <- left_join(by = c('GEOID' = 'county1') , SIM, by_cty[,c('county1',"mi_to_county",
                                                              "county2")])
SIM$county2.x <- SIM$mi_to_county.x <- NULL
SIM <- rename(SIM,mi_to_county = mi_to_county.y)
SIM <- rename(SIM,county2 = county2.y)
SIM$mi_to_county <- replace_na(SIM$mi_to_county, 500)


#Time Step 4: 2042
# SIMh <- as.h2o(SIM)
# REPEAT <- as.data.frame(h2o.predict(m,SIMh))
# SIM <- bind_cols(SIM, REPEAT)
# SIM$random <- runif(1,min = 0, max = 1)
# SIM$Id_2042 <- with(SIM, ifelse(is.na(Id_2036),
#                                 ifelse(random < present3, 1, NA),
#                                 1))
# #update machine learning dependent var
# SIM$EXPAND <- with(SIM, ifelse(is.na(Id_2042),
#                                "unknown",
#                                "present"))
# #update distance to nearest
# countyPigs <- left_join(by = c('county2' = 'GEOID') , countyPigs, SIM[,c('GEOID','Id_2042')])# join distances to presence
# by_cty <- countyPigs %>% group_by(county1)%>% 
#   filter(Id_2042 == 1) %>%
#   filter(mi_to_county == min(mi_to_county))
# SIM <- left_join(by = c('GEOID' = 'county1') , SIM, by_cty[,c('county1',"mi_to_county",
#                                                               "county2")])
# SIM$county2.x <- SIM$mi_to_county.x <- NULL
# SIM <- rename(SIM,mi_to_county = mi_to_county.y)
# SIM <- rename(SIM,county2 = county2.y)
# SIM$mi_to_county <- replace_na(SIM$mi_to_county, 500)


# Production in 2012 added to simulation data.set-----------------------

Presence <- SIM[,c("GEOID","Id_2020","Id_2028","Id_2036")]



SIM <- left_join(by = c('GEOID'='FIPS'),SIM,pr_corn_2012)
SIM <- left_join(by = c('GEOID'='FIPS'),SIM,pr_soy_2012)  
SIM <- left_join(by = c('GEOID'='FIPS'),SIM,pr_wheat_2012)
SIM <- left_join(by = c('GEOID'='FIPS'),SIM,pr_rice_2012)
SIM <- left_join(by = c('GEOID'='FIPS'),SIM,pr_peanuts_2012)
#rm(pr_corn_2012,pr_soy_2012,pr_wheat_2012,pr_rice_2012,pr_peanuts_2012)

#replace NA with 0
SIM$Id_2012na <- replace_na(SIM$Id_2012, 0)
SIM$Id_2020 <- replace_na(SIM$Id_2020, 0)
SIM$New_Id_2020 <- SIM$Id_2020 - SIM$Id_2012na

SIM$Id_2028 <- replace_na(SIM$Id_2028, 0)
SIM$New_Id_2028 <- SIM$Id_2028 - SIM$Id_2020

SIM$Id_2036 <- replace_na(SIM$Id_2036, 0)
SIM$New_Id_2036 <- SIM$Id_2036 - SIM$Id_2028
# add exog shock-----
AbstractballparkS4 <- read_csv("AbstractballparkS4.txt")

#2020
SIM$draw_corn_2020 <- rtriangle(nrow(SIM),
                                a=min(AbstractballparkS4$Corn), 
                                b=max(AbstractballparkS4$Corn),                                   c=(min(AbstractballparkS4$Corn)+max(AbstractballparkS4$Corn))/2)

SIM$EB_corn_2020 <- -1 * SIM$draw_corn_2020 * SIM$New_Id_2020

SIM$draw_soy_2020 <- rtriangle(nrow(SIM),
                               a=min(AbstractballparkS4$Soybeans), 
                               b=max(AbstractballparkS4$Soybeans),                      c=(min(AbstractballparkS4$Soybeans)+max(AbstractballparkS4$Soybeans))/2)
SIM$EB_soy_2020 <- -1 * SIM$draw_corn_2020 * SIM$New_Id_2020

SIM$draw_wheat_2020 <- rtriangle(nrow(SIM),
                                 a=min(AbstractballparkS4$Wheat), 
                                 b=max(AbstractballparkS4$Wheat),                      c=(min(AbstractballparkS4$Wheat)+max(AbstractballparkS4$Wheat))/2)
SIM$EB_wheat_2020 <- -1 * SIM$draw_wheat_2020 * SIM$New_Id_2020

SIM$draw_rice_2020 <- rtriangle(nrow(SIM),
                                a=min(AbstractballparkS4$Rice), 
                                b=max(AbstractballparkS4$Rice),                      c=(min(AbstractballparkS4$Rice)+max(AbstractballparkS4$Rice))/2)
SIM$EB_rice_2020 <- -1 * SIM$draw_rice_2020 * SIM$New_Id_2020

SIM$draw_peanuts_2020 <- rtriangle(nrow(SIM),
                                   a=min(AbstractballparkS4$Peanuts), 
                                   b=max(AbstractballparkS4$Peanuts),                      c=(min(AbstractballparkS4$Peanuts)+max(AbstractballparkS4$Peanuts))/2)
SIM$EB_peanuts_2020 <- -1 * SIM$draw_peanuts_2020 * SIM$New_Id_2020
#2028
SIM$draw_corn_2028 <- rtriangle(nrow(SIM),
                                a=min(AbstractballparkS4$Corn), 
                                b=max(AbstractballparkS4$Corn),                                   c=(min(AbstractballparkS4$Corn)+max(AbstractballparkS4$Corn))/2)
SIM$EB_corn_2028 <- -1 * SIM$draw_corn_2028 * SIM$New_Id_2028

SIM$draw_soy_2028 <- rtriangle(nrow(SIM),
                               a=min(AbstractballparkS4$Soybeans), 
                               b=max(AbstractballparkS4$Soybeans),                      c=(min(AbstractballparkS4$Soybeans)+max(AbstractballparkS4$Soybeans))/2)
SIM$EB_soy_2028 <- -1 * SIM$draw_corn_2028 * SIM$New_Id_2028

SIM$draw_wheat_2028 <- rtriangle(nrow(SIM),
                                 a=min(AbstractballparkS4$Wheat), 
                                 b=max(AbstractballparkS4$Wheat),                      c=(min(AbstractballparkS4$Wheat)+max(AbstractballparkS4$Wheat))/2)
SIM$EB_wheat_2028 <- -1 * SIM$draw_wheat_2028 * SIM$New_Id_2028

SIM$draw_rice_2028 <- rtriangle(nrow(SIM),
                                a=min(AbstractballparkS4$Rice), 
                                b=max(AbstractballparkS4$Rice),                      c=(min(AbstractballparkS4$Rice)+max(AbstractballparkS4$Rice))/2)
SIM$EB_rice_2028 <- -1 * SIM$draw_rice_2028 * SIM$New_Id_2028

SIM$draw_peanuts_2028 <- rtriangle(nrow(SIM),
                                   a=min(AbstractballparkS4$Peanuts), 
                                   b=max(AbstractballparkS4$Peanuts),                      c=(min(AbstractballparkS4$Peanuts)+max(AbstractballparkS4$Peanuts))/2)
SIM$EB_peanuts_2028 <- -1 * SIM$draw_peanuts_2028 * SIM$New_Id_2028

#2036
SIM$draw_corn_2036 <- rtriangle(nrow(SIM),
                                a=min(AbstractballparkS4$Corn), 
                                b=max(AbstractballparkS4$Corn),                                   c=(min(AbstractballparkS4$Corn)+max(AbstractballparkS4$Corn))/2)
SIM$EB_corn_2036 <- -1 * SIM$draw_corn_2036 * SIM$New_Id_2036

SIM$draw_soy_2036 <- rtriangle(nrow(SIM),
                               a=min(AbstractballparkS4$Soybeans), 
                               b=max(AbstractballparkS4$Soybeans),                      c=(min(AbstractballparkS4$Soybeans)+max(AbstractballparkS4$Soybeans))/2)
SIM$EB_soy_2036 <- -1 * SIM$draw_corn_2036 * SIM$New_Id_2036

SIM$draw_wheat_2036 <- rtriangle(nrow(SIM),
                                 a=min(AbstractballparkS4$Wheat), 
                                 b=max(AbstractballparkS4$Wheat),                      c=(min(AbstractballparkS4$Wheat)+max(AbstractballparkS4$Wheat))/2)
SIM$EB_wheat_2036 <- -1 * SIM$draw_wheat_2036 * SIM$New_Id_2036

SIM$draw_rice_2036 <- rtriangle(nrow(SIM),
                                a=min(AbstractballparkS4$Rice), 
                                b=max(AbstractballparkS4$Rice),                      c=(min(AbstractballparkS4$Rice)+max(AbstractballparkS4$Rice))/2)
SIM$EB_rice_2036 <- -1 * SIM$draw_rice_2036 * SIM$New_Id_2036

SIM$draw_peanuts_2036 <- rtriangle(nrow(SIM),
                                   a=min(AbstractballparkS4$Peanuts), 
                                   b=max(AbstractballparkS4$Peanuts),                      c=(min(AbstractballparkS4$Peanuts)+max(AbstractballparkS4$Peanuts))/2)
SIM$EB_peanuts_2036 <- -1 * SIM$draw_peanuts_2036 * SIM$New_Id_2036

SIM <- select(SIM, -starts_with('draw_'))
#+++++
#rm(by_cty,countyPigs,PigsIn1982,PigsIn1988,PigsIn2004,PigsIn2012,REPEAT,sf12010countydistancemiles_csv,SNOW)
h2o.shutdown(prompt = FALSE)
gc()

# data(continental_us_states)
# 
# map_2020 <- SIM %>%
#   select(GEOID,Id_2020)%>%
#   rename(region = GEOID, value = Id_2020)
# 
# map_2020$region <- as.numeric(map_2020$region)
# map_2020$value <- as.factor(map_2020$value)
# tabulate(map_2020$value)
# county_choropleth(map_2020,
#                   title = "2020 Predicted Wild Pig Presence",
#                   legend = "Presence",
#                   state_zoom = continental_us_states)
# map_2028 <- SIM %>%
#   select(GEOID,Id_2028)%>%
#   rename(region = GEOID, value = Id_2028)
# map_2028$region <- as.numeric(map_2028$region)
# map_2028$value <- as.factor(map_2028$value)
# 
# tabulate(map_2028$value)
# county_choropleth(map_2028,
#                   title = "2028 Predicted Wild Pig Presence",
#                   legend = "Presence",
#                   state_zoom = continental_us_states)
# 
# map_2036 <- SIM %>%
#   select(GEOID,Id_2036)%>%
#   rename(region = GEOID, value = Id_2036)
# map_2036$region <- as.numeric(map_2036$region)
# map_2036$value <- as.factor(map_2036$value)
# 
# tabulate(map_2036$value)
# county_choropleth(map_2036,
#                   title = "2036 Predicted Wild Pig Presence",
#                   legend = "Presence",
#                   state_zoom = continental_us_states)

# build dfs for price change calculations--------------
markets <- c("domestic","export")
s <- SIM[,'GEOID']
suppliers <- unique(c("imports",unlist(s)))
variables <- gen_var(crops,markets,suppliers)
EB <- gen_shock(crops, place=suppliers, side = "B")
EC <- gen_shock(crops, place=markets, side = "C")

# build elasticity of demand df------------
A1 <- as.tibble(e_domestic) %>%
    mutate(rowname = c("domestic_corn", "domestic_soy", "domestic_wheat", "domestic_rice", "domestic_peanut"))
wt_dom <- tibble(rowname = c("domestic_corn", "domestic_soy", "domestic_wheat", "domestic_rice", "domestic_peanut"),
                 val = diag(w_domestic))%>%
    rename(weight = val)
dom <- left_join(wt_dom,A1, by = "rowname") 
dom <- dom[['weight']] * dom[-(1:2)]#multiply elasticities by weight 
dom

# build elasticity of imports df--------------
A3 <- as.tibble(e_imports) %>%
    mutate(rowname = c("imports_corn", "imports_soy", "imports_wheat", "imports_rice", "imports_peanut"))
wt_imp <- tibble(rowname = c("imports_corn", "imports_soy", "imports_wheat", "imports_rice", "imports_peanut"),
                 val = diag(ss_imports))%>%
    rename(weight = val)
imp <- left_join(wt_imp,A3, by = "rowname")
imp$weight <- -1 * imp$weight #move import supply to left side of equal sign
imp <- imp[['weight']] * imp[-(1:2)]#multiply elasticities by weight 
imp

# build elasticity of exports df --------
A2 <- as.tibble(e_export) %>%
    mutate(rowname = c("export_corn", "export_soy", "export_wheat", "export_rice", "export_peanut"))

wt_exp <- tibble(rowname = c("export_corn", "export_soy", "export_wheat", "export_rice", "export_peanut"),
                 val = diag(w_export))%>%
    rename(weight = val)
exp <-  left_join(wt_exp,A2, by = "rowname")
exp <- exp[['weight']] * exp[-(1:2)]#multiply elasticities by weight 
exp

# build elasticity of domestic suppliers df ------------
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



EBL <- tibble(EB) %>%
    rename(name = EB)%>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    mutate(rowname = substr(name,4,18))

# rownames------------
rn <- EBL$rowname

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
# unweighted supply A df------------
A_dom_supply <-  bind_rows(list(A4_corn,A4_soy,A4_wheat,A4_rice,A4_peanut))
A_dom_supply
#supply weights------------
Total_Corn_Supply <- 16942000000
Total_Soy_Supply <-  4719000000 
Total_Wheat_Supply <- 3079000000
Total_Rice_Supply <- 249170000
Total_Peanut_Supply <- 8800000000

SIM$ss__corn <- SIM$p_corn_2012/Total_Corn_Supply
SIM$ss__soy <- SIM$p_soy_2012/Total_Soy_Supply
SIM$ss__wheat <- SIM$p_wheat_2012/Total_Wheat_Supply
SIM$ss__rice <- SIM$p_rice_2012/Total_Rice_Supply
SIM$ss__peanut <- SIM$p_peanuts_2012/Total_Peanut_Supply

SIM$ss__corn <- replace_na(SIM$ss__corn,0)
SIM$ss__soy <- replace_na(SIM$ss__soy,0)  
SIM$ss__wheat <- replace_na(SIM$ss__wheat,0)
SIM$ss__rice <- replace_na(SIM$ss__rice,0)
SIM$ss__peanut <- replace_na(SIM$ss__peanut,0)

tmp_Pres <- SIM %>%
    select(GEOID,ss__corn,ss__soy,ss__wheat,ss__rice,ss__peanut)

vw <-  variables %>%
    unlist() %>%
    str_subset("\\b[ss_]") %>%
    as.tibble() %>%
    rename(name = value)%>%
    filter(str_detect(name, "imports") == FALSE)

names <- vw$name
result <- list()

x <- paste0("ss__",str_extract(names,'(corn|soy|wheat|rice|peanut){1}'))
y <- str_extract(names,"[:digit:]{5}")

result <- map2_dbl(x,y,ssLookup,data=tmp_Pres)

vw <- vw %>%
    add_column(val = result)%>% 
    rename(rowname = name)%>%
    mutate(rowname = substr(rowname,4,19))

rm(Total_Corn_Supply,Total_Soy_Supply,Total_Wheat_Supply,
   Total_Rice_Supply,Total_Peanut_Supply,tmp_Pres,names,x,y,result)

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
A_s_corn <- dom[1,] + exp[1,] + imp[1,] + sup_corn 
A_s_soy <- dom[2,] + exp[2,] + imp[2,] + sup_soy
A_s_wheat  <- dom[3,] + exp[3,] + imp[3,] + sup_wheat 
A_s_rice <- dom[4,] + exp[4,] + imp[4,] + sup_rice
A_s_peanut <- dom[5,] + exp[5,] + imp[5,] + sup_peanut

A <- bind_rows(A_s_corn,
               A_s_soy,
               A_s_wheat,
               A_s_rice,
               A_s_peanut
)%>%
    as.matrix()
A

# build exogenous shock matrix b
# all shocks are domestic supply shocks
# because we are going with a 5x5 * 5x1 we do not need to
# evaluate other shocks as zeros to hold
# places

# production shocks -----------
#The shocks are on the production side of the equilibrium conditions. 
vws <- vw%>%
    rename(weight = val)
# 2020==================================================
EBL_2020 <- tibble(EB) %>%
    rename(name = EB)%>%
    add_column(val = c(0,SIM[,'EB_corn_2020', drop = TRUE],
                       0,SIM[,'EB_soy_2020', drop = TRUE],
                       0,SIM[,'EB_wheat_2020', drop = TRUE],
                       0,SIM[,'EB_rice_2020', drop = TRUE],
                       0,SIM[,'EB_peanuts_2020', drop = TRUE]))



b_sup_corn <- EBL_2020 %>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    filter(str_detect(name, 'corn\\b'))%>%
    mutate(rowname = substr(name,4,18))
b_sup_corn <- left_join(b_sup_corn,vws, by = 'rowname')
b_sup_corn$wtedshock <- b_sup_corn$val * b_sup_corn$weight

b_sup_soy <- EBL_2020 %>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    filter(str_detect(name, 'soy\\b'))%>%
    mutate(rowname = substr(name,4,18))
b_sup_soy <- left_join(b_sup_soy,vws, by = 'rowname')
b_sup_soy$wtedshock <- b_sup_soy$val * b_sup_soy$weight

b_sup_wheat <- EBL_2020 %>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    filter(str_detect(name, 'wheat\\b'))%>%
    mutate(rowname = substr(name,4,18))
b_sup_wheat <- left_join(b_sup_wheat,vws, by = 'rowname')
b_sup_wheat$wtedshock <- b_sup_wheat$val * b_sup_wheat$weight

b_sup_rice <- EBL_2020 %>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    filter(str_detect(name, 'rice\\b'))%>%
    mutate(rowname = substr(name,4,18))
b_sup_rice <- left_join(b_sup_rice,vws, by = 'rowname')
b_sup_rice$wtedshock <- b_sup_rice$val * b_sup_rice$weight

b_sup_peanut <- EBL_2020 %>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    filter(str_detect(name, 'peanut\\b'))%>%
    mutate(rowname = substr(name,4,18))
b_sup_peanut <- left_join(b_sup_peanut,vws, by = 'rowname')
b_sup_peanut$wtedshock <- b_sup_peanut$val * b_sup_peanut$weight
#combine into b mat------------------
shock_2020 <- 
    matrix(c(corn = sum(b_sup_corn[,5]),
             soy = sum(b_sup_soy[,5]),
             wheat = sum(b_sup_wheat[,5]),
             rice = sum(b_sup_rice[,5]),
             peanuts = sum(b_sup_peanut[,5])),nrow=5)
#2028========================================================
EBL_2028 <- tibble(EB) %>%
    rename(name = EB)%>%
    add_column(val = c(0,SIM[,'EB_corn_2028', drop = TRUE],
                       0,SIM[,'EB_soy_2028', drop = TRUE],
                       0,SIM[,'EB_wheat_2028', drop = TRUE],
                       0,SIM[,'EB_rice_2028', drop = TRUE],
                       0,SIM[,'EB_peanuts_2028', drop = TRUE]))
b_sup_corn <- EBL_2028 %>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    filter(str_detect(name, 'corn\\b'))%>%
    mutate(rowname = substr(name,4,18))
b_sup_corn <- left_join(b_sup_corn,vws, by = 'rowname')
b_sup_corn$wtedshock <- b_sup_corn$val * b_sup_corn$weight

b_sup_soy <- EBL_2028 %>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    filter(str_detect(name, 'soy\\b'))%>%
    mutate(rowname = substr(name,4,18))
b_sup_soy <- left_join(b_sup_soy,vws, by = 'rowname')
b_sup_soy$wtedshock <- b_sup_soy$val * b_sup_soy$weight

b_sup_wheat <- EBL_2028 %>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    filter(str_detect(name, 'wheat\\b'))%>%
    mutate(rowname = substr(name,4,18))
b_sup_wheat <- left_join(b_sup_wheat,vws, by = 'rowname')
b_sup_wheat$wtedshock <- b_sup_wheat$val * b_sup_wheat$weight

b_sup_rice <- EBL_2028 %>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    filter(str_detect(name, 'rice\\b'))%>%
    mutate(rowname = substr(name,4,18))
b_sup_rice <- left_join(b_sup_rice,vws, by = 'rowname')
b_sup_rice$wtedshock <- b_sup_rice$val * b_sup_rice$weight

b_sup_peanut <- EBL_2028 %>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    filter(str_detect(name, 'peanut\\b'))%>%
    mutate(rowname = substr(name,4,18))
b_sup_peanut <- left_join(b_sup_peanut,vws, by = 'rowname')
b_sup_peanut$wtedshock <- b_sup_peanut$val * b_sup_peanut$weight
#combine into b mat------------------
shock_2028 <- 
    matrix(c(corn = sum(b_sup_corn[,5]),
             soy = sum(b_sup_soy[,5]),
             wheat = sum(b_sup_wheat[,5]),
             rice = sum(b_sup_rice[,5]),
             peanuts = sum(b_sup_peanut[,5])),nrow=5)
#2036=======================================================
EBL_2036 <- tibble(EB) %>%
    rename(name = EB)%>%
    add_column(val = c(0,SIM[,'EB_corn_2036', drop = TRUE],
                       0,SIM[,'EB_soy_2036', drop = TRUE],
                       0,SIM[,'EB_wheat_2036', drop = TRUE],
                       0,SIM[,'EB_rice_2036', drop = TRUE],
                       0,SIM[,'EB_peanuts_2036', drop = TRUE]))
b_sup_corn <- EBL_2036 %>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    filter(str_detect(name, 'corn\\b'))%>%
    mutate(rowname = substr(name,4,18))
b_sup_corn <- left_join(b_sup_corn,vws, by = 'rowname')
b_sup_corn$wtedshock <- b_sup_corn$val * b_sup_corn$weight

b_sup_soy <- EBL_2036 %>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    filter(str_detect(name, 'soy\\b'))%>%
    mutate(rowname = substr(name,4,18))
b_sup_soy <- left_join(b_sup_soy,vws, by = 'rowname')
b_sup_soy$wtedshock <- b_sup_soy$val * b_sup_soy$weight

b_sup_wheat <- EBL_2036 %>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    filter(str_detect(name, 'wheat\\b'))%>%
    mutate(rowname = substr(name,4,18))
b_sup_wheat <- left_join(b_sup_wheat,vws, by = 'rowname')
b_sup_wheat$wtedshock <- b_sup_wheat$val * b_sup_wheat$weight

b_sup_rice <- EBL_2036 %>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    filter(str_detect(name, 'rice\\b'))%>%
    mutate(rowname = substr(name,4,18))
b_sup_rice <- left_join(b_sup_rice,vws, by = 'rowname')
b_sup_rice$wtedshock <- b_sup_rice$val * b_sup_rice$weight

b_sup_peanut <- EBL_2036 %>%
    filter(str_detect(name, "\\bEB_[:digit:]{5}."))%>%
    filter(str_detect(name, 'peanut\\b'))%>%
    mutate(rowname = substr(name,4,18))
b_sup_peanut <- left_join(b_sup_peanut,vws, by = 'rowname')
b_sup_peanut$wtedshock <- b_sup_peanut$val * b_sup_peanut$weight
#combine into b mat------------------
shock_2036 <- 
    matrix(c(corn = sum(b_sup_corn[,5]),
             soy = sum(b_sup_soy[,5]),
             wheat = sum(b_sup_wheat[,5]),
             rice = sum(b_sup_rice[,5]),
             peanuts = sum(b_sup_peanut[,5])),nrow=5)

#solve for price changes ------------------------------
chgs_2020 <- solve(A,shock_2020)
chgs_2028 <- solve(A,shock_2028)
chgs_2036 <- solve(A,shock_2036)

#new prices 
#2012 Prices from NASS
Prices <- tribble(
    ~P_2012,
    6.89,
    14.4,
    7.77,
    15.1,
    0.301
)
Prices <- Prices %>%
    add_column(chgs_2020 = as.vector(chgs_2020))%>%
    add_column(chgs_2028 = as.vector(chgs_2028))%>%
    add_column(chgs_2036 = as.vector(chgs_2036))

Prices <- Prices %>%
    mutate(chgs_2020 = chgs_2020 + 1)%>%
    mutate(chgs_2028 = chgs_2028 + 1)%>%
    mutate(chgs_2036 = chgs_2036 + 1)

Prices <- Prices %>%
    mutate(P_2020 = P_2012 * chgs_2020)%>%
    mutate(P_2028 = P_2020 * chgs_2028)%>%
    mutate(P_2036 = P_2028 * chgs_2036)%>%
    add_column(rownames = c("corn", "soy", "wheat", "rice", "peanuts"))%>%
    column_to_rownames(var="rownames")

#quantities
vet <- ve %>%
    mutate(GEOID = str_extract(name,"[:digit:]{5}"))%>%
    mutate(Var = paste0('e_',
                        str_extract(name, '(corn|soy|wheat|rice|peanut){1}'),"_",
                        str_extract(name, '(corn|soy|wheat|rice|peanut)\\b')))%>%
    select(-reg)%>%
    reshape2::dcast(GEOID ~ Var, value.var = 'val')

SIM <- left_join(SIM,vet, by = 'GEOID')


SIM$price_corn_2012 <- Prices[['corn','P_2012']]
SIM$price_soy_2012 <- Prices[['soy','P_2012']]
SIM$price_wheat_2012 <- Prices[['wheat','P_2012']]
SIM$price_rice_2012 <- Prices[['rice','P_2012']]
SIM$price_peanuts_2012 <- Prices[['peanuts','P_2012']]
SIM$price_corn_2020 <- Prices[['corn','P_2020']]
SIM$price_soy_2020 <- Prices[['soy','P_2020']]
SIM$price_wheat_2020 <- Prices[['wheat','P_2020']]
SIM$price_rice_2020 <- Prices[['rice','P_2020']]
SIM$price_peanuts_2020 <- Prices[['peanuts','P_2020']]
SIM$price_corn_2028 <- Prices[['corn','P_2028']]
SIM$price_soy_2028 <- Prices[['soy','P_2028']]
SIM$price_wheat_2028 <- Prices[['wheat','P_2028']]
SIM$price_rice_2028 <- Prices[['rice','P_2028']]
SIM$price_peanuts_2028 <- Prices[['peanuts','P_2028']]
SIM$price_corn_2036 <- Prices[['corn','P_2036']]
SIM$price_soy_2036 <- Prices[['soy','P_2036']]
SIM$price_wheat_2036 <- Prices[['wheat','P_2036']]
SIM$price_rice_2036 <- Prices[['rice','P_2036']]
SIM$price_peanuts_2036 <- Prices[['peanuts','P_2036']]
#---------------
SIM$EP_corn_2020 <- 1- Prices[['corn','chgs_2020']]
SIM$EP_soy_2020 <- 1- Prices[['soy','chgs_2020']]
SIM$EP_wheat_2020 <- 1- Prices[['wheat','chgs_2020']]
SIM$EP_rice_2020 <- 1- Prices[['rice','chgs_2020']]
SIM$EP_peanuts_2020 <- 1- Prices[['peanuts','chgs_2020']]
SIM$EP_corn_2028 <- 1- Prices[['corn','chgs_2028']]
SIM$EP_soy_2028 <- 1- Prices[['soy','chgs_2028']]
SIM$EP_wheat_2028 <- 1- Prices[['wheat','chgs_2028']]
SIM$EP_rice_2028 <- 1- Prices[['rice','chgs_2028']]
SIM$EP_peanuts_2028 <- 1- Prices[['peanuts','chgs_2028']]
SIM$EP_corn_2036 <- 1- Prices[['corn','chgs_2036']]
SIM$EP_soy_2036 <- 1- Prices[['soy','chgs_2036']]
SIM$EP_wheat_2036 <- 1- Prices[['wheat','chgs_2036']]
SIM$EP_rice_2036 <- 1- Prices[['rice','chgs_2036']]
SIM$EP_peanuts_2036 <- 1- Prices[['peanuts','chgs_2036']]
#------------------------------------------------------------

SIM$q_corn_2020 <- (SIM$p_corn_2012 * (1 + (SIM$ss__corn * (SIM$e_corn_corn * SIM$EP_corn_2020 + SIM$e_corn_soy * SIM$EP_soy_2020 + SIM$e_corn_wheat * SIM$EP_wheat_2020 + SIM$e_corn_rice * SIM$EP_rice_2020 + SIM$e_corn_peanut * SIM$EP_peanuts_2020))))%>%replace_na(0)

SIM$q_soy_2020 <- (SIM$p_soy_2012 * (1 + (SIM$ss__soy * (SIM$e_soy_corn * SIM$EP_corn_2020 + SIM$e_soy_soy * SIM$EP_soy_2020 + SIM$e_soy_wheat * SIM$EP_wheat_2020 + SIM$e_soy_rice * SIM$EP_rice_2020 + SIM$e_soy_peanut * SIM$EP_peanuts_2020))))%>%replace_na(0)

SIM$q_wheat_2020 <- (SIM$p_wheat_2012 * (1 + (SIM$ss__wheat * (SIM$e_wheat_corn * SIM$EP_corn_2020 + SIM$e_wheat_soy * SIM$EP_soy_2020 + SIM$e_wheat_wheat * SIM$EP_wheat_2020 + SIM$e_wheat_rice * SIM$EP_rice_2020 + SIM$e_wheat_peanut * SIM$EP_peanuts_2020))))%>%replace_na(0)

SIM$q_rice_2020 <- (SIM$p_rice_2012 * (1 + (SIM$ss__rice * (SIM$e_rice_corn * SIM$EP_corn_2020 + SIM$e_rice_soy * SIM$EP_soy_2020 + SIM$e_rice_wheat * SIM$EP_wheat_2020 + SIM$e_rice_rice * SIM$EP_rice_2020 + SIM$e_rice_peanut * SIM$EP_peanuts_2020))))%>%replace_na(0)

SIM$q_peanuts_2020 <- (SIM$p_peanuts_2012 * (1 + (SIM$ss__peanut * (SIM$e_peanut_corn * SIM$EP_corn_2020 + SIM$e_peanut_soy * SIM$EP_soy_2020 + SIM$e_peanut_wheat * SIM$EP_wheat_2020 + SIM$e_peanut_rice * SIM$EP_rice_2020 + SIM$e_peanut_peanut * SIM$EP_peanuts_2020))))%>%replace_na(0)
#---------------------------

SIM$q_corn_2028 <- (SIM$q_corn_2020 * (1 + (SIM$ss__corn * (SIM$e_corn_corn * SIM$EP_corn_2028 + SIM$e_corn_soy * SIM$EP_soy_2028 + SIM$e_corn_wheat * SIM$EP_wheat_2028 + SIM$e_corn_rice * SIM$EP_rice_2028 + SIM$e_corn_peanut * SIM$EP_peanuts_2028))))%>%replace_na(0)

SIM$q_soy_2028 <- (SIM$q_soy_2020 * (1 + (SIM$ss__soy * (SIM$e_corn_corn * SIM$EP_corn_2028 + SIM$e_corn_soy * SIM$EP_soy_2028 + SIM$e_soy_wheat * SIM$EP_wheat_2028 + SIM$e_soy_rice * SIM$EP_rice_2028 + SIM$e_soy_peanut * SIM$EP_peanuts_2028))))%>%replace_na(0)

SIM$q_wheat_2028 <- (SIM$q_wheat_2020 * (1 + (SIM$ss__wheat * (SIM$e_wheat_corn * SIM$EP_corn_2028 + SIM$e_wheat_soy * SIM$EP_soy_2028 + SIM$e_wheat_wheat * SIM$EP_wheat_2028 + SIM$e_wheat_rice * SIM$EP_rice_2028 + SIM$e_wheat_peanut * SIM$EP_peanuts_2028))))%>%replace_na(0)

SIM$q_rice_2028 <- (SIM$q_rice_2020 * (1 + (SIM$ss__rice * (SIM$e_rice_corn * SIM$EP_corn_2028 + SIM$e_rice_soy * SIM$EP_soy_2028 + SIM$e_rice_wheat * SIM$EP_wheat_2028 + SIM$e_rice_rice * SIM$EP_rice_2028 + SIM$e_rice_peanut * SIM$EP_peanuts_2028))))%>%replace_na(0)

SIM$q_peanuts_2028 <- (SIM$q_peanuts_2020 * (1 + (SIM$ss__peanut * (SIM$e_peanut_corn * SIM$EP_corn_2028 + SIM$e_peanut_soy * SIM$EP_soy_2028 + SIM$e_peanut_wheat * SIM$EP_wheat_2028 + SIM$e_peanut_rice * SIM$EP_rice_2028 + SIM$e_peanut_peanut * SIM$EP_peanuts_2028))))%>%replace_na(0)
#----------------------------------
SIM$q_corn_2036 <- (SIM$q_corn_2028 * (1 + (SIM$ss__corn * (SIM$e_corn_corn * SIM$EP_corn_2036 + SIM$e_corn_soy * SIM$EP_soy_2036 + SIM$e_corn_wheat * SIM$EP_wheat_2036 + SIM$e_corn_rice * SIM$EP_rice_2036 + SIM$e_corn_peanut * SIM$EP_peanuts_2036))))%>%replace_na(0)

SIM$q_soy_2036 <- (SIM$q_soy_2028 * (1 + (SIM$ss__soy * (SIM$e_soy_corn * SIM$EP_corn_2036 + SIM$e_soy_soy * SIM$EP_soy_2036 + SIM$e_soy_wheat * SIM$EP_wheat_2036 + SIM$e_soy_rice * SIM$EP_rice_2036 + SIM$e_soy_peanut * SIM$EP_peanuts_2036))))%>%replace_na(0)

SIM$q_wheat_2036 <- (SIM$q_wheat_2028 * (1 + (SIM$ss__wheat * (SIM$e_wheat_corn * SIM$EP_corn_2036 + SIM$e_wheat_soy * SIM$EP_soy_2036 + SIM$e_wheat_wheat * SIM$EP_wheat_2036 + SIM$e_wheat_rice * SIM$EP_rice_2036 + SIM$e_wheat_peanut * SIM$EP_peanuts_2036))))%>%replace_na(0)

SIM$q_rice_2036 <- (SIM$q_rice_2028 * (1 + (SIM$ss__rice * (SIM$e_rice_corn * SIM$EP_corn_2036 + SIM$e_rice_soy * SIM$EP_soy_2036 + SIM$e_rice_wheat * SIM$EP_wheat_2036 + SIM$e_rice_rice * SIM$EP_rice_2036 + SIM$e_rice_peanut * SIM$EP_peanuts_2036))))%>%replace_na(0)

SIM$q_peanuts_2036 <- (SIM$q_peanuts_2028 * (1 + (SIM$ss__peanut * (SIM$e_peanut_corn * SIM$EP_corn_2036 + SIM$e_peanut_soy * SIM$EP_soy_2036 + SIM$e_peanut_wheat * SIM$EP_wheat_2036 + SIM$e_peanut_rice * SIM$EP_rice_2036 + SIM$e_peanut_peanut * SIM$EP_peanuts_2036))))%>%replace_na(0)

#using USDA Crop Yearbook reports, supply and disapperance tables for 2012
domestic_total_use_2012 <- c(
    10352811000, 1784000000, 1176000000 , 119040000 , 3910000000   
)
domestic_total_use_2020 <- domestic_total_use_2012 * (1+ ((dom[,'corn'] * (Prices$chgs_2020[1] - 1))+(dom[,'soy'] * (Prices$chgs_2020[2] - 1))+(dom[,'wheat'] * (Prices$chgs_2020[3] - 1))+(dom[,'rice'] * (Prices$chgs_2020[4] - 1))+(dom[,'peanut'] * (Prices$chgs_2020[5] - 1))))

domestic_total_use_2028 <- domestic_total_use_2020 * (1+ ((dom[,'corn'] * (Prices$chgs_2028[1] - 1))+(dom[,'soy'] * (Prices$chgs_2028[2] - 1))+(dom[,'wheat'] * (Prices$chgs_2028[3] - 1))+(dom[,'rice'] * (Prices$chgs_2028[4] - 1))+(dom[,'peanut'] * (Prices$chgs_2028[5] - 1))))

domestic_total_use_2036 <- domestic_total_use_2028 * (1+ ((dom[,'corn'] * (Prices$chgs_2036[1] - 1))+(dom[,'soy'] * (Prices$chgs_2036[2] - 1))+(dom[,'wheat'] * (Prices$chgs_2036[3] - 1))+(dom[,'rice'] * (Prices$chgs_2036[4] - 1))+(dom[,'peanut'] * (Prices$chgs_2036[5] - 1))))

# consumer surplus
m_2012 <- (1/(diag(as.matrix(dom))))*Prices[['P_2012']]/domestic_total_use_2012
b_2012 <- Prices[['P_2012']]-m_2012 * domestic_total_use_2012
CS_2012 <- (((b_2012-Prices[['P_2012']]) * domestic_total_use_2012) / 2)

m_2020 <- (1/(diag(as.matrix(dom))))*Prices[['P_2020']]/domestic_total_use_2020
b_2020 <- Prices[['P_2020']]-m_2020 * domestic_total_use_2020
CS_2020 <- (((b_2020-Prices[['P_2020']]) * domestic_total_use_2020) / 2)

m_2028 <- (1/(diag(as.matrix(dom))))*Prices[['P_2028']]/domestic_total_use_2028
b_2028 <- Prices[['P_2028']]-m_2028 * domestic_total_use_2028
CS_2028 <- (((b_2028-Prices[['P_2028']]) * domestic_total_use_2028) / 2) 

m_2036 <- (1/(diag(as.matrix(dom))))*Prices[['P_2036']]/domestic_total_use_2036
b_2036 <- Prices[['P_2036']]-m_2036 * domestic_total_use_2036
CS_2036 <- (((b_2036-Prices[['P_2036']]) * domestic_total_use_2036) / 2) 

Delta_CS_2020 <-CS_2020 - CS_2012
Delta_CS_2028 <-CS_2028 - CS_2020
Delta_CS_2036 <-CS_2036 - CS_2028

# Producer surplus
# find slope
SIM$m_corn_2012 <- ((1/  SIM$e_corn_corn)*Prices[[1,'P_2012']]/SIM$p_corn_2012)%>%replace_na(0)
SIM$m_corn_2020 <- ((1/  SIM$e_corn_corn)*Prices[[1,'P_2020']]/SIM$q_corn_2020)%>%replace_na(0)
SIM$m_corn_2028 <- ((1/  SIM$e_corn_corn)*Prices[[1,'P_2028']]/SIM$q_corn_2028)%>%replace_na(0)
SIM$m_corn_2036 <- ((1/  SIM$e_corn_corn)*Prices[[1,'P_2036']]/SIM$q_corn_2036)%>%replace_na(0)

SIM$m_soy_2012 <- ((1/  SIM$e_soy_soy)*Prices[[2,'P_2012']]/SIM$p_soy_2012)%>%replace_na(0)
SIM$m_soy_2020 <- ((1/  SIM$e_soy_soy)*Prices[[2,'P_2020']]/SIM$q_soy_2020)%>%replace_na(0)
SIM$m_soy_2028 <- ((1/  SIM$e_soy_soy)*Prices[[2,'P_2028']]/SIM$q_soy_2028)%>%replace_na(0)
SIM$m_soy_2036 <- ((1/  SIM$e_soy_soy)*Prices[[2,'P_2036']]/SIM$q_soy_2036)%>%replace_na(0)

SIM$m_wheat_2012 <- ((1/  SIM$e_wheat_wheat)*Prices[[3,'P_2012']]/SIM$p_wheat_2012)%>%replace_na(0)
SIM$m_wheat_2020 <- ((1/  SIM$e_wheat_wheat)*Prices[[3,'P_2020']]/SIM$q_wheat_2020)%>%replace_na(0)
SIM$m_wheat_2028 <- ((1/  SIM$e_wheat_wheat)*Prices[[3,'P_2028']]/SIM$q_wheat_2028)%>%replace_na(0)
SIM$m_wheat_2036 <- ((1/  SIM$e_wheat_wheat)*Prices[[3,'P_2036']]/SIM$q_wheat_2036)%>%replace_na(0)

SIM$m_rice_2012 <- ((1/  SIM$e_rice_rice)*Prices[[4,'P_2012']]/SIM$p_rice_2012)%>%replace_na(0)
SIM$m_rice_2020 <- ((1/  SIM$e_rice_rice)*Prices[[4,'P_2020']]/SIM$q_rice_2020)%>%replace_na(0)
SIM$m_rice_2028 <- ((1/  SIM$e_rice_rice)*Prices[[4,'P_2028']]/SIM$q_rice_2028)%>%replace_na(0)
SIM$m_rice_2036 <- ((1/  SIM$e_rice_rice)*Prices[[4,'P_2036']]/SIM$q_rice_2036)%>%replace_na(0)

SIM$m_peanuts_2012 <- ((1/  SIM$e_peanut_peanut)*Prices[[5,'P_2012']]/SIM$p_peanuts_2012)%>%replace_na(0)
SIM$m_peanuts_2020 <- ((1/  SIM$e_peanut_peanut)*Prices[[5,'P_2020']]/SIM$q_peanuts_2020)%>%replace_na(0)
SIM$m_peanuts_2028 <- ((1/  SIM$e_peanut_peanut)*Prices[[5,'P_2028']]/SIM$q_peanuts_2028)%>%replace_na(0)
SIM$m_peanuts_2036 <- ((1/  SIM$e_peanut_peanut)*Prices[[5,'P_2036']]/SIM$q_peanuts_2036)%>%replace_na(0)

#find b
SIM$b_corn_2012 <- (Prices[[1,'P_2012']] - SIM$m_corn_2012 * SIM$p_corn_2012)%>%replace_na(0)
SIM$b_corn_2020 <- (Prices[[1,'P_2020']] - SIM$m_corn_2020 * SIM$q_corn_2020)%>%replace_na(0)
SIM$b_corn_2028 <- (Prices[[1,'P_2028']] - SIM$m_corn_2028 * SIM$q_corn_2028)%>%replace_na(0)
SIM$b_corn_2036 <- (Prices[[1,'P_2036']] - SIM$m_corn_2036 * SIM$q_corn_2036)%>%replace_na(0)

SIM$b_soy_2012 <- (Prices[[2,'P_2012']] - SIM$m_soy_2012 * SIM$p_soy_2012)%>%replace_na(0)
SIM$b_soy_2020 <- (Prices[[2,'P_2020']] - SIM$m_soy_2020 * SIM$q_soy_2020)%>%replace_na(0)
SIM$b_soy_2028 <- (Prices[[2,'P_2028']] - SIM$m_soy_2028 * SIM$q_soy_2028)%>%replace_na(0)
SIM$b_soy_2036 <- (Prices[[2,'P_2036']] - SIM$m_soy_2036 * SIM$q_soy_2036)%>%replace_na(0)

SIM$b_wheat_2012 <- (Prices[[3,'P_2012']] - SIM$m_wheat_2012 * SIM$p_wheat_2012)%>%replace_na(0)
SIM$b_wheat_2020 <- (Prices[[3,'P_2020']] - SIM$m_wheat_2020 * SIM$q_wheat_2020)%>%replace_na(0)
SIM$b_wheat_2028 <- (Prices[[3,'P_2028']] - SIM$m_wheat_2028 * SIM$q_wheat_2028)%>%replace_na(0)
SIM$b_wheat_2036 <- (Prices[[3,'P_2036']] - SIM$m_wheat_2036 * SIM$q_wheat_2036)%>%replace_na(0)

SIM$b_rice_2012 <- (Prices[[4,'P_2012']] - SIM$m_rice_2012 * SIM$p_rice_2012)%>%replace_na(0)
SIM$b_rice_2020 <- (Prices[[4,'P_2020']] - SIM$m_rice_2020 * SIM$q_rice_2020)%>%replace_na(0)
SIM$b_rice_2028 <- (Prices[[4,'P_2028']] - SIM$m_rice_2028 * SIM$q_rice_2028)%>%replace_na(0)
SIM$b_rice_2036 <- (Prices[[4,'P_2036']] - SIM$m_rice_2036 * SIM$q_rice_2036)%>%replace_na(0)

SIM$b_peanuts_2012 <- (Prices[[5,'P_2012']] - SIM$m_peanuts_2012 * SIM$p_peanuts_2012)%>%replace_na(0)
SIM$b_peanuts_2020 <- (Prices[[5,'P_2020']] - SIM$m_peanuts_2020 * SIM$q_peanuts_2020)%>%replace_na(0)
SIM$b_peanuts_2028 <- (Prices[[5,'P_2028']] - SIM$m_peanuts_2028 * SIM$q_peanuts_2028)%>%replace_na(0)
SIM$b_peanuts_2036 <- (Prices[[5,'P_2036']] - SIM$m_peanuts_2036 * SIM$q_peanuts_2036)%>%replace_na(0)

#find x_0
SIM$x0_corn_2012 <-    (-SIM$b_corn_2012/SIM$m_corn_2012)%>%replace_na(0)
SIM$x0_corn_2020 <-    (-SIM$b_corn_2020/SIM$m_corn_2020)%>%replace_na(0)
SIM$x0_corn_2028 <-    (-SIM$b_corn_2028/SIM$m_corn_2028)%>%replace_na(0)
SIM$x0_corn_2036 <-    (-SIM$b_corn_2036/SIM$m_corn_2036)%>%replace_na(0)

SIM$x0_soy_2012 <-     (-SIM$b_soy_2012/SIM$m_soy_2012)%>%replace_na(0)
SIM$x0_soy_2020 <-     (-SIM$b_soy_2020/SIM$m_soy_2020)%>%replace_na(0)
SIM$x0_soy_2028 <-     (-SIM$b_soy_2028/SIM$m_soy_2028)%>%replace_na(0)
SIM$x0_soy_2036 <-     (-SIM$b_soy_2036/SIM$m_soy_2036)%>%replace_na(0)

SIM$x0_wheat_2012 <-   (-SIM$b_wheat_2012/SIM$m_wheat_2012)%>%replace_na(0)
SIM$x0_wheat_2020 <-   (-SIM$b_wheat_2020/SIM$m_wheat_2020)%>%replace_na(0)
SIM$x0_wheat_2028 <-   (-SIM$b_wheat_2028/SIM$m_wheat_2028)%>%replace_na(0)
SIM$x0_wheat_2036 <-   (-SIM$b_wheat_2036/SIM$m_wheat_2036)%>%replace_na(0)

SIM$x0_rice_2012 <-    (-SIM$b_rice_2012/SIM$m_rice_2012)%>%replace_na(0)
SIM$x0_rice_2020 <-    (-SIM$b_rice_2020/SIM$m_rice_2020)%>%replace_na(0)
SIM$x0_rice_2028 <-    (-SIM$b_rice_2028/SIM$m_rice_2028)%>%replace_na(0)
SIM$x0_rice_2036 <-    (-SIM$b_rice_2036/SIM$m_rice_2036)%>%replace_na(0)

SIM$x0_peanuts_2012 <- (-SIM$b_peanuts_2012/SIM$m_peanuts_2012)%>%replace_na(0)
SIM$x0_peanuts_2020 <- (-SIM$b_peanuts_2020/SIM$m_peanuts_2020)%>%replace_na(0)
SIM$x0_peanuts_2028 <- (-SIM$b_peanuts_2028/SIM$m_peanuts_2028)%>%replace_na(0)
SIM$x0_peanuts_2036 <- (-SIM$b_peanuts_2036/SIM$m_peanuts_2036)%>%replace_na(0)

#area = [m(x1)^2/2+b(x1)]-[m(x0)^2/2+b(x0)]

SIM$PS_2012_corn <- ((SIM$m_corn_2012 * (SIM$p_corn_2012)^2/2+SIM$b_corn_2012 * (SIM$p_corn_2012))-(SIM$m_corn_2012 * (SIM$x0_corn_2012)^2/2+SIM$b_corn_2012 * (SIM$x0_corn_2012)))%>%replace_na(0)

SIM$PS_2020_corn <- ((SIM$m_corn_2020 * (SIM$q_corn_2020)^2/2+SIM$b_corn_2020 * (SIM$q_corn_2020))-(SIM$m_corn_2020 * (SIM$x0_corn_2020)^2/2+SIM$b_corn_2020 * (SIM$x0_corn_2020)))%>%replace_na(0)

SIM$PS_2028_corn <- ((SIM$m_corn_2028 * (SIM$q_corn_2028)^2/2+SIM$b_corn_2028 * (SIM$q_corn_2028))-(SIM$m_corn_2028 * (SIM$x0_corn_2028)^2/2+SIM$b_corn_2028 * (SIM$x0_corn_2028)))%>%replace_na(0)

SIM$PS_2036_corn <- ((SIM$m_corn_2036 * (SIM$q_corn_2036)^2/2+SIM$b_corn_2036 * (SIM$q_corn_2036))-(SIM$m_corn_2012 * (SIM$x0_corn_2036)^2/2+SIM$b_corn_2036 * (SIM$x0_corn_2036)))%>%replace_na(0)
#-----------------
SIM$PS_2012_soy <- ((SIM$m_soy_2012 * (SIM$p_soy_2012)^2/2+SIM$b_soy_2012 * (SIM$p_soy_2012))-(SIM$m_soy_2012 * (SIM$x0_soy_2012)^2/2+SIM$b_soy_2012 * (SIM$x0_soy_2012)))%>%replace_na(0)

SIM$PS_2020_soy <- ((SIM$m_soy_2020 * (SIM$q_soy_2020)^2/2+SIM$b_soy_2020 * (SIM$q_soy_2020))-(SIM$m_soy_2020 * (SIM$x0_soy_2020)^2/2+SIM$b_soy_2020 * (SIM$x0_soy_2020)))%>%replace_na(0)

SIM$PS_2028_soy <- ((SIM$m_soy_2028 * (SIM$q_soy_2028)^2/2+SIM$b_soy_2028 * (SIM$q_soy_2028))-(SIM$m_soy_2028 * (SIM$x0_soy_2028)^2/2+SIM$b_soy_2028 * (SIM$x0_soy_2028)))%>%replace_na(0)

SIM$PS_2036_soy <- ((SIM$m_soy_2036 * (SIM$q_soy_2036)^2/2+SIM$b_soy_2036 * (SIM$q_soy_2036))-(SIM$m_soy_2012 * (SIM$x0_soy_2036)^2/2+SIM$b_soy_2036 * (SIM$x0_soy_2036)))%>%replace_na(0)
#-------------

SIM$PS_2012_wheat <- ((SIM$m_wheat_2012 * (SIM$p_wheat_2012)^2/2+SIM$b_wheat_2012 * (SIM$p_wheat_2012))-(SIM$m_wheat_2012 * (SIM$x0_wheat_2012)^2/2+SIM$b_wheat_2012 * (SIM$x0_wheat_2012)))%>%replace_na(0)

SIM$PS_2020_wheat <- ((SIM$m_wheat_2020 * (SIM$q_wheat_2020)^2/2+SIM$b_wheat_2020 * (SIM$q_wheat_2020))-(SIM$m_wheat_2020 * (SIM$x0_wheat_2020)^2/2+SIM$b_wheat_2020 * (SIM$x0_wheat_2020)))%>%replace_na(0)

SIM$PS_2028_wheat <- ((SIM$m_wheat_2028 * (SIM$q_wheat_2028)^2/2+SIM$b_wheat_2028 * (SIM$q_wheat_2028))-(SIM$m_wheat_2028 * (SIM$x0_wheat_2028)^2/2+SIM$b_wheat_2028 * (SIM$x0_wheat_2028)))%>%replace_na(0)

SIM$PS_2036_wheat <- ((SIM$m_wheat_2036 * (SIM$q_wheat_2036)^2/2+SIM$b_wheat_2036 * (SIM$q_wheat_2036))-(SIM$m_wheat_2012 * (SIM$x0_wheat_2036)^2/2+SIM$b_wheat_2036 * (SIM$x0_wheat_2036)))%>%replace_na(0)
#----------------

SIM$PS_2012_rice <- ((SIM$m_rice_2012 * (SIM$p_rice_2012)^2/2+SIM$b_rice_2012 * (SIM$p_rice_2012))-(SIM$m_rice_2012 * (SIM$x0_rice_2012)^2/2+SIM$b_rice_2012 * (SIM$x0_rice_2012)))%>%replace_na(0)

SIM$PS_2020_rice <- ((SIM$m_rice_2020 * (SIM$q_rice_2020)^2/2+SIM$b_rice_2020 * (SIM$q_rice_2020))-(SIM$m_rice_2020 * (SIM$x0_rice_2020)^2/2+SIM$b_rice_2020 * (SIM$x0_rice_2020)))%>%replace_na(0)

SIM$PS_2028_rice <- ((SIM$m_rice_2028 * (SIM$q_rice_2028)^2/2+SIM$b_rice_2028 * (SIM$q_rice_2028))-(SIM$m_rice_2028 * (SIM$x0_rice_2028)^2/2+SIM$b_rice_2028 * (SIM$x0_rice_2028)))%>%replace_na(0)

SIM$PS_2036_rice <- ((SIM$m_rice_2036 * (SIM$q_rice_2036)^2/2+SIM$b_rice_2036 * (SIM$q_rice_2036))-(SIM$m_rice_2012 * (SIM$x0_rice_2036)^2/2+SIM$b_rice_2036 * (SIM$x0_rice_2036)))%>%replace_na(0)
#-----------------

SIM$PS_2012_peanuts <- ((SIM$m_peanuts_2012 * (SIM$p_peanuts_2012)^2/2+SIM$b_peanuts_2012 * (SIM$p_peanuts_2012))-(SIM$m_peanuts_2012 * (SIM$x0_peanuts_2012)^2/2+SIM$b_peanuts_2012 * (SIM$x0_peanuts_2012)))%>%replace_na(0)

SIM$PS_2020_peanuts <- ((SIM$m_peanuts_2020 * (SIM$q_peanuts_2020)^2/2+SIM$b_peanuts_2020 * (SIM$q_peanuts_2020))-(SIM$m_peanuts_2020 * (SIM$x0_peanuts_2020)^2/2+SIM$b_peanuts_2020 * (SIM$x0_peanuts_2020)))%>%replace_na(0)

SIM$PS_2028_peanuts <- ((SIM$m_peanuts_2028 * (SIM$q_peanuts_2028)^2/2+SIM$b_peanuts_2028 * (SIM$q_peanuts_2028))-(SIM$m_peanuts_2028 * (SIM$x0_peanuts_2028)^2/2+SIM$b_peanuts_2028 * (SIM$x0_peanuts_2028)))%>%replace_na(0)

SIM$PS_2036_peanuts <- (((SIM$m_peanuts_2036 * (SIM$q_peanuts_2036)^2/2+SIM$b_peanuts_2036 * (SIM$q_peanuts_2036))-(SIM$m_peanuts_2012 * (SIM$x0_peanuts_2036)^2/2+SIM$b_peanuts_2036 * (SIM$x0_peanuts_2036))))%>%replace_na(0)

#PS change
SIM$Delta_PS_2020_corn <- SIM$PS_2020_corn - SIM$PS_2012_corn
chgs_PS_2020_corn <- sum(SIM$Delta_PS_2020_corn, na.rm = TRUE)
SIM$Delta_PS_2028_corn <- SIM$PS_2028_corn - SIM$PS_2020_corn
chgs_PS_2028_corn <- sum(SIM$Delta_PS_2028_corn, na.rm = TRUE)
SIM$Delta_PS_2036_corn <- SIM$PS_2036_corn - SIM$PS_2028_corn
chgs_PS_2036_corn <- sum(SIM$Delta_PS_2036_corn, na.rm = TRUE)

SIM$Delta_PS_2020_soy <- SIM$PS_2020_soy - SIM$PS_2012_soy
chgs_PS_2020_soy <- sum(SIM$Delta_PS_2020_soy, na.rm = TRUE)
SIM$Delta_PS_2028_soy <- SIM$PS_2028_soy - SIM$PS_2020_soy
chgs_PS_2028_soy <- sum(SIM$Delta_PS_2028_soy, na.rm = TRUE)
SIM$Delta_PS_2036_soy <- SIM$PS_2036_soy - SIM$PS_2028_soy
chgs_PS_2036_soy <- sum(SIM$Delta_PS_2036_soy, na.rm = TRUE)

SIM$Delta_PS_2020_wheat <- SIM$PS_2020_wheat - SIM$PS_2012_wheat
chgs_PS_2020_wheat <- sum(SIM$Delta_PS_2020_wheat, na.rm = TRUE)
SIM$Delta_PS_2028_wheat <- SIM$PS_2028_wheat - SIM$PS_2020_wheat
chgs_PS_2028_wheat <- sum(SIM$Delta_PS_2028_wheat, na.rm = TRUE)
SIM$Delta_PS_2036_wheat <- SIM$PS_2036_wheat - SIM$PS_2028_wheat
chgs_PS_2036_wheat <- sum(SIM$Delta_PS_2036_wheat, na.rm = TRUE)

SIM$Delta_PS_2020_rice <- SIM$PS_2020_rice - SIM$PS_2012_rice
chgs_PS_2020_rice <- sum(SIM$Delta_PS_2020_rice, na.rm = TRUE) 
SIM$Delta_PS_2028_rice <- SIM$PS_2028_rice - SIM$PS_2020_rice
chgs_PS_2028_rice <- sum(SIM$Delta_PS_2028_rice, na.rm = TRUE)
SIM$Delta_PS_2036_rice <- SIM$PS_2036_rice - SIM$PS_2028_rice
chgs_PS_2036_rice <- sum(SIM$Delta_PS_2036_rice, na.rm = TRUE)

SIM$Delta_PS_2020_peanuts <- SIM$PS_2020_peanuts - SIM$PS_2012_peanuts
chgs_PS_2020_peanuts <- sum(SIM$Delta_PS_2020_peanuts, na.rm = TRUE)
SIM$Delta_PS_2028_peanuts <- SIM$PS_2028_peanuts - SIM$PS_2020_peanuts
chgs_PS_2028_peanuts <- sum(SIM$Delta_PS_2028_peanuts, na.rm = TRUE)
SIM$Delta_PS_2036_peanuts <- SIM$PS_2036_peanuts - SIM$PS_2028_peanuts
chgs_PS_2036_peanuts <- sum(SIM$Delta_PS_2036_peanuts, na.rm = TRUE)

return_items <- list(SIM = SIM,
                     Prices = Prices,
                     chgs_PS_2020_corn = chgs_PS_2020_corn,
                     chgs_PS_2028_corn = chgs_PS_2028_corn,
                     chgs_PS_2036_corn = chgs_PS_2036_corn,
                     chgs_PS_2020_soy = chgs_PS_2020_soy,
                     chgs_PS_2028_soy = chgs_PS_2028_soy,
                     chgs_PS_2036_soy = chgs_PS_2036_soy,
                     chgs_PS_2020_wheat = chgs_PS_2020_wheat,
                     chgs_PS_2028_wheat = chgs_PS_2028_wheat,
                     chgs_PS_2036_wheat = chgs_PS_2036_wheat,
                     chgs_PS_2020_rice = chgs_PS_2020_rice,
                     chgs_PS_2028_rice = chgs_PS_2028_rice,
                     chgs_PS_2036_rice = chgs_PS_2036_rice,
                     chgs_PS_2020_peanuts = chgs_PS_2020_peanuts,
                     chgs_PS_2028_peanuts = chgs_PS_2028_peanuts,
                     chgs_PS_2036_peanuts = chgs_PS_2036_peanuts,
                     Delta_CS_2020 = Delta_CS_2020,
                     Delta_CS_2028 = Delta_CS_2028,
                     Delta_CS_2036 = Delta_CS_2036
                     )
return(return_items)

}
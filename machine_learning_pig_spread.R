# incorporate predictions to snow data--------------
SIM <- as.data.frame(h2o.predict(m,test_h2o))
SIM <- bind_cols(filter(SNOW, TIME == 4),SIM)
#Time Step 1: 2020
SIM$random <- runif(nrow(SIM),min = 0, max = 1)
SIM$Id_2020 <- with(SIM, ifelse(is.na(Id_2012),
                                ifelse(random < present, 1, NA),
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

SIM$random <- runif(1,min = 0, max = 1)
SIM$Id_2028 <- with(SIM, ifelse(is.na(Id_2020),
                                ifelse(random < present1, 1, NA),
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

SIM$random <- runif(1,min = 0, max = 1)
SIM$Id_2036 <- with(SIM, ifelse(is.na(Id_2028),
                                ifelse(random < present2, 1, NA),
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
SIMh <- as.h2o(SIM)
REPEAT <- as.data.frame(h2o.predict(m,SIMh))
SIM <- bind_cols(SIM, REPEAT)
SIM$random <- runif(1,min = 0, max = 1)
SIM$Id_2042 <- with(SIM, ifelse(is.na(Id_2036),
                                ifelse(random < present3, 1, NA),
                                1))
#update machine learning dependent var
SIM$EXPAND <- with(SIM, ifelse(is.na(Id_2042),
                               "unknown",
                               "present"))
#update distance to nearest
countyPigs <- left_join(by = c('county2' = 'GEOID') , countyPigs, SIM[,c('GEOID','Id_2042')])# join distances to presence
by_cty <- countyPigs %>% group_by(county1)%>% 
  filter(Id_2042 == 1) %>%
  filter(mi_to_county == min(mi_to_county))
SIM <- left_join(by = c('GEOID' = 'county1') , SIM, by_cty[,c('county1',"mi_to_county",
                                                              "county2")])
SIM$county2.x <- SIM$mi_to_county.x <- NULL
SIM <- rename(SIM,mi_to_county = mi_to_county.y)
SIM <- rename(SIM,county2 = county2.y)
SIM$mi_to_county <- replace_na(SIM$mi_to_county, 500)


# Production in 2012 added to simulation data.set-----------------------

Presence <- SIM[,c("GEOID","Id_2020","Id_2028","Id_2036")]

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

Presence <- left_join(by = c('GEOID'='FIPS'),Presence,pr_corn_2012)
Presence <- left_join(by = c('GEOID'='FIPS'),Presence,pr_soy_2012)  
Presence <- left_join(by = c('GEOID'='FIPS'),Presence,pr_wheat_2012)
Presence <- left_join(by = c('GEOID'='FIPS'),Presence,pr_rice_2012)
Presence <- left_join(by = c('GEOID'='FIPS'),Presence,pr_peanuts_2012)
#rm(pr_corn_2012,pr_soy_2012,pr_wheat_2012,pr_rice_2012,pr_peanuts_2012)

#replace NA with 0

Presence$Id_2020 <- replace_na(Presence$Id_2020, 0)
Presence$Id_2028 <- replace_na(Presence$Id_2028, 0)
Presence$Id_2036 <- replace_na(Presence$Id_2036, 0)

# add exog shock-----
AbstractballparkS4 <- read_csv("AbstractballparkS4.txt")

#2020
Presence$draw_corn_2020 <- sample(AbstractballparkS4$Corn,nrow(Presence), replace = TRUE) 
Presence$EB_corn_2020 <- Presence$draw_corn_2020 * Presence$Id_2020

Presence$draw_soy_2020 <- sample(AbstractballparkS4$Soybeans,nrow(Presence), replace = TRUE) 
Presence$EB_soy_2020 <- Presence$draw_corn_2020 * Presence$Id_2020

Presence$draw_wheat_2020 <- sample(AbstractballparkS4$Wheat,nrow(Presence), replace = TRUE) 
Presence$EB_wheat_2020 <- Presence$draw_wheat_2020 * Presence$Id_2020

Presence$draw_rice_2020 <- sample(AbstractballparkS4$Rice,nrow(Presence), replace = TRUE) 
Presence$EB_rice_2020 <- Presence$draw_rice_2020 * Presence$Id_2020

Presence$draw_peanuts_2020 <- sample(AbstractballparkS4$Peanuts,nrow(Presence), replace = TRUE) 
Presence$EB_peanuts_2020 <- Presence$draw_peanuts_2020 * Presence$Id_2020
#2028
Presence$draw_corn_2028 <- sample(AbstractballparkS4$Corn,nrow(Presence), replace = TRUE) 
Presence$EB_corn_2028 <- Presence$draw_corn_2028 * Presence$Id_2028

Presence$draw_soy_2028 <- sample(AbstractballparkS4$Soybeans,nrow(Presence), replace = TRUE) 
Presence$EB_soy_2028 <- Presence$draw_corn_2028 * Presence$Id_2028

Presence$draw_wheat_2028 <- sample(AbstractballparkS4$Wheat,nrow(Presence), replace = TRUE) 
Presence$EB_wheat_2028 <- Presence$draw_wheat_2028 * Presence$Id_2028

Presence$draw_rice_2028 <- sample(AbstractballparkS4$Rice,nrow(Presence), replace = TRUE) 
Presence$EB_rice_2028 <- Presence$draw_rice_2028 * Presence$Id_2028

Presence$draw_peanuts_2028 <- sample(AbstractballparkS4$Peanuts,nrow(Presence), replace = TRUE) 
Presence$EB_peanuts_2028 <- Presence$draw_peanuts_2028 * Presence$Id_2028

#2036
Presence$draw_corn_2036 <- sample(AbstractballparkS4$Corn,nrow(Presence), replace = TRUE) 
Presence$EB_corn_2036 <- Presence$draw_corn_2036 * Presence$Id_2036

Presence$draw_soy_2036 <- sample(AbstractballparkS4$Soybeans,nrow(Presence), replace = TRUE) 
Presence$EB_soy_2036 <- Presence$draw_corn_2036 * Presence$Id_2036

Presence$draw_wheat_2036 <- sample(AbstractballparkS4$Wheat,nrow(Presence), replace = TRUE) 
Presence$EB_wheat_2036 <- Presence$draw_wheat_2036 * Presence$Id_2036

Presence$draw_rice_2036 <- sample(AbstractballparkS4$Rice,nrow(Presence), replace = TRUE) 
Presence$EB_rice_2036 <- Presence$draw_rice_2036 * Presence$Id_2036

Presence$draw_peanuts_2036 <- sample(AbstractballparkS4$Peanuts,nrow(Presence), replace = TRUE) 
Presence$EB_peanuts_2036 <- Presence$draw_peanuts_2036 * Presence$Id_2036

Presence <- select(Presence, -starts_with('draw_'))

rm(by_cty,countyPigs,PigsIn1982,PigsIn1988,PigsIn2004,PigsIn2012,REPEAT,sf12010countydistancemiles_csv,SNOW)
h2o.shutdown(prompt = FALSE)
gc()
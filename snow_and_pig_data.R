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
# remove individual components of countyPigs
#rm(PigsIn1982,PigsIn1988,PigsIn2004,PigsIn2012,sf12010countydistancemiles_csv)
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
#rm(snow_1,snow_2,snow_3,snow_4)

SNOW <- left_join(by = c('GEOID' = 'FIPS') , SNOW, PigsIn1982)
SNOW <- left_join(by = c('GEOID' = 'FIPS') , SNOW, PigsIn1988)
SNOW <- left_join(by = c('GEOID' = 'FIPS') , SNOW, PigsIn2004)
SNOW <- left_join(by = c('GEOID' = 'FIPS') , SNOW, PigsIn2012)

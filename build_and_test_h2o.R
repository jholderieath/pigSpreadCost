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


m <- h2o.randomForest(x,y,train_h2o, nfolds = 50, model_id = "RF_defaults",keep_cross_validation_predictions = TRUE)
summary(m)
mt <- h2o.performance(m, test_h2o)
mt
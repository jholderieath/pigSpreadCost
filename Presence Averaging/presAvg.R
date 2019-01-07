simaddy <- seq(from = 1, to = length(res), by = 20)
sims <- list()
location <- 0
for (x in simaddy) {
    location <- location + 1
    tmp <- paste0("pres_",location)
    sims[[location]] <- rbind(
        tibble(FIPS = res[[x]]$GEOID,year = '2020',tmp = res[[x]]$Id_2020),
        tibble(FIPS = res[[x]]$GEOID,year = '2028',tmp = res[[x]]$Id_2028),
        tibble(FIPS = res[[x]]$GEOID,year = '2036',tmp = res[[x]]$Id_2036))
}


sms <- sims %>% 
    reduce(full_join, by = c("FIPS","year"))


col_to <- paste0("pres", "_",1:100, collapse = " ,")
col_to <- paste("FIPS ,","year ,", col_to)

col_from <- names(sms)

colnames(sms) <- col_to

sms <- sms %>%
    rename_all(col_to)
    

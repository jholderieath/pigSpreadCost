# changes in consumer surplus=========
#2020
simaddy <- seq(from = 18, to = length(res), by = 20)
chg_cs_corn_2020 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_cs_corn_2020[location] <- res[[x]][[1]]
}

chg_cs_soy_2020 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_cs_soy_2020[location] <- res[[x]][[2]]
}

chg_cs_wheat_2020 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_cs_wheat_2020[location] <- res[[x]][[3]]
}


chg_cs_rice_2020 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_cs_rice_2020[location] <- res[[x]][[4]]
}

chg_cs_peanuts_2020 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_cs_peanuts_2020[location] <- res[[x]][[5]]
}
chg_cs_2020 <- tibble(
    chg_cs_corn_2020,
    chg_cs_soy_2020,
    chg_cs_wheat_2020,
    chg_cs_rice_2020,
    chg_cs_peanuts_2020) %>% 
    mutate(cs_sum_2020 = rowSums(.[1:5])) %>%
    gather(key = "key", value = "value")


cs_2020 <- ggplot(chg_cs_2020, aes(value)) + 
    stat_ecdf(geom = "step", pad = FALSE) +
    facet_grid(rows = vars(key)) +
    labs(x = "Change in Consumer Surplus", 
         title = 'Empirical Cumluative Distribution\nChange in Consumer Surplus for 2020')+ 
    scale_x_continuous(labels = dollar)

#2028
simaddy <- seq(from = 19, to = length(res), by = 20)
chg_cs_corn_2028 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_cs_corn_2028[location] <- res[[x]][[1]]
}

chg_cs_soy_2028 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_cs_soy_2028[location] <- res[[x]][[2]]
}

chg_cs_wheat_2028 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_cs_wheat_2028[location] <- res[[x]][[3]]
}


chg_cs_rice_2028 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_cs_rice_2028[location] <- res[[x]][[4]]
}

chg_cs_peanuts_2028 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_cs_peanuts_2028[location] <- res[[x]][[5]]
}
chg_cs_2028 <- tibble(
    chg_cs_corn_2028,
    chg_cs_soy_2028,
    chg_cs_wheat_2028,
    chg_cs_rice_2028,
    chg_cs_peanuts_2028) %>% 
    mutate(cs_sum_2028 = rowSums(.[1:5])) %>%
    gather(key = "key", value = "value")


cs_2028 <- ggplot(chg_cs_2028, aes(value)) + 
    stat_ecdf(geom = "step", pad = FALSE) +
    facet_grid(rows = vars(key)) +
    labs(x = "Change in Consumer Surplus", 
         title = 'Empirical Cumluative Distribution\nChange in Consumer Surplus for 2028')+ 
    scale_x_continuous(labels = dollar)

#2036
simaddy <- seq(from = 20, to = length(res), by = 20)
chg_cs_corn_2036 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_cs_corn_2036[location] <- res[[x]][[1]]
}

chg_cs_soy_2036 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_cs_soy_2036[location] <- res[[x]][[2]]
}

chg_cs_wheat_2036 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_cs_wheat_2036[location] <- res[[x]][[3]]
}


chg_cs_rice_2036 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_cs_rice_2036[location] <- res[[x]][[4]]
}

chg_cs_peanuts_2036 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_cs_peanuts_2036[location] <- res[[x]][[5]]
}
chg_cs_2036 <- tibble(
    chg_cs_corn_2036,
    chg_cs_soy_2036,
    chg_cs_wheat_2036,
    chg_cs_rice_2036,
    chg_cs_peanuts_2036) %>% 
    mutate(cs_sum_2036 = rowSums(.[1:5])) %>%
    gather(key = "key", value = "value")


cs_2036 <- ggplot(chg_cs_2036, aes(value)) + 
    stat_ecdf(geom = "step", pad = FALSE) +
    facet_grid(rows = vars(key)) +
    labs(x = "Change in Consumer Surplus", 
         title = 'Empirical Cumluative Distribution\nChange in Consumer Surplus for 2036')+ 
    scale_x_continuous(labels = dollar)
cs_2036


#changes in producer surplus==============
#2020
simaddy <- seq(from = 3, to = length(res), by = 20)
chg_ps_corn_2020 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_ps_corn_2020[location] <- res[[x]]
}
simaddy <- seq(from = 6, to = length(res), by = 20)
chg_ps_soy_2020 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_ps_soy_2020[location] <- res[[x]]
}
simaddy <- seq(from = 9, to = length(res), by = 20)
chg_ps_wheat_2020 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_ps_wheat_2020[location] <- res[[x]]
}
simaddy <- seq(from = 12, to = length(res), by = 20)
chg_ps_rice_2020 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_ps_rice_2020[location] <- res[[x]]
}
simaddy <- seq(from = 15, to = length(res), by = 20)
chg_ps_peanuts_2020 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_ps_peanuts_2020[location] <- res[[x]]
}
chg_ps_2020 <- tibble(
    chg_ps_corn_2020,
    chg_ps_soy_2020,
    chg_ps_wheat_2020,
    chg_ps_rice_2020,
    chg_ps_peanuts_2020) %>% 
    mutate(ps_sum_2020 = rowSums(.[1:5])) %>%
    gather(key = "key", value = "value")


ps_2020 <- ggplot(chg_ps_2020, aes(value)) + 
    stat_ecdf(geom = "step", pad = FALSE) +
    facet_grid(rows = vars(key)) +
    labs(x = "Change in Producer Surplus", 
         title = 'Empirical Cumluative Distribution\nChange Sum Across Counties in Producer Surplus\nfor 2020')+ 
    scale_x_continuous(labels = dollar)
ps_2020

#2028
simaddy <- seq(from = 4, to = length(res), by = 20)
chg_ps_corn_2028 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_ps_corn_2028[location] <- res[[x]]
}
simaddy <- seq(from = 7, to = length(res), by = 20)
chg_ps_soy_2028 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_ps_soy_2028[location] <- res[[x]]
}
simaddy <- seq(from = 10, to = length(res), by = 20)
chg_ps_wheat_2028 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_ps_wheat_2028[location] <- res[[x]]
}
simaddy <- seq(from = 13, to = length(res), by = 20)
chg_ps_rice_2028 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_ps_rice_2028[location] <- res[[x]]
}
simaddy <- seq(from = 16, to = length(res), by = 20)
chg_ps_peanuts_2028 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_ps_peanuts_2028[location] <- res[[x]]
}
chg_ps_2028 <- tibble(
    chg_ps_corn_2028,
    chg_ps_soy_2028,
    chg_ps_wheat_2028,
    chg_ps_rice_2028,
    chg_ps_peanuts_2028) %>% 
    mutate(ps_sum_2028 = rowSums(.[1:5])) %>%
    gather(key = "key", value = "value")


ps_2028 <- ggplot(chg_ps_2028, aes(value)) + 
    stat_ecdf(geom = "step", pad = FALSE) +
    facet_grid(rows = vars(key)) +
    labs(x = "Change in Producer Surplus", 
         title = 'Empirical Cumluative Distribution\nChange Sum Across Counties in Producer Surplus\nfor 2036')+ 
    scale_x_continuous(labels = dollar)
ps_2028

#2036
simaddy <- seq(from = 5, to = length(res), by = 20)
chg_ps_corn_2036 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_ps_corn_2036[location] <- res[[x]]
}
simaddy <- seq(from = 8, to = length(res), by = 20)
chg_ps_soy_2036 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_ps_soy_2036[location] <- res[[x]]
}
simaddy <- seq(from = 11, to = length(res), by = 20)
chg_ps_wheat_2036 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_ps_wheat_2036[location] <- res[[x]]
}
simaddy <- seq(from = 14, to = length(res), by = 20)
chg_ps_rice_2036 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_ps_rice_2036[location] <- res[[x]]
}
simaddy <- seq(from = 17, to = length(res), by = 20)
chg_ps_peanuts_2036 <- c()
location <- 0
for (x in simaddy) {
    location <- location + 1
    chg_ps_peanuts_2036[location] <- res[[x]]
}
chg_ps_2036 <- tibble(
    chg_ps_corn_2036,
    chg_ps_soy_2036,
    chg_ps_wheat_2036,
    chg_ps_rice_2036,
    chg_ps_peanuts_2036) %>% 
    mutate(ps_sum_2036 = rowSums(.[1:5])) %>%
    gather(key = "key", value = "value")


ps_2036 <- ggplot(chg_ps_2036, aes(value)) + 
    stat_ecdf(geom = "step", pad = FALSE) +
    facet_grid(rows = vars(key)) +
    labs(x = "Change in Producer Surplus", 
         title = 'Empirical Cumluative Distribution\nChange Sum Across Counties in Producer Surplus\nfor 2036')+ 
    scale_x_continuous(labels = dollar)
ps_2036

#put them together========
chg_s <- bind_rows(list(chg_cs_2020,chg_cs_2028,chg_cs_2036,
          chg_ps_2020,chg_ps_2028,chg_ps_2036))

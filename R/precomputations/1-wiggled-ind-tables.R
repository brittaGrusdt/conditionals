library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
# Generate tables ---------------------------------------------------------
n <- 1000
pa <- runif(n)
pc <- runif(n)

tables <- tibble(`AC`= pa * pc , `A-C`= pa * (1-pc), `-AC` = (1-pa) * pc,
                 `-A-C`= (1-pa) * (1-pc)) %>% rowid_to_column("table_id")

tables <- tables %>% gather(`AC`, `A-C`, `-AC`, `-A-C`, key=cell, value=entry)

# Wiggle tables wiggle_n times with different noise parameters v ------------------
noise_params <- c(10, 50, 100, 250, 500)
wiggled_all <- list()
wiggle_n <- 500
idx_v <- 1
for(v in noise_params){
  print(paste('wiggle tables with noise_v=', v))
  tables_new <- tables %>% mutate(alpha=entry*v, beta=(1-entry)*v)
  
  # wiggle each of the n tables 100 times
  wiggled_tables <- list()
  for(i in seq(1, wiggle_n)){
    t <- tables_new %>% 
      mutate(entry_wiggled= round(rbeta(n*4, alpha, beta), 4)) %>% 
      group_by(table_id) %>% 
      mutate(c=sum(entry_wiggled), 
             entry_wiggled=entry_wiggled/c,
             c=sum(entry_wiggled)
      ) %>% 
      select(-c)
    wiggled_tables[[i]] <- t
  }
  wiggled <- bind_rows(wiggled_tables, .id = "wiggle_id")
  
  wiggled_all[[idx_v]] <- wiggled
  idx_v <- idx_v + 1
}
wiggled_tibbles <- tibble(noise_v=noise_params, tables=wiggled_all)
saveRDS(wiggled_tibbles, "./data/precomputations/wiggled_tibbles.rds")

# Compute Log Likelihood and measurements ---------------------------------
measurements <- list()
for(i in seq(1, nrow(wiggled_tibbles))){
  print(paste('compute measurements for tables with noise_v =', wiggled_tibbles$noise_v[[i]]))
  wiggled <- wiggled_tibbles[i,]$tables[[1]]
  tables <- wiggled %>%
    mutate(logLik=log(dbeta(entry_wiggled, alpha, beta))) %>% 
    group_by(table_id, wiggle_id) %>% 
    mutate(logLik=sum(logLik)) %>% 
    filter(is.finite(logLik))
  
  new_tables <- tables %>% select(c(-beta, -alpha, -entry)) %>%  
    spread(key=cell, value=entry_wiggled) %>% 
    mutate(p_c= `-AC`+`AC`,
           p_a= `A-C`+`AC`,
           p_c_a= `AC` / p_a, 
           p_c_na= `-AC` / (1-p_a),
           x_1= `AC` - (p_c * p_a),
           x_2= p_c_a - p_c_na,
           x_3= p_c_a - p_c,
    ) %>%
    gather(x_1, x_2, x_3, key="measurement", value="strength") %>% 
    mutate(strength=round(strength, 7)) %>% 
    filter(is.finite(strength))
  
  measurements[[i]] <- new_tables    
}

measurement_tibbles <- tibble(noise_v=noise_params, table_measurements=measurements)
saveRDS(measurement_tibbles, "./data/precomputations/wiggled-tibbles-measurements.rds")


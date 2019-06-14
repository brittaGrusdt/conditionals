#library(Metrics)
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

noise_params <- c(10, 50, 100, 250)
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




# find good starting points for optimization -----------------------------------------------

log_likelihood <- function(params, data){
  if(length(unique(data$measurement)) != 1){stop("several metrics mixed")}
  llhs <- data %>% summarize(logL = sum(log(dbeta(abs(strength), params[[1]],
                                                  params[[2]]))))
  sum(llhs$logL)
}

negative_log_likelihood <- function(params, data){
  ll <- log_likelihood(params, data)
  return(-ll)
}

#  for particular noise param and metric
v <- 250
which_metric <- 3

new_tables <- measurement_tibbles %>% filter(noise_v==v) %>% pull(table_measurements)
new_tables <- new_tables[[1]]
metrics <- new_tables %>%
  select(measurement, strength, table_id, wiggle_id)

values_m <- levels(as.factor(metrics$measurement))
metrics_single <- list()
best_grid_param <- list()
for(i_m in c(which_metric)){
  m <- values_m[[i_m]]
  metric_m <- metrics %>% filter(measurement == m)
  metrics_single[[`m`]] <- metric_m
  metric_m_log_likelihood <- function(alpha, beta){
    log_likelihood(c(alpha, beta), metric_m)
  }
  
  models <- tibble(
    beta=c(10, 50, 100, 120),
    alpha=1
  )
  
  models <- models %>% mutate(logL= purrr::map2_dbl(alpha, beta,
                                                    metric_m_log_likelihood))
  
  p <- models %>% 
        ggplot(aes(beta, logL)) + 
        geom_point(aes(colour = logL)) + 
        labs(title=m)
  print(p)
  
  best <- models %>% filter(logL==max(logL)) %>% select(beta, logL)
  best_grid_param[[`m`]] <- best$beta
  print(paste('m=', m, ':alpha=1, beta=', best$beta, "logL:", best$logL))
  
  p <- metric_m %>% ggplot() + 
        geom_density(mapping=aes(x=abs(strength))) + 
        geom_density(mapping=aes(x=pdf), color="green",
                      data=tibble(pdf=rbeta(nrow(metric_m), 1, best$beta))) +
        labs(title = m)
  print(p)
}


# approximate distribution of measurements ------------------------------------
# best approximation for particular noise param--------------------------------
v <- 250
beta_starts <- tibble(x_1=130,
                      x_2=10,
                      x_3=50
)
values_m <- c("x_1", "x_2", "x_3")
best_optim_params <- list()
for(m in values_m){
  print(paste('optimize for noise param:', v, 'and metric:', m))
  new_tables <- measurement_tibbles %>% filter(noise_v==v) 
  new_tables <- new_tables$table_measurements[[1]]
  
  metric_m <- new_tables %>%
    select(measurement, strength, table_id, wiggle_id) %>% 
    mutate(strength=abs(strength)) %>% 
    filter(measurement==m)
  
  best <- optim(c(1, beta_starts[[`m`]]), fn = negative_log_likelihood,
                    data = metric_m)
    
  model <- tibble(x=rbeta(length(metric_m$strength),
                          best$par[[1]], best$par[[2]]))
    
  p <- ggplot() +
    geom_density(aes(x=x), data=tibble(x=abs(metric_m$strength))) +
    geom_density(aes(x=x), data = model, color="green")
  
  print(p)
  # simulate p-values:
  # given that the computed strength values (from the wiggled tables) are distributed
  # under the model, the number of values that are greater than X (P(x<=X)=0.95) should
  # be small
  x <- qbeta(0.95, best$par[[1]], best$par[[2]])
  nb_greater_x <- metric_m %>% filter(strength >= x) %>% nrow()
  proportion_greater_x <- nb_greater_x / nrow(metric_m)
  
  best_optim_params[[`m`]] <- tibble(alpha_hat=best$par[[1]],
                                     beta_hat=best$par[[2]],
                                     p_level=0.95,
                                     p_simulated=proportion_greater_x)
  print(best$par)
}

best_optim_params$x_1$metric <- "P(A,C)-P(A)*P(C)"
best_optim_params$x_2$metric <- "P(C|A)-P(C|-A)"
best_optim_params$x_3$metric <- "P(C|A)-P(C)"

best_optim_params <- bind_rows(best_optim_params, .id="metric_id")
best_optim_params$noise_v <- v
saveRDS(best_optim_params, paste("./data/precomputations/best-params-v-", v, ".rds", sep=""))















# Version 2 with density --------------------------------------------------
# densities <- strengths %>% density()
# observations <- tibble(x_obs=densities$x, y_obs=densities$y) %>% 
#   filter(x_obs>0)
# 
# y_obs <- observations$y_obs
# x_obs <-  observations$x_obs
# 
# compute_mse <- function(params){
#   y_pred <- dbeta(x_obs, params[[1]], params[[2]])
#   return(mse(y_obs, y_pred))
# }
# 
# params1 <- optim(c(1,10), fn = compute_mse)
# params1
# predictions <- tibble(x=x_obs, y_pred=dbeta(x, params1$par[[1]], params1$par[[2]]))
# 
# ggplot() +
#   geom_point(aes(x=x_obs, y=y_obs), data=observations) +
#   geom_point(aes(x=x, y=y_pred, color="green"), data=predictions)


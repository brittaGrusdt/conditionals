library(Metrics)

# Generate tables ---------------------------------------------------------
n <- 1000
pa <- runif(n)
pc <- runif(n)

tables <- tibble(`AC`= pa * pc , `A-C`= pa * (1-pc), `-AC` = (1-pa) * pc,
                 `-A-C`= (1-pa) * (1-pc)) %>% rowid_to_column("table_id")


# Wiggle tables n times ------------------------------------------------------
v <- 100
tables <- tables %>% gather(`AC`, `A-C`, `-AC`, `-A-C`, key=cell, value=entry)
tables <- tables %>% mutate(alpha=entry*v, beta=(1-entry)*v)

# wiggle each of the n tables 100 times
wiggled_tables <- list()
for(i in seq(1, 100)){
  if(i %% 100==0){print(i)}
  t <- tables %>% 
        mutate(entry_wiggled= rbeta(n*4, alpha, beta)) %>% 
        group_by(table_id) %>% 
        mutate(c=sum(entry_wiggled), 
               entry_wiggled=entry_wiggled/c,
               c=sum(entry_wiggled)
              ) %>% 
        select(-c)
  wiggled_tables[[i]] <- t
}
wiggled <- bind_rows(wiggled_tables, .id = "wiggle_id")



# Compute Log Likelihood --------------------------------------------------

tables <- wiggled %>%
          mutate(logLik=log(dbeta(entry_wiggled, alpha, beta))) %>% 
          group_by(table_id, wiggle_id) %>% 
          mutate(logLik=sum(logLik)) %>% 
          filter(is.finite(logLik))

# compute measurements ----------------------------------------------------

new_tables <- tables %>% select(c(-beta, -alpha, -entry)) %>%  
              spread(key=cell, value=entry_wiggled) %>% 
              mutate(p_c= `-AC`+`AC`,
                     p_a= `A-C`+`AC`,
                     p_c_a= `AC` / p_a, 
                     p_c_na= `-AC` / (1-p_a),
                     x_1= `AC` - (p_c* p_a),
                     x_2= p_c_a - p_c_na,
                     x_3= p_c_a - p_c,
                     ) %>%
              gather(x_1, x_2, x_3, key="measurement", value="strength") %>% 
              mutate(strength=round(strength, 7)) %>% 
              filter(is.finite(strength))

saveRDS(new_tables, paste("wiggled-tables-v-", v, ".rds", sep=""))




# plot relation between log likelihood and measurements -------------------
new_tables <- readRDS(paste("wiggled-tables-v-", v, ".rds", sep=""))

metrics <- new_tables %>%
              select(measurement, strength, table_id, wiggle_id) 
# spread(key=measurement, value=strength) 

new_tables  %>% 
  ggplot() + 
  geom_point(mapping=aes(x=strength, y=logLik, color=measurement)) +
  scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2') +
  # scale_x_continuous(trans='log10') +
  # scale_y_continuous(trans='log10') +
  facet_wrap(~measurement, scales="free")


# plot distribution of measurements ---------------------------------------
metrics %>% ggplot() + 
              geom_density(mapping=aes(x=abs(strength), color=measurement)) +
              facet_wrap(~measurement, scales="free")

metrics %>% ggplot() + 
  geom_density(mapping=aes(x=strength, color=measurement)) +
  facet_wrap(~measurement, scales="free")


# approximate distribution of measurements ------------------------------------

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

values_m <- levels(as.factor(metrics$measurement))
metrics_single <- list()
best_grid_param <- list()
for(i_m in c(2)){
  m <- values_m[[i_m]]
  metric_m <- metrics %>% filter(measurement == m)
  metrics_single[[`m`]] <- metric_m
  metric_m_log_likelihood <- function(alpha, beta){
    log_likelihood(c(alpha, beta), metric_m)
  }
  
  models <- tibble(
    beta=seq(5, 15),
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


# best approximation ------------------------------------------------------
beta_starts <- tibble(x_1=40,
                      x_2=11,# x_2=24,
                      x_3=40
                      )
best_optim_params <- list()
for(i_m in c(2)){
  m <- values_m[i_m]
  # metric_m <- metrics %>% filter(measurement == m)
  metric_m <- metrics_single[[`m`]]
  best <- optim(c(1, beta_starts[[`m`]]), fn = negative_log_likelihood,
                data = metric_m)

  model <- tibble(x=rbeta(length(metric_m$strength),
                          best$par[[1]], best$par[[2]]))
  
  p <- ggplot() +
        geom_density(aes(x=x), data=tibble(x=abs(metric_m$strength))) +
        geom_density(aes(x=x), data = model, color="green")

  print(p)
  # check p-values ----------------------------------------------------------
  
  x <- qbeta(0.95, best$par[[1]], best$par[[2]])
  nb_greater_x <- metric_m %>% filter(strength >= x) %>% nrow()
  proportion_greater_x <- nb_greater_x / nrow(metric_m)

  best_optim_params[[`m`]] <- tibble(alpha_hat=best$par[[1]],
                                     beta_hat=best$par[[2]],
                                     p_level=0.95,
                                     p_simulated=proportion_greater_x)
  print(best$par)
}

saveRDS(best_optim_params, paste("best-params-v-", v, ".rds"))


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


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

# find good starting points for optimization ----------------------------------
#  for particular noise param and metric
v <- 250
m <- "x_1"

measurements <- readRDS("./data/precomputations/wiggled-tibbles-measurements.rds")

new_tables <- measurements %>% filter(noise_v==v) %>%
  pull(table_measurements)
data_value_m <- new_tables[[1]] %>% filter(measurement==m) %>%
                select(measurement, strength, table_id, wiggle_id) %>% 
                add_column(noise_v=v)

metric_m_log_likelihood <- function(alpha, beta){
  log_likelihood(c(alpha, beta), data_value_m)
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
best

p <- data_value_m %>% ggplot() + 
      geom_density(mapping=aes(x=abs(strength))) + 
      geom_density(mapping=aes(x=pdf), color="green",
                    data=tibble(pdf=rbeta(nrow(data_value_m), 1, best$beta))) +
      labs(title = m)
print(p)



# approximate distribution of measurements ------------------------------------
# best approximation for particular noise params
alpha0 <- 1
beta0 <- 120

best_optim_params <- list()
print(paste('optimize for noise param:', v, 'and metric:', m))

best <- optim(c(alpha0, beta0), fn = negative_log_likelihood,
                  data = data_value_m)
  
model <- tibble(x=rbeta(length(data_value_m$strength),
                        best$par[[1]], best$par[[2]]))
  
p <- ggplot() +
  geom_density(aes(x=x), data=tibble(x=abs(data_value_m$strength))) +
  geom_density(aes(x=x), data = model, color="green") +
  xlim(0,0.03)

p


best_optim_params <- tibble(alpha_hat=best$par[[1]],
                            beta_hat=best$par[[2]],
                            value_m=m,
                            noise_v=v
                            )
best_optim_params

saveRDS(best_optim_params,
        paste("./data/precomputations/best-params.rds", sep=""))



# repeatedly sample n data points from model ------------------------------
alpha <- best_optim_params$alpha_hat
beta <- best_optim_params$beta_hat

ll_y_rep <- list()
for(i in seq(1,10**3)){
  # if(i %% 100 == 0){print(i)}
  y_rep <- rbeta(n, alpha, beta)
  ll_y_rep[[i]] <-  sum(dbeta(y_rep, alpha, beta))
}
ll_rep <- tibble(ll_y_rep) %>% unnest()

saveRDS(ll_rep, "./data/precomputations/ll-y_rep.rds")












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


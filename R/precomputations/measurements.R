source("R/helpers.R")
library(fitdistrplus)
# library(MASS)
library(dplyr)
library(tidyr)

tables <- readRDS("/home/britta/UNI/Osnabrueck/MA-project/conditionals/data/precomputations/model-general/tables-all.rds")
tables_long <- tables %>% filter(noise_v==500 & seed==1234 & n_tables == 1000) %>% unnest()
tables_long <- tables_long %>% rename(cell=vs, bn_id=id, val=ps) %>% add_column(level="none")


tables_wide <- tables_long %>% spread(key=cell, val=val) %>% dplyr::select(-noisy_or_beta, -noisy_or_theta)

tables <- tables_wide %>% compute_cond_prob("P(C|A)") %>% rename(p_c_given_a=p)
tables <- tables %>% compute_cond_prob("P(-C|-A)") %>% mutate(p_c_given_na=1-p) %>% dplyr::select(-p)
tables <- tables %>% mutate(pc=`AC`+`-AC`, pa=`AC`+`A-C`)

tables <- tables %>% mutate(x1=p_c_given_a-p_c_given_na,
                            x2=p_c_given_a-pc,
                            x3=(pc*pa)-`AC`)


measures <- tables %>% gather(x1, x2, x3, key=metric, val=p)

measures %>% ggplot() + 
  geom_density(aes(x=p, colour=metric)) + 
  facet_wrap(~cn, scales="free")

measures %>% filter(cn=="A || C") %>% ggplot() + 
  geom_density(aes(x=p, colour=metric)) + 
  facet_wrap(~cn, scales="free")


tables_ind <- tables %>% filter(cn=="A || C")
# x3
x <- tables_ind %>% filter(!is.na(x1)) %>% pull(x1)
model <- fitdist(x, "norm")
plot(model)

# x2
x <- tables_ind %>% filter(!is.na(x2)) %>% pull(x2)
model <- fitdist(x, "norm")
plot(model)

# x1
x <- tables_ind %>% filter(!is.na(x1)) %>% pull(x1)
model <- fitdist(x, "norm")
plot(model)

params <- model$estimate %>% as.list()


ll_ind <- tables %>% mutate(ll_ind=dnorm(x3, mean=params$mean, sd=params$sd))

ll_ind %>% ggplot() +
  geom_density(aes(x=ll_ind)) +
  facet_wrap(~cn, scales="free")

ll_ac <- tables %>% mutate(ll_ac=dbeta(p_c_given_a, 10, 1))



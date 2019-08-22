library(tidyverse)
library(latex2exp)


target_dir <- paste("data", "figs", sep=.Platform$file.sep)


# Distributions for parameter sweep ---------------------------------------
plot_beta <- function(params, xlab){
  p <- ggplot(data = data.frame(x = c(0, 1)), aes(x)) + scale_y_continuous(breaks = NULL)  
  for(i in seq(1, nrow(params))){
    p <- p + stat_function(fun = dbeta, n = 101, args = list(shape1 = params$alpha[[i]],
                                                             shape2 = params$beta[[i]]),
                           col = params$col[[i]]) + ylab("")
  }
  p <- p + labs(x = xlab) + theme_classic() + theme(text = element_text(size= 15))
  return(p)
}

# plot beta distributions for theta
params <- tibble(alpha=seq(4, 13), beta=1, col=rainbow(length(seq(4,13))))
# plot_beta(params, xlab=TeX("$\\theta$"))
p<-plot_beta(tibble(alpha=c(15), beta=1, col=rainbow(length(c(15)))), "")
ggsave(paste(target_dir, "beta-right.png", sep=.Platform$file.sep), p)

# plot beta distributions for background noise beta
params <- tibble(alpha=1, beta=c(10,15,20), col=rainbow(3))
plot_beta(params, xlab=TeX("$\\beta$"))



# Plot informativity of utterances

data_dir <- file.path(".", "data", "precomputations", "model-general",
                      fsep = .Platform$file.sep)

path_tables <- file.path(data_dir, "tables-all.rds", fsep = .Platform$file.sep)
tables <- read_rds(path_tables) %>%
            filter(n_tables==500 & noise_v==250 & param_nor_beta==10 & param_nor_theta==10) %>% 
            select(ps, vs) %>% unnest() %>%  rename(cell=vs, val=ps, bn_id=id) %>% 
            add_column(level="prior")
  
df1 <- marginalize(tables, c("A")) %>% ungroup() %>% select(bn_id, p) %>%
        mutate(pna=1-p) %>% rename(pa=p) %>% gather(pa, pna, key=val, val=p)
df2 <- marginalize(tables, c("C")) %>% ungroup() %>% select(bn_id, p) %>% 
        mutate(pnc=1-p) %>% rename(pc=p) %>% gather(pc, pnc, key=val, val=p)

df3 <- marginalize(tables, c("A", "C")) %>% ungroup() %>% select(bn_id, p) %>% add_column(val="pac")
df4 <- marginalize(tables, c("A", "-C")) %>% ungroup() %>% select(bn_id, p) %>% add_column(val="panc")
df5 <- marginalize(tables, c("-A", "C")) %>% ungroup() %>% select(bn_id, p) %>% add_column(val="pnac")
df6 <- marginalize(tables, c("-A", "-C")) %>% ungroup() %>% select(bn_id, p) %>% add_column(val="pnanc")
marginals <- bind_rows(df1,df2, df3, df4, df5, df6)

tables_wide <- tables %>% spread(key=cell, val=val)
df7 <- tables_wide %>% compute_cond_prob("P(C|A)") %>% ungroup() %>% select(bn_id, p) %>% 
  mutate(pnc_a=1-p) %>% rename(pc_a=p) %>% gather(pc_a, pnc_a, key=val, val=p)
df8 <- tables_wide %>% compute_cond_prob("P(-C|-A)") %>% ungroup() %>% select(bn_id, p) %>% 
  mutate(pc_na=1-p) %>% rename(pnc_na=p) %>% gather(pc_na, pnc_na, key=val, val=p)
df9 <- tables_wide %>% compute_cond_prob("P(A|C)") %>% ungroup() %>% select(bn_id, p) %>% 
  mutate(pna_c=1-p) %>% rename(pa_c=p) %>% gather(pa_c, pna_c, key=val, val=p)
df10 <- tables_wide %>% compute_cond_prob("P(-A|-C)") %>% ungroup() %>% select(bn_id, p) %>%
  mutate(pa_nc=1-p) %>% rename(pna_nc=p) %>% gather(pa_nc, pna_nc, key=val, val=p)

conditionals <- bind_rows(df7, df8, df9, df10) 

data <- bind_rows(conditionals, marginals)
data <- data %>% mutate(u_holds=case_when(p>=0.9 ~ TRUE,
                                          TRUE ~ FALSE),
                        u_maybe=case_when(p>=0.5 ~ TRUE,
                                          TRUE  ~ FALSE))
summary <- data %>% group_by(val) %>% summarize(s=sum(u_holds))

data %>% filter(val=="pc_a" | val=="pc") %>%  group_by(bn_id) %>% summarize(both=sum(u_holds)==2) -> s2 


# fn_utts <- paste("utterances-none.rds", sep="")
# path_utterances <- file.path(data_dir, fn_utts, fsep = .Platform$file.sep)
# utterances <- read_rds(path_utterances)
                                             
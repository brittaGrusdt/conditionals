library(tidyverse)
library(ggplot2)
source("R/default-model/helpers-tables.R")
source("R/plot-functions.R")

RESULT_DIR <- "data"
TARGET_DIR <- file.path(RESULT_DIR, "default-model", "figs", fsep=.Platform$file.sep)
dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)


params <- tibble(n_tables=500,
                 nor_beta=NA,
                 nor_theta=NA,
                 param_nor_beta=10,
                 param_nor_theta=10,
                 indep_sigma=0.001,
                 bias="none",
                 alpha=3,
                 cost_conditional=0,
                 seed=1234)


# Distributions of table entries ------------------------------------------
table_params <- params
table_params$n_tables <- 100000
tables_path <- file.path("data", "default-model", "tables-all.rds",
                         fsep=.Platform$file.sep)

if(!file.exists(tables_path)){
  tables <- create_tables(table_params, tables_path)
} else {
  tables <- readRDS(tables_path) %>% filter_tables(table_params)
  if(nrow(tables)==0){
    tables <- create_tables(table_params, tables_path)
  }
}

tables <- tables %>% unnest_tables()
tables_filtered <- tables %>% filter(cn=="A implies C" | cn=="A || C")
plot_tables(tables)

tables_long <- tables %>%  select(rowid, val, cell, cn) %>% rename(bn_id=rowid)
tables_wide <- tables_long %>% spread(key=cell, val=val)

# Default model: no bias --------------------------------------------------
fn_no_bias <- file.path(RESULT_DIR, "default-model", "results-none.rds")
data_no_bias <- read_rds(fn_no_bias)
data_wide <- data_no_bias %>% spread(key=cell, val=val)# %>% adapt_bn_ids()

pc <- marginalize(data_no_bias, c("C")) 
pa <- marginalize(data_no_bias, c("A"))

evs_all <- bind_rows(pc %>% expected_val("C"), pa %>% expected_val("A")) %>% add_column(bias="none")
evs_all
# plot_marginal_prob(data_no_bias, c("A"), evs=evs_all %>% filter(p=="A"))
p <- plot_marginal_prob(data_no_bias %>% filter(level!="prior"), c("C"))

vlines <- tibble(upper=0.9, lower=0.1)
p2 <- p + geom_vline(data=vlines, aes(xintercept=lower),
                    linetype="dotted", color="dimgray", size=1) + 
         geom_vline(data=vlines, aes(xintercept=upper),
                    linetype="dotted", color="dimgray", size=1) +
          theme_bw()
ggsave(paste(TARGET_DIR, "none-pc.png", sep=.Platform$file.sep), p2)

plot_cns_default(data_wide, save_as=paste(TARGET_DIR, "none-cns.png", sep=.Platform$file.sep))

pc_a <- compute_cond_prob(data_wide, "P(C|A)")
expected_val(pc_a, "P(C|A)")

pa_c <- compute_cond_prob(data_wide, "P(A|C)")
pa_nc <- compute_cond_prob(data_wide, "P(-A|-C)") %>%
  mutate(p=1-p) %>% filter(!is.na(p))
expected_val(pa_c, "P(A|C)")
expected_val(pa_nc, "P(A|-C)")

pnc_na <- compute_cond_prob(data_wide, "P(-C|-A)")
expected_val(pnc_na, "P(-C|-A)")


data_wide %>% group_by(level, intention) %>% summarize(marginal=sum(prob))



# Biscuit conditionals ----------------------------------------------------
fn_biscuits <- file.path(RESULT_DIR, "default-model", "results-pizza.rds")
data_biscuit <- read_rds(fn_biscuits)
data_wide <- data_biscuit %>% spread(key=cell, val=val)


pc <- marginalize(data_biscuit, c("C"))
pa <- marginalize(data_biscuit, c("A"))
evs <- bind_rows(expected_val(pc, c("C")), expected_val(pa, c("A"))) %>% add_column(bias="biscuits")
plot_evs_bar(evs, c("C"))
# plot_marginal_prob(data_biscuit, c("C"))

pa_c <- compute_cond_prob(data_wide, "P(A|C)")
pa_nc <- compute_cond_prob(data_wide, "P(-A|-C)") %>%
  mutate(p=1-p) %>% filter(!is.na(p))
expected_val(pa_c, "P(A|C)")
expected_val(pa_nc, "P(A|-C)")

# Conditional Perfection --------------------------------------------------
fn_cp <- file.path(RESULT_DIR, "default-model", "results-lawn.rds")
data_cp <- read_rds(fn_cp)


data_wide <- data_cp %>% spread(key=cell, val=val)
pc <- marginalize(data_cp, c("C")) 
pa <- marginalize(data_cp, c("A")) 
evs <- bind_rows(expected_val(pc, "C"), expected_val(pa, "A")) %>% add_column(bias="cp")

plot_cns_default(data_wide)

pa_c <- compute_cond_prob(data_wide, "P(A|C)")
pa_nc <- compute_cond_prob(data_wide, "P(-A|-C)") %>%
          mutate(p=1-p) %>% filter(!is.na(p))
expected_val(pa_c, "P(A|C)")
expected_val(pa_nc, "P(A|-C)")

# Value-of-interest for epistemic uncertainty (no bias) -------------------
voi_none_all <- readRDS(file.path("data", "default-model", "results-none-voi.rds"))
voi_none <- voi_none_all %>% filter(startsWith(key, "epistemic_uncertainty")) %>% filter_tables(params) %>%
                filter_by_model_params(params)

p <- voi_none %>% 
  mutate(value=round(as.numeric(value), 2)) %>% 
  ggplot() + 
  geom_bar(mapping = aes(x=level, y=value, fill=level),
           stat="identity")  +
  geom_text( aes( label = value, x = level,  y = value ),
             hjust = -0.1, size = 8) + 

  # facet_grid(intention~key,
  facet_wrap(~key,
             labeller = labeller(key = c(`epistemic_uncertainty_A` = "Antecedent",
                                         `epistemic_uncertainty_C` = "Consequent"))) +
  scale_x_discrete(limits = c("PL", "LL", "prior"), labels = c("Pragmatic interpretation",
                                                               "Literal interpretation",
                                                               "Belief before hearing 'If A, C'"),
                   position = "top") +
  scale_y_continuous(limits=c(0, 0.4)) +
  coord_flip() +
  theme(axis.text.x = element_text(size= 25),
        axis.text.y = element_text(size= 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        strip.text = element_text(size = 25),
        legend.position = "none", legend.title = element_blank(), legend.direction = "horizontal") +
  # labs(title=paste(strwrap("Degree of belief about consequent C to be true or false", width=25), collapse="\n")) +
  # labs(y=paste("Expected degree of belief about", t, "to be true or false"), x="") +
  labs(y=TeX("$F_X(1-\\theta) + 1-F_X(\\theta)$"), x="") 
  # theme_classic(base_size=20) +
p
ggsave(paste(TARGET_DIR, "none-voi-epistemic-uncertainty.png", sep=.Platform$file.sep), p, width=15, height=6)

# Value-of-interest for biscuit bias --------------------------------------
# voi_pizza_all <- readRDS(file.path("data", "default-model", "results-pizza-voi-sweep.rds"))
voi_pizza_all <- readRDS("data/default-model/results-pizza-voi.rds")
params_biscuits <- params
params_biscuits$bias <- "pizza"
voi_pizza <- voi_pizza_all %>% filter(key=="pc") %>% filter_tables(params_biscuits) %>%
  filter_by_model_params(params_biscuits) %>% mutate(bias="Biscuits")
voi_none <- voi_none_all %>% filter(key=="pc") %>% filter_tables(params) %>% filter_by_model_params(params) %>% 
  mutate(bias="Default context")
voi_data <- bind_rows(voi_pizza, voi_none)

p <- voi_data %>%
  mutate(value=round(as.numeric(value), 2)) %>% 
  ggplot() + 
  geom_bar(mapping = aes(x=level, y=value, fill=level),
           stat="identity")  +
  geom_text( aes( label = value, x = level,  y = value ),
             hjust = 1, size = 4) +
  # facet_wrap(~bias) + 
  # facet_wrap(~intention) + 
  facet_grid(bias~intention) + 
  # scale_x_discrete(limits = c("PL", "LL", "prior"),
  #                  labels = c("Pragmatic interpretation",
  #                             "Literal interpretation",
  #                             "Belief before hearing 'If A, C'"),
  #                position = "top") +
  scale_y_continuous(limits=c(0,1)) +
  coord_flip() +
  scale_fill_discrete(name="",
                      breaks=c("prior", "LL", "PL"),
                      labels=c("Belief before hearing 'If A, C'",
                               "Literal interpretation", "Pragmatic interpretation")) + 
  # theme_classic(base_size=20) +
  theme(axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    # axis.text.x = element_text(size=17),
    # axis.text.y = element_text(size = 17),
    axis.title.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    strip.text = element_text(size = 20),
    title = element_text(size = 20),
    legend.position = "right", legend.title = element_blank(), legend.direction = "vertical") +
  labs(y="Expected degree of belief in consequent", x="", title="")
p
ggsave(paste(TARGET_DIR, "pizza-voi-pc.png", sep=.Platform$file.sep), p, width=15, height=6)

# CP-strengths for lawn-bias and no-bias  ---------------------------------
# Strength measured by  **cp-bns**:
#   
# Expected value of hellinger distance between the joint probability
# distribution of each Bayes net and
# P(A,C)=0.5, P(-A,-C)=0.5 or P(A,-C)=0.5, P(-A,C)=0.5
# weighted by prior probability of respective Bayes net.
# 
# 
# Strength measured by **cp-cns**:
#   
# Hellinger distance between marginal probability over causal nets and
# P(A->C)=0.25, P(-A->-C)=0.25, P(C->A)=0.25, P(-C->-A)=0.25


# voi_lawn_all <- readRDS(file.path("data", "default-model", "results-lawn-voi-sweep.rds"))
voi_lawn_all <- readRDS("data/default-model/results-lawn-voi.rds")
voi_none_all <- readRDS("data/default-model/results-none-voi.rds")

params_cp <- params
params_cp$bias <- "lawn"

filter_vois_cp <- function(data, params){
  # data %>% filter(startsWith(key, "cp_bns") | startsWith(key, "cp_cns") | key=="p_nc_given_na") %>% 
  # data %>% filter(key=="cp_bns_ac" | key=="cp_cns_ac" | key=="p_nc_given_na") %>% 
  data %>% filter(key=="cp_bns_ac" | key=="cp_cns_ac" | key=="p_nc_given_na") %>% 
    filter_tables(params) %>%
    filter_by_model_params(params) %>% dplyr::select(level, value, key, bias)
}

voi_lawn_none <- filter_vois_cp(voi_none_all, params)

params_cp <- params %>% mutate(bias="lawn")
voi_lawn_cp <- filter_vois_cp(voi_lawn_all, params_cp)

data <- bind_rows(voi_lawn_none, voi_lawn_cp)

# plot only values for bias=none
p <- plot_cp_vois(data %>% filter(bias=="none"))
fn <- paste(TARGET_DIR, "none-vois-cp.png", sep=.Platform$file.sep)
ggsave(fn, p, width=15, height=6)


# plot only values for bias=lawn
fn <- paste(TARGET_DIR, "lawn-vois-cp.png", sep=.Platform$file.sep)
p <- plot_cp_vois(data %>% filter(bias=="lawn"))
ggsave(fn, p, width=15, height=6)



# plot comparison between cp-strengths for bias-none and bias-lawn
plot_cp_vois(data, save_as = paste(TARGET_DIR, "comparison-vois-cp.png", sep=.Platform$file.sep))


# Values of interest depending on combination of alpha and cost -----------



  
  
  
  
  
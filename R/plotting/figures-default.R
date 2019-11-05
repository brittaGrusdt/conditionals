library(tidyverse)
library(ggplot2)
source("R/helper-functions.R")
source("R/plot-functions.R")
source("R/default-model/helpers-tables.R")

# Data --------------------------------------------------------------------
TARGET_DIR <- file.path("data", "default-model", "figs", fsep=.Platform$file.sep)
dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
model_params <- list()
model_params$theta=0.9
model_params$alpha=3
model_params$cost_conditional=0

biscuit_voi <- read_rds(file.path("data", "default-model", "results-pizza-voi.rds"))
none_voi <- read_rds(file.path("data", "default-model", "results-none-voi.rds"))
cp_voi <- read_rds(file.path("data", "default-model", "results-lawn-voi.rds"))
data_voi <- bind_rows(biscuit_voi, none_voi, cp_voi) %>% filter_by_model_params(model_params)

data_biscuits <- read_rds(file.path("data", "default-model", "results-pizza.rds"))
data_default <- read_rds(file.path("data", "default-model", "results-none.rds"))
data_cp <- read_rds(file.path("data", "default-model", "results-lawn.rds"))
data_judy <- read_rds(file.path("data", "default-model", "results-judy.rds"))

data <- bind_rows(data_biscuits, data_default, data_cp, data_judy) 
data_wide <- data %>% spread(key=cell, val=val)


# Tables ------------------------------------------------------------------
table_params <- list()
table_params$n_tables <- 100000
table_params$nor_beta <- NA
table_params$nor_theta <- NA
table_params$indep_sigma <- 0.01
table_params$seed <- 1234
table_params$bias <- "none"

fn <- paste("tables-dependent-", table_params$n_tables, ".rds", sep="")
# fn <- "tables-dependent.rds"
tables_path <- file.path("data", "default-model", fn, fsep=.Platform$file.sep)

if(!file.exists(tables_path)){
  tables <- create_tables(table_params, tables_path)
} else {
  tables <- readRDS(tables_path) %>% filter_tables(table_params)
  if(nrow(tables)==0){
    tables <- create_tables(table_params, tables_path)
  }
}

tables <- tables %>% unnest_tables()
tables_filtered <- tables %>% filter(cn=="A implies C" | cn=="A || C" | cn=="C implies A")

table_plots <- plot_tables(tables_filtered)
for(i in seq(1, length(table_plots))){
  fn <- paste("table-plot-", i, ".png", sep="")
  p <- table_plots[[i]]
  ggsave(paste(TARGET_DIR, fn, sep=.Platform$file.sep), p, width=7, height=3.5)
}

# Values-of-interest ------------------------------------------------------
# 1. speaker's uncertainty 
voi_none <- data_voi %>% filter(startsWith(key, "epistemic_uncertainty") & bias=="none") 

p <- voi_none %>% mutate(value=round(as.numeric(value), 2)) %>% 
  ggplot(aes(x=level, y=value, fill=key)) + 
  geom_bar(stat="identity", position=position_dodge())  +
  geom_text(aes( label = value, x = level,  y = value ), size=5, vjust=-0.1,
            position=position_dodge(0.9)) + 
  scale_x_discrete(limits = c("prior", "LL", "PL"),
                   labels = c("Prior Belief",
                              paste(strwrap("Literal interpretation", width=15),
                                    collapse="\n"),
                              paste(strwrap("Pragmatic interpretation", width=15),
                                    collapse="\n"))) +
  scale_fill_discrete(name="",
                      breaks=c("epistemic_uncertainty_A", "epistemic_uncertainty_C"),
                      labels=c("Antecedent, X: P(A)", "Consequent, X: P(C)")
                      ) + 
  labs(y=TeX("$F_X(1-\\theta) + 1-F_X(\\theta)$"), x="") +
  theme(axis.text.x = element_text(size= 20),
        axis.text.y = element_text(size= 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.position = "bottom", legend.text = element_text(size=18))
p
ggsave(paste(TARGET_DIR, "none-voi-epistemic-uncertainty.png",
             sep=.Platform$file.sep), p, width=11, height=5)

# 2. CP-reading
voi_lawn_none <- data_voi %>%
                  filter(key=="cp_bns_ac" | key=="cp_cns_ac" | key=="p_nc_given_na") %>% 
                  filter(bias=="none" | bias=="lawn") %>% 
                  select(level, value, key, bias) %>% ungroup() %>%
                  mutate(level=as.factor(level),
                         bias=factor(bias, levels=c("none", "lawn")),
                         value=round(as.numeric(value),2)) 

p <- voi_lawn_none %>% filter(key=="p_nc_given_na") %>%
  ggplot(aes(x=level, y=value, fill=bias)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes( label = value, x = level,  y = value),  size=5, vjust=-0.1,
            position=position_dodge(0.9)) +
  labs(x="", y="", title="Expected degree of belief in P(¬C|¬A)") +
  scale_x_discrete(limits = c("prior", "LL", "PL"),
                   labels = c("Prior Belief",
                              paste(strwrap("Literal interpretation", width=20),
                                    collapse="\n"),
                              paste(strwrap("Pragmatic interpretation", width=20),
                                    collapse="\n")),
                   position="bottom") +
  theme(text = element_text(size= 20), legend.position = "bottom") + 
  scale_fill_discrete(name="Context",
                      breaks=c("none", "lawn"),
                      labels=c("Default", "CP")
                      )
p
ggsave(paste(TARGET_DIR, "comparison-vois-cp.png", sep=.Platform$file.sep),
       p, width=15, height=8)

####
# p <- plot_cp_vois(data %>% filter(bias=="none"))
# fn <- paste(TARGET_DIR, "none-vois-cp.png", sep=.Platform$file.sep)
# ggsave(fn, p, width=15, height=6)
####

# 3. marginal distribution over causal nets
df_none <- data_wide %>% filter(bias=="none") %>% group_by(level, cn) %>%
            summarize(prob=round(sum(prob), 2))
df_none$level <- factor(df_none$level, levels = c("prior", "LL", "PL"))
df_cp <- data_wide %>% filter(bias=="lawn") %>% group_by(level, cn) %>%
          summarize(prob=round(sum(prob), 2))
df_cp$level <- factor(df_cp$level, levels = c("prior", "LL", "PL"))

plot_cns <- function(data){
  p <- data %>% ggplot(aes(x=level, y=prob, fill=cn)) + 
    geom_bar(stat="identity", position=position_dodge()) + 
    geom_text(aes(x=level, y=prob, label=prob), size=5, vjust=-0.01,
              position=position_dodge(0.9)) +
    # facet_wrap(~level, labeller = labeller(
    #   level = c(`prior` = "Prior belief",
    #             `LL` = paste(strwrap("Literal interpretation", width=25),
    #                          collapse="\n"),
    #             `PL`= paste(strwrap("Pragmatic interpretation", width=25),
    #                         collapse="\n")))
    # ) + 
    labs(x="causal nets", y="probability") +
    scale_fill_discrete(limits=c("A implies C", "A implies -C",
                              "-A implies C", "-A implies -C",
                              "C implies A", "C implies -A",
                              "-C implies A", "-C implies -A",
                              "A || C"),
                     labels=c("A->C", "A->¬C", "¬A->C", "¬A->¬C",
                              "C->A", "C->¬A", "¬C->A", "¬C->¬A",
                              "A indep. C"),
                     ) +
    # scale_y_continuous(limits=c(0, 1)) +
    theme(text = element_text(size= 20),
          axis.text.x = element_text(size=20, angle=0),
          legend.position = "bottom", legend.title = element_blank(),
          legend.text = element_text(size=18)
    ) + 
    scale_x_discrete(name="",
                        breaks=c("prior", "LL", "PL"),
                        labels=c("Prior belief", "Literal interpretation",
                                 "Pragmatic interpretation"))
  return(p)
}
# no bias
p <- df_none %>% filter(prob > 0) %>%  plot_cns()
p
ggsave(paste(TARGET_DIR, "none-cns.png", sep=.Platform$file.sep), p, width=15, height=7)

# cp-bias
p <- df_cp %>% filter(prob > 0) %>% plot_cns()
p    
ggsave(paste(TARGET_DIR, "lawn-cns.png", sep=.Platform$file.sep), p, width=15, height=7)





# Biscuit conditionals ----------------------------------------------------
# 1) Speaker's degree of belief in consequent and 2) marginal speaker_intents
evs_pc <- data_voi %>% filter((key=="pc") & (bias=="pizza")) %>%
          mutate(value=round(as.numeric(value), 2))

data_wide_bc <- data_biscuits %>% spread(key=cell, val=val)
marginal_intents_bc <- data_wide_bc %>% group_by(level, intention, bias) %>%
                        summarize(p=sum(prob))

df <- marginal_intents_bc %>% add_column(key="sp_intent") %>% rename(value=p) %>% 
        bind_rows(evs_pc %>% select(-cost,-alpha)) %>% mutate(value=round(value,2))

biscuit_plots <- function(data, plot_title){
  p <-  data %>% ggplot(aes(x=level, y=value, fill=intention)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes( label = value, x = level,  y = value ),
              position = position_dodge(0.9), size=5, vjust=-0.1) +
    scale_x_discrete(limits = c("prior", "LL", "PL"),
                     labels = c("Prior belief",
                                paste(strwrap("Literal interpretation", width=15),
                                      collapse="\n"),
                                paste(strwrap("Pragmatic interpretation", width=15),
                                      collapse="\n")),
                     position="bottom") +
    labs(y=TeX(""), x="", title=plot_title) +
    theme(axis.ticks.y = element_blank(), 
          axis.text.y = element_text(size= 20),
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size= 20,  angle=0),
          strip.text = element_text(size = 20),
          legend.position = "bottom"
          #, panel.spacing = unit(1, "lines")
    )  
  return(p)
}

p <- biscuit_plots(df %>% filter(key=="sp_intent"),
                   "Marginal distribution over intentions") 
p
fn <- paste(TARGET_DIR, "pizza-intents.png", sep=.Platform$file.sep)
ggsave(fn, p, width=8, height=6)

p <- biscuit_plots(df %>% filter(key=="pc"),"Expected value of P(C)") 
p <- p + theme(legend.position="none") + labs(title="")
p
fn <- paste(TARGET_DIR, "pizza-pc.png", sep=.Platform$file.sep)
ggsave(fn, p, width=10, height=4.8)



# 3. Speaker Average distributions
fn_default <- "results-none-given-C-without-intents-avg-speaker.rds"
speaker_default <- read_rds(file.path("data", "default-model", fn_default)) %>% 
                    select(-intention)

fn_bc <- "results-pizza-given-C-with-intents-avg-speaker.rds"
# fn_bc <- "results-pizza-given-C-without-intents-avg-speaker.rds"
speaker_bc <- read_rds(file.path("data", "default-model", fn_bc)) %>% 
                unite("bias", c("intention", "bias")) %>% 
                mutate(bias=factor(bias,levels=c("isa_none", "pa_none",
                                                 "isa_pizza", "pa_pizza",
                                                 "_pizza")))
speaker <- bind_rows(speaker_default, speaker_bc) %>%
              rename(value=mean_per_intention) %>% 
              mutate(value=round(value, 2)) %>% filter(value>0)

p <- speaker %>%
      ggplot(aes(x=utterance, y=value, fill=bias)) +
        geom_bar(stat="identity", position="dodge") + 
        geom_text(aes(label=value, x=utterance,  y=value),
                  position = position_dodge(0.9), size=3.5, vjust=-0.1) +
        labs(x="", y="", title="Average speaker") +
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size=20),
              text = element_text(size= 20),
              legend.position = "bottom", legend.direction = "horizontal") + 
        scale_fill_discrete(name="context/intention",
                            breaks=c("none", "isa_pizza", "pa_pizza", "_pizza"),
                            labels=c("default", "biscuit/isa", "biscuit/pa", "biscuit"))
p
ggsave(paste(TARGET_DIR, "speaker-default-biscuits.png",
             sep=.Platform$file.sep), p, width=12, height=6)

# Default context ---------------------------------------------------------
default <- data_wide %>% filter(bias=="none") %>% compute_cond_prob("P(C|A)")

prior <- default %>% filter(level=="prior") %>%
        mutate(p=case_when(p>=0.9 ~ TRUE, TRUE ~ FALSE)) %>% 
        mutate(cn=case_when(cn=="A || C" ~ "indep", TRUE ~ "dep")) %>%
        group_by(cn, p) %>% summarize(s=sum(prob)) %>% add_column(level="prior")

posterior <- default %>% filter(level!="prior") %>%
              mutate(cn=case_when(cn=="A || C" ~ "indep", TRUE ~ "dep")) %>% 
              group_by(cn, level) %>% summarize(s=sum(prob)) %>% add_column(p=TRUE)
  
df <- bind_rows(posterior, prior) %>% mutate(s=round(s, 2)) %>% unite("cn_p", cn, p) %>% 
        mutate(level=factor(level, levels=c("prior", "LL", "PL")))

p <- df %>% ggplot(aes(x=level, y=s, fill=cn_p)) +
      geom_bar(aes(x=level, y=s, fill=cn_p), stat="identity") +
      geom_text(aes( label = s, x = level,  y = s ), size=5, vjust=-0.1,
                position=position_stack(vjust = 0.25)) +
      labs(x="", y="probability") + 
      scale_x_discrete(limits = c("prior", "LL", "PL"),
                       labels = c("Prior belief",
                                paste(strwrap("Literal interpretation", width=15),
                                      collapse="\n"),
                                paste(strwrap("Pragmatic interpretation", width=15),
                                      collapse="\n"))) +
      scale_fill_discrete(name="",
                          breaks=c("dep_FALSE", "dep_TRUE", "indep_FALSE", "indep_TRUE"),
                          labels=c("P(dep. ∧ P(C|A)<0.9)",
                                   "P(dep. ∧ P(C|A)>=0.9)",
                                   "P(indep. ∧ P(C|A)<0.9)",
                                   "P(indep. ∧ P(C|A)>=0.9)"),
                          ) +
      theme(axis.text = element_text(size=20),
            axis.title = element_text(size=20),
            legend.position = "right", legend.direction = "vertical",
            legend.text = element_text(size=18),
            legend.key.size = unit(1.5, "cm"))

p
fn <- paste(TARGET_DIR, "prior-ll-pl-dep-indep-ifac-true.png", sep=.Platform$file.sep)
ggsave(fn, p, width=14, height=4)



# Judy Benjamin -----------------------------------------------------------
df <- data %>% filter(bias=="judy")
df_wide <- data_wide %>% filter(bias=="judy")
pa <- df %>% marginalize(c("A"))
pc <- df %>% marginalize(c("C"))
expected_val(pa, "P(A)")
expected_val(pc, "P(C)")
pca <- df_wide %>% compute_cond_prob("P(C|A)")
expected_val(pca, "P(C|A)")


library(tidyverse)
library(ggplot2)
source("R/helper-functions.R")
source("R/plot-functions.R")

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

# Values-of-interest ------------------------------------------------------
# 1. speaker's uncertainty 
voi_none <- data_voi %>% filter(startsWith(key, "epistemic_uncertainty") & bias=="none") 

p <- voi_none %>% mutate(value=round(as.numeric(value), 2)) %>% 
  ggplot(aes(x=level, y=value, fill=key)) + 
  geom_bar(stat="identity", position=position_dodge())  +
  geom_text(aes( label = value, x = level,  y = value ), size=3.5, vjust=-0.1,
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
        legend.position = "bottom", legend.text = element_text(size=20))
p
ggsave(paste(TARGET_DIR, "none-voi-epistemic-uncertainty.png",
             sep=.Platform$file.sep), p, width=8, height=4)

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
  geom_text(aes( label = value, x = level,  y = value),  size=3.5, vjust=-0.1,
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
       p, width=12, height=4.5)

####
# p <- plot_cp_vois(data %>% filter(bias=="none"))
# fn <- paste(TARGET_DIR, "none-vois-cp.png", sep=.Platform$file.sep)
# ggsave(fn, p, width=15, height=6)
####

# 3. marginal distribution over causal nets
df_none <- data_wide %>% filter(bias=="none") %>% group_by(level, cn) %>%
            summarize(prob=round(sum(prob), 3))
df_none$level <- factor(df_none$level, levels = c("prior", "LL", "PL"))
df_cp <- data_wide %>% filter(bias=="lawn") %>% group_by(level, cn) %>%
          summarize(prob=round(sum(prob), 3))
df_cp$level <- factor(df_cp$level, levels = c("prior", "LL", "PL"))

plot_cns <- function(data){
  p <- data %>% ggplot(aes(x=cn, y=prob, fill=level)) + 
    geom_bar(stat="identity", position=position_dodge()) + 
    geom_text(aes(x=cn, y=prob, label=prob), size=3, vjust=-0.01,
              position=position_dodge(0.9)) +
    # facet_wrap(~level, labeller = labeller(
    #   level = c(`prior` = "Prior belief",
    #             `LL` = paste(strwrap("Literal interpretation", width=25),
    #                          collapse="\n"),
    #             `PL`= paste(strwrap("Pragmatic interpretation", width=25),
    #                         collapse="\n")))
    # ) + 
    labs(x="causal nets", y="probability") +
    scale_x_discrete(limits=c("A implies C", "A implies -C",
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
          axis.text.x = element_text(size=12),
          legend.position = "bottom", legend.title = element_blank()
    ) + 
    scale_fill_discrete(name="",
                        breaks=c("prior", "LL", "PL"),
                        labels=c("Prior belief", "Literal interpretation",
                                 "Pragmatic interpretation"))
  return(p)
}
# no bias
p <- df_none %>% plot_cns()
p
ggsave(paste(TARGET_DIR, "none-cns.png", sep=.Platform$file.sep), p, width=10, height=3)

# cp-bias
p <- df_cp %>% plot_cns()
p    
ggsave(paste(TARGET_DIR, "lawn-cns.png", sep=.Platform$file.sep), p, width=10, height=3)





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
              position = position_dodge(0.9), size=3.5, vjust=-0.1) +
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
          axis.text.x = element_text(size= 15,  angle=0),
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
ggsave(fn, p, width=6, height=5)

p <- biscuit_plots(df %>% filter(key=="pc"),"Expected value of consequent") 
p
fn <- paste(TARGET_DIR, "pizza-pc.png", sep=.Platform$file.sep)
ggsave(fn, p, width=6, height=5)



# 3. Speaker Average distributions
fn <- "results-pizza-given-C-without-intents-avg-speaker.rds"
speaker_bc <- read_rds(file.path("data", "default-model", fn))
fn <- "results-none-given-C-without-intents-avg-speaker.rds"
speaker_default <- read_rds(file.path("data", "default-model", fn))
speaker <- bind_rows(speaker_default, speaker_bc) %>% rename(value=mean_per_intention) %>% 
            mutate(value=round(value, 2))

p <- speaker %>% ggplot(aes(x=utterance, y=value, fill=bias)) +
      geom_bar(stat="identity", position="dodge") +
      geom_text(aes( label = value, x = utterance,  y = value),
            position = position_dodge(0.9), size=3.5, vjust=-0.1) +
      # facet_wrap(~bias) +
      labs(x="", y="", title="Average speaker") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size=20),
            text = element_text(size= 20),
            legend.position = c(0.8, 0.9), legend.direction = "horizontal") +
      scale_fill_discrete(name="Context",
                          breaks=c("none", "pizza"),
                          labels=c("Default", "Biscuit"))
p
fn <- paste(TARGET_DIR, "speaker-default-biscuits.png", sep=.Platform$file.sep)
ggsave(fn, p, width=12, height=5)




# Judy Benjamin -----------------------------------------------------------
df <- data %>% filter(bias=="judy")
df_wide <- data_wide %>% filter(bias=="judy")
pa <- df %>% marginalize(c("A"))
pc <- df %>% marginalize(c("C"))
expected_val(pa, "P(A)")
expected_val(pc, "P(C)")
pca <- df_wide %>% compute_cond_prob("P(C|A)")
expected_val(pca, "P(C|A)")


library(tidyverse)
library(config)
library(ggplot2)
library(latex2exp)
library(cowplot)
library(grid)
source("R/helper-functions.R")
source("R/default-model/helpers-tables.R")
source("R/plotting/plotting-functions.R")

params <- configure(c('none', "debug"))
dat.none = read_rds(paste(params$target_dir, params$target_fn, sep=.Platform$file.sep))
dat.none_wide <- dat.none %>% spread(key=cell, val=val)

# 0. Data --------------------------------------------------------------------
PLOT_DIR <- file.path(params$target_dir, "figs", fsep=.Platform$file.sep)
dir.create(PLOT_DIR, recursive = TRUE)

# speaker plots -----------------------------------------------------------
params_unc <- configure(c("none", "speaker_uncertain", "debug"))
params_cert <- configure(c('none', 'speaker_certain', 'debug'))

fn_unc <- paste(str_sub(params_unc$target_fn, 1, -5), "-avg.rds", sep="");
dat.unc <- read_rds(paste(params_unc$target_dir, fn_unc, sep=.Platform$file.sep)) %>% 
  select(-intention, -bias) %>% add_column(condition="uncertain") %>%
  arrange(mean_per_intention)

fn_cert <- paste(str_sub(params_cert$target_fn, 1, -5), "-avg.rds", sep="");
dat.cert <- read_rds(paste(params_cert$target_dir, fn_cert, sep=.Platform$file.sep)) %>% 
  select(-intention, -bias)  %>% add_column(condition="certain") %>% 
  arrange(mean_per_intention)

data <- bind_rows(dat.cert, dat.unc) %>% rename(p=mean_per_intention) 
data <- within(data, utterance <- factor(utterance, levels=dat.cert$utterance))

p <- data %>% mutate(p=round(as.numeric(p), 2)) %>% 
  ggplot(aes(x=utterance, y=p, fill=condition)) + 
  geom_bar(stat="identity", position=position_dodge(preserve = "single"))  +
  # facet_wrap(~condition) +
  scale_y_continuous(limits=c(0,0.25)) +
  labs(y=TeX("$\\frac{1}{|S_{c/u}|} \\cdot \\sum_{s \\in S_{c/u}} P_S(u|s)$"), x="utterance") +
  # theme_classic(base_size=25) +
  theme_bw(base_size=25) +
  theme(legend.position = c(.2, .95), legend.justification=c("right", "top"),
        axis.text.x = element_text(angle = 30, hjust = 1)) 
  # theme(legend.position="right", panel.spacing = unit(2, "lines"))
  # coord_flip()
p

ggsave(paste(PLOT_DIR, "speaker.png",
             sep=.Platform$file.sep), p, width=13, height=5)



# biscuit_voi_intents <- read_rds(
#   file.path("data", "default-model", "results-pizza-with-intents-voi.rds"))
# biscuit_voi_no_intents <- read_rds(
#   file.path("data", "default-model", "results-pizza-without-intents-voi.rds"))

# none_voi <- read_rds(file.path("data", "default-model", "results-none-voi.rds"))
# cp_voi <- read_rds(file.path("data", "default-model", "results-lawn-voi.rds"))
# data_voi <- bind_rows(biscuit_voi_no_intents, none_voi, cp_voi) %>%
#   filter_by_model_params(model_params)

data_voi <- params$data_voi %>% filter_by_model_params(params)

# data_biscuits_intents <- read_rds(
#   file.path("data", "default-model", "results-pizza-with-intents.rds"))
# data_biscuits_no_intents <- read_rds(
#   file.path("data", "default-model", "results-pizza-without-intents.rds"))
data_default <- read_rds(file.path("data", "default-model", "results-none.rds"))
# data_cp <- read_rds(file.path("data", "default-model", "results-lawn.rds"))
# data_judy <- read_rds(file.path("data", "default-model", "results-judy.rds"))

# data <- bind_rows(data_biscuits_no_intents, data_default, data_cp, data_judy) 
# data_wide <- data %>% spread(key=cell, val=val)

data <- data_default
data_wide <- params$data %>% spread(key=cell, val=val)

# 1. Tables ------------------------------------------------------------------
# table_params <- list()
# table_params$n_tables <- 100000
# table_params$nor_beta <- NA
# table_params$nor_theta <- NA
# table_params$indep_sigma <- 0.01
# table_params$seed <- 1234
# table_params$bias <- "none"

fn <- paste("tables-dependent-", params$n_tables, ".rds", sep="")
# fn <- "tables-dependent.rds"
tables_path <- file.path(params$target_dir, fn, fsep=.Platform$file.sep)

if(!file.exists(tables_path)){
  tables <- create_tables(params, tables_path, params$cns)
} else {
  tables <- readRDS(tables_path) %>% filter_tables(params)
  if(nrow(tables)==0){
    tables <- create_tables(params, tables_path)
  }
}

tables <- tables %>% unnest_tables()
tables_filtered <- tables %>%
  filter(cn=="A implies C" | cn=="A || C" | cn=="A,B->D->C") %>%  # cn=="C implies A") %>% 
          # cn=="-A implies C" | cn=="A implies -C") %>%
  mutate(cell=factor(cell, levels=c("AC", "A-C", "-AC", "-A-C")),
         cn=factor(cn, levels=c("A || C", "A implies C",  "A,B->D->C")))
                                # "-A implies C", "A implies -C")))

table_plots <- plot_tables(tables_filtered)
p <- plot_grid(table_plots[[1]], table_plots[[2]], table_plots[[3]],
               label_size = 12, ncol=3)
ggsave(paste(PLOT_DIR, "table_plots_filtered.png",
             sep=.Platform$file.sep), p, width=15, height=5)

table_plots <- plot_tables(tables)
p <- plot_grid(table_plots[[1]], table_plots[[2]], table_plots[[3]],
               table_plots[[4]], table_plots[[5]], table_plots[[6]],
               table_plots[[7]], table_plots[[8]], table_plots[[9]],
               label_size = 12, ncol=3)
ggsave(paste(PLOT_DIR, "table_plots_all.png",
             sep=.Platform$file.sep), p, width=10, height=15)

# 2. Prior Default context ---------------------------------------------------
# 2.1. prior P(A) and P(C)
df <- data %>% filter(bias=="none")
df <- marginalize(df, c("A")) %>% rename(pa=p) %>%
  gather('AC', '-AC', 'A-C', '-A-C', key=cell, val=val)
df_wide <- marginalize(df, c("C")) %>% rename(pc=p) %>%
  gather(pa,pc, key='marginal', val=p_marginal)

df_wide %>% ggplot(aes(x=p_marginal, color=marginal)) +
  facet_wrap(~cn) + geom_density()

# 2.2 prior table entries
df <- data %>% filter(bias=="none" & level=="prior") 

plots <- df %>% mutate(val=prob*val) %>% plot_tables()
p_cns <- df %>% spread(key=cell, val=val) %>% group_by(cn) %>% summarise(marginal=sum(prob))
all_bars <- list(); idx<-1;
for(causal_net in p_cns$cn){
  df_val <- p_cns %>% filter(cn==causal_net) %>%
    mutate(marginal=round(marginal,2), rest=1-marginal) %>% gather(marginal, rest, key=p, val=val)
  
  p <- df_val %>% ggplot(aes(x=cn, y=val, fill=p)) +
        geom_bar(stat="identity", width=0.05, position=position_stack(reverse=TRUE)) + 
        # geom_text(aes(label=val, x=cn,  y=val), hjust=1, size=6,
        #           position=position_dodge(0.9)) +
        annotate(geom="text", x=0.95, y=0.01, label="0") +
        annotate(geom="text", x=0.95, y=1, label="1") +
        scale_y_continuous(limits=c(0,1)) +
        scale_fill_manual(
          values = c("rest" = "gray", "marginal" = "red"),
          breaks = c("rest", "marginal")
        ) +
        scale_x_discrete(breaks=c(0,1), labels=c("0", "1")) +
        # coord_flip() + 
        theme_void(base_size=20) +
        theme(legend.position="none")
  all_bars[[idx]] <- p
  idx <- idx + 1
  fn <- paste("prior-3x3-plots-", idx, ".png", sep="")
  target <- paste(PLOT_DIR, fn, sep=.Platform$file.sep)
  ggsave(target, p, width=2, height=6)
}
p <- plot_grid(plots[[1]], plots[[2]], plots[[3]], 
               plots[[4]], plots[[5]], plots[[6]], 
               plots[[7]], plots[[8]], plots[[9]],
               label_size = 12, ncol=3)
target <- paste(PLOT_DIR, "prior-3x3-plots.png", sep=.Platform$file.sep)
ggsave(target, p, width=20, height=25)



# 3. Distribution of acceptance conditions -----------------------------------
df <- data %>% filter(level=="prior" & bias=="none") %>%
        spread(key=cell, val=val) %>% 
        filter(!is.infinite(p_rooij)) %>% 
        gather(p_delta, p_rooij, key="accept_cond", val="accept_cond_val") 
        

p <- df %>% ggplot(aes(x=accept_cond_val)) + geom_density() +
      facet_wrap(~accept_cond, scales="free",
                 labeller =
                   labeller(accept_cond=c(`p_delta`="△P", `p_rooij`="△*P"))
                 ) + labs(x="") + theme_classic(base_size=20)

fn <- paste(PLOT_DIR, "distributions-accept-conditions.png", sep=.Platform$file.sep)
ggsave(fn, p, width=11, height=5)


# 4. Values-of-interest ------------------------------------------------------
# 4.1 speaker given certain about A or C 
dat.none.voi <- read_rds(file.path(params$target_dir, "results-none-voi.rds",
                                   fsep = .Platform$file.sep)) %>%
  filter(startsWith(key, "epistemic_certainty") & bias=="none") 

p <- dat.none.voi %>% mutate(value=round(as.numeric(value), 2)) %>% 
  ggplot(aes(x=level, y=value, fill=key)) + 
  geom_bar(stat="identity")  +
  geom_text(aes(label = value, x = level,  y = value ), hjust=-0.1, size=6,
            position=position_dodge(0.9)) + 
  scale_x_discrete(limits = c("prior", "LL", "PL"),
                   labels = c("Prior Belief",
                              paste(strwrap("Literal interpretation", width=12),
                                    collapse="\n"),
                              paste(strwrap("Pragmatic interpretation", width=15),
                                    collapse="\n"))) +
  scale_y_continuous(limits=c(0,0.5)) +
  # scale_fill_discrete(name="",
  #                     breaks=c("epistemic_uncertainty_A", "epistemic_uncertainty_C"),
  #                     labels=c("Antecedent (X=A)", "Consequent (X=C)")
  #                     ) + 
  labs(y=TeX("$\\sum_{s\\in Certain_s(A) \\bigcup Certain_s(C)\\}} Pr(s|u)$"), x="") +
  theme_classic(base_size=25) +
  theme(legend.position = "none") +
  coord_flip()
p
ggsave(paste(PLOT_DIR, "ignorance-inferences.png",
             sep=.Platform$file.sep), p, width=11, height=5)

# 4.2 CP-reading
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
  theme_classic(base_size = 20) + 
  theme(legend.position = "bottom") + 
  scale_fill_discrete(name="Context",
                      breaks=c("none", "lawn"),
                      labels=c("Default", "CP")
                      )
p
ggsave(paste(PLOT_DIR, "comparison-vois-cp.png", sep=.Platform$file.sep),
       p, width=15, height=8)

####
# p <- plot_cp_vois(data %>% filter(bias=="none"))
# fn <- paste(PLOT_DIR, "none-vois-cp.png", sep=.Platform$file.sep)
# ggsave(fn, p, width=15, height=6)
####

# 4.3 marginal distribution over causal nets
df_none <- data_wide %>% filter(bias=="none") %>% group_by(level, cn) %>%
            summarise(prob=round(sum(prob), 2))
df_none$level <- factor(df_none$level, levels = c("prior", "LL", "PL"))
df_cp <- data_wide %>% filter(bias=="lawn") %>% group_by(level, cn) %>%
          summarise(prob=round(sum(prob), 2))
df_cp$level <- factor(df_cp$level, levels = c("prior", "LL", "PL"))

plot_cns <- function(data){
  p <- data %>% ggplot(aes(x=cn, y=prob)) + 
    geom_bar(position="dodge", stat="identity") + 
    scale_y_continuous(limits=c(0, 0.3)) +
    coord_flip() + 
    facet_wrap(~level, labeller = labeller(
      level = c(`prior` = "Prior belief",
                `LL` = paste(strwrap("Literal interpretation", width=12),
                             collapse="\n"),
                `PL`= paste(strwrap("Pragmatic interpretation", width=15),
                            collapse="\n")))) +
    geom_text(aes(x=cn, y=prob, label=prob), size=5, hjust=-0.05,
              position=position_dodge(0.9)) +
    labs(x="causal nets", y="probability") +
    theme_bw(base_size=25)
  return(p)
}
# no bias
p <- df_none %>% filter(prob > 0) %>%  plot_cns()
p
ggsave(paste(PLOT_DIR, "none-cns.png", sep=.Platform$file.sep), p, width=15, height=7)

# cp-bias
p <- df_cp %>% filter(prob > 0) %>% plot_cns()
p    
ggsave(paste(PLOT_DIR, "lawn-cns.png", sep=.Platform$file.sep), p, width=15, height=7)


# 5. Biscuit conditionals ----------------------------------------------------
biscuit_barplot <- function(data, plot_title){
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
    labs(y="", x="", title=plot_title) +
    theme_classic(base_size = 20) + 
    theme(axis.ticks.y = element_blank(), 
          legend.position = "bottom"
          #, panel.spacing = unit(1, "lines")
    )  
  return(p)
}
# 5.1. Speaker's degree of belief in consequent
evs_pc <- data_voi %>% filter((key=="pc") & (bias=="pizza")) %>%
          mutate(value=round(as.numeric(value), 2))
p <- evs_pc %>% biscuit_barplot("") +
  theme(legend.position="none")
p
fn <- paste(PLOT_DIR, "pizza-pc.png", sep=.Platform$file.sep)
ggsave(fn, p, width=12, height=5.5)

# 5.2. marginal distribution of speaker_intents
df <- data_biscuits_intents %>% spread(key=cell, val=val)
marginal_intents_bc <- df %>% group_by(level, intention, bias) %>%
  summarise(p=sum(prob)) %>% rename(value=p)

evs <- marginal_intents_bc %>% mutate(value=round(as.double(value),2))
p <- evs %>% biscuit_barplot("")
p
fn <- paste(PLOT_DIR, "pizza-intents.png", sep=.Platform$file.sep)
ggsave(fn, p, width=12, height=6)


# 5.3 speaker average distributions biscuits + default
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
        labs(x="", y="", title="") +
        theme_classic(base_size = 20) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size=20),
              legend.position = "bottom", legend.direction = "horizontal") + 
        scale_fill_discrete(name="context/intention",
                            breaks=c("none", "isa_pizza", "pa_pizza", "_pizza"),
                            labels=c("default", "biscuit/isa", "biscuit/pa", "biscuit"))
p
fn <- paste(PLOT_DIR, "speaker-default-biscuits.png", sep=.Platform$file.sep)
ggsave(fn, p, width=12, height=6)



# 6. Speaker Distributions ---------------------------------------------------
# 6.1 conditioned on p_delta / p_roooij > threshold
fn_default <- "results-none-given-p_delta-without-intents-avg-speaker.rds"
speaker_p_delta <- read_rds(file.path("data", "default-model", fn_default)) %>% 
                    select(-intention) %>% add_column(acceptance="p_delta")
fn_default <- "results-none-given-p_rooij-without-intents-avg-speaker.rds"
speaker_p_rooij <- read_rds(file.path("data", "default-model", fn_default)) %>% 
                    select(-intention) %>% add_column(acceptance="p_rooij")
speaker <- bind_rows(speaker_p_delta, speaker_p_rooij) %>%
            mutate(mean_per_intention=round(mean_per_intention, 2))

p <- speaker %>%
  ggplot(aes(x=utterance, y=mean_per_intention)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  facet_wrap(~acceptance, labeller = labeller(acceptance=c(`p_delta` = "△P>=0 ∧ P(C|A)>=0.75",
                                                           `p_rooij` = "△*P>=0.75"))) +
  geom_text(aes(label=mean_per_intention, x=utterance,  y=mean_per_intention),
            position = position_dodge(0.9), size=3.5, hjust=-0.1) +
  coord_flip() +
  labs(x="", y="", title="Average speaker") +
  theme_classic(base_size=20) +
  theme(legend.position = "none") 
p
ggsave(paste(PLOT_DIR, "speaker-given-accept-conditions-fulfilled.png",
             sep=.Platform$file.sep), p, width=15, height=6)

# 6.2 speaker wrt assertability conditions
fn_default <- "results-none-speaker.rds"
speaker_default <- read_rds(file.path("data", "default-model", fn_default)) %>% 
                    select(-intention) 
df_wide <- speaker_default %>% group_by(bn_id) %>% add_pspeaker_max_conj_lit()
df_long <- df_wide %>% gather(p_delta, p_rooij, key=condition, val=val) 

accept_condition_speaker_plot <-function(df_wide, ifac_applicable){
    if(ifac_applicable){
      df_wide <- df_wide %>% filter(`A > C` > 0)
    }
    df_long <- df_wide %>% gather(p_delta, p_rooij, key="condition", val="val")
    p1 <- plot_conditions_and_speaker(df_long)
    p2 <- plot_conditions_vs_speaker(df_long)
    return(list(and=p1, vs=p2))
}
# 2.1. △P/△*P is small (<0)
# check cases where "A>C" may be assertable (P(C|A) large), but △*P is not small
df_neg_wide <- df_wide %>% filter(p_delta<0 & p_c_given_a>=0.5)
p <- accept_condition_speaker_plot(df_neg_wide, ifac_applicable=TRUE)
fn <- paste(PLOT_DIR, "accept_conditions_negative", sep=.Platform$file.sep)
ggsave(paste(fn, "and.png", sep="_"), p$and, width=15, height=8)
ggsave(paste(fn, "vs.png", sep="_"), p$vs, width=15, height=8)

df_neg_wide$`A > C` %>% summary()
pos <- df_neg_wide %>% filter(`A > C` > 0)
pos$`A > C` %>% summary()

# 2.2 △P/△*P is positive
df_pos_wide <- df_wide %>% filter(p_delta>0 & p_rooij>0)
p <- accept_condition_speaker_plot(df_pos_wide, ifac_applicable = FALSE)
fn <- paste(PLOT_DIR, "accept_conditions_positive", sep=.Platform$file.sep)
ggsave(paste(fn, "and.png", sep="_"), p$and, width=18, height=8)
ggsave(paste(fn, "vs.png", sep="_"), p$vs, width=18, height=8)

# 2.3 both into one plot
df_wide_both <- df_wide %>% filter(p_c_given_a>=0.5)
p <- accept_condition_speaker_plot(df_wide_both, ifac_applicable = FALSE)
fn <- paste(PLOT_DIR, "accept_conditions", sep=.Platform$file.sep)
ggsave(paste(fn, "and.png", sep="_"), p$and, width=18, height=8)
ggsave(paste(fn, "vs.png", sep="_"), p$vs, width=18, height=8)

df_pos_wide$`A > C` %>% summary()
pos <- df_pos_wide %>% filter(`A > C` > 0)
pos$`A > C` %>% summary()

# 2.3. p_delta vs. p_rooij
comparison_plot <- function(df){
  # different shape for states P(C|A)>theta
  theta <- 0.9 
  # color conditional probability P(C|A)
  p <- df %>%
    ggplot(aes(x=p_delta, y=p_rooij, color=p_c_given_a, shape=p_c_given_a>=theta))
  low <- round(min(df$p_c_given_a), 3)
  high <- round(max(df$p_c_given_a), 3)
  plot_cond_prob <- plot_pdelta_vs_prooij(p, list(str="P(C|A)", min=low, max=high))
  
  # color probability of speaker to choose "If A, C"
  p <- df %>% ggplot(aes(x=p_delta, y=p_rooij, color=`A > C`, shape=p_c_given_a>=theta))
  low <- round(min(df$`A > C`), 3)
  high <- round(max(df$`A > C`), 3)
  legend_str <- TeX("$P_S(u=A\\rightarrow C)$")
  plot_pspeaker_ifac <- plot_pdelta_vs_prooij(p, list(str=legend_str, min=low, max=high))
  
  p_compare <- plot_grid(plot_cond_prob, plot_pspeaker_ifac, label_size=25, ncol=2)
  return(list("speaker"=plot_pspeaker_ifac, "cond_prob"=plot_cond_prob, 
              "both"=p_compare))
}
# 2.3.1 p_delta<0 and therefore p_rooij<0
df_neg <- df_long %>% filter(val<0) %>% spread(key=condition, val=val)
plot_neg <- comparison_plot(df_neg)
ggsave(paste(PLOT_DIR, "p_delta_vs_p_rooij_neg.png",
             sep=.Platform$file.sep), plot_neg$both, width=20, height=8)

# 2.3.2 p_delta>0 and therefore p_rooij>0
df_pos <- df_long %>% filter(val>0) %>% spread(key=condition, val=val)
plot_pos <- comparison_plot(df_pos)
ggsave(paste(PLOT_DIR, "p_delta_vs_p_rooij_pos.png",
             sep=.Platform$file.sep), plot_pos$both, width=20, height=8)


# Default context ---------------------------------------------------------
# ratio for dep/independent causal nets when P(C|A) is >= or < theta
# (to explain increase in LL for bayes nets where P(C)>=theta or P(C)<=1-theta) ?
default <- dat.none_wide %>% filter(bias=="none") %>% compute_cond_prob("P(C|A)")

prior <- default %>% filter(level=="prior") %>%
        mutate(p=case_when(p>=params$theta ~ TRUE, TRUE ~ FALSE)) %>% 
        mutate(cn=case_when(cn=="A || C" ~ "indep", TRUE ~ "dep")) %>%
        group_by(cn, p) %>% summarise(s=sum(prob)) %>% add_column(level="prior")

posterior <- default %>% filter(level!="prior") %>%
              mutate(cn=case_when(cn=="A || C" ~ "indep", TRUE ~ "dep")) %>% 
              group_by(cn, level) %>% summarise(s=sum(prob)) %>% add_column(p=TRUE)
  
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
      theme_classic(base_size = 20) +
      theme(legend.position = "right", legend.direction = "vertical",
            legend.text = element_text(size=18),
            legend.key.size = unit(1.5, "cm"))

p
fn <- paste(PLOT_DIR, "prior-ll-pl-dep-indep-ifac-true.png", sep=.Platform$file.sep)
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


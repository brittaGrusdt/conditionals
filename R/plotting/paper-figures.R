library(tidyverse)
library(config)
library(ggplot2)
library(latex2exp)
library(cowplot)
library(grid)
source("R/helper-functions.R")
source("R/default-model/helpers-tables.R")

target_dir <- "./data/test-default"
params <- read_rds(paste(target_dir, "results-none-params.rds", sep=.Platform$file.sep))

dat.none = read_rds(paste(params$target_dir, params$target_fn, sep=.Platform$file.sep))
dat.none_wide <- dat.none %>% spread(key=cell, val=val)

PLOT_DIR <- file.path(params$target_dir, "figs", fsep=.Platform$file.sep)
dir.create(PLOT_DIR, recursive = TRUE)

# Tables ------------------------------------------------------------------
tables_per_cns <- function(tables_path, fn){
  tables_wide <- readRDS(tables_path) %>% filter_tables(params) %>% unnest_tables() %>%
    rename(bn_id=rowid) %>% group_by(bn_id, cn) %>% 
    pivot_wider(names_from = cell, values_from = val) %>% ungroup()
  tables <- tables_wide %>% 
    mutate(`-A-C` = case_when(is.na(`-A-C`) ~ rowSums(select(., starts_with("-A-C_"))),
                              TRUE ~ `-A-C`),
           `-AC` = case_when(is.na(`-AC`) ~ rowSums(select(., starts_with("-AC_"))),
                             TRUE ~ `-AC`), 
           `A-C` = case_when(is.na(`A-C`) ~ rowSums(select(., starts_with("A-C_"))),
                             TRUE ~ `A-C`),
           `AC` = case_when(is.na(`AC`) ~ rowSums(select(., starts_with("AC_"))),
                            TRUE ~ `AC`)) %>% 
    select(!ends_with("_Da") & !ends_with("_Db") & !ends_with("_Dna")) %>% 
    group_by(bn_id, cn) %>%
    pivot_longer(cols = c(AC, `A-C`, `-AC`, `-A-C`), names_to = "cell", values_to = "val") %>% 
    ungroup() %>% 
    mutate(cell=factor(cell, levels=c("AC", "A-C", "-AC", "-A-C")), cn=as.factor(cn)) %>% 
    group_by(bn_id, cn)
  
  table_plots <- plot_tables(tables)
  p <- do.call(plot_grid, table_plots)
  ggsave(paste(PLOT_DIR, fn, sep=.Platform$file.sep), p, width=15, height=10)
}

# bias: none
tables_path <- file.path(params$target_dir, params$tables_fn, fsep=.Platform$file.sep)
tables_per_cns(tables_path, "table_plots_none.png")

# bias: lawn
params_lawn <- read_rds(paste(target_dir, "results-lawn-params.rds", sep=.Platform$file.sep))
tables_path <- file.path(params_lawn$target_dir, params_lawn$tables_fn, fsep=.Platform$file.sep)
tables_per_cns(tables_path, "table_plots_lawn.png")


# Ignorance inferences ----------------------------------------------------
# todo: if loaded in beginning error with plot_grid in plot_tables above
source("R/plot-functions.R")

# prior + listener interpretations of conditional If A, C
# given speaker is certain about A or C or both
dat.none.voi <- read_rds(file.path(params$target_dir, "results-none-voi.rds",
                                   fsep = .Platform$file.sep)) %>%
  filter(startsWith(key, "epistemic_certainty"))

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
  labs(y=TeX("$\\sum_{s\\in Certain_s(A) \\bigcup Certain_s(C)\\} Pr(s|u=If A, C)$"), x="") +
  theme_classic(base_size=25) +
  theme(legend.position = "none") +
  coord_flip()
p
ggsave(paste(PLOT_DIR, "ignorance-inferences.png",
             sep=.Platform$file.sep), p, width=11, height=5)


# speaker plots certain/uncertain -----------------------------------------
params <- read_rds(paste(target_dir, "results-speaker-params.rds", sep=.Platform$file.sep))

utterances <- read_rds(paste(params$target_dir, params$utts_fn, sep=.Platform$file.sep))
fn <- paste(str_sub(params$target_fn, 1, -5), "-avg.rds", sep="");
data <- read_rds(paste(params$target_dir, fn, sep=.Platform$file.sep)) %>%
  ungroup() %>%
  select(-intention, -bias) %>% add_column(condition="default") %>%
  rename(p=mean_per_intention) %>%
  arrange(p)
data <- within(data, utterance <- factor(utterance, levels=sort_utterances(utterances)))

p <- data %>% mutate(p=round(as.numeric(p), 2)) %>% 
  ggplot(aes(x=utterance, y=p, fill=condition)) + 
  geom_bar(stat="identity", position=position_dodge(preserve = "single"))  +
  scale_y_continuous(limits=c(0,0.25)) +
  # facet_wrap(~cn) +
  labs(y=TeX("$\\frac{1}{N} \\cdot \\sum_{i=1}^{N} P_S(u|s_i)$"), x="utterance") +
  theme_bw(base_size=25) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1)) 
p

ggsave(paste(PLOT_DIR, "speaker.png",
             sep=.Platform$file.sep), p, width=13, height=5)

# split into uncertain/certain states
params_unc <- read_rds(paste(target_dir, "results-speaker-uncertain-params.rds", sep=.Platform$file.sep))
params_cert <- read_rds(paste(target_dir, "results-speaker-certain-params.rds", sep=.Platform$file.sep))

plot_speaker_un_certain <- function(split_cns){
  fn_unc <- paste(str_sub(params_unc$target_fn, 1, -5), "-avg.rds", sep="");
  fn_cert <- paste(str_sub(params_cert$target_fn, 1, -5), "-avg.rds", sep="");
  if(split_cns) {
    fn_unc <- paste(str_sub(params_unc$target_fn, 1, -5), "-avg-cns.rds", sep="");
    fn_cert <- paste(str_sub(params_cert$target_fn, 1, -5), "-avg-cns.rds", sep="");
  }
  dat.unc <- read_rds(paste(params_unc$target_dir, fn_unc, sep=.Platform$file.sep)) %>%
    ungroup() %>% 
    select(-intention, -bias) %>% add_column(condition="uncertain") %>%
    arrange(mean_per_intention)
  
  dat.cert <- read_rds(paste(params_cert$target_dir, fn_cert, sep=.Platform$file.sep)) %>%
    ungroup() %>% 
    select(-intention, -bias)  %>% add_column(condition="certain") %>% 
    arrange(mean_per_intention)
  
  data <- bind_rows(dat.cert, dat.unc) %>% rename(p=mean_per_intention)
  data <- within(data, utterance <- factor(utterance,  levels=sort_utterances(utterances)))

  p <- data %>% mutate(p=round(as.numeric(p), 2)) %>% 
    ggplot(aes(x=utterance, y=p, fill=condition)) + 
    geom_bar(stat="identity", position=position_dodge(preserve = "single"))  +
    scale_y_continuous(limits=c(0,0.25)) +
    labs(y=TeX("$\\frac{1}{|S_{c/u}|} \\cdot \\sum_{s \\in S_{c/u}} P_S(u|s)$"), x="utterance") +
    theme_bw(base_size=25)
  if(split_cns) {
    p <- p + facet_wrap(~cn) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 15),
            legend.position = "bottom")
    w <- 18
    h <- 8
    fn <- "speaker-un_certain_cns.png"
  } else {
    p <- p +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 15),
            legend.position = c(.2, .95), legend.justification=c("right", "top"))
    w <- 13
    h <- 5
    fn <- "speaker-un_certain.png"
  }
  ggsave(paste(PLOT_DIR, fn, sep=.Platform$file.sep), p,
         width=w, height=h)
  return(p)
}
plot_speaker_un_certain(split_cns = FALSE)
plot_speaker_un_certain(split_cns = TRUE)


# speaker delta_p ----------------------------------------------------------
params <- read_rds(paste(target_dir, "results-speaker-params.rds", sep=.Platform$file.sep))

speaker_default <- read_rds(file.path(params$target_dir, params$target_fn)) %>%
  select(-intention) %>% 
  compute_cond_prob("P(C|A)") %>% rename(p_c_given_a=p) %>%
  compute_cond_prob("P(C|-A)") %>% rename(p_c_given_na=p) %>% 
  spread(key=utterance, val=probs) %>%
  select(bn_id, cn, `AC`, `A-C`, `-AC`, `-A-C`, `A > C`, p_delta, p_rooij, p_diff) %>% 
  pivot_longer(cols = c(p_delta, p_rooij, p_diff), names_to = "condition", values_to = "val") %>% 
  filter(val > 0)

p <- speaker_default %>%
  ggplot(aes(x=val, y=`A > C`, color=condition)) +
  geom_point(size=2, alpha=0.7) +
  facet_wrap(~cn, scales="free", strip.position = "top",
             labeller=labeller(cn=c(`A || C`="A,C independent",
                                    `A implies C`="A implies C",
                                    `C implies A`="C implies A",
                                    `A implies -C`="A implies ¬C",
                                    `C implies -A`="C implies ¬A",
                                    `only A implies C`="only A implies C"
                                    ))        
             ) +
  theme_bw(base_size=25) + 
  scale_color_discrete(name="accept/assert condition",
                      breaks=c("p_diff", "p_delta", "p_rooij"),
                      labels=c(unname(c(TeX("$p_{diff}$"))), "△P", "△*P")) +
  labs(y=TeX("$P_S(u=If A, C|s)$"), x="value condition") +
  theme(legend.position="bottom",
        legend.box = "vertical",
        legend.key.width = unit("2.5", "cm")
  ) 
p
ggsave(paste(PLOT_DIR, "accept-conditions.png",
             sep=.Platform$file.sep), p, width=18, height=8)


# Conditional Perfection --------------------------------------------------
# params <- read_rds(paste(target_dir, "results-none-params.rds", sep=.Platform$file.sep))
params <- read_rds(paste(target_dir, "results-lawn-params.rds", sep=.Platform$file.sep))

data <- read_rds(file.path(params$target_dir, params$target_fn, fsep = .Platform$file.sep)) %>%
  ungroup() %>% group_by(bn_id, level) %>% select(-p_delta, -p_rooij, -p_diff, -bias, -intention)
data.wide <- data %>%
  pivot_wider(names_from = "cell", values_from = "val") %>% 
  compute_cond_prob("P(-C|-A)") %>%  rename(`-C_-A` = p) %>% 
  compute_cond_prob("P(A|C)") %>% rename(`A_C` = p)

# Expected values for P(-C|-A) and P(A|C) and for causal nets
ev_nc_na = data.wide %>% rename(p=`-C_-A`) %>% expected_val("P(-C|-A)")
ev_a_c = data.wide %>% rename(p=`A_C`) %>% expected_val("P(A|C)") 
ev_probs <- bind_rows(ev_a_c, ev_nc_na) %>%
  mutate(level=factor(level, levels=c("prior", "LL", "PL")))

ev_cns = data.wide %>% ungroup() %>% group_by(level, cn) %>% 
  summarise(ev=sum(prob), .groups="drop_last") %>%
  mutate(s=sum(ev), level=factor(level, levels=c("prior", "LL", "PL")))

plot_evs_cp <- function(data, evs_probs){
  lim1 = ifelse(max(data$ev) < 1-0.1, max(data$ev)+0.1, 1)
  lim2 = ifelse(max(evs_probs$ev) < 1-0.1, max(evs_probs$ev)+0.1, 1)
  lim = ifelse(lim1 > lim2, lim1, lim2)
  p <- data %>% ggplot(aes(x=cn, y=ev, fill=level)) + 
    geom_bar(position=position_dodge2(preserve = "single"), stat="identity") + 
    geom_bar(position=position_dodge2(preserve = "single"), stat="identity",
             data = evs_probs, aes(x=p, y=ev, fill=level)) +
    scale_y_continuous(limits=c(0, lim)) +
    scale_x_discrete(name=paste(strwrap("causal nets / conditional probs", width=20),
                                collapse="\n"),
                     # breaks=c("P(A|C)", "P(-C|-A)", "A || C",
                     #          "A implies C", "only A implies C", "A implies -C",
                     #          "C implies A", "C implies -A"
                     #         ),
                     # labels=c("P(A|C)", "P(¬C|¬A)",  "A,C indep.",
                     #          "A implies C", "only A implies C", "A implies ¬C",
                     #          "C implies A", "C implies ¬A")
                     # ) +
                     breaks=c("P(A|C)", "P(-C|-A)", "A || C",
                              "A,B->C", "only A implies C", "A,B->D->C", "A implies C"
                     ),
                     labels=c("P(A|C)", "P(¬C|¬A)",  "A,C indep.",
                              "A,B implies C", "only A implies C", "A xor B implies C", "A implies C")
    ) +

    scale_fill_discrete(name="Interpretation Level",
                        breaks=c("prior", "LL", "PL"),
                        labels=c("A priori", "Literal", "Pragmatic")) +
    coord_flip() +
    # facet_wrap(~level, labeller = labeller(
    #   level = c(`prior` = "Prior belief",
    #             `LL` = paste(strwrap("Literal interpretation", width=12),
    #                          collapse="\n"),
    #             `PL`= paste(strwrap("Pragmatic interpretation", width=15),
    #                         collapse="\n")))) +
    labs(y="Degree of belief") +
    theme_bw(base_size=25) +
    theme(legend.position = c(.9, .3), legend.justification=c("right", "top"))
  return(p)
}

p <- plot_evs_cp(ev_cns, ev_probs)
ggsave(paste(PLOT_DIR, "none-evs-cp.png", sep=.Platform$file.sep), p, width=16, height=8)

p <- plot_evs_bar(ev_probs)
ggsave(paste(PLOT_DIR, "none-evs-cp-probs.png", sep=.Platform$file.sep), p, width=18, height=8)

# todo: look at hellinger distance etc.
# dat.none.voi <- read_rds(file.path(params$target_dir, "results-none-voi.rds",
#                                    fsep = .Platform$file.sep))

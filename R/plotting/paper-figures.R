library(tidyverse)
library(config)
library(ggplot2)
library(latex2exp)
library(cowplot)
library(grid)
source("R/helper-functions.R")
source("R/default-model/helpers-tables.R")

target_dir <- "./data/default-model"
params <- read_rds(paste(target_dir, "results-none-prior-params.rds", sep=.Platform$file.sep))

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
params_lawn <- read_rds(paste("./data/special-conditionals",
                              "results-lawn-PL-params.rds", sep=.Platform$file.sep))
tables_path <- file.path(params_lawn$target_dir, params_lawn$tables_fn, fsep=.Platform$file.sep)
tables_per_cns(tables_path, "table_plots_lawn.png")


# Ignorance inferences ----------------------------------------------------
# todo: if loaded in beginning error with plot_grid in plot_tables above
source("R/plot-functions.R")

# prior + listener interpretations of conditional If A, C
# given speaker is certain about A or C or both
dat.none.voi <- read_rds(file.path(params$target_dir, "results-none-PL-voi.rds",
                                   fsep = .Platform$file.sep)) %>%
  filter(key == "epistemic_uncertainty")

df <- dat.none.voi %>% mutate(value=round(as.numeric(value), 2)) %>%
  group_by(level, intention, uncertainty) %>% 
  pivot_wider(names_from = uncertainty, values_from= value) %>% 
  summarise(`A or C`= sum(both, `only A`, `only C`), `A and C` = both,
            .groups = "keep") %>% 
  pivot_longer(cols = c(`A or C`, `A and C`), names_to = "key", values_to = "val") %>% 
  filter(level != "prior") 

p <- df %>% filter(key == "A and C") %>% 
  ggplot(aes(x=level, y=val)) + 
  geom_bar(stat="identity", position="dodge")  +
  geom_text(aes(label = val, x = level,  y = val ), hjust=-0.1, size=6,
            position=position_dodge(0.9)) + 
  scale_x_discrete(limits = c("LL", "PL"),               
    labels=c(paste(strwrap("Literal interpretation", width=20), collapse="\n"),
             paste(strwrap("Pragmatic interpretation", width=20), collapse="\n"))
    ) + 
  labs(y= TeX("$\\sum_{s\\in Uncertain_s(A) \\bigcap Uncertain_s(C) \\}
            Pr(s|u=A\\rightarrow C)$"), x="") +
  theme_classic(base_size=25) +
  theme(legend.position = "none") +
  coord_flip()
p
ggsave(paste(PLOT_DIR, "ignorance-inferences.png",
             sep=.Platform$file.sep), p, width=13.5, height=4)


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

# split into uncertain/certain states TODO here!!
params_unc <- read_rds(paste(target_dir, "results-speaker-uncertain-params.rds", sep=.Platform$file.sep))
params_cert <- read_rds(paste(target_dir, "results-speaker-certain-params.rds", sep=.Platform$file.sep))

plot_speaker_un_certain <- function(split_cns=FALSE, chunk_utts=FALSE){
  fn_unc <- paste(str_sub(params_unc$target_fn, 1, -5), "-avg.rds", sep="");
  fn_cert <- paste(str_sub(params_cert$target_fn, 1, -5), "-avg.rds", sep="");
  if(split_cns) {
    fn_unc <- paste(str_sub(params_unc$target_fn, 1, -5), "-avg-cns.rds", sep="");
    fn_cert <- paste(str_sub(params_cert$target_fn, 1, -5), "-avg-cns.rds", sep="");
  }
  dat.unc <- read_rds(paste(params_unc$target_dir, fn_unc, sep=.Platform$file.sep)) %>%
    ungroup() %>% 
    select(-intention, -bias) %>% add_column(condition="uncertain") %>%
    arrange(mean_per_intention) %>% rename(p=mean_per_intention)
  dat.cert <- read_rds(paste(params_cert$target_dir, fn_cert, sep=.Platform$file.sep)) %>%
    ungroup() %>% 
    select(-intention, -bias)  %>% add_column(condition="certain") %>% 
    arrange(mean_per_intention) %>% rename(p=mean_per_intention)
  
  if(split_cns) {
    dat.unc <- dat.unc %>%
      mutate(cn=case_when(cn=="A || C" ~ "A,C independent",
                          TRUE ~ "A,C dependent")) %>% 
      group_by(cn) %>%
      mutate(norm=sum(p), p=p/norm)
      
    dat.cert <- dat.cert %>%
      mutate(cn=case_when(cn=="A || C" ~ "A,C independent",
                          TRUE ~ "A,C dependent")) %>% 
      group_by(cn) %>%
      mutate(norm=sum(p), p=p/norm)
  }
  
  if(chunk_utts) {
    fn = "_chunked.png"
    # utts <- c("conjunctions", "conditionals", "literals", "likely")
    utts <- c("conj_lit", "conditionals",  "likely")
    
    dat.unc = dat.unc %>%
      summarise(likely=sum(p[str_detect(utterance, "likely")]),
                conditionals=sum(p[str_detect(utterance, ">")]),
                conjunctions=sum(p[str_detect(utterance, "and")]),
                literals=1-sum(p[str_detect(utterance, "likely") |
                                 str_detect(utterance, "and") |
                                 str_detect(utterance, ">")]), .groups="keep") %>% 
      mutate(conj_lit = conjunctions + literals) %>% 
      select(conj_lit, likely, conditionals) %>% 
      pivot_longer(cols=utts, names_to = "utterance", values_to = "p") %>%
      add_column(condition="uncertain")
                  
    dat.cert = dat.cert %>%
      summarise(likely=sum(p[str_detect(utterance, "likely")]),
                conjunctions=sum(p[str_detect(utterance, "and")]),
                conditionals=sum(p[str_detect(utterance, ">")]),
                literals=1-sum(p[str_detect(utterance, "likely") |
                                 str_detect(utterance, "and") |
                                 str_detect(utterance, ">")]), .groups="keep"
                ) %>%
      mutate(conj_lit = conjunctions + literals) %>% 
      select(conj_lit, likely, conditionals) %>% 
      pivot_longer(cols=utts, names_to = "utterance", values_to = "p") %>%
      add_column(condition="certain")
  } else {
    fn <- ".png"
    utts <- utterances
  }
  
  data <- bind_rows(dat.cert, dat.unc)
  data <- within(data, utterance <- factor(utterance,  levels=sort_utterances(utts)))

  p <- data %>% mutate(p=round(as.numeric(p), 2)) %>% 
    ggplot(aes(x=utterance, y=p, fill=condition)) + 
    geom_bar(stat="identity", position=position_dodge(preserve = "single"))  +
    labs(y=TeX("$\\frac{1}{|S|} \\cdot \\sum_{s \\in S} P_S(u|s)$"), x="utterance") +
    theme_bw(base_size=25)
  
  if(split_cns) {
    p <- data %>% mutate(p=round(as.numeric(p), 2)) %>% 
      ggplot(aes(x=utterance, y=p, fill=cn)) + 
      geom_bar(stat="identity", position=position_dodge(preserve = "single"))  +
      labs(y=TeX("$\\frac{1}{|S|} \\cdot \\sum_{s \\in S} P_S(u|s)$"), x="utterance") +
      theme_bw(base_size=25)
    
    p <- p + facet_wrap(~condition) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 15),
            legend.position = "bottom")
    w <- 18
    h <- 8
    fn <- paste("speaker-un_certain_cns", fn, sep="")
  } else {
    p <- p +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 15),
            legend.position = c(.97, .95), legend.justification=c("right", "top"))
    w <- 13
    h <- 5
    fn <- paste("speaker-un_certain", fn, sep="")
  }
  ggsave(paste(PLOT_DIR, fn, sep=.Platform$file.sep), p,
         width=w, height=h)
  return(p)
}
plot_speaker_un_certain(split_cns = FALSE, chunk_utts = FALSE)
plot_speaker_un_certain(split_cns = FALSE, chunk_utts = TRUE)
plot_speaker_un_certain(split_cns = TRUE, chunk_utts = FALSE)
plot_speaker_un_certain(split_cns = TRUE, chunk_utts=TRUE)

# speaker delta_p ----------------------------------------------------------
params_prior <- read_rds(paste(target_dir, "results-none-priorN-params.rds", sep=.Platform$file.sep))
prior <-  read_rds(file.path(params_prior$target_dir, params_prior$target_fn)) %>%
  pivot_wider(names_from = "cell", values_from = "val") %>% 
  mutate(level = "prior") %>% 
  select(-bias)
params_literal <- read_rds(paste(target_dir, "results-speaker-literal-params.rds", sep=.Platform$file.sep))
params_speaker <- read_rds(paste(target_dir, "results-speaker-20000-params.rds", sep=.Platform$file.sep))

getSpeaker <- function(params, cat) {
  speaker <- read_rds(file.path(params$target_dir, params$target_fn)) %>%
    select(-intention, -bias) %>%
    group_by(bn_id, cn) %>%
    mutate(utterance = paste("utt", utterance, sep="_")) %>% 
    pivot_wider(names_from = utterance, values_from=probs) %>%
    mutate(level = cat)
  return(speaker)
}

speaker <- getSpeaker(params_speaker, "speaker") %>%
  pivot_longer(cols = starts_with("utt_"), names_to = "utterance",
               values_to = "probs") %>% 
  group_by(bn_id, level, cn) %>% 
  mutate(p_best=max(probs), u_best=list(utterance[probs == max(probs)])) %>%
  unnest(u_best)

# get data from implemented literal speaker
# (20000 samples since conditioned s.t. A > C is true)
speaker_literal <- read_rds(file.path(params_literal$target_dir, params_literal$target_fn)) %>%
  pivot_wider(names_from = "cell", values_from = "val") %>%
  mutate(level = "literal-speaker") %>%
  select(-bias)
  #  %>% compute_cond_prob("P(C|A)")

# just filter speaker model predictions for A > C best utterance
# speaker_literal2 <- speaker %>%
#   pivot_wider(names_from = "utterance", values_from = "probs") %>%
#   filter(`utt_A > C` > 0) %>% 
#   mutate(level = "literal-speaker") %>%
#   select(-u_best, -p_best)
    
speaker_pragmatic <- speaker %>%
  filter(u_best == "utt_A > C") %>%
  mutate(level = "pragmatic-speaker") %>%
  select(-u_best, -p_best) %>%
  pivot_wider(names_from = "utterance", values_from = "probs") 

df <- bind_rows(prior, speaker_literal, speaker_pragmatic) %>%
  group_by(bn_id, cn, level) %>%
  pivot_longer(cols=c(p_delta, p_rooij, p_diff), names_to="condition", values_to = "val") %>% 
  select(-starts_with("utt")) %>%
  mutate(
    level=factor(level, levels = c("prior", "literal-speaker", "pragmatic-speaker")), 
    cn = case_when(cn == "A || C" ~ "A,C indep.",
                   TRUE ~str_replace(cn, "-", "¬")
                  ),
    condition = factor(condition, levels = c("p_rooij", "p_delta", "p_diff"))
  ) %>% 
  group_by(bn_id, level, condition)

plot_accept_conditions <- function(dat, fn){
  x_labels <- c(c(-100000, -1000, -50, -10, -1),
                seq(from=-0.75, by=0.25, to=0.7),
                seq(from=0.75, by=0.05, to=1))
  x_breaks <- seq(from=0.5, by=1, length.out=length(x_labels))
  level_labels = as_labeller(
    c(`literal-speaker` = paste(strwrap("Literal speaker", width=15), collapse="\n"),
      `pragmatic-speaker` = paste(strwrap("Pragmatic speaker", width=15), collapse="\n"),
      `prior` = "Prior")
  )
  condition_labels = as_labeller(c(`p_rooij`= "△*P", `p_delta`="△P", `p_diff`="P(C|A)-P(C)"))
  cn_labels = as_labeller(dat$cn %>% unique())
  getGroup = function(val) {
    i<-1
    x <- x_labels[[1]]
    while(val > x){
      i <- i + 1
      x <- x_labels[[i]]
    } 
    return(i-1)
  }

  data <- dat %>% group_by(bn_id, level, condition) %>%
    mutate(group = getGroup(val)) %>%
    group_by(level, condition, group, cn)
  data.sum <- data %>% 
    summarise(count=n(), .groups="drop_last") %>%
    group_by(level, condition) %>%
    mutate(ratio = count/sum(count))
    
    p <- data.sum %>% ggplot(aes(x=group, fill=cn)) +
      facet_grid(level~condition, scales="free", labeller=
                 labeller(level=level_labels, condition=condition_labels))
  
  p <- p + geom_bar(aes(y=ratio), stat="identity", position="dodge") +
    scale_x_continuous(breaks=x_breaks, labels=x_labels) +
    labs(x=paste(strwrap("accept/assert condition value intervals", width=25), collapse="\n"),
         y="ratio", fill="causal net") +
    theme_bw(base_size=25) +
    theme(legend.position="bottom", axis.text.x=element_text(angle=0, size=11))
  
  ggsave(paste(PLOT_DIR, fn, sep=.Platform$file.sep), p, width=18, height=10)
  return(p)
}

plot_accept_conditions(df %>% filter(condition != "p_diff"), "accept-conditions.png")


#analyze
df %>% filter(condition == "p_rooij" & val > 0 & val < 0.25)

# analyze: bns where large p_rooij does not necessarily represent causal relation
# best utterance other than conditionl although p_rooij large
df %>% filter(cn == "A,C indep." & val > 0 & condition == "p_rooij" &
              category %in% c("pragmatic-best"))

df <- df_best %>% mutate(u_best=factor(u_best), category=u_best)
plot_accept_conditions(df %>% filter(val > 0), "accept-conditions-all.png", FALSE)
       
# Accept conditions given best utterance
speaker_pragmatic <- speaker %>%
  mutate(u_best = str_replace(u_best, "-", "¬"),
         u_best = str_replace(u_best, "utt_", ""),
         u_best = str_replace(u_best, ">", "%-%"),
         u_best = factor(u_best),
         cn = case_when(cn == "A || C" ~ "A,C indep.",
                        TRUE ~str_replace(cn, "-", "¬"))
        ) %>%
  ungroup() %>% 
  select(-category, -level) %>%
  group_by(bn_id, `AC`, `A-C`, `-AC`, `-A-C`) %>% 
  pivot_wider(names_from = "utterance", values_from = "probs") %>% 
  filter(`utt_A > C` > 0) %>% 
  pivot_longer(cols=c(p_delta, p_rooij, p_diff), names_to = "condition", values_to = "val")

p <- speaker_pragmatic %>%
      filter(condition != "p_diff" & val > 0) %>% 
      ggplot(aes(x=cn, y=val, color=condition)) +
      geom_boxplot(aes(y=val)) +
      # geom_smooth(method=lm) +
      # geom_jitter(aes(x=cn, y=val, color=condition), size=2, alpha=0.7) +
      # facet_grid(cn~u_best, scales="free") + # strip.position = "top") +
  facet_wrap(~u_best, scales="free", strip.position = "top", ncol = 4, nrow=2) +    
  scale_color_discrete(name=paste(strwrap("accept/assert condition", width=25), collapse="\n"),
                           breaks=c("p_delta", "p_rooij"),
                           labels=c( "△P", "△*P")) +
                           # labels=c(unname(c(TeX("$p_{diff}$")))) +
      labs(x="", y="value accept/assert condition") +
      theme_bw(base_size=25) +
      theme(legend.position="bottom", axis.text.x = element_text(angle=25))

ggsave(paste(PLOT_DIR, "accept-conditions-u-best.png",
             sep=.Platform$file.sep), p, width=18, height=8)




  # signed_log <- function(val) {
  #   return(sign(val) * log(abs(val)))
  # }
  # inv_signed_log <- function(val){
  #   return(sign(val) * exp(abs(val)))
  # }
  # data <- dat %>% mutate(val_trans = signed_log(val)) 
  # x_breaks <- c(-10, -5, 0, 5, 10)
  # x_labels <- map(x_breaks, inv_signed_log)
    # scale_x_continuous(trans_new("signed_log", signed_log, inv_signed_log))
    # scale_x_continuous(breaks = x_breaks, labels = x_labels)

# # test some stuff
# df0 <- df %>% select(bn_id, `A > C`, cn, p_best, u_best, condition, val, category) %>% 
#   pivot_wider(names_from = condition, values_from = val) %>% 
#   filter(category=="conditional")
# # df1 <- df0 %>% filter(`A > C` <= 0.2)
# # df2 <- df0 %>% filter(`A > C` > 0.2 & `A > C` <= 0.4)
# # df3 <- df0 %>% filter(`A > C` > 0.4 & `A > C` <= 0.6)
# # df4 <- df0 %>% filter(`A > C` > 0.6 & `A > C` <= 0.8)
# # df5 <- df0 %>% filter(`A > C` > 0.8 & `A > C` <= 1)
# # df6 <- df0 %>% filter(`A > C` > 0.5)
# # 
# # cor(df1$p_rooij, df1$`A > C`)
# # cor(df2$p_rooij, df2$`A > C`)
# # cor(df3$p_rooij, df3$`A > C`)
# # cor(df4$p_rooij, df4$`A > C`)
# # cor(df5$p_rooij, df5$`A > C`)
# # cor(df6$p_rooij, df6$`A > C`)
# 
# cor(df0 %>% filter(cn=="A implies C") %>% pull(p_rooij),
#     df0 %>% filter(cn=="A implies C") %>% pull(`A > C`))
# cor(df0 %>% filter(cn=="A implies C") %>% pull(p_delta),
#     df0 %>% filter(cn=="A implies C") %>% pull(`A > C`))
# cor(df0 %>% filter(cn=="A implies C") %>% pull(p_diff),
#     df0 %>% filter(cn=="A implies C") %>% pull(`A > C`))
# 
# df0 %>% 
#   pivot_longer(cols = c("p_delta", "p_rooij", "p_diff"), names_to = "condition", values_to = "val") %>% 
#   group_by(bn_id, condition) %>% 
#   ggplot(aes(x=val, y=`A > C`)) +
#   geom_point(aes(color=condition)) +
#   geom_smooth(method=lm) +
#   # geom_jitter(aes(x=cn, y=val, color=condition), size=2, alpha=0.7) +
#   # facet_wrap(~condition, scales="free", strip.position = "top") +
#   facet_grid(cn~condition) + 
#   scale_color_discrete(name=paste(strwrap("accept/assert condition", width=15),
#                                   collapse="\n"),
#                        breaks=c("p_diff", "p_delta", "p_rooij"),
#                        labels=c(unname(c(TeX("$p_{diff}$"))), "△P", "△*P")) +
#   # scale_x_discrete(breaks=x_breaks, labels = x_labels) + 
#   # labs(x="causal net", y="value accept/assert condition") +
#   theme_bw(base_size=25) + theme(legend.position="bottom")
# 
# 
#   # labs(y=TeX("$P_S(u=A \\rightarrow C | s)$"), x="value condition") +
#   # theme(legend.position="bottom",
#   #       legend.box = "vertical",
#   #       legend.key.width = unit("2.5", "cm")
#   # ) 
# ggsave(paste(PLOT_DIR, "accept-conditions.png",
#              sep=.Platform$file.sep), p, width=18, height=8)
# 
# 
# speaker_default %>% 
#   ggplot() +
#   geom_boxplot(aes(x=cn, y=`A > C`)) +
#   labs(y=TeX("$P_S(A\\rightarrow C|s)$"))
# 
# 
# p <- speaker_default %>% 
#   ggplot() +
#   geom_point(aes(x=val, y=`A > C`, color=condition, shape=cn)) +
#   labs(y=TeX("$P_S(A\\rightarrow C|s)$"), x="measure val")
# 
# p
# p + facet_wrap(~cn)
# 


# Conditional Perfection --------------------------------------------------
data_cp_plots <- function(params){
  data <- read_rds(file.path(params$target_dir, params$target_fn, fsep = .Platform$file.sep)) %>%
    ungroup() %>% group_by(bn_id, level) %>% select(-p_delta, -p_rooij, -p_diff, -bias)
  if("intention" %in% names(params)){
    data <- data %>% select(-intention)
  }
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
  
  return(list(probs=ev_probs, cns=ev_cns))  
}

plot_evs_cp <- function(data, evs_probs, breaks, labels){
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
                     breaks=breaks, labels=labels) +
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

params_none <- read_rds(paste(target_dir, "results-none-params.rds", sep=.Platform$file.sep))
dat.none = data_cp_plots(params_none)

breaks=c("P(A|C)", "P(-C|-A)", "A || C", "A implies C", "A implies -C",
         "C implies A", "C implies -A")
labels=c("P(A|C)", "P(¬C|¬A)",  "A,C indep.", "A implies C", "A implies -C",
         "C implies A", "C implies ¬A")
p <- plot_evs_cp(dat.none$cns, dat.none$probs, breaks, labels)
ggsave(paste(PLOT_DIR, "none-evs-cp.png", sep=.Platform$file.sep), p, width=16, height=8)


breaks[[5]] = "A,B->C"; breaks[[6]] = "only A implies C"; breaks[[7]] = "A,B->D->C";
labels[[5]] = "A,B implies C"; labels[[6]] = "only A implies C"; labels[[7]] = "A xor B implies C";
params_lawn <- read_rds(paste(target_dir, "results-lawn-params.rds", sep=.Platform$file.sep))
dat.lawn = data_cp_plots(params_lawn)
p <- plot_evs_cp(dat.lawn$cns, dat.lawn$probs, breaks, labels)
ggsave(paste(PLOT_DIR, "lawn-evs-cp.png", sep=.Platform$file.sep), p, width=16, height=8)



p <- plot_evs_bar(ev_probs)
ggsave(paste(PLOT_DIR, "none-evs-cp-probs.png", sep=.Platform$file.sep), p, width=18, height=8)

# todo: look at hellinger distance etc.
# dat.none.voi <- read_rds(file.path(params$target_dir, "results-none-voi.rds",
#                                    fsep = .Platform$file.sep))

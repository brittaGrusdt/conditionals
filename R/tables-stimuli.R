library(tidyverse)
library(here)
source("R/helper-functions.R")
source("R/default-model/helpers-tables.R")
source("R/helpers-webppl.R")
source("R/helpers-values-of-interest.R")
library(rwebppl)

# Tables for qualitative model check --------------------------------------
if1_tables = function(pos, neg, marginal, id, n=1500, cn="A implies C"){
  df <- tibble(pc_given_a = rbeta(n, pos$alpha, pos$beta),
               pc_given_na = rbeta(n, neg$alpha, neg$beta),
               pa = rbeta(n, marginal$alpha, marginal$beta))
  if(cn == "A implies -C"){
    df <- df %>% mutate(pc_given_a=1-pc_given_a, pc_given_na=1-pc_given_na)
  } 
  if(startsWith(cn, "C implies")){
    df <- df %>% 
      mutate(`AC`=pc_given_a * pa,
             `A-C`=pc_given_na * (1-pa),
             `-AC`=(1-pc_given_a) * pa,
             `-A-C`=(1-pc_given_na) * (1-pa)
      )
  } else {
    df <- df %>%
      mutate(`AC`=pc_given_a * pa,
             `A-C`=(1-pc_given_a) * pa,
             `-AC`=pc_given_na * (1-pa),
             `-A-C`=(1-pc_given_na) * (1-pa)
      )
  }
  df <- df %>% 
    add_column(stimulus_id=(!! id)) %>%
    select(-pc_given_a, -pc_given_na, -pa)
  return(df)
}

if2_tables = function(pp, nn, np, pn, marginal_a, marginal_b, id, n=1500, cn="A,B implies C"){
  df <- tibble(pc_given_ab = rbeta(n, pp$alpha, pp$beta),
               pc_given_nanb = rbeta(n, nn$alpha, nn$beta),
               pc_given_nab = rbeta(n, np$alpha, np$beta),
               pc_given_anb = rbeta(n, pn$alpha, pn$beta),
               pa = rbeta(n, marginal_a$alpha, marginal_a$beta),
               pb = rbeta(n, marginal_b$alpha, marginal_b$beta)
  )
  if(cn == "A,B implies -C"){
    df <- df %>% mutate(pc_given_ab=1-pc_given_ab, pc_given_nanb=1-pc_given_nanb,
                        pc_given_nab=1-pc_given_nab, pc_given_anb=1-pc_given_anb)
  }
  df <- df %>%
    mutate(`AC`=pc_given_ab * pa * pb + pc_given_anb * pa * (1-pb),
           `A-C`=(1-pc_given_ab) * pa * pb + (1-pc_given_anb) * pa * (1-pb),
           `-AC`=pc_given_nab * (1-pa) * pb + pc_given_nanb * (1-pa) * (1-pb),
           `-A-C`=(1-pc_given_nab) * (1-pa) * pb + (1-pc_given_nanb) * (1-pa) * (1-pb)
    ) %>%
    add_column(stimulus_id=(!! id)) %>%
    select(-pc_given_ab, -pc_given_nanb, -pc_given_nab, -pc_given_anb, -pa, -pb)
  return(df)
}

indep_tables <- function(param_prior_a, param_prior_c, id, n=6000){
  df <- tibble(pa = rbeta(n, param_prior_a$alpha, param_prior_a$beta),
               pc = rbeta(n, param_prior_c$alpha, param_prior_c$beta))
  df <- df %>%
    mutate(`AC`=pa * pc,
           `A-C`=(1-pc) * pa,
           `-AC`=pc * (1-pa),
           `-A-C`=(1-pc) * (1-pa)
    ) %>%
    add_column(stimulus_id=(!! id)) %>%
    select(-pa, -pc)
  return(df)
}
# Tables ------------------------------------------------------------------
# 1.independent
tables.all <- indep_tables(list(alpha=1, beta=1), list(alpha=1, beta=1), "independent");
# 2.if1
for(net in c("A implies C", "A implies -C", "C implies A", "C implies -A")) {
  for(strength in c("low", "uncertain", "high")) {
    if(net == "A implies C"){ id="if1-ac"
    } else if(net == "A implies -C") { id="if1-anc"
    } else if(net == "C implies A") { id = "if1-ca"
    } else { id="if1-cna"};
    
    if(strength=="low") {a=1; b=10; id=paste(id, "_Xl", sep="")
    } else if(strength=="uncertain") {a=25; b=25; id=paste(id, "_Xu", sep="")
    } else if(strength=="high") {a=10; b=1; id=paste(id, "_Xh", sep="")}
    
    tables.all <- tables.all %>%
        bind_rows(if1_tables(pos=list(alpha=a, beta=b),
                             neg=list(alpha=1, beta=50),
                             marginal=list(alpha=1, beta=1), id, n=250, cn=net));
  }
}
# 3.If2
for(net in c("A implies C", "A implies -C", "C implies A", "C implies -A")) {
  if(net == "A implies C"){ id="if2-ac"
  } else if(net == "A implies -C") { id="if2-anc"
  } else if(net == "C implies A") { id = "if2-ca"
  } else { id="if2-cna"};
  
  for(pa in c("low", "uncertain", "high")) {
    if(pa=="low") {a=1; b=10;
    } else if(pa=="uncertain") {a=25; b=25; 
    } else if(pa=="high") {a=10; b=1;}
    
    tables.all <- tables.all %>%
      bind_rows(if1_tables(pos=list(alpha=10, beta=1),
                           neg=list(alpha=1, beta=10/2),
                           marginal=list(alpha=a, beta=b), id, n=250, cn=net));
  }
}

# associate tables with stimuli
tables_to_stimuli <- function(tables.all.wide, t=0.8){
  tables <- tables.all.wide %>%
    mutate(pa=`AC` + `A-C`, pc=`AC`+`-AC`) %>%
    mutate(stimulus_id=case_when(
      str_detect(stimulus_id, "independent|if2") & pa>=t & pc>=t ~ paste(stimulus_id, "hh", sep="_"),
      str_detect(stimulus_id, "independent|if2") & pa>=t & pc<=1-t ~ paste(stimulus_id, "hl", sep="_"),
      str_detect(stimulus_id, "independent|if2") & pa>=t & pc>=0.4 & pc<=0.6 ~ paste(stimulus_id, "hu", sep="_"), 
      
      str_detect(stimulus_id, "independent|if2") & pa<=1-t & pc>=t ~ paste(stimulus_id, "lh", sep="_"),
      str_detect(stimulus_id, "independent|if2") & pa<=1-t & pc<=1-t ~ paste(stimulus_id, "ll", sep="_"),
      str_detect(stimulus_id, "independent|if2") & pa<=1-t & pc>=0.4 & pc<=0.6 ~ paste(stimulus_id, "lu", sep="_"),
      
      str_detect(stimulus_id, "independent|if2") & pa>=0.4 & pa<=0.6 & pc>=t ~ paste(stimulus_id, "uh", sep="_"),
      str_detect(stimulus_id, "independent|if2") & pa>=0.4 & pa<=0.6 & pc<=1-t ~ paste(stimulus_id, "ul", sep="_"),
      str_detect(stimulus_id, "independent|if2") & pa>=0.4 & pa<=0.6 & pc>=0.4 & pc<=0.6 ~ paste(stimulus_id, "uu", sep="_"),
      
      str_detect(stimulus_id, "if1") & pa>=0.4 & pa<=0.6 ~ str_replace(stimulus_id, "_X", "_u"),
      str_detect(stimulus_id, "if1") & pa>=t ~ str_replace(stimulus_id, "_X", "_h"),
      str_detect(stimulus_id, "if1") & pa<=1-t ~ str_replace(stimulus_id, "_X", "_l"), 
      TRUE ~ stimulus_id)
    );
  return(tables)
}
tables.all <- tables_to_stimuli(tables.all)
# Add likelihoods ---------------------------------------------------------
sigma = 0.01; 
tables.enriched <- tables.all %>% likelihood(sigma) %>%
  add_column(indep_sigma=sigma, nor_theta=NA, nor_beta=NA) %>%
  rowid_to_column("id")
stopifnot(tables.enriched %>% 
  filter(!is.na(logL_ind) & !is.infinite(logL_ind)) %>% 
  nrow() == nrow(tables.enriched))
# Save tables -------------------------------------------------------------
save_tables_as <- here("data", "tables-stimuli.rds")
tables.enriched %>% saveRDS(save_tables_as)
# tables.all <- readRDS(here("data", "tables-stimuli.rds"))

most_likely_cn <- function(tables.enriched){
  # check own cn is most likely one!
  max.ll = tables.enriched %>%
    pivot_longer(cols=starts_with("logL"), names_to="logL.key", values_to="logL.val") %>%
    group_by(id, stimulus_id) %>%
    summarize(max_ll=max(logL.val), ll=logL.key[logL.val==max_ll], .groups='drop_last')
  
  max.ll.match = max.ll %>%
    mutate(match=case_when(str_detect(stimulus_id, "independent") & ll=="logL_ind" ~ TRUE,
                           str_detect(stimulus_id, "if1") & ll=="logL_if1" ~ TRUE,
                           str_detect(stimulus_id, "if2") & ll=="logL_if2" ~ TRUE,
                           TRUE ~ FALSE))
  df <- max.ll.match %>% mutate(cn=substr(stimulus_id, 1, 3)) %>%
    group_by(cn) %>% summarize(N=n(), n=sum(match))
  return(df)
}
most_likely_cn(tables.enriched)

# ---------------------------------------------------------------------------- #
# plot mean values of tables for each stimulus # 
tables.mean = tables.enriched %>% group_by(stimulus_id) %>%
  summarize(AC=mean(AC), `A-C`=mean(`A-C`), `-AC`=mean(`-AC`), `-A-C`=mean(`-A-C`),
            .groups="drop_last") %>% 
  pivot_longer(cols=c(`AC`, `A-C`, `-AC`, `-A-C`), names_to="vs", values_to="ps") %>%
  mutate(ps=round(ps, 4), vs=factor(vs, levels=c("AC", "A-C", "-AC", "-A-C"))) %>% 
  rename(cell=vs, val=ps)

plot_table_means <- function(data){
  p <- data %>%
    ggplot() +
    geom_bar(aes(x=factor(0), y=val, fill=stimulus_id), stat="identity", position="dodge") +
    facet_wrap(~cell, ncol = 2, scales = "free",
               labeller = labeller(cell = c(
                                            `AC` = "P(A,C)",
                                            `A-C` = "P(A,¬C)",
                                            `-AC`= "P(¬A,C)",
                                            `-A-C` = "P(¬A,¬C)"
                                            ))
    ) +
    ylim(0,1) +
    theme_classic(base_size = 20)
  return(p)
}
# plot_table_means(tables.mean %>% filter(stimulus_id %in% c("if1_uh", "if2_ul")))
# plot_table_means(tables.mean %>% filter(stimulus_id %in% c("if1_ll", "if2_ll", "independent_ll", "if1_lh")))
# plot_table_means(tables.mean %>% filter(stimulus_id %in% c("if1_hh", "if2_hh", "independent_hh")))
# plot_table_means(tables.mean %>% filter(stimulus_id %in% c("if2_uh", "independent_uh", "if1_uu")))
# plot_table_means(tables.mean %>% filter(stimulus_id %in% c( "independent_hl", "independent_ul")))
plot_table_means(tables.mean %>% filter(stimulus_id %in% c("independent_uh")))

# p <- plot_table_means(tables.mean %>% filter(stimulus_id %in% c("if1_uh"))) +
#   theme(legend.position = "none") + labs(x="", y="")
# ggsave(here("data", "default-model", "figs", "example-table-if.png"), p, width=10, height=7)

tables.mean.probs = tables.mean %>% unnest(c(cell, val)) %>%
  group_by(stimulus_id) %>%
  pivot_wider(names_from = "cell", values_from = "val") %>%
  add_meaning_probs();


tables.mean <- tables.mean %>% group_by(stimulus_id) %>%
  # unite(stimulus_id, id, stimulus_id, sep="_") %>%
  rename(vs=cell, ps=val) %>% group_by(stimulus_id) %>%
  mutate(ps = list(ps)) %>%
  mutate(vs=list(c("AC", "A-C", "-AC", "-A-C"))) %>%
  distinct()

# ---------------------------------------------------------------------------- #
tables.long <- tables.enriched %>%
  unite(stimulus_id, id, stimulus_id, sep=".") %>% 
  pivot_longer(cols=c(`AC`, `A-C`, `-AC`, `-A-C`), names_to="vs", values_to="ps") %>%
  mutate(ps=round(ps, 4))
analyze_tables("", 0.8, tables.long)
# tables.long %>%
# separate(stimulus_id, c("id", "stimulus"), sep="\\.") %>% select(-id) %>% 
# rename(cn=stimulus, cell=vs, val=ps) %>%
# plot_tables()

tables.wide <- tables.long %>% 
  mutate(vs=list(c("AC", "A-C", "-AC", "-A-C"))) %>% 
  group_by(stimulus_id, vs, logL_ind, logL_if1, logL_if2) %>%
  summarise(ps = list(ps), .groups = 'drop') 


# Theoretical tables used in model ----------------------------------------
tables.model <- readRDS("data/default-model/tables-default.rds") 
tables.model.wide = tables.model %>% unnest(c(vs, ps)) %>%
  select(-seed, -n_tables) %>%
  group_by(id) %>% pivot_wider(names_from="vs", values_from="ps") %>% 
  likelihood(tables.model$indep_sigma[[1]]) %>%
  mutate(stimulus_id=case_when(cn=="A || C" ~ "independent",
                               TRUE ~ "if1_X"))# %>%
  # filter_at(vars(starts_with("logL_")), all_vars(!is.na(.)))
tables.model.wide <- tables_to_stimuli(tables.model.wide)

tables.model.long <- tables.model.wide %>%
  pivot_longer(cols=c(`AC`, `A-C`, `-AC`, `-A-C`),
               names_to="vs", values_to="ps")
tables.model.enriched <- tables.model.long %>%
  mutate(vs=list(c("AC", "A-C", "-AC", "-A-C"))) %>% 
  group_by(id, stimulus_id, vs, cn, nor_theta, nor_beta, indep_sigma,
           logL_ind, logL_if1, logL_if2) %>%
  summarise(ps = list(ps), .groups = 'drop')

# most_likely_cn(tables.model.enriched)

# Run Model ---------------------------------------------------------------
stimuli = c("independent_ul", "independent_uh", "independent_ll", "independent_hl", "independent_hh",
            "if2_ul", "if2_uh", "if2_ll", "if2_hl", "if2_hh",
            "if1_uu", "if1_uh", "if1_lh", "if1_hh")
params <- configure(c("speaker_tables_stimuli"))
bns = tables.model.enriched %>% filter(stimulus_id %in% stimuli)
# bns = tables.wide %>%
  # separate("stimulus_id", into=c("id", "stimulus"), sep="\\.") %>%
  # filter(stimulus %in% stimuli) %>%
  # unite("stimulus_id", "id", "stimulus", sep=".");
  # tables.mean %>% filter(stimulus_id %in% stimuli);

params$bn_ids =  bns %>% pull(stimulus_id) %>% unique
params$add_accept_conditions = FALSE
params$verbose = TRUE
# params$tables = tables.wide
params$tables = tables.model.enriched

if(!file.exists(params$utts_path)){
  utterances <- generate_utts(params)
} else {
  utterances <- readRDS(params$utts_path)
  print(paste("utterances read from:", params$utts_path))
}
params$utterances <- utterances
  
params$target <- file.path(
  params$target_dir,
  paste(str_sub(params$target_fn, 1, -5), "-", params$level_max, ".rds", sep=""),
  fsep=.Platform$file.sep
)
params$target_params <- str_replace(params$target, "results", "params")
# dir.create(params$target_dir, recursive = TRUE)

posterior <- run_webppl(params$model_path, params)
if(params$level_max %in% c("prior", "LL")) {
  data <- posterior %>% structure_listener_data(params)
  data.wide <- data %>% group_by(bn_id) %>%
    pivot_wider(names_from = "cell", values_from="val") %>%
    likelihood(params$indep_sigma) %>%
    select(-p_c_given_a, -p_c_given_na, -pa, -pc)
  
} else if(params$level_max == "speaker"){
  data <- posterior %>% structure_speaker_data(params) %>%
    select(-p_delta, -p_rooij, -p_diff) %>%
    pivot_wider(names_from="utterance", names_prefix="utt.", values_from=probs) %>%
    add_column(stimulus_id=posterior$bns$id) %>%
    separate(stimulus_id, into=c("id", "stimulus"), sep="\\.") %>% 
    select(-bias, -level, -bn_id)
  data.long <- data %>%
    pivot_longer(cols=starts_with("utt."), names_prefix="utt.",
                 names_to="response", values_to="probs")
  
  speaker_avg <- data.long %>%
    group_by(response, stimulus, cn) %>%
    summarise(avg=mean(probs), .groups="drop_last") %>% arrange(avg)
  
  speaker_avg.wide <- speaker_avg %>% group_by(stimulus, cn) %>%
    pivot_wider(names_from = "response", values_from="avg")
  df.green_blue <- speaker_avg %>%
    filter(str_detect(stimulus, "if1_uh|if1_uu|if2_hh|if2_ll|independent_ul")) %>%
    mutate(response=case_when(
      str_detect(response, "-C") ~ str_replace(response, "-C", "the blue block does not fall"),
      str_detect(response, "C") ~ str_replace(response, "C", "the blue block falls"),
      TRUE ~ response)) %>%
    mutate(response=case_when(
      str_detect(response, "-A") ~ str_replace(response, "-A", "the green block does not fall"),
      str_detect(response, "A") ~ str_replace(response, "A", "the green block falls"),
      TRUE ~ response));
  
  df.blue_green <- speaker_avg %>%
    filter(!str_detect(stimulus, "if1_uh|if1_uu|if2_hh|if2_ll|independent_ul")) %>%
    mutate(response=case_when(
      str_detect(response, "-A") ~ str_replace(response, "-A", "the blue block does not fall"),
      str_detect(response, "A") ~ str_replace(response, "A", "the blue block falls"),
      TRUE ~ response)) %>%
    mutate(response=case_when(
      str_detect(response, "-C") ~ str_replace(response, "-C", "the green block does not fall"),
      str_detect(response, "C") ~ str_replace(response, "C", "the green block falls"),
      TRUE ~ response));
  
  df <- bind_rows(df.blue_green, df.green_blue) %>%
    mutate(response=case_when(str_detect(response, " >") ~
                                paste("if", str_replace(response, " >", "")),
                              TRUE ~ response)) %>%
    mutate(response=str_replace(response, "likely", "probably"),
           response=case_when(response=="the green block falls and the blue block falls" ~ "both blocks fall",
                              response=="the blue block falls and the green block falls" ~ "both blocks fall",
                              response=="the green block does not fall and the blue block does not fall" ~
                                "neither block falls",
                              response=="the blue block does not fall and the green block does not fall" ~
                                "neither block falls",
                              TRUE ~ response)) %>%
    mutate(response=str_replace(response, "and", "but")) %>%
    mutate(response=case_when(
      response=="the green block does not fall but the blue block falls" ~
        "the blue block falls but the green block does not fall",
      response=="the blue block does not fall but the green block falls" ~
        "the green block falls but the blue block does not fall",
      TRUE ~ response)) %>%
    ungroup() 
    
  df %>% select(response) %>% unique()
  df %>% saveRDS(here("data", "speaker-predictions-means-stimuli.rds"))
}






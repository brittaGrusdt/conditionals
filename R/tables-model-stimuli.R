library(tidyverse)
library(here)
source("R/helper-functions.R")
source("R/default-model/helpers-tables.R")
source("R/helpers-webppl.R")
source("R/helpers-values-of-interest.R")
library(rwebppl)

# Tables for qualitative model check --------------------------------------

# associate tables with stimuli
tables_to_stimuli <- function(tables.all.wide, t=0.8){
  tables <- tables.all.wide %>%
    mutate(pa=`AC` + `A-C`, pc=`AC`+`-AC`) %>%
    compute_cond_prob("P(C|A)") %>% rename(pc_given_a=p) %>%
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
      
      str_detect(stimulus_id, "if1") & pa>=0.4 & pa<=0.6 ~ paste(stimulus_id, "u", sep="_"),
      str_detect(stimulus_id, "if1") & pa>=t ~ paste(stimulus_id, "h", sep="_"),
      str_detect(stimulus_id, "if1") & pa<=1-t ~ paste(stimulus_id, "l", sep="_"), 
      TRUE ~ paste(stimulus_id, "X", sep="_"))) %>% 
    mutate(stimulus_id=
             case_when(str_detect(stimulus_id, "if1") & pc_given_a >=t ~ paste(stimulus_id, "h", sep=""),
                       str_detect(stimulus_id, "if1") & pc_given_a <=1-t ~ paste(stimulus_id, "l", sep=""),
                       str_detect(stimulus_id, "if1") & pc_given_a >=0.4 & pc_given_a<=0.6 ~
                         paste(stimulus_id, "u", sep=""),
                       TRUE ~ stimulus_id)
           );
  return(tables)
}

# Theoretical tables used in model ----------------------------------------
tables.model <- readRDS("data/default-model/tables-default.rds") 
tables.model.wide = tables.model %>% unnest(c(vs, ps)) %>%
  select(-seed, -n_tables) %>%
  group_by(id) %>% pivot_wider(names_from="vs", values_from="ps") %>% 
  likelihood(tables.model$indep_sigma[[1]]) %>%
  mutate(stimulus_id=case_when(cn=="A || C" ~ "independent",
                               TRUE ~ "if1")) %>%
  tables_to_stimuli()

tables.model.long <- tables.model.wide %>%
  pivot_longer(cols=c(`AC`, `A-C`, `-AC`, `-A-C`),
               names_to="vs", values_to="ps")
tables.model.towppl <- tables.model.long %>%
  mutate(vs=list(c("AC", "A-C", "-AC", "-A-C"))) %>% 
  group_by(id, stimulus_id, vs, cn, nor_theta, nor_beta, indep_sigma,
           logL_ind, logL_if1, logL_if2) %>%
  summarise(ps = list(ps), .groups = 'drop')

# Save tables -------------------------------------------------------------
save_tables_as <- here("data", "tables-default-with-stimuli.rds")
tables.model.towppl %>% saveRDS(save_tables_as)

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
analyze_tables("", 0.8, tables.model.long)
# tables.model.long %>%
# separate(stimulus_id, c("id", "stimulus"), sep="\\.") %>% select(-id) %>% 
# rename(cn=stimulus, cell=vs, val=ps) %>%
# plot_tables()

# Run Model ---------------------------------------------------------------
stimuli = c("independent_ul", "independent_uh", "independent_ll", "independent_hl", "independent_hh",
            "if2_ul", "if2_uh", "if2_ll", "if2_hl", "if2_hh",
            "if1_uu", "if1_uh", "if1_lh", "if1_hh")
params <- configure(c("speaker_tables_stimuli"))
bns = tables.model.towppl %>% filter(stimulus_id %in% stimuli)
params$bn_ids =  bns %>% pull(stimulus_id) %>% unique
params$add_accept_conditions = FALSE
params$verbose = TRUE
# params$tables = tables.wide
params$tables = tables.model.towppl

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
    rename(id=bn_id, stimulus=stimulus_id) %>% 
    select(-bias, -level)
  data.long <- data %>%
    pivot_longer(cols=starts_with("utt."), names_prefix="utt.",
                 names_to="response", values_to="probs")
  
  data.long %>% group_by(stimulus, cn) %>% summarize(n=n())
  
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






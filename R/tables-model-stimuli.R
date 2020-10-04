library(tidyverse)
library(here)
source("R/helper-functions.R")
source("R/default-model/helpers-tables.R")
source("R/helpers-webppl.R")
source("R/helpers-values-of-interest.R")
library(rwebppl)

# Tables for qualitative model check --------------------------------------
params <- configure(c("speaker_tables_stimuli"))
if(params$generate_tables || !file.exists(params$tables_path)){
  tables.model <- create_tables(params)
  # Theoretical tables used in model ----------------------------------------   
  tables.model.wide = tables.model %>% #unnest(c(vs, ps)) %>%
    select(-seed, -vs, -ps) %>%
    mutate(stimulus_id=case_when(cn=="A || C" ~ "independent",
                                 TRUE ~ "if")) %>%
    tables_to_stimuli()
  
  tables.model.long <- tables.model.wide %>%
    pivot_longer(cols=c(`AC`, `A-C`, `-AC`, `-A-C`),
                 names_to="vs", values_to="ps")
  
  tables.model.towppl <- tables.model.long %>%
    mutate(vs=list(c("AC", "A-C", "-AC", "-A-C"))) %>% 
    group_by(logL_if_ac, logL_if_anc, logL_if_ca, logL_if_cna, logL_ind, id, stimulus_id, vs, cn) %>%
    summarise(ps = list(ps), .groups = 'drop')
  
  tables.model.long %>% saveRDS(here(str_replace(params$tables_path, "-to-wppl", "-long")))
  tables.model.wide %>% saveRDS(here(str_replace(params$tables_path, "-to-wppl", "-wide")))
  tables.model.towppl %>% saveRDS(here(params$tables_path))
  
  # some table checks
  tables.per_stimuli <- tables.model.wide %>%
    select(id, stimulus_id, AC, `A-C`, `-AC`, `-A-C`) %>%
    group_by(stimulus_id) %>% summarize(n=n(), .groups="drop_last")
  
  #likelihoods
  max.ll=tables.model.wide %>%
    select(id, cn, starts_with("logL"), `AC`, `-AC`, `A-C`, `-A-C`) %>%
    group_by(id) %>% pivot_longer(starts_with("logL"), names_to="logL.cn",
                                  names_prefix="logL_", values_to="logL.val") %>%
    mutate(max=max(logL.val)) %>% filter(logL.val==max)
  # max.ll %>% filter((cn=="A || C" & logL.cn != "ind"))
  # max.ll %>% filter((cn!="A || C" & logL.cn == "ind"))
  
    
} else {
  tables.model.towppl <- readRDS(params$tables_path)
  tables.model.long <- readRDS(str_replace(params$tables_path, "-to-wppl", "-long"))
  print(paste("tables read from:", params$tables_path))
}

# Run ---------------------------------------------------------------------
# most_likely_cn <- function(tables.enriched){
#   # check own cn is most likely one!
#   max.ll = tables.enriched %>%
#     pivot_longer(cols=starts_with("logL"), names_to="logL.key", values_to="logL.val") %>%
#     group_by(id, stimulus_id) %>%
#     summarize(max_ll=max(logL.val), ll=logL.key[logL.val==max_ll], .groups='drop_last')
#   
#   max.ll.match = max.ll %>%
#     mutate(match=case_when(str_detect(stimulus_id, "independent") & ll=="logL_ind" ~ TRUE,
#                            str_detect(stimulus_id, "if1") & ll=="logL_if1" ~ TRUE,
#                            str_detect(stimulus_id, "if2") & ll=="logL_if2" ~ TRUE,
#                            TRUE ~ FALSE))
#   df <- max.ll.match %>% mutate(cn=substr(stimulus_id, 1, 3)) %>%
#     group_by(cn) %>% summarize(N=n(), n=sum(match))
#   return(df)
# }
# most_likely_cn(tables.model.towppl)
analyze_tables("", 0.8, tables.model.long)
# tables.model.long %>%
# separate(stimulus_id, c("id", "stimulus"), sep="\\.") %>% select(-id) %>% 
# rename(cn=stimulus, cell=vs, val=ps) %>%
# plot_tables()

# Run Model ---------------------------------------------------------------
stimuli = c("independent_ul", "independent_uh", "independent_ll", "independent_hl", "independent_hh",
            "if2_uh", "if2_hh", "if1_uu", "if1_uh", "if1_lh", "if1_hh",
            "if2_ll", "if2_hl", "if2_ul")

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
    arrange(desc(prob)) 
  
  dat <- data.wide %>% mutate(bn.id=substr(bn.id, 1,3)) %>%
    group_by(bn.id) %>% summarize(P=sum(prob)) 

} else if(params$level_max == "priorN") {
  tables = tibble(cn=posterior$bns$cn, bn_id=seq(1,nrow(posterior$bns)),
                  ps=posterior$bns$table.probs, vs=posterior$bns$table.support) %>%
            unnest(c(ps,vs)) %>% group_by(bn_id)
  tables.wide = tables %>% pivot_wider(names_from="vs", values_from="ps")
  
  tables.wide %>% filter(cn=="A implies C") %>%
    compute_cond_prob("P(C|A)") %>%
    filter(p>=0.8) %>%
    mutate(pa=`AC`+`A-C`, pc=AC+`-AC`) %>% 
    filter(pa<0.8 & pc<0.8 & pa>0.1 & pc>0.1 &
           `AC`<0.8 & `A-C`<0.8 & `-AC`<0.8 & `-A-C`<0.8 &
           `AC`>0.1 & `A-C`>0.1 & `-AC`>0.1 & `-A-C`>0.1)
  
  tables.wide %>% group_by(cn) %>% summarize(n=n())
  analyze_tables("", 0.8, TABLES=tables)
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
  speaker_avg %>% filter(avg>0 & str_detect(stimulus, "independent") &
                         str_detect(response, ">") & cn == "A || C") %>%
    arrange(desc(avg))
  
  df %>% saveRDS(here("data", "speaker-predictions-means-stimuli.rds"))
}






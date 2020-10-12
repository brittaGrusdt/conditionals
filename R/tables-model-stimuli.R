library(tidyverse)
library(here)
source("R/helper-functions.R")
source("R/default-model/helpers-tables.R")
source("R/helpers-webppl.R")
source("R/helpers-values-of-interest.R")
library(rwebppl)

# Tables for qualitative model check --------------------------------------
# load data for speaker: speaker distribution for N states sampled from prior
speaker = readRDS(here("data", "test-default", "results-speaker.rds"))
speaker.samples =  speaker %>%
  select(-p_diff, -bias, -p_delta, -p_rooij, -level)
params = readRDS(here("data", "test-default", "params-speaker.rds"))

# add stimulus id
df.speaker = speaker.samples %>%
  mutate(stimulus_id = case_when(cn == "A || C" ~ "independent", TRUE ~ "if")) %>%
  tables_to_stimuli();

stimuli = c("if1_uh", "if1_lh", "if1_hh", "if2_ll", "if2_hl", "if2_ul",
  "independent_ul", "independent_uh", "independent_ll", "independent_hl", "independent_hh"
);
speaker.means = df.speaker %>%
  select(-pa, -pc, -pc_given_a, -pc_given_na) %>%
  group_by(stimulus_id, cn, utterance) %>% 
  summarize(response=mean(probs), .groups="drop_last") %>%
  add_column(predictor="model") %>%
  filter(stimulus_id %in% stimuli)


# map to colors to compare with empirical data
speaker.means.wide <- speaker.means %>% group_by(stimulus_id, cn) %>%
  pivot_wider(names_from = "utterance", values_from="response")

df.green_blue <- speaker.means %>%
  # filter(str_detect(stimulus, "if1_uh|if1_uu|if2_hh|if2_ll|independent_ul")) %>%
  mutate(utterance=case_when(
    str_detect(utterance, "-C") ~ str_replace(utterance, "-C", "the blue block does not fall"),
    str_detect(utterance, "C") ~ str_replace(utterance, "C", "the blue block falls"),
    TRUE ~ utterance)) %>%
  mutate(utterance=case_when(
    str_detect(utterance, "-A") ~ str_replace(utterance, "-A", "the green block does not fall"),
    str_detect(utterance, "A") ~ str_replace(utterance, "A", "the green block falls"),
    TRUE ~ utterance));

df.blue_green <- speaker.means %>%
  # filter(!str_detect(stimulus, "if1_uh|if1_uu|if2_hh|if2_ll|independent_ul")) %>%
  mutate(utterance=case_when(
    str_detect(utterance, "-A") ~ str_replace(utterance, "-A", "the blue block does not fall"),
    str_detect(utterance, "A") ~ str_replace(utterance, "A", "the blue block falls"),
    TRUE ~ utterance)) %>%
  mutate(utterance=case_when(
    str_detect(utterance, "-C") ~ str_replace(utterance, "-C", "the green block does not fall"),
    str_detect(utterance, "C") ~ str_replace(utterance, "C", "the green block falls"),
    TRUE ~ utterance));

df <- bind_rows(df.blue_green, df.green_blue) %>%
  mutate(utterance=case_when(str_detect(utterance, " >") ~
                              paste("if", str_replace(utterance, " >", "")),
                            TRUE ~ utterance)) %>%
  mutate(utterance=case_when(str_detect(utterance, "likely") ~ str_replace(utterance, "falls", "might fall"),
                             TRUE ~ utterance)) %>%
  mutate(utterance=case_when(str_detect(utterance, "likely") ~ str_replace(utterance, "does not fall", "might not fall"),
                            TRUE ~ utterance)) %>% 
  mutate(utterance=case_when(str_detect(utterance, "might") ~ str_replace(utterance, "likely", ""),
                             TRUE ~ utterance)) %>%
  mutate(utterance=case_when(utterance=="the green block falls and the blue block falls" ~ "both blocks fall",
                            utterance=="the blue block falls and the green block falls" ~ "both blocks fall",
                            utterance=="the green block does not fall and the blue block does not fall" ~
                              "neither block falls",
                            utterance=="the blue block does not fall and the green block does not fall" ~
                              "neither block falls",
                            TRUE ~ utterance)) %>%
  mutate(utterance=str_replace(utterance, "and", "but")) %>%
  mutate(utterance=case_when(
    utterance=="the green block does not fall but the blue block falls" ~
      "the blue block falls but the green block does not fall",
    utterance=="the blue block does not fall but the green block falls" ~
      "the green block falls but the blue block does not fall",
    TRUE ~ utterance)) %>%
  mutate(utterance=str_trim(utterance)) %>%
  ungroup() 

df %>% select(utterance) %>% unique()
df %>% filter(response>0 & str_detect(stimulus_id, "independent") &
              str_detect(utterance, ">") & cn == "A || C") %>%
  arrange(desc(response))

df %>% saveRDS(here("data", "speaker-predictions-means-stimuli.rds"))


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
# analyze_tables("", 0.8, tables.model.long)
# tables.model.long %>%
# separate(stimulus_id, c("id", "stimulus"), sep="\\.") %>% select(-id) %>% 
# rename(cn=stimulus, cell=vs, val=ps) %>%
# plot_tables()

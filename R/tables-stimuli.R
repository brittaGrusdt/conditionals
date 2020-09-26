library(tidyverse)
library(here)
source("R/helper-functions.R")
source("R/default-model/helpers-tables.R")
source("R/helpers-webppl.R")
source("R/helpers-values-of-interest.R")
library(rwebppl)


# Tables for qualitative model check --------------------------------------
# @tables: wide format, with column 'p' present which will be filtered for
# @prior: "high"|"uncertain"|"low"
# filterPrior <- function(tables, prior){
#   if(prior=="high") {
#     df <- tables %>% filter(p >= 0.9)
#   } else if(prior=="uncertain") {
#     df <- tables %>% filter(p >= 0.4 & p <= 0.6)
#   }else if(prior == "low"){
#     df <- tables %>% filter(p <= 0.1)
#   }
#   return(df)
# }
# 
# qualitative_tables_experiment <- function(){
#   params.tables <- configure("tables")
#   if(!file.exists(params.tables$tables_path)) {
#     tables <- create_tables(params.tables)
#   }else {
#     tables <- readRDS(params.tables$tables_path)
#     print(paste("tables read from", params.tables$tables_path))
#   }
#   
#   tables <- tables %>% unnest_tables() %>%
#     select(-nor_theta, -nor_beta, -indep_sigma, -seed, -rowid) %>%
#     pivot_wider(names_from="cell", values_from = "val") %>%
#     mutate(pa=`AC`+`A-C`, pc=`AC`+`-AC`)
#   
#   return(tables)
# }
# 
# tables_stimuli <- function(cn, prior_a, prior_c){
#   tables <- qualitative_tables_experiment() %>% filter(cn==(!! cn)) %>%
#     select(-n_tables, -id);
#   df <- (tables %>% rename(p=pa)) %>% filterPrior(prior_a)
#   df <- df %>% rename(pa=p, p=pc) %>% filterPrior(prior_c) %>%
#     select(-pa, -p, -cn)
#   return(df)
# }
# 
# # nb.tables <- df %>% group_by(cn, pa, pc) %>% summarize(n=n())
# # randomly sample 1000 per "stimulus_id"
# 
# create_tables_stimuli = function(cn, id, probs_a, probs_c=c("low", "uncertain", "high")){
#   tables.all = tibble()
#   for(pa in probs_a){
#     for(pc in c("low", "uncertain", "high")) {
#       df <- tables_stimuli(cn, pa, pc) %>%
#         add_column(stimulus_id=paste(id, "_", substr(pa, 1, 1), substr(pc, 1, 1), sep=""));
#       tables.all <- tables.all %>% bind_rows(df);
#     }
#   }
#   return(tables.all)
# }

if1_tables = function(pos, neg, marginal, id, n=500){
  df <- tibble(pc_given_a = rbeta(n, pos$alpha, pos$beta),
               pc_given_na = rbeta(n, neg$alpha, neg$beta),
               pa = rbeta(n, marginal$alpha, marginal$beta))
  df <- df %>%
    mutate(`AC`=pc_given_a * pa,
           `A-C`=(1-pc_given_a) * pa,
           `-AC`=pc_given_na * (1-pa),
           `-A-C`=(1-pc_given_na) * (1-pa)
    ) %>%
    add_column(stimulus_id=(!! id)) %>%
    select(-pc_given_a, -pc_given_na, -pa)
  return(df)
}

if2_tables = function(pp, nn, np, pn, marginal_a, marginal_b, id, n=500){
  df <- tibble(pc_given_ab = rbeta(n, pp$alpha, pp$beta),
               pc_given_nanb = rbeta(n, nn$alpha, nn$beta),
               pc_given_nab = rbeta(n, np$alpha, np$beta),
               pc_given_anb = rbeta(n, pn$alpha, pn$beta),
               pa = rbeta(n, marginal_a$alpha, marginal_a$beta),
               pb = rbeta(n, marginal_b$alpha, marginal_b$beta)
  )
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

indep_tables <- function(param_prior_a, param_prior_c, id, n=1000){
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
tables.all <- indep_tables(list(alpha=10, beta=1), list(alpha=10, beta=1), "independent_hh") %>% 
  bind_rows(indep_tables(list(alpha=10, beta=1), list(alpha=1, beta=10), "independent_hl")) %>% 
  bind_rows(indep_tables(list(alpha=1, beta=10), list(alpha=1, beta=10), "independent_ll")) %>% 
  bind_rows(indep_tables(list(alpha=25, beta=25), list(alpha=10, beta=1), "independent_uh")) %>% 
  bind_rows(indep_tables(list(alpha=25, beta=25), list(alpha=1, beta=10), "independent_ul")) %>%
  
  bind_rows(indep_tables(list(alpha=25, beta=25), list(alpha=25, beta=25), "independent_uu")) %>%
  bind_rows(indep_tables(list(alpha=1, beta=10), list(alpha=25, beta=25), "independent_lu")) %>%
  bind_rows(indep_tables(list(alpha=1, beta=10), list(alpha=10, beta=1), "independent_lh")) %>%
  bind_rows(indep_tables(list(alpha=10, beta=1), list(alpha=25, beta=25), "independent_hu"));

# If1 ---------------------------------------------------------------------
tables.all <- tables.all %>%
  bind_rows(if1_tables(pos=list(alpha=10, beta=1), neg=list(alpha=1, beta=10),
                       marginal=list(alpha=1, beta=10), "if1_lh")) %>%
  bind_rows(if1_tables(pos=list(alpha=10, beta=1), neg=list(alpha=1, beta=10),
                       marginal=list(alpha=10, beta=1), "if1_hh")) %>%
  bind_rows(if1_tables(pos=list(alpha=10, beta=1), neg=list(alpha=1, beta=10),
                       marginal=list(alpha=25, beta=25), "if1_uh")) %>%
  bind_rows(if1_tables(pos=list(alpha=1, beta=10), neg=list(alpha=1, beta=10),
                       marginal=list(alpha=1, beta=10), "if1_ll")) %>%
  bind_rows(if1_tables(pos=list(alpha=1, beta=10), neg=list(alpha=1, beta=10),
                       marginal=list(alpha=10, beta=1), "if1_hl")) %>% 
  bind_rows(if1_tables(pos=list(alpha=1, beta=10), neg=list(alpha=1, beta=10),
                       marginal=list(alpha=25, beta=25), "if1_ul")) %>% 
  bind_rows(if1_tables(pos=list(alpha=25, beta=25), neg=list(alpha=1, beta=10),
                       marginal=list(alpha=10, beta=1), "if1_hu")) %>% 
  bind_rows(if1_tables(pos=list(alpha=25, beta=25), neg=list(alpha=1, beta=10),
                       marginal=list(alpha=1, beta=10), "if1_lu")) %>% 
  bind_rows(if1_tables(pos=list(alpha=25, beta=25), neg=list(alpha=1, beta=10),
                       marginal=list(alpha=25, beta=25), "if1_uu"));

# If2 ---------------------------------------------------------------------
tables.all <- tables.all %>%
  bind_rows(if2_tables(pp=list(alpha=25, beta=1), nn=list(alpha=1, beta=10),
                       np=list(alpha=10, beta=1), pn=list(alpha=10, beta=1),
                       marginal_a=list(alpha=25, beta=25),
                       marginal_b=list(alpha=1, beta=10),
                       "if2_ul")) %>% 
  bind_rows(if2_tables(pp=list(alpha=25, beta=1), nn=list(alpha=1, beta=10),
                       np=list(alpha=10, beta=1), pn=list(alpha=10, beta=1),
                       marginal_a=list(alpha=25, beta=25),
                       marginal_b=list(alpha=10, beta=1),
                       "if2_uh")) %>% 
  bind_rows(if2_tables(pp=list(alpha=25, beta=1), nn=list(alpha=1, beta=10),
                       np=list(alpha=10, beta=1), pn=list(alpha=10, beta=1),
                       marginal_a=list(alpha=1, beta=10),
                       marginal_b=list(alpha=1, beta=10),
                       "if2_ll")) %>%
  bind_rows(if2_tables(pp=list(alpha=25, beta=1), nn=list(alpha=1, beta=10),
                       np=list(alpha=10, beta=1), pn=list(alpha=10, beta=1),
                       marginal_a=list(alpha=10, beta=1),
                       marginal_b=list(alpha=1, beta=10),
                       "if2_hl")) %>%
  bind_rows(if2_tables(pp=list(alpha=25, beta=1), nn=list(alpha=1, beta=10),
                       np=list(alpha=10, beta=1), pn=list(alpha=10, beta=1),
                       marginal_a=list(alpha=10, beta=1),
                       marginal_b=list(alpha=10, beta=1),
                       "if2_hh")) %>%
  bind_rows(if2_tables(pp=list(alpha=25, beta=1), nn=list(alpha=1, beta=10),
                       np=list(alpha=10, beta=1), pn=list(alpha=10, beta=1),
                       marginal_a=list(alpha=25, beta=25),
                       marginal_b=list(alpha=25, beta=25),
                       "if2_uu")) %>%
  bind_rows(if2_tables(pp=list(alpha=25, beta=1), nn=list(alpha=1, beta=10),
                       np=list(alpha=10, beta=1), pn=list(alpha=10, beta=1),
                       marginal_a=list(alpha=1, beta=10),
                       marginal_b=list(alpha=25, beta=25),
                       "if2_lu")) %>%
  bind_rows(if2_tables(pp=list(alpha=25, beta=1), nn=list(alpha=1, beta=10),
                       np=list(alpha=10, beta=1), pn=list(alpha=10, beta=1),
                       marginal_a=list(alpha=1, beta=10),
                       marginal_b=list(alpha=10, beta=1),
                       "if2_lh")) %>%
  bind_rows(if2_tables(pp=list(alpha=25, beta=1), nn=list(alpha=1, beta=10),
                       np=list(alpha=10, beta=1), pn=list(alpha=10, beta=1),
                       marginal_a=list(alpha=10, beta=1),
                       marginal_b=list(alpha=25, beta=25),
                       "if2_hu"));


tables.all <- tables.all %>% 
  # mutate(cn=case_when(str_detect(stimulus_id, "independent") ~ "A || C",
  #                     TRUE ~ "A implies C")) %>%
  add_column(indep_sigma=0.001, nor_theta=NA, nor_beta=NA) %>%
  rowid_to_column("id")

save_tables_as <- here("data", "tables-stimuli.rds")
tables.all %>% saveRDS(save_tables_as)
# tables.all <- readRDS(here("data", "tables-stimuli.rds"))

# ---------------------------------------------------------------------------- #
# plot mean values of tables for each stimulus # 
tables.mean = tables.all %>% group_by(stimulus_id) %>%
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
plot_table_means(tables.mean %>% filter(stimulus_id %in% c("if1_uh", "if2_ul")))
plot_table_means(tables.mean %>% filter(stimulus_id %in% c("if1_ll", "if2_ll", "independent_ll", "if1_lh")))
plot_table_means(tables.mean %>% filter(stimulus_id %in% c("if1_hh", "if2_hh", "independent_hh", "if2_hl")))
plot_table_means(tables.mean %>% filter(stimulus_id %in% c("if2_uh", "independent_uh", "if1_uu")))
plot_table_means(tables.mean %>% filter(stimulus_id %in% c( "independent_hl", "independent_ul")))


# ---------------------------------------------------------------------------- #
tables.long <- tables.all %>%
  pivot_longer(cols=c(`AC`, `A-C`, `-AC`, `-A-C`), names_to="vs", values_to="ps") %>%
  mutate(ps=round(ps, 4))
tables.wide <- tables.long %>% group_by(stimulus_id, id) %>%
  summarise(ps = list(ps), .groups = 'drop') %>%
  mutate(vs=list(c("AC", "A-C", "-AC", "-A-C"))) %>% dplyr::select(-id)

# tables.long %>%
#   rename(cn=stimulus_id, cell=vs, val=ps) %>%
#   plot_tables()

analyze_tables("", 0.8, tables.long %>% select(-indep_sigma,-nor_theta, -nor_beta))

# Run Model ---------------------------------------------------------------
params <- configure(c("speaker_tables_stimuli"))
bns = tables.mean %>% group_by(stimulus_id) %>%
  rename(vs=cell, ps=val) %>%
  summarise(ps = list(ps), .groups = 'drop') %>%
  mutate(vs=list(c("AC", "A-C", "-AC", "-A-C"))) %>% 
  rowid_to_column("id") %>%
  unite(stimulus_id, id, stimulus_id, sep="_");
params$bn_ids =  bns %>% pull(stimulus_id)
params$add_accept_conditions = FALSE
params$verbose = TRUE
params$tables = tables.wide %>%
  bind_rows(bns)
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
} else if(params$level_max == "speaker"){
  data <- posterior %>% structure_speaker_data(params) %>%
    select(-p_delta, -p_rooij, -p_diff) %>%
    pivot_wider(names_from="utterance", names_prefix="utt.", values_from=probs) %>%
    add_column(id=posterior$bns$id) %>%
    select(-bias, -cn, -level)
  
  data %>% group_by(id) %>% summarize(n=n()) 
}

df.long = data %>% 
  pivot_longer(cols=starts_with("utt."), names_to="response",
               names_prefix="utt.", values_to="ratio")
df.long.pos = df.long %>% filter(ratio>0)

df <- df.long %>%
  group_by(response, id) %>%
  summarize(ratio=mean(ratio), .groups="keep")

df.green_blue <- df %>%
  filter(id %in% c("uh", "uu", "hh", "ll", "independent_ul")) %>%
  mutate(response=case_when(
    str_detect(response, "-C") ~ str_replace(response, "-C", "the blue block does not fall"),
    str_detect(response, "C") ~ str_replace(response, "C", "the blue block falls"),
    TRUE ~ response)) %>%
  mutate(response=case_when(
    str_detect(response, "-A") ~ str_replace(response, "-A", "the green block does not fall"),
    str_detect(response, "A") ~ str_replace(response, "A", "the green block falls"),
    TRUE ~ response));

df.blue_green <- df %>%
  filter(!id %in% c("uh", "uu", "hh", "ll", "independent_ul")) %>%
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





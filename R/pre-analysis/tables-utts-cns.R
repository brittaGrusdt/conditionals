library(rwebppl)
library(tidyverse)
library(ggplot2)

source("./R/helpers.R")

# Parameters --------------------------------------------------------------
bias <- "none"
noise_param <- 0.05
n_tables <- 1000
seed <- 123

# Setup -------------------------------------------------------------------
sep = .Platform$file.sep
target_dir <- file.path(".", "data", "model-general-tbl-utts-cns", fsep = sep)

name_tables <- paste("tables-", n_tables, "-seed-", seed, ".rds", sep="")
name_utts <- paste("utterances-seed-", seed, ".rds", sep="")

target_tables <- file.path(target_dir, name_tables, fsep = sep)
target_utterances <- file.path(target_dir, name_utts, fsep = sep)
target_cns <- file.path(target_dir, "cns.rds", fsep = sep)

# Run WebPPL --------------------------------------------------------------
data <- webppl(program_file = "./R/pre-analysis/tables-utts-cns.wppl",
               data = list(noise=noise_param,
                           n_tables=n_tables,
                           bias=bias,
                           verbose=TRUE),
               data_var = "data",
               random_seed=seed)

utterances <- data$utterances
causal_nets <- data$cns
data_tables <- data$tables %>%
               matrix(n_tables * 9, 5) %>%
               as_tibble() %>%
               mutate("V1" = as.numeric(V1),
                      "V2" = as.numeric(V2),
                      "V3" = as.numeric(V3),
                      "V4" = as.numeric(V4),
                      ) %>%
               rowid_to_column() %>%
               gather("V1", "V2", "V3", "V4", key="cell", value="val")

data_tables <- data_tables %>%
               mutate(V5 = as.factor(V5), cell = as.factor(cell),
                      cell = fct_recode(cell, `AC`="V1", `A-C`="V2",
                                              `-AC`="V3", `-A-C`="V4")
                      )
# data_cns <- data_tables  %>% select(V5)
# causal_nets <- levels(data_cns$V5)

plot_table <- function(data){
    cn <- data$V5[[1]]
    ggplot(data, aes(x=val,  color = cell)) +
    geom_density() +
    facet_wrap(~cell, scales = "free") +
    labs(title = cn)
}

# Plot all table distributions for each causal network respectively
causal_nets %>%
map(function(x){filter(data_tables, V5==x)}) %>%
map(function(x){plot_table(x)})



# Test supplying webppl with tables from R --------------------------------

# table_distrs <- list(tibble(ps=list(c(0.1,0.2,0.3, 0.4)),
#                        vs=list(c("AC", "A-C", "-AC", "-A-C"))),
#                   tibble(ps=list(c(0.9, 0.05, 0.0, 0.05)),
#                          vs=list(c("AC", "A-C", "-AC", "-A-C")))
#                   )
# d <- list(tables=table_distrs, x="A")

# webppl(program_file = "./R/pre-analysis/test.wppl",
       # data = d, data_var="data")



# Restructure tables -------------------------------------------------
tables <- data_tables %>%
          spread(key=cell, value = val) %>%
          group_by(rowid) %>%
          transmute(ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`)),
                    vs=list(c("AC", "A-C", "-AC", "-A-C")))

make_tibble <- function(i){
  if(i %% 1000 == 0){ print(i)}
  return(tables[i, c("ps", "vs")])
}
tables_list <- 1:nrow(tables) %>%  map(make_tibble)

# Save data ---------------------------------------------------------------
save <- function(data, target_path){
  data %>% write_rds(target_path)
  print(paste("saved to:", target_path))
}

tables_list %>% save(target_tables)
utterances %>% save(target_utterances)
causal_nets %>% save(target_cns)

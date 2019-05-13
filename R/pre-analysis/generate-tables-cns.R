library(rwebppl)
library(tidyverse)
library(ggplot2)
source(file.path("R", "helpers.R", fsep = .Platform$file.sep))

# Parameters --------------------------------------------------------------
noise <- 0.03
n_tables <- 1000
seed <- 123
model <- "model-general"

# Setup -------------------------------------------------------------------
main_folder <- get_target_folder(seed, noise, n_tables)
target_dir <- file.path(".", "data", "precomputations", model, main_folder,
                        fsep = .Platform$file.sep)
dir.create(target_dir, recursive = TRUE)

target_tables <- file.path(target_dir, "tables.rds", fsep = .Platform$file.sep)
target_cns <- file.path(target_dir, "cns.rds", fsep = .Platform$file.sep)

# Run WebPPL --------------------------------------------------------------
data <- webppl(program_file = "./R/pre-analysis/tables-cns.wppl",
               data = list(noise=noise,
                           n_tables=n_tables,
                           verbose=TRUE),
               data_var = "data",
               random_seed=seed)

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

plot_table <- function(data){
  cn <- data$V5[[1]]
  ggplot(data, aes(x=val,  color = cell)) +
    geom_density() +
    facet_wrap(~cell, scales = "free") +
    labs(title = cn)
}

# Plot all table distributions for each causal network respectively
# causal_nets %>%
#   map(function(x){filter(data_tables, V5==x)}) %>%
#   map(function(x){plot_table(x)})



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
causal_nets %>% save(target_cns)

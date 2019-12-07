library(tidyverse)
library(ggplot2)
source("R/helper-functions.R")
source("R/helpers-webppl.R")
source("R/helper-functions.R")
# source("R/plot-functions.R")

RESULT_DIR <- file.path("data", "douven-examples", fsep=.Platform$file.sep)
TARGET_DIR <- file.path(RESULT_DIR, "figs", fsep=.Platform$file.sep)
dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)

params <- list()
params$bias <- ""
params$target <- ""
params$save <- FALSE
# 1. Skiing ---------------------------------------------------------------
# Data
fn <- file.path(RESULT_DIR, "results-skiing.rds")
data_long <- readRDS(fn)
data_wide <- data_long %>% spread(key=cell, val=val, fill = 0)
# df <- data_wide %>% adapt_bn_ids()

trust <- data_long %>% listener_beliefs("PL", params, c("C"))
trust_df <- trust %>% select(-marginal_cn_int, -keep) %>%
  add_column(level="trust", prob=1, bn_id="0")
  
data <- bind_rows(data_long, trust_df)

# Plots -------------------------------------------------------------------
pe <- marginalize(data, c("E")) 
ev_pe <-  pe %>% expected_val("E") %>% mutate(level=as.factor(level))

p <- plot_evs(ev_pe)
title <- "Expected degree of belief in P(E)"
p <- p +
  facet_wrap(~p, labeller=labeller(p=c(`E`=paste(strwrap(title, width=20), collapse="\n"))))  +
  scale_x_discrete(
    limits = c("trust", "PL", "LL", "prior"),
    labels = c(
     paste(strwrap("Listener's beliefs conditioned on C", width=20), collapse="\n"),
     paste(strwrap("Pragmatic interpretation", width=15), collapse="\n"),
     paste(strwrap("Literal interpretation", width=15), collapse="\n"),
     "Prior Belief"
     )) +
  scale_y_continuous(limits=c(0,1))
p
fn <- paste(TARGET_DIR, "skiing.png", sep=.Platform$file.sep)
  ggsave(fn, p, height = 5, width=6)


# 2. Sundowners -----------------------------------------------------------
# Data
fn <- file.path(RESULT_DIR, "results-sundowners.rds")
data_long <- readRDS(fn)
data_wide <- data_long %>% spread(key=cell, val=val, fill = 0)
# df <- data_wide %>% adapt_bn_ids()

prs <- marginalize(data_long, c("R", "S")) 
ev_prs <- prs %>% expected_val("RS")

pr <- marginalize(data_long, c("R"))
ev_pr <- pr %>% expected_val("R")

data <- bind_rows(ev_pr, ev_prs)

# Plots
p <- plot_evs(data)

title1 <-"Expected degree of belief in P(R)"
title2 <- "Expected degree of belief in P(R ∧ S)"
ll <- "Literal interpretation"
pl <- "Pragmatic interpretation"
p <- p + facet_wrap(~p,
      labeller=labeller(p=c(`R`=paste(strwrap(title1, width=25), collapse="\n"),
                            `RS`=paste(strwrap(title2, width=25), collapse="\n")))
                       ) +
      scale_x_discrete(limits = c("prior", "LL", "PL"),
                       labels = c(
                                  paste(strwrap(pl, width=10), collapse="\n"),
                                  paste(strwrap(ll, width=10), collapse="\n"),
                                  "Prior Belief")
                       ) + 
      scale_y_continuous(limits=c(0,1))
p
fn <- paste(TARGET_DIR, "sundowners.png", sep=.Platform$file.sep)
ggsave(fn, p, height = 5, width=9)


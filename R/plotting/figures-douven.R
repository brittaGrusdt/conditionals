library(tidyverse)
library(ggplot2)
source("R/helper-functions.R")
source("R/helpers-webppl.R")
# source("R/plot-functions.R")

RESULT_DIR <- file.path("data", "douven-examples", fsep=.Platform$file.sep)
TARGET_DIR <- file.path(RESULT_DIR, "figs", fsep=.Platform$file.sep)
dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)

plot_evs <- function(data){
  p <- data %>% ggplot() +
    geom_bar(mapping = aes(x=level, y=ev, fill=level), stat="identity", position="dodge") +
    labs(x="", y="", title="") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size=12),
          text = element_text(size= 25),
          legend.position = "none") 
  return(p)
}

# 1. Skiing ---------------------------------------------------------------
# Data
fn <- file.path(RESULT_DIR, "results-skiing.rds")
data_long <- readRDS(fn)
data_wide <- data_long %>% spread(key=cell, val=val, fill = 0)
# df <- data_wide %>% adapt_bn_ids()

trust <- data_long %>% listener_beliefs("PL", c("C"))
trust_df <- trust %>% select(-marginal_cn_int, -keep) %>% add_column(level="trust", prob=1, bn_id=NA)
  
data <- bind_rows(data_long, trust_df)

# Plots -------------------------------------------------------------------
pe <- marginalize(data, c("E")) 
ev_pe <-  pe %>% expected_val("E") %>% mutate(level=as.factor(level))

p <- plot_evs(ev_pe)
p <- p + facet_wrap(~p, labeller=labeller(p=c(`E`=paste(strwrap("Expected degree of belief in P(E)", width=40),
                                            collapse="\n"))))  +
  scale_x_discrete(limits = c("prior", "LL", "PL", "trust"),
                     labels = c("Prior Belief",
                     paste(strwrap("Literal interpretation", width=10), collapse="\n"),
                     paste(strwrap("Pragmatic interpretation", width=10), collapse="\n"),
                     paste(strwrap("Listener's beliefs conditioned on C", width=20), collapse="\n"))
                     ) 
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
p <- p + facet_wrap(~p, labeller=labeller(p=c(`R`=paste(strwrap("Expected degree of belief in P(R)", width=25),
                                                 collapse="\n"),
                                              `RS`=paste(strwrap("Expected degree of belief in P(R ∧ S)",
                                                                 width=25), collapse="\n")))) +
      scale_x_discrete(limits = c("prior", "LL", "PL"),
                       labels = c("Prior Belief",
                                  paste(strwrap("Literal interpretation", width=10), collapse="\n"),
                                  paste(strwrap("Pragmatic interpretation", width=10), collapse="\n"))) 
p
fn <- paste(TARGET_DIR, "sundowners.png", sep=.Platform$file.sep)
ggsave(fn, p, height = 6, width=9)


library(tidyverse)
library(ggplot2)
source("R/helpers.R")
source("R/plot-functions.R")

RESULT_DIR <- file.path("data", "douven-examples", fsep=.Platform$file.sep)
TARGET_DIR <- file.path(RESULT_DIR, "figs", fsep=.Platform$file.sep)
dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)



# Skiing ------------------------------------------------------------------
fn <- file.path(RESULT_DIR, "results-skiing.rds")
data_long <- readRDS(fn)
data_wide <- data_long %>% spread(key=cell, val=val, fill = 0)
df <- data_wide %>% adapt_bn_ids()

# todo: plot-cns nur für default model
plot_cns(df, level=NULL, save_as=NULL)

# plot distribution of each Bayes net
plot_distr <- function(d, x){
  t <- paste(x$level, " P(bn", x$bn_id, ")=", d$prob,
             " cn: ", d$cn, sep="")
  
  p <- d  %>% dplyr::select(-cn, -prob) %>% gather(., key=cell, val=val) %>% 
    ggplot() + geom_bar(mapping=aes(x=cell, y=val), stat="identity") +
               labs(title = t)
  return(p)
}
data_wide %>% filter(level=="prior") %>% group_by(bn_id, level) %>% group_map(plot_distr)

# plot distribution over Bayes nets 
plot_bns(df)

pe <- marginalize(data_long, c("E")) 

p <- plot_evs_bar(ev_pe, "E") 
p <- p + scale_x_discrete(limits = c("PL-beliefs", "PL", "LL", "prior"),
                          labels = c("pragmatic interpretation conditioned on B",
                                  "pragmatic interpretation",
                                  "literal interpretation",
                                  "belief before hearing 'If E, S'"),
                          position = "top") +
      scale_y_continuous(limits=c(0,1)) +
      coord_flip() +
      labs(title="expected degree of belief in E", x="", y="") +
      theme_classic(base_size=25) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=25),
            axis.text.y = element_text(size= 25),
            # text = element_text(size= 25),
            legend.position = "none", legend.title = element_blank(), legend.direction = "horizontal")

fn <- paste(TARGET_DIR, "skiing-pe.png", sep=.Platform$file.sep)
ggsave(fn, p, height = 6, width=15)

# Sweep-prior-exam --------------------------------------------------------
fn <- file.path(RESULT_DIR, "results-skiing-voi-sweep.rds")
sweep_data <- readRDS(fn) %>% filter(level=="PL-beliefs")

p <- sweep_data %>% ggplot() + 
      geom_point(mapping=aes(x=pe, y=value, color=level), size=4) +
      theme_bw() + 
      labs(x="prior P(E)", y="E[P(E)]", title="Listener beliefs") +
      theme(legend.position = "none", 
            axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size= 25))
ggsave(paste(TARGET_DIR, "skiing-voi-sweep.png", sep=.Platform$file.sep),
       p, height = 6, width=6)

# Sundowners --------------------------------------------------------------
fn <- file.path(RESULT_DIR, "results-sundowners.rds")
data_long <- readRDS(fn)
data_wide <- data_long %>% spread(key=cell, val=val, fill = 0)
df <- data_wide %>% adapt_bn_ids()

plot_cns(df,level = NULL, save_as=paste(TARGET_DIR, "sundowners-cns.png", sep=.Platform$file.sep))

# plot distribution of each Bayes net
plot_distr <- function(d, x){
  t <- paste(x$level, " P(bn", x$bn_id, ")=", round(d$prob,3),
             " cn: ", d$cn, sep="")
  
  p <- d  %>% dplyr::select(-cn, -prob) %>% gather(., key=cell, val=val) %>% 
    ggplot() + geom_bar(mapping=aes(x=cell, y=val), stat="identity") +
    labs(title = t)
  return(p)
}
data_wide %>% filter(level=="prior") %>% group_by(bn_id, level) %>% group_map(plot_distr)

# plot distribution over Bayes nets 
plot_bns(df)

# Plot expected values for P(R,S)
prs <- marginalize(data_long, c("R", "S")) 
ev_prs <- prs %>% expected_val("RS")
plot_evs_bar(ev_prs, "R,S")


# Plot entire distributions for P(R)
pr <- marginalize(data_long, c("R")) 
plot_marginal_prob(pr, "R", density=FALSE)


# Plot expected values for P(R)
ev_pr <- pr %>% expected_val("R")
plot_evs_bar(ev_pr, "R")





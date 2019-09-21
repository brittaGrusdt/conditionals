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

p <- plot_cns(df, level=NULL, save_as=NULL)
fn <- paste(TARGET_DIR, "skiing-cns.png", sep=.Platform$file.sep)
ggsave(fn, p, height = 6, width=15)

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
ev_pe <-  pe %>% expected_val("E")
p <- plot_evs_bar(ev_pe, "E") 
p <- p + scale_x_discrete(limits = c("PL-beliefs", "PL", "LL", "prior"),
                          labels = c("Listener's beliefs",
                                    paste(strwrap("Pragmatic interpretation", width=10), collapse="\n"),
                                    paste(strwrap("Literal interpretation", width=10), collapse="\n"),
                                    "Prior Belief")) + 
                          # position = "top") +
      # scale_y_continuous(limits=c(0,1)) +
      # coord_flip() +
      labs(title="Expected degree of belief in passing exam", y="", x="") +
      theme(axis.text.x = element_text(angle = 27, size=25),
            axis.text.y = element_text(size= 25),
            legend.position = "none", legend.title = element_blank(), legend.direction = "horizontal")

fn <- paste(TARGET_DIR, "skiing.png", sep=.Platform$file.sep)
ggsave(fn, p, height = 6, width=15)

# skiing sweep-prior-exam --------------------------------------------------------
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

plot_cns(df, save_as=paste(TARGET_DIR, "sundowners-cns.png", sep=.Platform$file.sep))

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
plot_evs_bar(ev_prs, "R ∧ S")
p <- ev_prs %>% ggplot() +
  geom_bar(mapping = aes(x=level, y=ev, fill=level),
           stat="identity")  +
  scale_x_discrete(limits = c("PL", "LL", "prior"),
                   labels = c("pragmatic interpretation",
                              "literal interpretation",
                              "belief before hearing 'If A, C'"),
                   position = "top") +
  scale_y_continuous(limits=c(0,1)) +
  coord_flip() +
  labs(title="expected degree of belief in probability of rain and sundowners",
       x="", y="") +
  theme_classic(base_size=20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=25),
        axis.text.y = element_text(size= 25),
        # text = element_text(size= 25),
        legend.position = "none", legend.title = element_blank(),
        legend.direction = "horizontal")

ggsave(paste(TARGET_DIR, "sundowners-prs.png", sep=.Platform$file.sep),
       p, height = 6, width=15)


# Plot expected values for P(R)
pr <- marginalize(data_long, c("R")) 
ev_pr <- pr %>% expected_val("R")
plot_evs_bar(ev_pr, "R")
p <- ev_pr %>% ggplot() +
  geom_bar(mapping = aes(x=level, y=ev, fill=level),
           stat="identity")  +
  scale_x_discrete(limits = c("PL", "LL", "prior"),
                   labels = c("pragmatic interpretation",
                              "literal interpretation",
                              "belief before hearing 'If A, C'"),
                   position = "top") +
  scale_y_continuous(limits=c(0,1)) +
  coord_flip() +
  labs(title="expected degree of belief in probability of rain", x="", y="") +
  theme_classic(base_size=20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=25),
        axis.text.y = element_text(size= 25),
        # text = element_text(size= 25),
        legend.position = "none", legend.title = element_blank(),
        legend.direction = "horizontal")

ggsave(paste(TARGET_DIR, "sundowners-pr.png", sep=.Platform$file.sep),
       p, height = 6, width=15)



# Plot both expected values for P(R) and for P(R∧S)
pr <- marginalize(data_long, c("R")) 
ev_pr <- pr %>% expected_val("R")
prs <- marginalize(data_long, c("R", "S")) 
ev_prs <- prs %>% expected_val("RS")

data <- bind_rows(ev_pr, ev_prs)

p <- data %>% ggplot() +
      geom_bar(mapping = aes(x=level, y=ev, fill=level),
               stat="identity")  +
      scale_x_discrete(limits = c("PL", "LL", "prior"),
                       labels = c(paste(strwrap("Pragmatic interpretation", width=10), collapse="\n"),
                                  paste(strwrap("Literal interpretation", width=10), collapse="\n"),
                                  "Prior belief")) +
                       # position = "top") +
      # scale_y_continuous(limits=c(0,1)) +
      facet_wrap(~p, labeller = labeller(p =
                      c(`R` = paste(strwrap("Expected degree of belief in probability of rain", width=30), collapse="\n"),
                        `RS` = paste(strwrap("Expected degree of belief in probability of rain and sundowners", width=30),
                                     collapse="\n")))) +
      # coord_flip() +
      labs(x="", y="") +
      theme(axis.text.y = element_text(size= 25),
            axis.text.x = element_text(size= 25, hjust=1, angle=30),
            strip.text = element_text(size=25),
            legend.position = "none", legend.title = element_blank(), legend.direction = "horizontal")


ggsave(paste(TARGET_DIR, "sundowners.png", sep=.Platform$file.sep), p, height = 6, width=15)





# sweep sundowners example ------------------------------------------------
fn <- file.path(RESULT_DIR, "results-sundowners-voi-sweep.rds")
sweep_data <- readRDS(fn) %>% unnest(prior_pr) %>% unite("prior_pr", pr1, pr2, pr3, sep="_")




sweep_data %>% filter(cost==0 & alpha==3 & prior_pr=="0.6_0.7_0.8") %>% 
  ggplot() + 
  geom_bar(mapping=aes(x=value, y=prior_pl_diff), size=2) +
  theme_bw() + 
  facet_wrap(~key) +
  labs(x="", y="", title="") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size= 25))





# diffs <- sweep_data %>% group_by(alpha,cost, prior_pr) %>% spread(key=level, val=value) %>%
#   mutate(prior_pl_diff=prior-PL)
# 
# 
# p <- diffs %>% filter(key=="R") %>% 
#   ggplot() + 
#   geom_point(mapping=aes(x=prior_pr, y=prior_pl_diff), size=2) +
#   theme_bw() + 
#   # facet_grid(key~level) +
#   facet_grid(alpha~cost) + 
#   labs(x="", y="", title="") +
#   theme(legend.position = "none", 
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         text = element_text(size= 25))
# p
# 



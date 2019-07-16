library(tidyverse)
library(latex2exp)


# Distributions for parameter sweep ---------------------------------------
plot_beta <- function(params, xlab){
  p <- ggplot(data = data.frame(x = c(0, 1)), aes(x)) + scale_y_continuous(breaks = NULL)  
  for(i in seq(1, nrow(params))){
    p <- p + stat_function(fun = dbeta, n = 101, args = list(shape1 = params$alpha[[i]], shape2 = params$beta[[i]]), col = params$col[[i]]) + ylab("")
  }
  p <- p + labs(x = xlab)
  print(p)
}

# plot beta distributions for theta
params <- tibble(alpha=seq(4, 13), beta=1, col=rainbow(length(seq(4,13))))
plot_beta(params, xlab=TeX("$\\theta$"))

# plot beta distributions for background noise beta
params <- tibble(alpha=1, beta=c(10,15,20), col=rainbow(3))
plot_beta(params, xlab=TeX("$\\beta$"))



# Plot informativity of utterances

data_dir <- file.path(".", "data", "precomputations", "model-general",
                      fsep = .Platform$file.sep)

path_tables <- file.path(data_dir, "tables-all.rds", fsep = .Platform$file.sep)
tables <- read_rds(path_tables) %>% filter(n_tables==500 & noise_v==500)

fn_utts <- paste("utterances-none.rds", sep="")
path_utterances <- file.path(data_dir, fn_utts, fsep = .Platform$file.sep)
utterances <- read_rds(path_utterances)
                                             
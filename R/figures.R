library(tidyverse)
library(latex2exp)
# parameter sweep

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


library(tidyverse)
library(ggplot2)
library(scales)

colors <- hue_pal()(3)
level2color <- tribble(~level, ~col,
                       "prior", colors[[3]],
                       "LL", colors[[1]], 
                       "PL", colors[[2]])

plot_cns <- function(data, level){
  df <- data %>% group_by(level, cn) %>% summarize(prob=sum(prob))
  if(is.null(level)){
    p <- df %>% 
          ggplot() + 
          geom_bar(mapping = aes(x=cn, y=prob, fill=level),
                   stat="identity", position="dodge") + 
          # facet_wrap(~level) + 
          labs(x="causal nets", y="probability") +
          theme(axis.text.x = element_text(angle = 90))
  }else{
    col <- level2color %>% filter(level== (!!level)) %>% pull(col)
    p <- df %>% filter(level==(!! level)) %>% 
          ggplot() + 
          geom_bar(mapping = aes(x=cn, y=prob), fill=col, stat="identity") + 
          labs(x="causal nets", y="probability") +
          theme(axis.text.x = element_text(angle = 90))
  }
  return(p)
}

plot_density <- function(df, xlab){
  df %>%  ggplot() + 
    geom_density(mapping = aes(x=p, col=level)) + 
    labs(x=xlab, y="density")
}

plot_marginal <- function(data, val_marginal, level=NULL){
  if(val_marginal == "cn"){
    plot_cns(data, level)
  } else {
    df <- data %>% gather(`AC`, `A-C`, `-AC`, `-A-C`, key=cell, val=val)
    df <- marginalize(df, val_marginal)
    xlab <- paste("P(", paste(val_marginal, collapse = ","), ")", sep="")
    plot_density(df, xlab)
  }
}

plot_conditional <- function(data, p){
  df <- compute_cond_prob(data, p)
  plot_density(df, p)
}

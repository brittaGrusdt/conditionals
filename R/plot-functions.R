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

plot_density <- function(df, xlab, level, evs){
  if(is.null(level)){
      p <- df %>%  ggplot() + 
                    geom_density(mapping = aes(x=support, col=level)) +
                    labs(x=xlab, y="density")
      if(!is.null(evs)){
        p <- p + geom_vline(data=evs, aes(xintercept=ev, color=level),
                            linetype="dotted",size=1)
      }
  } else{
      col <- level2color %>% filter(level == (!!level)) %>% pull(col)
      p <- df %>% filter(level==(!! level)) %>% 
        ggplot() + 
        geom_density(mapping = aes(x=support, col=col)) + 
        labs(title=level, x=xlab, y="probability") +
        theme(axis.text.x = element_text(angle = 90), legend.position = "none")
      
      if(!is.null(evs)){
        evs <- evs %>% filter(level == (!!level)) 
        p <- p + geom_vline(data=evs, aes(xintercept=ev, color=col),
                            linetype="dotted",size=1)
      }
  }
  return(p)
}

plot_marginal_prob <- function(data_wide, val_marginal, level=NULL, evs=NULL, 
                               save_as="plot-marginal.png"){
  if(val_marginal == "cns"){
    p <- plot_cns(data_wide, level)
  } else {
    # df <- data_wide %>% gather(`AC`, `A-C`, `-AC`, `-A-C`, key=cell, val=val)
    # df <- marginalize(df, val_marginal)
    samples <- list()
    for(lev in unique(data_wide$level)){
      d <- data_wide %>% filter(level==lev)
      s <- get_samples(tibble(support=d$p, prob=d$prob), 5000000)
      samples[[`lev`]] <- s %>% add_column(level=lev)
    }
    data_marginal <- bind_rows(samples)
    xlab <- paste("P(", paste(val_marginal, collapse = ","), ")", sep="")
    p <- plot_density(data_marginal, xlab, level, evs)
  }
  ggsave(save_as, p)
  return(p)
}

plot_conditional_prob <- function(data, p, level=NULL, evs=NULL){
  df <- compute_cond_prob(data, p)
  plot_density(df, p, level, evs)
}


plot_marginal_bar <- function(data, val_marginal_str, level=NULL, save_as="plot-marginal-bar.png", title=""){
  xlab <- paste("P(", paste(val_marginal_str, collapse = ","), ")", sep="")
  df <- data %>% group_by(level, p) %>% summarize(prob=sum(prob))
  if(is.null(level)){
    p <- df %>% 
      ggplot() + 
      geom_bar(mapping = aes(x=p, y=prob, fill=level),
               stat="identity", position="dodge") + 
      # facet_wrap(~level) + 
      labs(x=xlab, y="probability", title=title) +
      theme(axis.text.x = element_text(angle = 90))
  }else{
    col <- level2color %>% filter(level== (!!level)) %>% pull(col)
    p <- df %>% filter(level==(!! level)) %>% 
      ggplot() + 
      geom_bar(mapping = aes(x=p, y=prob), fill=col, stat="identity") + 
      labs(x=xlab, y="probability", title=level) +
      theme(axis.text.x = element_text(angle = 90))
  }
  if(!is.null(save_as)){
    ggsave(save_as, p)
  }
  return(p)
}



plot_voi_alpha_cost <- function(data, model, key, level){
  df <- data %>% filter(model_id==model & key==(!! key) & level== (!! level)) %>% 
    mutate(value=as.numeric(value), cost=as.factor(cost))
  
  p <-  df %>% ggplot(aes(x=alpha, y=value, col=cost)) +
          geom_point() +
          scale_x_continuous(breaks = data$alpha %>% unique()) +
          ylab(key) +
          ggtitle(paste("model:", model, level))
  
  if(startsWith(key, "cp")){
    p <- p + facet_wrap(~dir)
  }
  print(p)
  
  # df %>% ggplot(aes(x=alpha, y=value, col=cost)) +
  #         geom_point() +
  #         scale_x_continuous(breaks = data$alpha %>% unique()) +
  #         ylab(key) +
  #         ggtitle(level)
}

plot_evs <- function(data, marginal, save_as){
  df <- data %>% filter(p==marginal) 
    p <- df %>% 
      ggplot() + 
      geom_bar(mapping = aes(x=bias, y=ev, fill=level),
               stat="identity", position="dodge") + 
      # facet_wrap(~level) + 
      labs(x="biases", y=paste("E[P(", marginal, ")]", sep=""))
    ggsave(save_as, p)
    return(p)
}

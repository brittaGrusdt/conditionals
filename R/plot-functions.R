library(tidyverse)
library(ggplot2)
library(scales)
library(latex2exp)

colors <- hue_pal()(3)
level2color <- tribble(~level, ~col,
                       "prior", colors[[3]],
                       "LL", colors[[1]], 
                       "PL", colors[[2]])

plot_bns <- function(data_wide){
  df <- data_wide %>% group_by(level, bn_id) %>% summarize(prob=sum(prob))
  df$level <- factor(df$level, levels = c("prior", "LL", "PL"))
    p <- df %>% 
      ggplot() + 
      geom_bar(mapping = aes(x=bn_id, y=prob, fill=level),
               stat="identity", position="dodge") + 
      facet_wrap(~level) + 
      labs(x="Bayes nets", y="probability") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size= 15),
            legend.position = "bottom", legend.title = element_blank(),
            legend.direction = "horizontal")
    return(p)
}

plot_cns <- function(data_wide, level=NULL, save_as=NULL){
  df <- data_wide %>% group_by(level, cn) %>% summarize(prob=sum(prob))

  df$level <- factor(df$level, levels = c("prior", "LL", "PL"))
  
  df$cn_nice <- case_when(df$cn=="A implies C"  ~ "A->C",
                          df$cn=="A implies -C" ~ "A->¬C",
                          df$cn=="-A implies C" ~ "¬A->C",
                          df$cn=="-A implies -C" ~ "¬A->¬C",
                          df$cn=="C implies A" ~ "C->A",
                          df$cn=="C implies -A" ~ "C->¬A",
                          df$cn=="-C implies A" ~ "¬C->A",
                          df$cn=="-C implies -A" ~ "¬C->¬A",
                          df$cn=="A || C" ~ "A ind. C",
                          TRUE~"") 
  if(is.null(level)){
    p <- df %>% 
          ggplot() + 
          geom_bar(mapping = aes(x=cn_nice, y=prob, fill=level),
                   stat="identity", position="dodge") + 
          facet_wrap(~level) + 
          labs(x="causal nets", y="probability") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
               text = element_text(size= 15),
               legend.position = "none", legend.title = element_blank(),
               legend.direction = "horizontal")
  }else{
    col <- level2color %>% filter(level== (!!level)) %>% pull(col)
    p <- df %>% filter(level==(!! level)) %>% 
          ggplot() + 
          geom_bar(mapping = aes(x=cn, y=prob), fill=col, stat="identity") + 
          labs(x="causal nets", y="probability") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                text = element_text(size= 15),
                legend.position = "bottom", legend.title = element_blank(), legend.direction = "horizontal")
  }
  if(!is.null(save_as)){ggsave(save_as, p)}
  return(p)
}

plot_density <- function(df, xlab, level, evs){
  if(is.null(level)){
      p <- df %>%  ggplot() + 
                    geom_density(mapping = aes(x=support, col=level),
                                 adjust = 5) +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1),
                                                text = element_text(size= 15)) + 
                    labs(x=xlab, y="density")
      if(!is.null(evs)){
        p <- p + geom_vline(data=evs, aes(xintercept=ev, color=level),
                            linetype="dotted",size=1)
      }
  } else{
      col <- level2color %>% filter(level == (!!level)) %>% pull(col)
      p <- df %>% filter(level==(!! level)) %>% 
        ggplot() + 
        geom_density(mapping = aes(x=support, col=col),  adjust = 5) + 
        labs(title=level, x=xlab, y="probability") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
              text = element_text(size= 15))
      
      if(!is.null(evs)){
        evs <- evs %>% filter(level == (!!level)) 
        p <- p + geom_vline(data=evs, aes(xintercept=ev, color=col),
                            linetype="dotted",size=1)
      }
  }
  p <- p + scale_x_continuous(breaks=c(0.0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0))
  return(p)
}

plot_discrete <- function(df, xlab){
  data <- df %>% group_by(level) %>%  count(support)%>% mutate(obs=sum(n), perc=n/obs)
  
  p <- data %>% group_by(level) %>% 
        ggplot(aes(x = support, y = perc, fill=level)) +
        geom_bar(stat = "identity", position="dodge") +
        labs(x=xlab, y="probability") +
        facet_wrap(~level)
  return(p)
}

plot_marginal_prob <- function(data_long, vals_marginal, level=NULL, evs=NULL, save_as=NULL){
  marginal <- data_long %>% marginalize(vals_marginal)
  marginal_samples <- sample_webppl_distr(marginal)
  xlab <- paste("P(", paste(vals_marginal, collapse = ","), ")", sep="")
  p <- plot_density(marginal_samples, xlab, level, evs)
  if(!is.null(save_as)){ggsave(save_as, p)}
  return(p)
}

plot_conditional_prob <- function(data, p, level=NULL, evs=NULL){
  df <- compute_cond_prob(data, p)
  data_samples <- sample_webppl_distr(df)
  p <- plot_density(data_samples, p, level, evs)
  return(p)
}

plot_evs_bar <- function(data_evs, val_marginal_str, level=NULL, save_as=NULL, title=""){
  xlab <- paste("P(", paste(val_marginal_str, collapse = ","), ")", sep="")
  df <- data_evs %>% mutate(level=as.factor(level))
  if(is.null(level)){
    p <- df %>% 
      ggplot() + 
      geom_bar(mapping = aes(x=level, y=ev, fill=level),
               stat="identity", position="dodge") + 
      # facet_wrap(~level) + 
      labs(x=xlab, y="probability", title=title) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size= 15),
            legend.position = "bottom", legend.title = element_blank(),
            legend.direction = "horizontal")
  }else{
    col <- level2color %>% filter(level== (!!level)) %>% pull(col)
    p <- df %>% filter(level==(!! level)) %>% 
      ggplot() + 
      geom_bar(mapping = aes(x=p, y=prob), fill=col, stat="identity") + 
      labs(x=xlab, y="probability", title=level) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size= 15),
            legend.position = "bottom", legend.title = element_blank(), legend.direction = "horizontal")
  }
  if(!is.null(save_as)){ggsave(save_as, p)}
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
}

plot_cp_vois <- function(data, save_as=NULL){
  # data needs column order!
  data <- data %>% mutate(key_val=case_when(startsWith(key, "cp_bns") ~ 0,
                                            startsWith(key, "cp_cns") ~ 1,
                                            key=="p_nc_given_na" ~ 2,
                                            TRUE ~ -1))
  
  p <- data %>% mutate(value=as.numeric(value)) %>% ggplot() + 
    geom_bar(aes(x=key_val, y=value, fill=level, group=order), stat="identity", position="dodge") + 
    facet_wrap(~bias) +
    labs(y="", x="") +
    scale_x_continuous(breaks=c(0,1, 2),
                       labels=c(TeX('$\\mathbf{E}(f(X))$'),
                                TeX('$hel(P_{cp}, P_{cns})$'),
                                TeX('$P(\\neg C| \\neg A)$'))) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size= 15),
          legend.position = "right", legend.title = element_blank(), legend.direction = "vertical")
  if(!is.null(save_as)){ggsave(save_as, p)}
  return(p)
}

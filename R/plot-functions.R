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

plot_cns_default <- function(data_wide, level=NULL, save_as=NULL){
    df <- data_wide %>% group_by(level, cn) %>% summarize(prob=round(sum(prob), 3))
    df$level <- factor(df$level, levels = c("prior", "LL", "PL"))
    
    if(is.null(level)){
      p <- df %>% 
            ggplot() + 
            geom_bar(mapping = aes(x=cn, y=prob, fill=level),
                     stat="identity", position="dodge") + 
            geom_text(data=df, aes(x=cn, y=prob, label=prob), size=4,
                      position=position_dodge(0.9),
                      hjust = -0.2) +
            facet_wrap(~level
               ,labeller = labeller(
                 level = c(`prior` =
                             paste(strwrap("Belief before hearing 'If A, C'", width=25), collapse="\n"),
                           `LL` = paste(strwrap("Literal interpretation", width=25), collapse="\n"),
                           `PL`= paste(strwrap("Pragmatic interpretation", width=25), collapse="\n"))
               )
            ) + 
            labs(x="causal nets", y="probability") +
            scale_x_discrete(limits=c("A implies C", "A implies -C",
                                      "-A implies C", "-A implies -C",
                                      "C implies A", "C implies -A",
                                      "-C implies A", "-C implies -A",
                                      "A || C"),
                             labels=c("A->C", "A->¬C", "¬A->C", "¬A->¬C",
                                      "C->A", "C->¬A", "¬C->A", "¬C->¬A",
                                      "A indep. C")) +
          scale_y_continuous(limits=c(0, 0.7)) +
          coord_flip() + 
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
            scale_x_discrete(limits=c("A implies C", "A implies -C",
                                  "-A implies C", "-A implies -C",
                                  "C implies A", "C implies -A",
                                  "-C implies A", "-C implies -A",
                                  "A || C"),
                         labels=c("A->C", "A->¬C", "¬A->C", "¬A->¬C",
                                  "C->A", "C->¬A", "¬C->A", "¬C->¬A",
                                  "A indep. C")) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  text = element_text(size= 15),
                  legend.position = "bottom", legend.title = element_blank(), legend.direction = "horizontal")
    }
    if(!is.null(save_as)){ggsave(save_as, p)}
    return(p)
}


plot_cns <- function(data_wide, level=NULL, save_as=NULL){
  df <- data_wide %>% group_by(level, cn) %>% summarize(prob=sum(prob))
  df$level <- factor(df$level, levels = c("prior", "LL", "PL"))
  
  if(is.null(level)){
    p <- df %>% 
      ggplot() + 
      geom_bar(mapping = aes(x=cn, y=prob, fill=level),
               stat="identity", position="dodge") + 
      facet_wrap(~level, labeller = labeller(
        level = c(`prior` =
                    paste(strwrap("belief before hearing 'If A, C'", width=15), collapse="\n"),
                  `LL` = paste(strwrap("literal interpretation", width=15), collapse="\n"),
                  `PL`= paste(strwrap("pragmatic interpretation", width=15), collapse="\n"))
      )) + 
      labs(x="causal nets", y="probability") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size= 25),
            legend.position = "none", legend.title = element_blank(),
            legend.direction = "horizontal")
  }else{
    col <- level2color %>% filter(level== (!!level)) %>% pull(col)
    p <- df %>% filter(level==(!! level)) %>% 
      ggplot() + 
      geom_bar(mapping = aes(x=cn, y=prob), fill=col, stat="identity") + 
      labs(x="causal nets", y="probability") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size= 25),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.direction = "horizontal")
  }
  if(!is.null(save_as)){ggsave(save_as, p, width=15, height=6)}
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
  xlab <- paste("E[P(", paste(val_marginal_str, collapse = ","), ")]", sep="")
  df <- data_evs %>% mutate(level=as.factor(level))
  if(is.null(level)){
    p <- df %>% 
      ggplot() + 
      geom_bar(mapping = aes(x=level, y=ev, fill=level),
               stat="identity", position="dodge") + 
      # facet_wrap(~level) + 
      labs(x=xlab, y="probability", title=title) + theme_bw() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size= 25),
            legend.position = "none")
            # legend.title = element_blank(),
            # legend.direction = "horizontal")
  }else{
    col <- level2color %>% filter(level== (!!level)) %>% pull(col)
    p <- df %>% filter(level==(!! level)) %>% 
      ggplot() + 
      geom_bar(mapping = aes(x=p, y=prob), fill=col, stat="identity") + 
      labs(x=xlab, y="probability", title=level) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size= 15), legend.position = "none")
            # legend.position = "bottom", legend.title = element_blank(), legend.direction = "horizontal")
  }
  if(!is.null(save_as)){ggsave(save_as, p)}
  return(p)
}

# Plot all table distributions for each causal network respectively
plot_tables <- function(data){
  cns <- data$cn %>% as.factor() %>% levels()
  data <- data %>% mutate(cell=factor(cell, levels=c("AC", "A-C", "-AC", "-A-C")))
  for(causal_net in cns){
    if(causal_net == "A || C"){
      cn_title <- "A independent C"
    } else {
      cn_title <- causal_net
    }
    p <- data %>% filter(cn==causal_net) %>%
      ggplot(aes(x=val,  color = cell)) +
      geom_density() +
      facet_wrap(~cell, scales = "free_y"
                 ,
                 labeller = labeller(
                   cell = c(`AC` = "A ∧ C", `A-C` = "A ∧ ¬C",
                            `-AC`= "¬A ∧ C", `-A-C` = "¬A ∧ ¬C"))
                 ) +
      labs(title = cn_title, x="p") +
      theme(legend.position = "none", text = element_text(size=20))
    print(p)
  }
}

plot_beta <- function(alpha, beta, xlab, color="black"){
  p <- ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
    scale_y_continuous(breaks = NULL)  
  p <- p + stat_function(fun = dbeta, n = 101, col=color,
                           args = list(shape1 = alpha, shape2 = beta)) +
        labs(x = xlab, y="density", title=t) +
        theme_classic() +
        theme(text = element_text(size= 25))
  return(p)
}

# Values-of-interest ------------------------------------------------------
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
  data <- data %>% mutate(level=as.factor(level))
  
  p <- data %>% filter(key=="cp_bns_ac" | key=="p_nc_given_na") %>%
    mutate(value=as.numeric(value)) %>%
    ggplot() + 
    geom_bar(aes(x=level, y=value, fill=level), stat="identity", position="dodge") + 
    facet_wrap(~key, labeller = labeller(
      key = c(`cp_bns_ac` = paste(strwrap(
                              "expected hellinger distance btw. perfectly
                              biconditional distribution and joint probability
                              tables", width=50), collapse="\n"),
              `p_nc_given_na`= paste(strwrap(
                                "belief in consequent to be false given
                                antecedent is false", width=40), collapse="\n"))
    )) +
    labs(y="", x="") +
    scale_x_discrete(limits = c("PL", "LL", "prior"), 
                    labels=c("pragmatic interpretation",
                             "literal interpretation",
                            "belief before hearing 'If A, C'"
                             ), 
                    position = "top") +
    coord_flip() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size= 25),
          strip.text.x = element_text(size = 20),
          legend.position = "none", legend.title = element_blank())
  if(!is.null(save_as)){ggsave(save_as, p)}
  return(p)
}

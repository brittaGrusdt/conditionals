plot_conditions_vs_speaker <- function(df_long){
  p <- df_long %>%
    ggplot(aes(x=val, y=`A > C`, color=pmax_conj_lit)) +
    geom_point(size=2, alpha=0.75) +
    scale_color_gradient(low="blue", high="red",
                         limits = c(0,1), breaks = seq(0,1,0.25)) + 
    facet_grid(cn~condition, scales="free", labeller=
                 labeller(condition=c(`p_delta`="△P", p_rooij="△*P"),
                          cn=c(`A || C`="A indep. C",
                               `A implies C`=TeX("$A\\rightarrow C$"),
                               `C implies A`=TeX("$C\\rightarrow A$"),
                               `C implies -A`=TeX("$C\\rightarrow -A$"))))+
    theme_bw(base_size=20) + 
    theme(legend.position="bottom",
          legend.box="vertical",
          legend.key.width = unit("2.5", "cm")
    ) + labs(color=TeX("$P_s(u^*)| u* \\in U_a \\wedge u* \\in U_l$"),
             y=TeX("$P_S(u=A\\rightarrow C)$"), x="")
  return(p)
}
plot_conditions_and_speaker <- function(df_long){
  df <- df_long %>% rowid_to_column("idx") %>%
          arrange(cn) %>% 
          mutate(`A > C`=case_when(`A > C`==0 ~ NA_real_,
                                    TRUE ~ `A > C`))
  p <- df %>%
    ggplot(aes(x=idx, y=val, color=`A > C`, shape=cn)) +
    geom_point(size=2, alpha=0.75) +
    scale_color_gradient(low="blue", high="red", na.value = "gray") +
    facet_wrap(~condition, scales="free", strip.position = "top",
               labeller=labeller(condition=c(`p_delta`="△P", p_rooij="△*P"))
    ) +
    labs(color=TeX("$P_S(u=A\\rightarrow C)$"),
         y="", x="state id", shape="causal net") +
    guides(color=guide_colorbar(title.position="top"),
           shape=guide_legend(title.position="top")) +
    theme_classic(base_size=20) + 
    theme(legend.position="bottom",
          legend.box = "vertical",
          legend.key.width = unit("2.5", "cm")
    ) 
  return(p)
}

plot_pdelta_vs_prooij <- function(p, color_var){
  p <- p +
    geom_point() +
    scale_color_gradient(low="blue", high="red",
                         breaks=c(color_var$min, color_var$max),
                         labels=c(color_var$min, color_var$max),
                         limits=c(color_var$min, color_var$max)) +
    facet_wrap(~cn, ncol = 2) +
    theme_classic(base_size=25) +
    labs(color=color_var$str,  y="△*P", x="△P", shape="P(C|A)>=0.9") +
    theme(legend.position="bottom",
          legend.title=element_text(size=16),
          legend.text = element_text(size=16),
          legend.direction = "horizontal", legend.box="horizontal") + 
    guides(color=guide_colorbar(title.position="top"),
           shape=guide_legend(title.position="top"))
  return(p)
}

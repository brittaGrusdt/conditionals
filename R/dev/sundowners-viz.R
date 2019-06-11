data <- sundowners$prior %>% mutate(cell=as.factor(cell))
data <- sundowners$PL %>% mutate(cell=as.factor(cell))

data %>% filter(cn=="R > W > S") %>% 
  ggplot() +
    geom_bar(mapping=aes(x=cell, y=val, fill=bn_id), 
             stat="identity") +
    facet_wrap(~bn_id) +
    labs(title="cn: R > W > S")

data %>% filter(cn=="R||S") %>% 
  ggplot() +
  geom_bar(mapping=aes(x=cell, y=val, fill=bn_id), 
           stat="identity") +
  facet_wrap(~bn_id) +
  labs(title="cn: R||S")


data %>% filter(cn=="R > W > S") %>%
  ggplot() +
  geom_bar(mapping=aes(x=bn_id, fill=bn_id), 
          )


data %>% mutate(cell=as.character(cell)) %>%filter(!str_detect(cell, "-R")) %>% 
  summarize(sum=sum(bn_probs*val))


distr <- posterior$d

expected_val <- function(){
  
}
df <- tibble(prob=distr$probs, support=distr$support)
support_new <- df$support %>% rowid_to_column() %>% rename("prob"=probs) %>% unnest()




##########################


pl <- indicative_no_bias$PL
ll <- indicative_no_bias$LL


pl <- indicative_biscuit$PL
ll <- indicative_biscuit$LL

pl <- indicative_cp$PL
ll <- indicative_cp$LL


samples_cns <- function(data){
  data <- data %>% spread(key=cell, value=val)
  df <- tibble(prob=data$bn_probs, support=data$cn)
  samples <- get_samples(df, 100000)
  return(samples)
}
pl_cns <- samples_cns(pl) %>% mutate(listener="PL")

pl_cns %>% mutate(support=as.factor(support), listener=as.factor(listener)) %>%
  ggplot() + geom_bar(aes(x=support, fill=listener))

ll_cns <- samples_cns(ll) %>% mutate(listener="LL")
data <- bind_rows(pl_cns, ll_cns)

data %>% mutate(support=as.factor(support), listener=as.factor(listener)) %>%
  ggplot() + geom_bar(aes(x=support, fill=listener), position = "dodge") + 
  theme(axis.text.x=element_text(angle=90)) +
  xlab("causal net")

samples_p_nc_na <- function(data){
  p_na <- data %>% filter(cell=="-AC" | cell=="-A-C") %>%  group_by(bn_id) %>% mutate(p_na=sum(val)) %>% 
    spread(key=cell, value=val)
  
  p_ncna <- data  %>% filter(cell=="-A-C") %>% group_by(bn_id) %>% mutate(p_ncna=sum(val)) %>% 
    spread(key=cell, value=val)
  
  new_df <- bind_cols(p_ncna, p_na) %>% group_by(bn_id) %>%  mutate(p_nc_na=p_ncna/p_na)
  
  df <- tibble(prob=new_df$bn_probs, support=new_df$p_nc_na)
  samples <- get_samples(df, 1000000)
  return(samples)
}
data_pl <- samples_p_nc_na(pl) %>% mutate(listener="PL")
data_ll <- samples_p_nc_na(ll)%>% mutate(listener="LL")
data <- bind_rows(data_pl, data_ll)
data %>% group_by(listener) %>% summarize(mean=mean(support))
data %>%  ggplot() + geom_density(aes(x=support, color=listener))



samples_pc <- function(data){
  pc <- data %>% filter(cell=="AC" | cell=="-AC") %>%  group_by(bn_id) %>% mutate(pc=sum(val)) %>% 
      spread(key=cell, value=val)

  df <- tibble(prob=pc$bn_probs, support=pc$pc)
  samples <- get_samples(df, 1000000)
  return(samples)
}

pl_pc <- samples_pc(pl) %>% mutate(listener="PL")
ll_pc <- samples_pc(ll) %>% mutate(listener="LL")

pc <- bind_rows(pl_pc, ll_pc) %>% rename("pc"=support)

ggplot(pc) + geom_density(aes(x=pc, color=listener))


pc <- pc %>% mutate(c=case_when(pc>0.9 ~ TRUE,TRUE ~ FALSE),
                    nc=case_when(pc<0.1 ~ TRUE, TRUE ~FALSE))

summary_pc <- pc %>% group_by(listener) %>% summarize(num_c=sum(c)/length(c), num_nc=sum(nc)/length(c)) %>% 
  rename("ratio P(C)>0.9"=num_c)
ggplot(summary_pc) + geom_bar(aes(x=listener, y=`ratio P(C)>0.9`, fill=listener), stat="identity") 


# P(A) --------------------------------------------------------------------
samples_pa <- function(data){
  pc <- data %>% filter(cell=="A-C" | cell=="AC") %>%  group_by(bn_id) %>% mutate(pc=sum(val)) %>% 
    spread(key=cell, value=val)
  
  df <- tibble(prob=pc$bn_probs, support=pc$pc)
  samples <- get_samples(df, 1000000)
  return(samples)
}

pl_pa <- samples_pa(pl) %>% mutate(listener="PL")
ll_pa <- samples_pa(ll) %>% mutate(listener="LL")

pa <- bind_rows(pl_pa, ll_pa) %>% rename("pa"=support)

ggplot(pa) + geom_density(aes(x=pa, color=listener))

pa %>% filter(pa>=0.9) %>% 
  ggplot() + geom_density(aes(x=pa, color=listener))

pa <- pa %>% mutate(a=case_when(pa>0.9 ~ TRUE,TRUE ~ FALSE),
                    na=case_when(pa<0.1 ~ TRUE, TRUE ~FALSE))

summary_pa <- pa %>% group_by(listener) %>% summarize(num_a=sum(a)/length(a), num_na=sum(na)/length(a)) %>% 
  rename("ratio P(A)>0.9"=num_a)
ggplot(summary_pa) + geom_bar(aes(x=listener, y=`ratio P(A)>0.9`, fill=listener), stat="identity") 


bind_cols(pc,pa) %>% select(-listener1) %>% gather(pc, pa, key="p", value="prob") %>% 
  ggplot() + 
  geom_density(aes(x=prob, color=p)) + facet_wrap(~listener) 

library(tcpl)
library(data.table)
library(dplyr)
library(ggplot2)

setwd("L:/Lab/NCCT_ToxCast/Jason_Brown/tcpl")
load("./data/l3.rda")
load("./data/l4.rda")
load("./data/l5.rda")


l4_dat <- l4 %>% filter(row_number() == 2)
l3_dat <- l3 %>% filter(spid == l4_dat %>% pull(spid))
hill_ga <- l4_dat %>% pull(hill_ga)
hill_gw <- l4_dat %>% pull(hill_p)
hill_tp <- l4_dat %>% pull(hill_tp)
gnls_ga <- l4_dat %>% pull(gnls_ga)
gnls_gw <- l4_dat %>% pull(gnls_p)
gnls_tp <- l4_dat %>% pull(gnls_tp)
gnls_la <- l4_dat %>% pull(gnls_la)
gnls_lw <- l4_dat %>% pull(gnls_q)

#need functions for every curve
gnls_curve <- function(top, ga, gw, la, lw, lconc){
  gain <- 1/(1+10^((ga - lconc)*gw))
  loss <- 1/(1+10^((lconc - la)*lw))
  return(top*gain*loss)
}

hill_curve <- function(hill_tp, hill_ga, hill_gw, lconc){
  hill_tp/(1+10^((hill_ga - lconc)*hill_gw))
}



ggplot(l3_dat,
       aes(x=logc, 
           y=resp)) +
  stat_function(fun = hill_curve, 
                args=list(hill_tp = hill_tp, 
                          hill_ga = hill_ga, 
                          hill_gw = hill_gw),
                alpha = 1,
                color = "red", 
                size = 1) +
  stat_function(fun = gnls_curve, 
                args=list(top = gnls_tp, 
                          ga = gnls_ga, 
                          gw = gnls_gw, 
                          la = gnls_la, 
                          lw = gnls_lw),
                alpha = 1, 
                color = "blue", 
                size = 1,
                linetype = 2) +
  theme_bw() +
  geom_point(size=5,alpha=1) +
  theme(legend.position="none", legend.title=element_blank()) +
  ylab("Percent Activity") +
  xlab("Log Concentration")

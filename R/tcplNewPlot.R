# library(tcpl)
# library(data.table)
# library(dplyr)
# library(ggplot2)
# 
# setwd("L:/Lab/NCCT_ToxCast/Jason_Brown/tcpl")
# load("./data/l3.rda")
# load("./data/l4.rda")
# load("./data/l5.rda")
# 
# 
# l4_dat <- l4 %>% filter(row_number() == 2)
# l3_dat <- l3 %>% filter(spid == l4_dat %>% pull(spid))
# hill_ga <- l4_dat %>% pull(hill_ga)
# hill_gw <- l4_dat %>% pull(hill_p)
# hill_tp <- l4_dat %>% pull(hill_tp)
# gnls_ga <- l4_dat %>% pull(gnls_ga)
# gnls_gw <- l4_dat %>% pull(gnls_p)
# gnls_tp <- l4_dat %>% pull(gnls_tp)
# gnls_la <- l4_dat %>% pull(gnls_la)
# gnls_lw <- l4_dat %>% pull(gnls_q)
# 
# #need functions for every curve
# gnls_curve <- function(top, ga, gw, la, lw, lconc){
#   gain <- 1/(1+10^((ga - lconc)*gw))
#   loss <- 1/(1+10^((lconc - la)*lw))
#   return(top*gain*loss)
# }
# 
# hill_curve <- function(hill_tp, hill_ga, hill_gw, lconc){
#   hill_tp/(1+10^((hill_ga - lconc)*hill_gw))
# }
# 
# exp2_a <- l4_dat %>% pull(exp2_a)
# exp2_b <- l4_dat %>% pull(exp2_b)
# #function definition for exp2
# exp2_curve <- function(a, b, lconc){
#   return(a*(exp(lconc/b) - 1)  )
# }
# 
# 
# 
# p <- ggplot(l3_dat,
#        aes(x=logc, 
#            y=resp)) +
#   stat_function(fun = hill_curve, 
#                 args=list(hill_tp = hill_tp, 
#                           hill_ga = hill_ga, 
#                           hill_gw = hill_gw),
#                 aes(color = "Hill")) +
#   stat_function(fun = gnls_curve, 
#                 args=list(top = gnls_tp, 
#                           ga = gnls_ga, 
#                           gw = gnls_gw, 
#                           la = gnls_la, 
#                           lw = gnls_lw),
#                 aes(color = "Gnls")) +
#   stat_function(fun = exp2_curve, 
#                 args=list(a = exp2_a, 
#                           b = exp2_b),
#                 aes(color = "Exp2")) +
#   theme_bw() +
#   scale_color_viridis_d() +
#   geom_point(size=1,alpha=1) +
#   theme(legend.position="none", legend.title=element_blank()) +
#   ylab("Percent Activity") +
#   xlab("Log Concentration")
# 
# p
# library(plotly)
# ggplotly(p)
# htmlwidgets::saveWidget(as_widget(ggplotly(p)), "exampleplot.html")

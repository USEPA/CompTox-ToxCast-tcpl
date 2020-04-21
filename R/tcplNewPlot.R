tcplNewPlot <- function(){
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

cnst = function(ps,x){
  #ignores ps
  return(rep(0,length(x)))
}

exp2_a <- l4_dat %>% pull(exp2_a)
exp2_b <- l4_dat %>% pull(exp2_b)
#function definition for exp2
exp2 <- function(a, b, x){
  x <- 10^x
  return(a*(exp(x/b) - 1)  )
}

# Exponential 3 Model
exp3_a <- l4_dat %>% pull(exp3_a)
exp3_b <- l4_dat %>% pull(exp3_b)
exp3_p <- l4_dat %>% pull(exp3_p)

exp3 = function(a,b,p,x){
  x <- 10^x
  #a = ps[1], b = ps[2], p = ps[3]
  return(a*(exp((x/b)^p) - 1)  )
}

# Exponential 4 Model
exp4_tp <- l4_dat %>% pull(exp4_tp)
exp4_ga <- l4_dat %>% pull(exp4_ga)
exp4 = function(tp,ga,x){
  x <- 10^x
  #tp = ps[1], ga = ps[2]
  return(tp*(1-2^(-x/ga))  )
}

# Exponential 5 Model
exp5_tp <- l4_dat %>% pull(exp5_tp)
exp5_ga <- l4_dat %>% pull(exp5_ga)
exp5_p <- l4_dat %>% pull(exp5_p)
exp5 = function(tp,ga,p,x){
  x <- 10^x
  #tp = ps[1], ga = ps[2], p = ps[3]
  return(tp*(1-2^(-(x/ga)^p))  )
}



# Polynomial 1 Model
poly1_a <- l4_dat %>% pull(poly1_a)
poly1 = function(a,x){
  x <- 10^x
  #a = ps[1]
  return(a*x)
}

# Polynomial 2 Model
poly2_a <- l4_dat %>% pull(poly2_a)
poly2_b <- l4_dat %>% pull(poly2_b)
poly2 = function(a,b,x){
  x <- 10^x
  #a = ps[1], b = ps[2]
  x0 = x/b
  return(a*(x0 + x0*x0))
}

# Power Model
pow_a <- l4_dat %>% pull(pow_a)
pow_p <- l4_dat %>% pull(pow_p)
pow = function(a,p,x){
  x <- 10^x
  #a = ps[1], p = ps[2]
  return(a*x^p  )
}



p <- ggplot(l3_dat,
       aes(x=logc,
           y=resp)) +
  stat_function(fun = hill_curve,
                args=list(hill_tp = hill_tp,
                          hill_ga = hill_ga,
                          hill_gw = hill_gw),
                aes(color = "Hill")) +
  stat_function(fun = gnls_curve,
                args=list(top = gnls_tp,
                          ga = gnls_ga,
                          gw = gnls_gw,
                          la = gnls_la,
                          lw = gnls_lw),
                aes(color = "Gnls")) +
  stat_function(fun = exp2,
                args=list(a = exp2_a,
                          b = exp2_b),
                aes(color = "Exp2")) +
  stat_function(fun = exp3,
                args=list(a = exp3_a,
                          b = exp3_b,
                          p = exp3_p),
                aes(color = "Exp3")) +
  stat_function(fun = exp4,
                args=list(tp = exp4_tp,
                          ga = exp4_ga),
                aes(color = "Exp4")) +
  stat_function(fun = exp5,
                args=list(tp = exp5_tp,
                          ga = exp5_ga,
                          p = exp5_p),
                aes(color = "Exp5")) +
  stat_function(fun = poly1,
                args=list(a = poly1_a),
                aes(color = "Poly1")) +
  stat_function(fun = poly2,
                args=list(a = poly2_a,
                          b = poly2_b),
                aes(color = "Poly2")) +
  stat_function(fun = pow,
                args=list(a = pow_a,
                          p = pow_p),
                aes(color = "Pow")) +
  theme_bw() +
  scale_color_viridis_d() +
  geom_point(size=1,alpha=1) +
  theme(legend.position="none", legend.title=element_blank()) +
  ylab("Percent Activity") +
  xlab("Log Concentration")

p
library(plotly)
ggplotly(p)
htmlwidgets::saveWidget(as_widget(ggplotly(p)), "exampleplot.html")
}

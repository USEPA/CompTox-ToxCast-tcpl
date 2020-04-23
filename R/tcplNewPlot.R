tcplNewPlot <- function(){
library(tcpl)
library(data.table)
library(dplyr)
library(ggplot2)
tcplConf(user='jbrown20', pass='Sp1tfire!', db='invitrodb', drvr='MySQL', host='localhost')

setwd("L:/Lab/NCCT_ToxCast/Jason_Brown/tcpl")
load("./data/l3.rda")
load("./data/l4.rda")
load("./data/l5.rda")


l4_dat <- l4 %>% filter(row_number() == 12)
l4_dat <- l4_dat %>% as.data.table %>%  tcplPrepOtpt()
l3_dat <- l3 %>% filter(spid == l4_dat %>% pull(spid))
l5_dat <- l5 %>% filter(spid == l4_dat %>% pull(spid))
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
# extract range from level 3 data for creating plotting all the functions
# increase resolution to get smoother curves
resolution <- 1000
l3_range <- l3_dat %>% pull(logc) %>% range %>% .[]*resolution
x_range <- floor(l3_range[1]):ceiling(l3_range[2])/resolution

# calculate y values for each function
y_hill <- hill_curve(hill_tp = hill_tp, hill_ga = hill_ga, hill_gw = hill_gw, lconc = x_range)
y_gnls <- gnls_curve(top = gnls_tp, ga = gnls_ga, gw = gnls_gw, la=gnls_la, lw = gnls_lw, lconc = x_range)
y_exp2 <- exp2(a = exp2_a,b = exp2_b, x = x_range)
y_exp3 <- exp3(a = exp3_a, b = exp3_b, p = exp3_p, x = x_range)
y_exp4 <- exp4(tp = exp4_tp, ga = exp4_ga, x = x_range)
y_exp5 <- exp5(tp = exp5_tp, ga = exp5_ga, p = exp5_p, x = x_range)
y_poly1 <- poly1(a = poly1_a, x = x_range)
y_poly2 <- poly2(a = poly2_a, b = poly2_b, x = x_range)
y_pow <- pow(a = pow_a, p = pow_p, x = x_range)






# pull some info needed for plot annotation
aenm <- l4_dat %>% pull(aenm)
hitcall <- l5_dat %>% pull(hitc)
chnm <- l4_dat %>% pull(chnm)
casn <- l4_dat %>% pull(casn)
dsstox_id <- l4_dat %>% pull(dsstox_substance_id)
spid <- l4_dat %>% pull(spid)

library(plotly)
fig <- plot_ly(data = l3_dat,
               x = ~logc,
               y = ~resp,
               type = "scatter",
               mode = "markers",
               hoverinfo = 'text',
               text = ~paste('</br> Assay Plate ID: ', apid,
                             '</br> Log Concentration: ', logc,
                             '</br> Response: ', resp))
# formatting for y axis
y <- list(
  title = "Percent Activity",
  #set zeroline to false otherwise there would be a big horizontal line at y = 0
  zeroline = FALSE
)
# formatting for x axis
x <- list(
  title = "Log Concentration",
  #set zeroline to false so there is no vertical line at x = 0
  zeroline = FALSE
)
# apply axis to figure
fig <- fig %>% layout(xaxis = x, yaxis = y)


line.fmt = list(dash="solid", width = 1.5, color=NULL, alpha = .2)
fig <- fig %>% add_lines(x=x_range,
                         y=y_hill,
                         line=line.fmt,
                         name="hill",
                         opacity = .2,
                         inherit = FALSE,
                         hoverinfo = 'text',
                         text = ~paste('</br> Hill',
                                       '</br> ac50: ',l4_dat %>% pull(hill_ac50),
                                       '</br> Log Concentration: ', x_range,
                                       '</br> Response: ', y_hill)
                         )

#gnls function
fig <- fig %>% add_lines(x=x_range,
                         y=y_gnls,
                         line=line.fmt,
                         name="gnls",
                         opacity = .2,
                         inherit = FALSE,
                         hoverinfo = 'text',
                         text = ~paste('</br> Gain-Loss',
                                       '</br> ac50: ',l4_dat %>% pull(gnls_ac50),
                                       '</br> Log Concentration: ', x_range,
                                       '</br> Response: ', y_gnls)
)

#exp 2
fig <- fig %>% add_lines(x=x_range,
                         y=y_exp2,
                         line=line.fmt,
                         name="exp2",
                         opacity = .2,
                         inherit = FALSE,
                         hoverinfo = 'text',
                         text = ~paste('</br> exp2',
                                       '</br> ac50: ',l4_dat %>% pull(exp2_ac50),
                                       '</br> Log Concentration: ', x_range,
                                       '</br> Response: ', y_exp2)
)

#exp3
fig <- fig %>% add_lines(x=x_range,
                         y=y_exp3,
                         line=line.fmt,
                         name="exp3",
                         opacity = .2,
                         inherit = FALSE,
                         hoverinfo = 'text',
                         text = ~paste('</br> exp3',
                                       '</br> ac50: ',l4_dat %>% pull(exp3_ac50),
                                       '</br> Log Concentration: ', x_range,
                                       '</br> Response: ', y_exp3)
)

#exp4
fig <- fig %>% add_lines(x=x_range,
                         y=y_exp4,
                         line=line.fmt,
                         name="exp4",
                         opacity = .2,
                         inherit = FALSE,
                         hoverinfo = 'text',
                         text = ~paste('</br> exp4',
                                       '</br> ac50: ',l4_dat %>% pull(exp4_ac50),
                                       '</br> Log Concentration: ', x_range,
                                       '</br> Response: ', y_exp4)
)

#exp5
fig <- fig %>% add_lines(x=x_range,
                         y=y_exp5,
                         line=line.fmt,
                         name="exp5",
                         opacity = .2,
                         inherit = FALSE,
                         hoverinfo = 'text',
                         text = ~paste('</br> exp5',
                                       '</br> ac50: ',l4_dat %>% pull(exp5_ac50),
                                       '</br> Log Concentration: ', x_range,
                                       '</br> Response: ', y_exp5)
)

#poly1
fig <- fig %>% add_lines(x=x_range,
                         y=y_poly1,
                         line=line.fmt,
                         name="poly1",
                         opacity = .2,
                         inherit = FALSE,
                         hoverinfo = 'text',
                         text = ~paste('</br> poly1',
                                       '</br> ac50: ',l4_dat %>% pull(poly1_ac50),
                                       '</br> Log Concentration: ', x_range,
                                       '</br> Response: ', y_poly1)
)

#poly2
fig <- fig %>% add_lines(x=x_range,
                         y=y_poly2,
                         line=line.fmt,
                         name="poly2",
                         opacity = .2,
                         inherit = FALSE,
                         hoverinfo = 'text',
                         text = ~paste('</br> poly2',
                                       '</br> ac50: ',l4_dat %>% pull(poly2_ac50),
                                       '</br> Log Concentration: ', x_range,
                                       '</br> Response: ', y_poly2)
)

#pow
fig <- fig %>% add_lines(x=x_range,
                         y=y_pow,
                         line=line.fmt,
                         name="pow",
                         opacity = .2,
                         inherit = FALSE,
                         hoverinfo = 'text',
                         text = ~paste('</br> pow',
                                       '</br> ac50: ',l4_dat %>% pull(pow_ac50),
                                       '</br> Log Concentration: ', x_range,
                                       '</br> Response: ', y_pow)
)


#winning model
fig <- fig %>% add_lines(x=x_range,
                         y=get(paste0("y_",l5_dat %>% pull(modl))),
                         line=line.fmt,
                         name=l5_dat %>% pull(modl),
                         opacity = 1,
                         inherit = FALSE,
                         hoverinfo = 'text',
                         text = ~paste('</br> ',l5_dat %>% pull(modl),
                                       '</br> ac50: ',l5_dat %>% pull(ac50),
                                       '</br> Log Concentration: ', x_range,
                                       '</br> Response: ', get(paste0("y_",l5_dat %>% pull(modl))))
)

#add ac50 line
vline <- function(x = 0, color = "red") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(dash="dash", width = 1.5, color=NULL, alpha = .2)
  )
}
fig <- fig %>% layout(shapes = list(vline(l5_dat %>% pull(ac50))))

# add annotations
fig <- fig %>% add_annotations(text = paste0(aenm,"<br>",
                                             case_when(hitcall == 1 ~"ACTIVE",
                                                       hitcall == 0 ~ "INACTIVE",
                                                       hitcall == -1 ~ "NO CALL",
                                                       TRUE ~ "NA"),"<br>",
                                             chnm, " (",casn,")","<br>",
                                             dsstox_id, "<br>",
                                             spid),
                               xref = "paper",
                               x = 0.05,
                               yref = "paper",
                               y = .95,
                               showarrow = FALSE,
                               textposition = 'bottom right',
                               align = "left")

fig

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
#   stat_function(fun = exp2,
#                 args=list(a = exp2_a,
#                           b = exp2_b),
#                 aes(color = "Exp2")) +
#   stat_function(fun = exp3,
#                 args=list(a = exp3_a,
#                           b = exp3_b,
#                           p = exp3_p),
#                 aes(color = "Exp3")) +
#   stat_function(fun = exp4,
#                 args=list(tp = exp4_tp,
#                           ga = exp4_ga),
#                 aes(color = "Exp4")) +
#   stat_function(fun = exp5,
#                 args=list(tp = exp5_tp,
#                           ga = exp5_ga,
#                           p = exp5_p),
#                 aes(color = "Exp5")) +
#   stat_function(fun = poly1,
#                 args=list(a = poly1_a),
#                 aes(color = "Poly1")) +
#   stat_function(fun = poly2,
#                 args=list(a = poly2_a,
#                           b = poly2_b),
#                 aes(color = "Poly2")) +
#   stat_function(fun = pow,
#                 args=list(a = pow_a,
#                           p = pow_p),
#                 aes(color = "Pow")) +
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
}

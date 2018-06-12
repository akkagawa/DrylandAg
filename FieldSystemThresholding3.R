## FieldSystemThresholding2.R
## Project: DrylandAg
## Author: A. Kagawa-Viviani
## Date: revised 9 Jan 2017, revised 16 Dec 2017
##   Initial version 12 Dec 2017 based on output from Matt Lucas 
## Notes: use output from MP Lucas scripts: tmin_rf_cond_loop_dryag.R, tmin_dry_cond_loop_dryag.R
##   For scripts and files, see folder MPLucas_crop_runs 

library(lattice)
library(plotly)

#Set Working Directory
setwd("C:/Users/Aurora/OneDrive/Documents/Projects/DrylandAgEcohydrology/Data_Figures/MPLucas_crop_runs")
dat_rf_tmean_all<-read.csv("rf_tair_per_crop_FINAL_fine.csv")  # old: rf_tmin_per_crop_FINAL.csv
dat_arid_tmean_all<-read.csv("arid_tair_per_crop_FINAL_fine.csv") # old: dry_tmin_per_crop_FINAL_redo.csv

dat_rf_tmean<-dat_rf_tmean_all[which(dat_rf_tmean_all$rf_mm>=50 & 
                                       dat_rf_tmean_all$rf_mm<=160 &
                                       dat_rf_tmean_all$tair_c>=16 & 
                                       dat_rf_tmean_all$tair_c<=24),]
dat_arid_tmean<-dat_arid_tmean_all[which(dat_arid_tmean_all$aridity>1 & 
                                           dat_arid_tmean_all$aridity<3.5&
                                           dat_rf_tmean_all$tair_c>=16 & 
                                           dat_rf_tmean_all$tair_c<=24),]
names(dat_rf_tmean)[4:6]<-c("Kohala", "Kona", "Kau")
names(dat_arid_tmean)[4:6]<-c("Kohala", "Kona", "Kau")

tair_c.crit<-c(18, rep(NA, times=dim(dat_rf_tmean)[1]-1))
rf_mm.crit<-c(90, rep(NA, times=dim(dat_rf_tmean)[1]-1))
dat_rf_tmean<-cbind(dat_rf_tmean, rf_mm.crit, tair_c.crit)

tair_c.crit<-c(18, rep(NA, times=dim(dat_arid_tmean)[1]-1))
aridity.crit<-c(2.5, rep(NA, times=dim(dat_arid_tmean)[1]-1))
dat_arid_tmean<-cbind(dat_arid_tmean, aridity.crit, tair_c.crit)

# Set up API credentials: https://plot.ly/r/getting-started
Sys.setenv("plotly_username"="akkagawa")
Sys.setenv("plotly_api_key"="K3irULbxqnQcod4UKsOy")

## RF and Tair thresholding
p.koh<- dat_rf_tmean %>%
  plot_ly() %>%
  add_trace(x = ~rf_mm, y = ~tair_c, z = ~Kohala, showlegend=FALSE,
            type = "contour", autocontour = F, 
            contours = list(start = 0.5, end = 0.85, size = 0.05,
                            showlabels=TRUE, showscale=FALSE)) %>%
  add_trace(x = ~rf_mm.crit, y = ~tair_c.crit, type = 'scatter', 
            mode = 'markers', marker=list(symbol='x', size=10, color='black'),
            showlegend=FALSE) 
  
p.kon<- dat_rf_tmean %>%
  plot_ly() %>%
  add_trace(x = ~rf_mm, y = ~tair_c, z = ~Kona, showlegend=FALSE,
            type = "contour", autocontour = F, 
            contours = list(start = 0.5, end = 0.85, size = 0.05,
                            showlabels=TRUE, showscale=FALSE)) %>%
  add_trace(x = ~rf_mm.crit, y = ~tair_c.crit, type = 'scatter', 
            mode = 'markers', marker=list(symbol='x', size=10, color='black'),
            showlegend=FALSE) 

p.kau<- dat_rf_tmean %>%
  plot_ly() %>%
  add_trace(x = ~rf_mm, y = ~tair_c, z = ~Kau, showlegend=FALSE,
            type = "contour", autocontour = F, 
            contours = list(start = 0.5, end = 0.85, size = 0.05,
                            showlabels=TRUE, showscale=FALSE)) %>%
  add_trace(x = ~rf_mm.crit, y = ~tair_c.crit, type = 'scatter', 
            mode = 'markers', marker=list(symbol='x', size=10, color='black'),
            showlegend=FALSE) 


pRF <- subplot(p.koh, p.kon, p.kau, 
               nrows=3, shareX=T, which_layout=2) %>%
  layout(title = "Fraction Cultivable Area",
         xaxis = list(title = 'Monthly Rainfall (mm)'),
         yaxis = list(title=''), 
         yaxis2 = list(title='Temperature (°C)'),
         yaxis3 = list(title=''))

## Aridity and Tair thresholding
p2.koh<- dat_arid_tmean %>%
  plot_ly() %>%
  add_trace(x = ~aridity, y = ~tair_c, z = ~Kohala, showlegend = FALSE,
            type = "contour", autocontour = F, 
            contours = list(start = 0.5, end = 0.85, size = 0.05,
                            showlabels=TRUE, showscale=FALSE)) %>%
  add_trace(x = ~aridity.crit, y = ~tair_c.crit, type = 'scatter', 
            mode = 'markers', marker=list(symbol='x', size=10, color='black'),
            showlegend=FALSE)

p2.kon<- dat_arid_tmean %>%
  plot_ly() %>%
  add_trace(x = ~aridity, y = ~tair_c, z = ~Kona, showlegend=FALSE,
            type = "contour", autocontour = F, 
            contours = list(start = 0.5, end = 0.85, size = 0.05,
                            showlabels=TRUE, showscale=FALSE)) %>%
  add_trace(x = ~aridity.crit, y = ~tair_c.crit, type = 'scatter', 
            mode = 'markers', marker=list(symbol='x', size=10, color='black'),
            showlegend=FALSE)

p2.kau<- dat_arid_tmean %>%
  plot_ly() %>%
  add_trace(x = ~aridity, y = ~tair_c, z = ~Kau, showlegend=FALSE,
            type = "contour", autocontour = F, 
            contours = list(start = 0.5, end = 0.85, size = 0.05,
                            showlabels=TRUE, showscale=FALSE)) %>%
  add_trace(x = ~aridity.crit, y = ~tair_c.crit, type = 'scatter', 
            mode = 'markers', marker=list(symbol='x', size=10, color='black'),
            showlegend=FALSE)

pArid <- subplot(p2.koh, p2.kon, p2.kau,
               nrows=3,shareX=T, shareY=T) %>%
  layout(title = "Fraction Cultivable Area",
         xaxis = list(title = 'Monthly Aridity (mm/mm)'),
         yaxis = list(title=''), 
         yaxis3 = list(title=''),
         yaxis2 = list(side='left', title='Temperature (°C)'),
         showlegend=FALSE)

# Create a shareable link to your chart

chart_link.RF = api_create(pRF, filename="DrylandAg_rf_tmean_per_fine")
chart_link.Arid = api_create(pArid, filename="DrylandAg_arid_tmean_per_fine")

chart_link.RF
chart_link.Arid


###############
# Regular R way
# x<-unique(dat_arid_tmean$aridity)
# y<-unique(dat_arid_tmean$tair_c)
# per<-formula(dat_arid_tmean[,c(4,3,2)])
# koh<-dat_arid_tmean[,c(4,2,3)]
# dat<-reshape(koh, v.names="koh_per", idvar="aridity", timevar="tair_c", 
#              direction="wide")
# datm<-as.matrix(dat[,-1])
# contour(x, y, z=datm, levels=seq(0.5, 0.85, by=0.05))

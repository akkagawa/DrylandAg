rm(list = ls())
	
#install.packages("raster")
library(raster)
#install.packages("rgdal")
library(rgdal)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
#install.packages("reshape2")
library(reshape2)

#add monthly suit raster data
setwd("C:\\Users\\mpluc\\Documents\\noa_projects\\dry_ag_sytems\\rexports\\raster_outputs")
thresh<-brick("thresh.rf90.temp18.tif",package="raster")#per pixel count of months rf>100mm & tsurf>=21c 
print(thresh)
plot(thresh)

#add vector data
setwd("C:\\Users\\mpluc\\Documents\\noa_projects\\dry_ag_sytems\\dry_data\\ContiguousPolygons")
dryag<- readOGR(".", "ContiguousPolyDryland_wgs84_coast_clipped")
head(dryag)

#forloop setup
m<-unique(dryag$Name)
monthy_moku_suit_area<-data.frame(Moku = factor(),month=integer(),AreaHA=numeric(),crops=integer(),stringsAsFactors=FALSE)

#forloop
  for(i in 1:3){
    for(k in 1:12){
		moku<-subset(dryag,dryag$Name==m[i])
		maskras<- mask(thresh[[k]], moku)
		datadf<-as.data.frame(maskras,na.rm=T,df=T)
		head(datadf)
		names(datadf)<-"suitable"
		AreaHA<-(sum(datadf$suitable)*6.25)
		datatab<-data.frame(Moku=as.character(m[i]),month=as.integer(k),AreaHA=as.numeric(AreaHA))
		monthy_moku_suit_area<-as.data.frame(rbind(monthy_moku_suit_area,datatab))
		}}
head(monthy_moku_suit_area)

#add percent area field & clean data 
monthy_moku_suit_area$OBJECTID<-as.integer(monthy_moku_suit_area$Moku)
final_monthy_moku_suit_area<-merge(monthy_moku_suit_area,head(dryag),by="OBJECTID")
final_monthy_moku_suit_area$Moku<-final_monthy_moku_suit_area$Name
final_monthy_moku_suit_area$DryAgPer<-round(final_monthy_moku_suit_area$AreaHA.x/final_monthy_moku_suit_area$AreaHA.y,3)*100
head(final_monthy_moku_suit_area)
names(final_monthy_moku_suit_area)


#make plots & open data (if needed)
#setwd("C:\\Users\\mpluc\\Documents\\noa_projects\\dry_ag_sytems\\rexports\\rf90.temp18")
#final_monthy_moku_suit_area<-read.csv("thresh.rf90.temp18 moku_month_crop_area_annual.csv")
#head(final_monthy_moku_suit_area)
final_monthy_moku_suit_area$DryAgPer<-final_monthy_moku_suit_area$DryAgPer/100
mon_levels<-month.abb[c(12,1:11)]
final_monthy_moku_suit_area$month.ab<-factor(month.abb[final_monthy_moku_suit_area$month],levels = mon_levels)
final_monthy_moku_suit_area$Moku<-factor(final_monthy_moku_suit_area$Moku,levels(final_monthy_moku_suit_area$Moku)[c(2,3,1)])
str(final_monthy_moku_suit_area)
head(final_monthy_moku_suit_area)

#plot of % area over month per moku		
ggp4<-   ggplot(data=final_monthy_moku_suit_area, aes(x=month.ab, y=DryAgPer, group=Moku)) +
  geom_point(aes(shape=Moku),size=5)+
  geom_line(aes(linetype=Moku), size=1.5)+
  scale_linetype_manual(values=c(1,2,3))+
  scale_y_continuous(labels = scales::percent,breaks = seq(0, 1, .2),limits=c(0,1))+
  xlab("Month") + ylab("Percent of Dry Agricultural System") + # Set axis labels
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text=element_text(size=36,face="bold"),
        legend.key.width = unit(3, "cm"),
        legend.key.height = unit(2.5, "cm"),
        axis.title=element_text(size=32,face="bold",colour = "black"),
        axis.title.x=element_blank(),
        axis.text=element_text(size=30,colour = "black"),
        axis.text.y = element_text(angle = 90),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black",size=2))

# Position legend inside
# This must go after theme_b
#export plots	
tiff(file="rf90_temp18_montly_percent2.tif",width=1800, height=600)
  print(ggp4)
  dev.off()
  



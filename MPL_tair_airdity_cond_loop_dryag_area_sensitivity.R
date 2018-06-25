#Sensitivy analysis:Sensitivity analysis percent area in response to co-varying of aridity and air temp values 
	#this code takes monthly aridity & tair (c) rasters and reclasses multiple combinations of these 
	#two varibles on a monthly basis to define a crop-able area, then combines all 12 months to calculate annual % of dry ag system
	#crop-able, as defined by >=1 month of crop-able area. 

rm(list = ls())
	
#install.packages("raster")
library(raster)

#install.packages("rgdal")
library(rgdal)

#install.packages("reshape2")
library(reshape2)

#install.packages("ggplot2")
library(ggplot2)

#add raster data
setwd("C:\\Users\\mpluc\\Documents\\noa_projects\\dry_ag_sytems\\FS3_geotiff_ContiguousPolyDryland_coast_clipped\\FS3_geotiff_ContiguousPolyDryland_coast_clipped")
tairmo<-brick("tmeanstack.tif",package="raster")#stack of 12 mo min air temp 
print(tairmo)
plot(tairmo)
tair_max<-round(max(maxValue(tairmo)),0)#max temp value in all rasters
tair_min<-round(min(minValue(tairmo)),0)#max temp value in all rasters

aridmo<-brick("aridity.mostack.tif",package="raster")#stack of 12 arid mm
print(aridmo)
plot(aridmo)
aridmo_max<-round(max(maxValue(aridmo)),2)#max arid value in all rasters
#add vector data
setwd("C:\\Users\\mpluc\\Documents\\noa_projects\\dry_ag_sytems\\dry_data\\ContiguousPolygons")
dryag<- readOGR(".", "ContiguousPolyDryland_wgs84")
head(dryag)
plot(dryag)aridmo_min<-round(min(minValue(aridmo)),2)#min arid value in all rasters

#set up loop conditions
arid_vec<-seq(0.3,5,.05)#vec of arid thresh values min (0.3) to 5 by 0.05
tair_vec<-seq(tair_min,tair_max,1)#vec of tair thresh values from max tair and min tair in raster

#loop objects
sum_year<-aridmo[[1]]-aridmo[[1]]#blank raster for annual run calculations
allones<-aridmo[[1]]/aridmo[[1]]
data_table_arid<-data.frame(run = numeric(), aridity = numeric(), tair_c = numeric(), koh_per = numeric(), kon_per = numeric(), kau_per = numeric())
run <- 1 #count runs
breaks <- 0:12
colors <- c("red",rep("yellow",4),rep("green",4),rep("forestgreen",4))

setwd("C:\\Users\\mpluc\\Documents\\noa_projects\\dry_ag_sytems\\rexports\\crop_runs\\arid_tair_runs_fine")

#for loop
t1<-Sys.time()#time start
print(t1)
for(j in arid_vec){
	for(k in tair_vec){
		for(i in 1:12){
	arid<-(aridmo[[i]])
#plot(arid)
	tair<-tairmo[[i]]
#plot(tair)
	arid_thresh <- reclassify(arid, c(-Inf,j,1,j,Inf,0))#arid reclass = 1
#plot(arid_thresh)
	tair_thresh <- reclassify(tair, c(-Inf,k,0,k,Inf,1))#tair reclass = 1
#plot(tair_thresh)
	sum_mo<-arid_thresh+tair_thresh
#plot(sum_mo)
	sum_mo[sum_mo<2] <- 0 #make <2=0
	sum_mo[sum_mo==2] <- 1 #make 2=1
	mo_thresh<-sum_mo
#plot(mo_thresh)
#text(-155.5, 20.2, paste("month: ",i))
	sum_year<-sum(sum_year,mo_thresh)
}#montly annual loop done

jpeg(paste(run,"_arid_",j," tair_",k,".jpg",sep=""))
plot(sum_year,breaks=breaks,col=colors,
     legend.args=list(text='Cropable months', side=4, font=2, line=2.5, cex=0.8))
text(-155.365, 20.12, paste("aridity:",j," tair:",k))
text(-155.5, 20.2, paste("run:",run))

dev.off()
	sum_year[sum_year>1] <- 1 #make all pix >1 = 1
	sum_year_re<-sum_year
#calculate per annual area of each system
#koh
	koh_mask<-crop(sum_year_re, subset(dryag,OBJECTID==1))#kohala dry ag system
	koh_crop_cells<-sum(as.vector(koh_mask),na.rm = T)
	koh_all_cells<-sum(as.vector(crop(allones, subset(dryag,OBJECTID==1))),na.rm = T)
	koh_per<-(koh_crop_cells/koh_all_cells) #kohala per croppable
#kona
	kon_mask<-crop(sum_year_re, subset(dryag,OBJECTID==2))#kona dry ag system
	kon_crop_cells<-sum(as.vector(kon_mask),na.rm = T)
	kon_all_cells<-sum(as.vector(crop(allones, subset(dryag,OBJECTID==2))),na.rm = T)
	kon_per<-(kon_crop_cells/kon_all_cells) #kona per croppable
#kau
	kau_mask<-crop(sum_year_re, subset(dryag,OBJECTID==3))#kau dry ag system
	kau_crop_cells<-sum(as.vector(kau_mask),na.rm = T)
	kau_all_cells<-sum(as.vector(crop(allones, subset(dryag,OBJECTID==3))),na.rm = T)
	kau_per<-(kau_crop_cells/kau_all_cells) #kau per croppable

#make rows for table
	aridity<-j
	tair_c<-k
	row<-as.data.frame(cbind(run,aridity,tair_c,koh_per,kon_per,kau_per))
	data_table_arid<-as.data.frame(rbind(data_table_arid,row))
	run <- run + 1
	names(row)

#reset raster run
	sum_year<-aridmo[[1]]-aridmo[[1]]#reset blank zero raster
	}}
t2<-Sys.time()#time end loop
time<-t2-t1
print(time)

head(data_table_arid,12)
tail(data_table_arid,12)

#write table
setwd("C:\\Users\\mpluc\\Documents\\noa_projects\\dry_ag_sytems\\rexports\\crop_runs")
write.csv(data_table_arid,"arid_tair_per_crop_FINAL_fine.csv",row.names=F)

#plot all arid solutions where temp==18 for each system
data_table_arid_long18c<-melt(subset(data_table_arid, tair_c==18),id=c("run","aridity", "tair_c"))
head(data_table_arid_long18c)
ggplot(data_table_arid_long18c, aes(aridity, value))+
	geom_point(aes(colour = factor(variable)))+
	geom_hline(yintercept = 0.85)+
	scale_x_continuous(breaks= seq(0,5,by=.1))

#plot temp solutions where arid==2.5 for each system
data_table_arid_long2.5dry<-melt(subset(data_table_arid, aridity==2.5),id=c("run","aridity", "tair_c"))
head(data_table_arid_long2.5dry)
ggplot(data_table_arid_long2.5dry, aes(tair_c, value))+
	geom_point(aes(colour = factor(variable)))+
	geom_hline(yintercept = 0.85)+
	scale_x_continuous(breaks= seq(14,26,by=1))

#end code



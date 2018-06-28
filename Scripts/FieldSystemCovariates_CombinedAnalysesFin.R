## FieldSystemCovariates_CombinedAnalysesFin.R
## Project: DrylandAg
## Author: A. Kagawa-Viviani
## Date: 23 Jan 2017 
## Notes: Script associated with FieldSystemCovariates_Polygons_GraphsFin.R
##        -Generate rasters for seasonality metrics of aridity
##        -Explore intra-annual shifting of optimal growing conditions
## Figure 4
## Figure 6

# References: 
#    Feng X, Porporato A, Rodriguez-Iturbe I (2013) Changes in rainfall seasonality 
#      in the tropics. Nature Clim Change 3:811-815. doi: 10.1038/nclimate1907
#    Valenzuela, H., S. Fukuda, and A. Arakaki (1994) Sweetpotato Production Guides for 
#      Hawaii. Pages 1-10. Hawaii Institute of Tropical Agriculture and Human Resources.


## Set up workspace -----
setwd('DIRECTORY HERE')
op <- par()

library('raster')
library('rgdal')

########### DEFINE FUNCTIONS to calculate seasonality statistics ########################
#' Pmonthly calculates the monthly probability distribution
#' p_m = monthly rf/ annual rf
#' @param rfmonths A 12-raster stack or brick of monthly rainfall
#' @param rfann    A raster of annual rainfall
#' @return A raster brick of fraction of annual rainfall in a given month (probability distribution)
Pmonthly <- function(rfmonths, rfann) {return (rfmonths/rfann)}

#' RelEntropy calculates the relative entropy of rainfall within a year
#' D = sum across months m for a give year k: (p_k,m * log2(p_k,m / q_m)) where q_m=1/12, uniform distribution
#' @param p_m A 12-raster brick of monthly probability distributions
#' @return A raster layer of relative entropy, or deviation from a uniform distribution
RelEntropy <- function(p_m) {return (sum(p_m*log2(p_m*12)))}

#' Seasonality calculates the seasonality S of rainfall within a year by scaling D 
#' S = D * rfann/Rmax, where Rmax is the maximum value in the raster
#' @param rfmonths A 12-raster stack or brick of monthly rainfall
#' @return A raster layer of seasonality, which scales relative entropy
Seasonality <- function(D, rfann) {
  Rmax<-cellStats(rfann, stat="max")
  return (D*rfann/Rmax)}

#' WaterYearMean rearranges mean monthly rainfall to start from water year
#' @param rfmonths A 12-raster stack or brick of mean monthly rainfall
#' @return wystart An integer indicating first month of water year (follows driest month)
#' @return rfmonths.wy A 12-raster stack or brick of mean monthly rainfall
WaterYearMean<-function(rfmonths) {
  rastermean<-cellStats(rfmonths, stat="mean")
  wystart<-as.numeric(which.min(rastermean))+1
  
  if(wystart==1){wy<-1:12
  }else {wy<-c(wystart:12, 1:(wystart-1))}
  rfmonths.wy<-rfmonths[[wy]]       # rearrange months for water year
  list(wystart=wystart, rfmonths.wy=rfmonths.wy)
}

#' Centroid (demodulated phase) indicates wet season timing, first moment of rfmonths
#' Calculate centroid based on start to water year; find this with cellStats(stack(months), stat="mean")
#' C=1/rfann*sum(rfmonths*(1:12))
#' @param rfmonths.wy A 12-raster stack or brick of monthly rainfall arranged for water year
#' @param wystart  An integer from 1:12 indicating the month following the lowest rainfall month; 
#' @return A raster layer of wet season centroid
Centroid <- function (rfmonths.wy, wystart, calendar=T){
  ann.wy<-sum(rfmonths.wy)
  centroid.wy<-1/ann.wy*sum(rfmonths.wy*(1:12))  # centroid based on wy
  centroid.cal<-wystart+centroid.wy-1            # centroid based on cal yr
  if(calendar==T){
    return(centroid.cal)
    print("calendar year")}
  else{return(centroid.wy)
    print("water year")}
}

#' Entropic spread indicates duration of rainfall, second moment of rfmonths
#' Calculate spread based on start to water year; find this with cellStats(stack(months), stat="mean")
#' Z=1/rfann*sum for m from 1:12 [(m-C)^2*rfmonth(m))
#' @param rfmonths A 12-raster stack or brick of monthly rainfall
#' @param wystart  An integer from 1:12 indicating the month following the lowest rainfall month; 
#' @return A raster layer of wet season duration
Duration <- function(rfmonths.wy, wystart){
  C<-Centroid(rfmonths.wy, wystart, calendar=F)
  z1<-list()
  for (m in 1:12){
    z1[[m]]<-(m-C)^2*rfmonths.wy[[m]]
    print(m)}
  return (sqrt((1/sum(rfmonths.wy))* sum(brick(z1))))
}

############ READ IN DATA: long term mean monthly and annual data from GeoTiff ##########
# Read in RF (P) geoTiff
rf<-stack("FS3_geotiff/rf.mostack.tif"); names(rf)<-month.abb
rf.ann<-raster("FS3_geotiff/rf.tif")

# Read in PET geoTiff
pet<-stack("FS3_geotiff/petstack.tif"); names(pet)<-month.abb
pet.ann<-raster("FS3_geotiff/pet.tif")

# Read in aridity (PET/P) geoTiff
aridity<-stack("FS3_geotiff/aridity.mostack.tif"); names(aridity)<-month.abb
aridity.ann<-raster("FS3_geotiff/aridity.tif")

# Read in AET
aet<-stack("FS3_geotiff/aetstack.tif"); names(aet)<-month.abb
aet.ann<-raster("FS3_geotiff/aet.tif")

# Read in AET/P geoTiff
aetrf<-stack("FS3_geotiff/aetrf.mostack.tif"); names(aetrf)<-month.abb
aetrf.ann<-raster("FS3_geotiff/aetrf.tif")

# Read in RF Seasonality goTiff
rfseason<-stack("FS3_geotiff/rfseasonstack.tif")
names(rfseason)=c("CV_MeanRF","RelEntropy_D",
                  "Seasonality_S", "Centroid_C", "Duration_Z")


############ CALCULATE SEASONALITY METRICS FOR ARIDITY ##################################
# Coefficient of Variation (sd/mean)
aridity_cv<-cv(aridity)  # month-to-month variability

# Calculate the mean aridity for each month (use this for entropy later?)
aridmean<-mean(aridity)  # compare this to 1/12 for uniform rainfall distrib
Parid<-Pmonthly(aridity, aridmean)
lims<-c(min(cellStats(Parid, stat="min")),
        max(cellStats(Parid, stat="max")))
plot(Parid, zlim=lims, main=names(Parid))

# calculate relative entropy (Feng et al 2013)
D<-RelEntropy(Parid)

# calculate seasonality (Feng et al 2013)
S<-Seasonality(D, aridity.ann)

###### Centroid and Duration: more complicated
# Identify start of water year (dryness/aridity year) for each pixel (Kona/Kohala/Kau all diff)
min.mo<-which.min(aridity) # which LAYER has minimum value (return raster of layer indices)
max.mo<-which.max(aridity) # which layer index has max value at given pixel

wy.mo<-min.mo+1     # water year begins the month after lowest value
wy.mo[wy.mo==13]<-1 # convert month 13 to month 1

# repeat for all months: creating index rasters
wy2<-wy.mo+1; wy2[wy2==13]<-1  # each pixel indicates month of water year
wy3<-wy2+1; wy3[wy3==13]<-1
wy4<-wy3+1; wy4[wy4==13]<-1
wy5<-wy4+1; wy5[wy5==13]<-1
wy6<-wy5+1; wy6[wy6==13]<-1
wy7<-wy6+1; wy7[wy7==13]<-1
wy8<-wy7+1; wy8[wy8==13]<-1
wy9<-wy8+1; wy9[wy9==13]<-1
wy10<-wy9+1; wy10[wy10==13]<-1
wy11<-wy10+1; wy11[wy11==13]<-1
wy12<-wy11+1; wy12[wy12==13]<-1

h2o.mo<-aridity # raster stack

for (i in 1:12){  # For each month of the calendar year
  h2o.mo[[1]][wy.mo==i]<-aridity[[i]][wy.mo==i] # where water year month pixel = Jan, Feb, Mar
  h2o.mo[[2]][wy2==i]<-aridity[[i]][wy2==i]     # write monthly aridity for that month
  h2o.mo[[3]][wy3==i]<-aridity[[i]][wy3==i]
  h2o.mo[[4]][wy4==i]<-aridity[[i]][wy4==i]
  h2o.mo[[5]][wy5==i]<-aridity[[i]][wy5==i]
  h2o.mo[[6]][wy6==i]<-aridity[[i]][wy6==i]
  h2o.mo[[7]][wy7==i]<-aridity[[i]][wy7==i]
  h2o.mo[[8]][wy8==i]<-aridity[[i]][wy8==i]
  h2o.mo[[9]][wy9==i]<-aridity[[i]][wy9==i]
  h2o.mo[[10]][wy10==i]<-aridity[[i]][wy10==i]
  h2o.mo[[11]][wy11==i]<-aridity[[i]][wy11==i]
  h2o.mo[[12]][wy12==i]<-aridity[[i]][wy12==i]
}
wys<-stack(h2o.mo)  # raster stack

# Centroid of aridity year, pixel-by-pixel start to "water year"
C<-Centroid(rfmonths.wy=wys, wystart= wy.mo, calendar=T)
C[C>12]<-C[C>12]-12

# # Duration of dryness/aridity year, pixel-by-pixel start to "water year"
# Z<-Duration(rfmonths.wy=wys, wystart= wy.mo)

## Prepare for mapping: ahupuaa ---------------------------------------------------------
library(rgeos)  # crop the ahupuaa shapefile too for plotting
ahupuaa<-readOGR(dsn="GIS FILES HERE/Ahupuaa", layer="Ahupuaa")
proj4string(ahupuaa)
ext.HI3dry<-extent(c(-156.2, -155.4, 18.9, 20.3)) # Zoom in to leeward HI
ahucrop<-crop(ahupuaa, ext.HI3dry)

#########################################################################################
################## Make pretty figures for publication ##################################

my.colors<-colorRampPalette(c("blue", "green", "light green", "yellow", "orange", "red"))
rf.colors<-colorRampPalette(c("red", "orange", "yellow", "light green", "blue"), bias=3)
centr.colors<-colorRampPalette(c("blue", "light green",  "yellow", "purple", "blue"))

##### FIGURE 4: Maps of seasonality: P and aridity (PET/P) ##############################
pdf("Fig4_Seasonality_P_Aridity_fin.pdf", width=6, height=5)

### Map RAINFALL (P) seasonality metrics ####
# Mean annual rainfall
par(mfrow=c(2,3), oma=c(0,0,0,0), mar=c(1, 1, 3, 2) + 0.1)
plot(ahucrop, lwd=0.5, main="Mean annual P (mm)")
plot(rf.ann, alpha=0.9, add=T, col=rev(my.colors(24)), legend=F) 
plot(rf.ann, legend.width = 1.5, legend.only=T, add=T,
     col=rev(my.colors(24)), smallplot=c(.75, .78, .3, .65)) #smallplot: x1,x2,y1,y2
mtext("(a)", side=3, line=-1.3, adj=0.04)

# Coefficient of variation, seasonality proxy
plot(ahucrop, lwd=0.5, main="CV of monthly P")
plot(rfseason$CV_MeanRF, alpha=0.9, add=T, col=my.colors(24), legend=F)
plot(rfseason$CV_MeanRF, legend.width = 1.5, legend.only=T, add=T, 
     col=my.colors(24), smallplot=c(.75, .78, .3, .65))
scalebar(d=40, type="bar", divs=4, below = "kilometers", xy=c(-156.315, y=18.95))
compassRose(x=-156.13, y=19.3, cex=0.5) 
mtext("(b)", side=3, line=-1.3, adj=0.04)

# Centroid of rainy season
plot(ahucrop, lwd=0.5, main="Rainy season centroid (mo)")
plot(rfseason$Centroid_C, alpha=0.9, add=T, col=centr.colors(24), legend=F)
plot(rfseason$Centroid_C, legend.width = 1.5, legend.only=T, add=T,
     col=centr.colors(24), smallplot=c(.75, .78, .3, .65))
mtext("(c)", side=3, line=-1.3, adj=0.04)

# Duration of rainy season
# plot(ahucrop, lwd=0.5, main="Wet season spread (mos)")
# plot(rfseason$Duration_Z, alpha=0.9, add=T, col=rf.colors(24))

### Map ARIDITY (PET/P) seasonality metrics ####
# Mean annual aridity
plot(ahucrop, lwd=0.5, main="Mean annual PET/P")
plot(aridity.ann, alpha=0.9, add=T, col=my.colors(24), legend=F)
plot(aridity.ann, legend.width = 1.5, legend.only=T, add=T,
     col=my.colors(24), smallplot=c(.75, .78, .3, .65)) 
mtext("(d)", side=3, line=-1.3, adj=0.04)

# Coefficient of variation of aridity
plot(ahucrop, lwd=0.5, main="CV of monthly PET/P")
plot(aridity_cv, alpha=0.9, add=T, col=my.colors(24), legend=F)
plot(aridity_cv, legend.width = 1.5, legend.only=T, add=T,
     col=my.colors(24), smallplot=c(.75, .78, .3, .65)) 
mtext("(e)", side=3, line=-1.3, adj=0.04)

# Centroid of dry season
plot(ahucrop, lwd=0.5, main="Dry season centroid (mo)")
plot(C, col=centr.colors(24), add=T, alpha=0.9, legend=F)
plot(C, legend.width = 1.5, legend.only=T, add=T,
     col=centr.colors(24), smallplot=c(.75, .78, .3, .65)) 
mtext("(f)", side=3, line=-1.3, adj=0.04)

# Duration of dry season
# plot(ahucrop, lwd=0.5, main="Aridity spread (mos)")
# plot(Z, col=rev(rf.colors(24)), add=T, alpha=0.8) 
# # this is a little confusing- longer dry season in Kona may be due to low seasonality?
# # not sure centroid and duration work for aridity metric

dev.off()


#########################################################################################
################### THRESHOLDING with planting variables  ###############################
### Read in Tmin and Tmean and align projection #####
tmin<-projectRaster(stack("FS3_geotiff/tminstack.tif"), aridity) 
tmean<-projectRaster(stack("FS3_geotiff/tmeanstack.tif"), aridity) 
names(tmin)<-month.abb
names(tmean)<-month.abb

plant<-list(rf, rf.ann, aridity, aetrf, pet, aet, rfseason, tmean, tmin)
names(plant)<-c("rf", "rf.ann", "aridity", "aetrf", "pet", "aet", "rfseason", "tmean", "tmin")

### Write some functions to help with thresholding #####
### Consider the locations of optimal growing conditions
### Consider the length of the growing season

## thresh1: a function to threshold based on a single condition
thresh1<-function(mogrids,    # mogrids is a rasterstack
                  threshold, 
                  direction=c("min", "max"), 
                  namegrid){  # namegrid is character
  if(direction=="min"){
    thresh.grids<-mogrids > threshold
    plot(thresh.grids, main= paste(namegrid, ">", threshold))
    thresh.mos<-sum(thresh.grids)  # how many months at this condition?
    plot(thresh.mos, main=paste("Number of months", namegrid, ">", threshold))
    
  }else if(direction=="max"){
    thresh.grids<-mogrids < threshold
    plot(thresh.grids, main= paste(namegrid, "<", threshold))
    thresh.mos<-sum(thresh.grids)  # how many months at this condition?
    plot(thresh.mos, main=paste("Number of months", namegrid, "<", threshold))
  }
  
  return(list(thresh=thresh.grids, 
              months=thresh.mos,
              descrip=paste("set", direction, namegrid)))
}

## thresh2: a function to identify pixels that meet these conditions
thresh2<-function(thresh.grids1,   # thresh.grids is a raster
                  thresh.grids2, 
                  conds){          # conds is a character vector of conditions
  thresh.grids_1.2<-thresh.grids1 & thresh.grids2
  plot(thresh.grids_1.2, main=paste(conds[1], "&", conds[2]))
  thresh.grids_1.2mos<-sum(thresh.grids_1.2) # how many months at this condition?
  plot(thresh.grids_1.2mos, 
       main=paste("Number of months", conds[1], "&", conds[2]))
  return(list(thresh=thresh.grids_1.2,
              months=thresh.grids_1.2mos,
              descrip=paste(conds[1], "&", conds[2])))
}

### Apply the thresholding functions to the data #####
# Set desired criteria
rfthresh.min<- 90      # Set rainfall threshold in mm
aridthresh.max<- 2.5   # Set aridity threshold as ratio
tmeanthresh.min<- 18   # Set air temperature threshold in C

# Monthly rainfall > threshold
rf90<-thresh1(mogrids=rf, 
              threshold=rfthresh.min, 
              direction="min",
              namegrid="rainfall (mm)")

# Monthly temperature > threshold
tmean18<-thresh1(mogrids=tmean, 
                 threshold=tmeanthresh.min, 
                 direction="min",
                 namegrid="mean air temperature (C)")

# Monthly aridity< threshold  
arid2.5<-thresh1(mogrids=aridity, 
               threshold=aridthresh.max, 
               direction="max",
               namegrid="PET/P")

### Intersection of 2 criteria
# Monthly rainfall > 90 AND Tmean >20
rf90.tmean18<-thresh2(tmean18$thresh, rf90$thresh, 
        conds=c("Tmean > 18C", "P>90 mm"))

# Monthly aridity < 2.5 AND Tmean >18
arid2.5.tmean18<-thresh2(tmean18$thresh, arid2.5$thresh, 
        conds=c("Tmean > 18C", "Aridity<2.5")) 
  
### Difference when you consider aridity vs rainfall
diff_arid2.5.rf90<-arid2.5.tmean18$months-rf90.tmean18$months
mask0<-abs(diff_arid2.5.rf90)<2
plot(mask(diff_arid2.5.rf90,mask0, maskvalue=T), col=rev(my.colors(20)))

####### Look closer: VISUALIZE STATISTICS by field system #########################
### Prepare thresholding grids for visualization -----
plant2<-c(plant, 
          rf90$months, rf90$thresh,
          tmean18$months, tmean18$thresh,
          arid2.5$months, arid2.5$thresh,
          rf90.tmean18$months, rf90.tmean18$thresh,
          arid2.5.tmean18$months, arid2.5.tmean18$thresh,
          diff_arid2.5.rf90)
names(plant2)<-c(names(plant), 
                 "rf90","rf90.12",
                 "tmean18","tmean18.12",
                 "arid2.5","arid2.5.12",
                 "rf90.tmean18","rf90.tmean18.12",
                 "arid2.5.tmean18","arid2.5.tmean18.12",
                 "diff_arid2.5.rf90")

### Read in polygons and disaggregate field systems -----
HI3dry.wgs84coast<-readOGR(dsn="GIS FILES HERE/ContiguousPolyDryland_wgs84_coast_clipped.shp", 
                    layer="ContiguousPolyDryland_wgs84_coast_clipped")
proj4string(HI3dry.wgs84coast)  
crs<-proj4string(aridity) # "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
HI3dry<-spTransform(HI3dry.wgs84coast, CRS=crs)

Kohala<-SpatialPolygons(list(test=HI3dry@polygons[[1]]), proj4string=CRS(crs))
Kona<-SpatialPolygons(list(test=HI3dry@polygons[[2]]), proj4string=CRS(crs))
Kau<-SpatialPolygons(list(test=HI3dry@polygons[[3]]), proj4string=CRS(crs))

plant.kohala<-lapply(plant2, FUN= function(x) crop(x, Kohala))
plant.kona<-lapply(plant2, FUN= function(x) crop(x, Kona))
plant.kau<-lapply(plant2, FUN= function(x) crop(x, Kau))

ahu.kohala<-crop(ahupuaa, extent(Kohala)+c(-0.02, 0.02, -0.02, 0.02))
ahu.kona<-crop(ahupuaa, extent(Kona)+c(-0.02, 0.02, -0.02, 0.02))
ahu.kau<-crop(ahupuaa, extent(Kau)+c(-0.02, 0.02, -0.02, 0.02))

# Rainfall and seasonality
par(mfrow=c(2,3))
plot(ahu.kohala, lwd=0.5, main="annual P, Kohala")
plot(plant.kohala$rf.ann, col=rf.colors(24), add=T, alpha=0.8, legend.width=1.5)
plot(ahu.kona, lwd=0.5, main="annual P, Kona")
plot(plant.kona$rf.ann, col=rf.colors(24), add=T, alpha=0.8, legend.width=1.5)
plot(ahu.kau, lwd=0.5, main="annual P, Kau")
plot(plant.kau$rf.ann, col=rf.colors(24), add=T, alpha=0.8, legend.width=1.5)

plot(ahu.kohala, lwd=0.5, main="CV of monthly P, Kohala")
plot(plant.kohala$rfseason$CV_MeanRF, col=my.colors(24), add=T, alpha=0.8, legend.width=1.5)
plot(ahu.kona, lwd=0.5, main="CV of monthly P, Kona")
plot(plant.kona$rfseason$CV_MeanRF, col=my.colors(24), add=T, alpha=0.8, legend.width=1.5)
plot(ahu.kau, lwd=0.5, main="CV of monthly P, Kau")
plot(plant.kau$rfseason$CV_MeanRF, col=my.colors(24), add=T, alpha=0.8, legend.width=1.5)

# Centroid
plot(ahu.kohala, lwd=0.5, main="Wet season centroid, Kohala")
plot(plant.kohala$rfseason$Centroid_C, col=centr.colors(24), add=T, alpha=0.8)
plot(ahu.kona, lwd=0.5, main="Wet season centroid, Kona")
plot(plant.kona$rfseason$Centroid_C, col=centr.colors(24), add=T, alpha=0.8)
plot(ahu.kau, lwd=0.5, main="Wet season centroid, Kau")
plot(plant.kau$rfseason$Centroid_C, col=centr.colors(24), add=T, alpha=0.8)

# Duration
plot(ahu.kohala, lwd=0.5, main="Wet season Duration, Kohala")
plot(plant.kohala$rfseason$Duration_Z, col=rf.colors(24), add=T, alpha=0.8)
plot(ahu.kona, lwd=0.5, main="Wet season Duration, Kona")
plot(plant.kona$rfseason$Duration_Z, col=rf.colors(24), add=T, alpha=0.8)
plot(ahu.kau, lwd=0.5, main="Wet season Duration, Kau")
plot(plant.kau$rfseason$Duration_Z, col=rf.colors(24), add=T, alpha=0.8)


##### Maps of optimal growing conditions by month #######################################
# gisdir<-"GIS FILES HERE"
# HI3dry.UTM<-readOGR(dsn=paste0(gisdir, "ContiguousPolyDryland_wgs84_coast_clipped.shp"), 
#                     layer="ContiguousPolyDryland_wgs84_coast_clipped")
# proj4string(HI3dry.UTM)  # UTM Zone 5...
# HI3dry<-spTransform(HI3dry.UTM, 
#                     CRS="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
poly.koh<-crop(HI3dry, extent(Kohala))
poly.kon<-crop(HI3dry, extent(Kona))
poly.kau<-crop(HI3dry, extent(Kau))

# Optimal uala growth, Tmean>18, rf>90
pdf("Tmean18_P90.pdf")
par(mfrow=c(3,4), mar=c(2,1,2,4))
for (i in 1:12){
  plot(ahu.kohala, lwd=0.5, main=month.abb[i])
  plot(plant.kohala$rf90.tmean18.12[[i]], col.regions=rev(terrain.colors(2)), alpha=0.9, add=T)
  plot(poly.koh, add=T)
}

for (i in 1:12){
  plot(ahu.kona, lwd=0.5, main=month.abb[i])
  plot(plant.kona$rf90.tmean18.12[[i]], col.regions=rev(terrain.colors(2)), alpha=0.9, add=T)
  plot(poly.kon, add=T)
}
for (i in 1:12){
  plot(ahu.kau, lwd=0.5, main=month.abb[i])
  plot(plant.kau$rf90.tmean18.12[[i]], col.regions=rev(terrain.colors(2)), alpha=0.9, add=T)
  plot(poly.kau, add=T)
}

# Optimal uala growth, Tmean>18, Aridity<2.5
pdf("Tmean18_Arid2_5.pdf")
par(mfrow=c(3,4), mar=c(2,1,2,4))
for (i in 1:12){
  plot(ahu.kohala, lwd=0.5, main=month.abb[i])
  plot(plant.kohala$arid2.5.tmean18.12[[i]], col.regions=rev(terrain.colors(2)), alpha=0.9, add=T)
  plot(poly.koh, add=T)
}

for (i in 1:12){
  plot(ahu.kona, lwd=0.5, main=month.abb[i])
  plot(plant.kona$arid2.5.tmean18.12[[i]], col.regions=rev(terrain.colors(2)), alpha=0.9, add=T)
  plot(poly.kon, add=T)
}
for (i in 1:12){
  plot(ahu.kau, lwd=0.5, main=month.abb[i])
  plot(plant.kau$arid2.5.tmean18.12[[i]], col.regions=rev(terrain.colors(2)), alpha=0.9, add=T)
  plot(poly.kau, add=T)
}
dev.off()

##### FIGURE 6: Maps of months of optimal growing conditions ############################
pdf("Fig6_CultivableMonths2_fin.pdf", width=5.5, height=7)

# Optimal uala growth, Tmean>18, P>90
par(mfrow=c(3,3), mar=c(0,0,1,1), oma=c(1,2,1,1))

ahu.kohala@bbox[2]<-20.02
ahu.kohala@bbox[4]<-20.29
ahu.kona@bbox[2]<-19.29

brks<-0:12
# Kohala
plot(ahu.kohala, lwd=0.5, main="Kohala")
plot(plant.kohala$rf90.tmean18, breaks=brks, 
     col=rev(terrain.colors(12)), add=T, alpha=0.8, legend=F)
plot(poly.koh, add=T)
scalebar(d=16, type="bar", divs=4, below = "km")
mtext("(a)", side=3, line=-1.3, adj=0.04)
mtext(text=expression(paste(T[mean],">18", degree*C, ", P >90mm")), 
      side=2, line = 0, font=1.5)

# Kona
plot(ahu.kona, lwd=0.5, main="Kona")
plot(plant.kona$rf90.tmean18, breaks=brks, 
     col=rev(terrain.colors(12)), add=T, alpha=0.8, legend=F)
plot(poly.kon, add=T)
mtext("(b)", side=3, line=-1.3, adj=0.04)

plot(plant.kona$rf90.tmean18, breaks=brks, add=T,   # legend
     legend.width = 1.5, legend.only=T, 
     col=rev(terrain.colors(12)), smallplot=c(.73, .77, .1, .8)) # x1,x2,y1,y2
scalebar(d=20, type="bar", divs=4, below = "km")

# Kau
plot(ahu.kau, lwd=0.5, main="Kau")
plot(plant.kau$rf90.tmean18, breaks=brks, 
     col=rev(terrain.colors(12)), add=T, alpha=0.8, legend=F)
plot(poly.kau, add=T)
scalebar(d=20, type="bar", divs=4, below = "km")
mtext("(c)", side=3, line=-1.3, adj=0.04)

############# Optimal uala growth, Tmean>18, aridity<2.5
# Kohala
plot(ahu.kohala, lwd=0.5, main="")
plot(plant.kohala$arid2.5.tmean18, breaks=brks,
     col=rev(terrain.colors(12)), add=T, alpha=0.8, legend=F)
plot(poly.koh, add=T)
mtext("(d)", side=3, line=-1.3, adj=0.04)
mtext(text=expression(paste(T[mean],">18", degree*C, ", PET/P <2.5")), 
      side=2, line = 0, font=1.5)
# Kona
plot(ahu.kona, lwd=0.5, main="")
plot(plant.kona$arid2.5.tmean18, breaks=brks,
     col=rev(terrain.colors(12)), add=T, alpha=0.8, legend=F)
plot(poly.kon, add=T)

plot(plant.kona$arid2.5.tmean18, breaks=brks, add=T,  # legend
     col=rev(terrain.colors(12)), 
     legend.width = 1.5, legend.only=T, 
     smallplot=c(.73, .77, .1, .8)) #smallplot: x1,x2,y1,y2
mtext("(e)", side=3, line=-1.3, adj=0.04)

# Kau
plot(ahu.kau, lwd=0.5, main="")
plot(plant.kau$arid2.5.tmean18, breaks=brks,
     col=rev(terrain.colors(12)), add=T, alpha=0.8, legend=F)
plot(poly.kau, add=T)
mtext("(f)", side=3, line=-1.3, adj=0.04)

### DIFFERENCE when you use Aridity threshold vs Rainfall
brk.diff<-seq(-10,12,2)
diff.cols<-rev(my.colors(length(brk.diff)))
diff.cols[brk.diff<1 & brk.diff>-1]<-"#FFFFFF"

mask(diff_arid2.5.rf90,mask0, maskvalue=T)
# Kohala
plot(ahu.kohala, lwd=0.5, main="")
plot(plant.kohala$diff_arid2.5.rf90,
     breaks=brk.diff, col=diff.cols, add=T, alpha=0.9, legend=F)
plot(poly.koh, add=T)
mtext("(g)", side=3, line=-1.3, adj=0.04)
mtext(text=expression(Months[PET/P<2.5] - Months[P>90]), side=2, line = 0, font=1.5)

# Kona
plot(ahu.kona, lwd=0.5, main="")
plot(plant.kona$diff_arid2.5.rf90, 
     breaks=brk.diff, col=diff.cols, add=T, alpha=0.8, legend=F)
plot(poly.kon, add=T)
plot(plant.kona$diff_arid2.5.rf90, 
     breaks=brk.diff, add=T,
     col=diff.cols, legend.width = 1.5, 
     legend.only=T, smallplot=c(.73, .77, .1, .8)) #smallplot: x1,x2,y1,y2
mtext("(h)", side=3, line=-1.3, adj=0.04)

# Kau
plot(ahu.kau, lwd=0.5, main="")
plot(plant.kau$diff_arid2.5.rf90, 
     breaks=brk.diff, col=diff.cols, add=T, alpha=0.8, legend=F)
plot(poly.kau, add=T)
mtext("(i)", side=3, line=-1.3, adj=0.04)

dev.off()

## save all this to GeoTiff
threshold<-plant2[10:17]
for (i in 1:length(threshold)){
  writeRaster(threshold[[i]], filename=paste0("threshold/", names(threshold)[i]),
            format="GTiff", overwrite=T)
  print(names(threshold)[i])
}

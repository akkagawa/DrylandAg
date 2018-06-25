## FieldSystemCovariates_Polygons_GraphsFinal.R
## Project: DrylandAg
## Author: A. Kagawa-Viviani
## Date: 23 January 2018
## Notes: 1) Extract values of gridded climate data for known locations
##        2) Visualize these
## Figure 2
## Figure 3
## Figure S2

###### Set up the workspace ##############################################
## Load necessary libraries
library('sp')
library('raster')
library('rgdal')

## Set working directories
setwd("DIRECTORY HERE")

## Set location of GIS files
## These should include DEM and climate layers
##   from http://evapotranspiration.geography.hawaii.edu/, http://rainfall.geography.hawaii.edu
gisdir<-"GIS FILES HERE"


###### Read in shapefiles ################################################
## Read in field systems shapefile provided by Seth Quintus
HI3dry.UTM<-readOGR(dsn=paste0(gisdir, 
                               "Project_DrylandAgEcohydrology/ContiguousPolyDryland_wgs84_coast_clipped.shp"), 
                    layer="ContiguousPolyDryland_wgs84_coast_clipped")
proj4string(HI3dry.UTM)  # UTM Zone 5...

## Set up different projections/datums since rasters are differen CRS
HI3dry<-spTransform(HI3dry.UTM, 
                    CRS="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

HI3dry.temp<-spTransform(HI3dry.UTM, 
                    CRS="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

## Read in ahupuaa shapefile for mapping later
ahupuaa<-readOGR(dsn="GIS FILES HERE/Ahupuaa", layer="Ahupuaa")
proj4string(ahupuaa)    # "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


###### Read in raster data ###############################################
# Elevation (m) from USGS 10m DEM (source: National Map)
dem<-dir(paste0(gisdir, "HITemp_rastercovariates"), 
         pattern="clipdem_wgs84", full.names=T)

# Rainfall Atlas data, annual and monthly (source: Online Rainfall Atlas of HI)
rfdirs<-list.dirs(paste0(gisdir, "StateRFGrids_mm"), full.names=T)
staterf<-rfdirs[grep("staterf_mm", rfdirs)]

# ET-relevant data, annual and monthly (source: Evapotranspiration of HI) ------------
# Solar Radiation (SW)
swdirs<-list.dirs(paste0(gisdir, "CloudySWGlobTerrain_month_raster"), full.names=T)
sw<-swdirs[grep("cl_sw", swdirs)] 

# Diffuse Radiation (diffuse)
diffdirs<-list.dirs(paste0(gisdir, "DiffuseRadiation_month_raster"), full.names=T)
dif<-diffdirs[grep("dif_rd", diffdirs)]

# Penman Monteith PET
petdirs<-list.dirs(paste0(gisdir, "Penman_ET0_mm_month_raster"), full.names=T)
pet<-petdirs[grep("pen_mm", petdirs)]

# Actual ET
aetdirs<-list.dirs(paste0(gisdir, "AET_mm_month_raster"), full.names=T)
aet<-aetdirs[grep("aet_mm", aetdirs)]

# Soil Moisture
smdirs<-list.dirs(paste0(gisdir, "SoilMoisture_month_raster"), full.names=T)
sm<-smdirs[grep("sl_mst", smdirs)]

# Relative Humidity
rhdirs<-list.dirs(paste0(gisdir, "RH_month_raster"), full.names=T)
rh<-rhdirs[grep("rh_", rhdirs)]

# Vapor pressure deficit
vpddirs<-list.dirs(paste0(gisdir, "VPD_month_raster"), full.names=T)
vpd<-vpddirs[grep("vpd_", vpddirs)]

# Temperature --------------------------------------------------
## Tair Max
tmaxdirs<-list.dirs(paste0(gisdir, "Tmax_month_raster"), full.names=T)
tmax<-tmaxdirs[grep("tmax", tmaxdirs)]

## Tair Min
tmindirs<-list.dirs(paste0(gisdir, "Tmin_month_raster"), full.names=T)
tmin<-tmindirs[grep("tmin", tmindirs)]

## Tair Mean
tmeandirs<-list.dirs(paste0(gisdir, "Tair_month_raster"), full.names=T)
tmean<-tmeandirs[grep("tair", tmeandirs)]

## Tsurf
tsurfdirs<-list.dirs(paste0(gisdir, "TSurf_month_raster"), full.names=T)
tsurf<-tsurfdirs[grep("tsurf", tsurfdirs)]

# Cloud and Wind --------------------------------------------------
# Cloud cover  
clouddirs<-list.dirs(paste0(gisdir, "HITemp_rastercovariates/CloudFreq_month_raster"), full.names=T)
clouds<-clouddirs[grep("cl_frq", clouddirs)]

# Wind speed  
winddirs<-list.dirs(paste0(gisdir, "HITemp_rastercovariates/WindSpeed_ann_hr_raster"), full.names=T)
winds<-winddirs[grep("wind_sd", winddirs)]

###### Read in rasters ##############################################################
# DEM
dem.r<-raster(dem) # single layer
# Rainfall
staterf.mo<-stack(staterf[1:12]); staterf.ann<-raster(staterf[13])
# Clouds
clouds.mo<-stack(clouds[1:12]); clouds.ann<-raster(clouds[13])
# Wind
winds.24<-stack(winds[1:24])

# RF Seasonality, calculated previously
rfseason<-stack("OUTPUT FILE/RFseason_stack.tif") 
names(rfseason)=c("CV_MeanRF","RelEntropy_D", "Seasonality_S", "Centroid_C", "Duration_Z")

# Create a list of directories for monthly series labeled with abbreviations (vs numbers)
alldirs     <-list ( sw,   dif,   pet,   aet,   sm,   rh,   vpd,  tmax,   tmin,   tmean,   tsurf) 
names(alldirs) <-c ("sw", "dif", "pet", "aet", "sm", "rh", "vpd","tmax", "tmin", "tmean", "tsurf")

# create index for plotting in order, Jan-Dec
mo.order<-c(1, order(month.abb)+1)  
mo.sort<-numeric(length=13)
mo.sort[mo.order]<-1:13

# Organize directories for monthly rasters in order of month (rather than alphabetically)
mofiles<-lapply(alldirs, FUN=function(x) x[mo.sort[2:13]]) 
mostack<-lapply(mofiles, FUN=stack)  # read into raster stack

# Pull out directories of annual rasters
annfiles<-lapply(alldirs, FUN=function(x) x[mo.sort[1]]) 
annraster<-lapply(annfiles, FUN=raster)  # read into raster

###### Calculate other climate indices ######################################################
# Calculate ARIDITY = PET/RF  
aridstack.mo<-mostack$pet/staterf.mo    # for each month
names(aridstack.mo)<-paste0("PET.P_", month.abb)
aridity.ann<-annraster$pet/staterf.ann  # for year
names(aridity.ann)<-"PET.P_ann"
  
# Calculate AET/RF, Actual ET/Precip (>1 indicates irrigation)
aetrfstack.mo<-mostack$aet/staterf.mo   # for each month
names(aetrfstack.mo)<-paste0("AET.P_", month.abb)
aetrf.ann<-annraster$aet/staterf.ann    # for year
names(aetrf.ann)<-"AET.P_ann"

# Summarize winds:
meanwinds<-mean(winds.24)
maxwinds<-max(winds.24)
minwinds<-min(winds.24)
cvwinds<-cv(winds.24)
windsp<-stack(meanwinds, maxwinds, minwinds, cvwinds)
names(windsp)<-c("meanws", "maxws", "minws", "cvws")

###### Combine all of the above ############################################################
allstacks<-c(dem=dem.r, rf.ann=staterf.ann, cl.ann=clouds.ann,  # annual
             annraster, 
             aridity.ann=aridity.ann,
             aetrf.ann=aetrf.ann,
             windsp=windsp,
             
             rf.mo=staterf.mo, cl.mo=clouds.mo,                 # monthly
             mostack, 
             aridity.mo=aridstack.mo, 
             aetrf.mo=aetrfstack.mo,
             rfseason=rfseason)

# Visualize all of these rasters as maps using plot default setting
pdf("allplots2.pdf", onefile=T)
for (i in 1:length(allstacks)){
  plot(allstacks[[i]], main=names(allstacks)[i])
  print(i)
}
dev.off()


###### Zoom into the field systems ##################################################
## Clip only the field systems- use extent(), crop(), and mask()

## First CROP all rasters to leeward Hawaii to zoom in and speed up processing
ext.HI3dry<-extent(c(-156.2, -155.4, 18.9, 20.3)) # Zoom in to leeward HI
allcropped<-lapply(allstacks, FUN= function(x) crop(x, ext.HI3dry))

## Create MASKS so we focus on field systems 
## Note DEM, Temp layers, and 250m ET atlas layers have diff CRS, resolutions!
HI3dry.crop10<-is.na(mask(allcropped$dem, HI3dry))    # DEM mask, note 10m resolution
HI3dry.crop250<-is.na(mask(allcropped$rf.ann, HI3dry)) # All others: 250m, +ellps=WGS84 
HI3dry.crop250temp<-is.na(mask(allcropped$tmax, HI3dry.temp))# Temp layer mask, +ellps=GRS80

## Create a list of masked rasters 
allmasked<-list()
for (i in 1:length(allcropped)){
  if(names(allcropped)[i]=="dem"){
    allmasked[[i]]<-mask(allcropped[[i]], HI3dry.crop10, maskvalue=T)
  }else if (names(allcropped)[i]=="tmax"|
            names(allcropped)[i]=="tmin"){
    allmasked[[i]]<-mask(allcropped[[i]], HI3dry.crop250temp, maskvalue=T)
  }else{
    allmasked[[i]]<-mask(allcropped[[i]], HI3dry.crop250, maskvalue=T)
}}
names(allmasked)<-names(allcropped)

###### Set up some tools to aid data visualization ####################################
## Set up colors for plotting raster data
my.colors<-colorRampPalette(c("blue", "green", "light green", "yellow", "orange", "red"))
rf.colors<-colorRampPalette(c("red", "orange", "yellow", "light green", "blue"))  #add'l: bias=3...

## set up some functions for plotting
addHI3dryline<-function(){plot(HI3dry, add=TRUE)} # show outline
fixstackcolors<-function(x,n){    # set scale for stacks
  seq(min(minValue(x)), 
      max(maxValue(x)),
      length.out=n)}

## Crop the ahupuaa shapefile for plotting
library(rgeos)  
ahucrop<-crop(ahupuaa, ext.HI3dry)

###### Plot rasters to compare the field systems ########################################
## Make a series of pdfs-----------------------------------------------------------------
pdf("allplots_fieldsystems5.pdf", onefile=T)
for (i in 1:length(allmasked)){
  rng<- range(cellStats(allmasked[[i]], stat='range')) # set value range
  labs<-seq(from=rng[1], to=rng[2], length=5)          # identify breaks
  arg<- list(at=labs, labels=round(labs, 2))          
  fixbreaks<-fixstackcolors(allmasked[[i]], 15) # fix color scale for monthly raster stacks
  
  if(names(allmasked)[i] %in% c("rfseason","windsp")){ # DON'T fix colors for these stacks
    for (j in 1:nlayers(allmasked[[i]])){
      plot(ahucrop, main=names(allmasked[[i]][[j]]), lwd=0.5)
      plot(allmasked[[i]][[j]], col=rev(my.colors(15)), alpha=0.8, add=T)
      #addHI3dryline()
    }
  }else if(names(allmasked)[i] %in%                    # DO fix colors for these stacks
           c("sw","pet","vpd", "tmax", "tmin", "tmean", "tsurf", 
             "aridity.ann", "aridity.mo")){    
    for (j in 1:nlayers(allmasked[[i]])){
      plot(ahucrop,main=names(allmasked[[i]][[j]]), lwd=0.5)
      plot(allmasked[[i]][[j]], breaks=fixbreaks, col=rev(rf.colors(15)),
           axis.args=arg, alpha=0.9, add=T)
      #addHI3dryline()
    }
  }else{
    for (j in 1:nlayers(allmasked[[i]])){              # DO fix colors for these stacks
      plot(ahucrop,main=names(allmasked[[i]][[j]]), lwd=0.5)
      plot(allmasked[[i]][[j]], breaks=fixbreaks, col=rf.colors(15),
           axis.args=arg, alpha=0.9, add=T)
      #addHI3dryline()
    }
  }
}
dev.off()

## Now save these files as geotiffs------------------------------------------------------
for (i in 1:length(allmasked)){
  if(nlayers(allmasked[[i]])==1){
    writeRaster(allmasked[[i]], filename=paste0("FS3_geotiff/", names(allmasked)[i]),
                format="GTiff", overwrite=T)
  }else{writeRaster(allmasked[[i]], 
                    filename=paste0("FS3_geotiff/", names(allmasked)[i],"stack.tif"),
                    format="GTiff", bylayer=F, overwrite=T)}
}


###### VISUALIZE STATISTICS by field system #############################################
## Separate and prepare the data for the 3 field systems: Kohala, Kona, Kau--------------

# CRS for most of the raster layers
crs<-proj4string(HI3dry)  

# Select each polygon
Kohala<-SpatialPolygons(list(test=HI3dry@polygons[[1]]), proj4string=CRS(crs))
Kona<-SpatialPolygons(list(test=HI3dry@polygons[[2]]), proj4string=CRS(crs))
Kau<-SpatialPolygons(list(test=HI3dry@polygons[[3]]), proj4string=CRS(crs))

# Crop each raster using each of the polygons
all.kohala<-lapply(allmasked, FUN= function(x) crop(x, Kohala))
all.kona<-lapply(allmasked, FUN= function(x) crop(x, Kona))
all.kau<-lapply(allmasked, FUN= function(x) crop(x, Kau))

ann<-which(lapply(all.kohala, nlayers)==1) # annual
mo<-which(lapply(all.kohala, nlayers)==12) # monthly

# Which raster stacks need to be analyzed separately?
sep<-which(lapply(all.kohala, nlayers) %in% c(4,5)) # wind, rfseason

## Plot annual metrics across field systems----------------------------------------------
pdf("annualmetrics.pdf")
par(mfrow=c(4,4), c(3, 4, 3, 2))
for (i in 1:length(ann)){
  d.koh<-density(all.kohala[[i]], plot=F)
  d.kon<-density(all.kona[[i]], plot=F)
  d.kau<-density(all.kau[[i]], plot=F)
  ylim<-range(d.koh$y, d.kon$y, d.kau$y)
  xlim<-range(d.koh$x, d.kon$x, d.kau$x)
  
  plot(d.koh, xlim=xlim, ylim=ylim, col="#000000", lty=1,
       xlab="", main=names(all.kohala)[[i]])
  lines(d.kon, col="#0072B2", lty=2)
  lines(d.kau, col="#D55E00", lty=3, lwd=2)
  if(i==1){
    legend("right", legend=c("Kohala", "Kona", "Kau"),
           lty=1:3, lwd=c(1,1,2), bty="n",
           col=c("#000000","#0072B2","#D55E00"))}
}

#### Plot the wind metrics------------------------
par(mfrow=c(2,2))
for(i in 1:nlayers(all.kohala[[sep[1]]])){
  d.koh<-density(raster(all.kohala[[sep[1]]], layer=i), plot=F)
  d.kon<-density(raster(all.kona[[sep[1]]], layer=i), plot=F)
  d.kau<-density(raster(all.kau[[sep[1]]], layer=i), plot=F)
  ylim<-range(d.koh$y, d.kon$y, d.kau$y)
  xlim<-range(d.koh$x, d.kon$x, d.kau$x)
  
  plot(d.koh, xlim=xlim, ylim=ylim, col="#000000", lty=1,
       xlab="", main=names(all.kohala[[sep[1]]])[i])
  lines(d.kon, col="#0072B2", lty=2)
  lines(d.kau, col="#D55E00", lty=3, lwd=2)
  
  if(i==1){
    legend("topright", legend=c("Kohala", "Kona", "Kau"),
           lty=1:3, lwd=c(1,1,2), bty="n",
           col=c("#000000","#0072B2","#D55E00"))}
}

#### Plot the intra-annual rainfall metrics-------
par(mfrow=c(3,2))
# Mean annual RF first
  d.koh<-density(all.kohala$rf.ann, plot=F)
  d.kon<-density(all.kona$rf.ann, plot=F)
  d.kau<-density(all.kau$rf.ann, plot=F)
  ylim<-range(d.koh$y, d.kon$y, d.kau$y)
  xlim<-range(d.koh$x, d.kon$x, d.kau$x)
  
  plot(d.koh, xlim=xlim, ylim=ylim, col="#000000", lty=1,
       xlab="", main=names(all.kohala$rf.ann))
  lines(d.kon, col="#0072B2", lty=2)
  lines(d.kau, col="#D55E00", lty=3, lwd=2)
  legend("topright", legend=c("Kohala", "Kona", "Kau"),
         lty=1:3, lwd=c(1,1,2), bty="n",
         col=c("#000000","#0072B2","#D55E00"))

# RF seasonality next
for(i in 1:nlayers(all.kohala[[sep[2]]])){
  d.koh<-density(raster(all.kohala[[sep[2]]], layer=i), plot=F)
  d.kon<-density(raster(all.kona[[sep[2]]], layer=i), plot=F)
  d.kau<-density(raster(all.kau[[sep[2]]], layer=i), plot=F)
  ylim<-range(d.koh$y, d.kon$y, d.kau$y)
  xlim<-range(d.koh$x, d.kon$x, d.kau$x)
  
  plot(d.koh, xlim=xlim, ylim=ylim, col="#000000", lty=1,
       xlab="", main=names(all.kohala[[sep[2]]])[i])
  lines(d.kon, col="#0072B2", lty=2)
  lines(d.kau, col="#D55E00", lty=3, lwd=2)
}

dev.off()

### Plot monthly metrics across field systems -------------------------------------------
pdf("monthlymetrics.pdf")
par(mfrow=c(3,3))
for (i in mo){
  ylim<-range(minValue(all.kohala[[i]]), 
              minValue(all.kona[[i]]), 
              minValue(all.kau[[i]]), 
              maxValue(all.kohala[[i]]), 
              maxValue(all.kona[[i]]), 
              maxValue(all.kau[[i]]))
  boxplot(all.kohala[[i]], ylim=ylim, main="Kohala", 
          border="#000000", notch=T, #boxwex=1.2,
          xlab="month", ylab=names(all.kohala)[i])
  boxplot(all.kona[[i]], ylim=ylim, main="Kona", 
          border="#0072B2", notch=T, #add=T,
          xlab="month", ylab=names(all.kona)[i])
  boxplot(all.kau[[i]], ylim=ylim, main="Kau", 
          border="#D55E00", notch=T, #add=T, boxwex=0.5,
          xlab="month", ylab=names(all.kau)[i])
  print(i)
}
dev.off()

plot(all.kohala$aridity.ann, all.kohala$aetrf.ann)
plot(all.kona$aridity.ann, all.kona$aetrf.ann, col="blue", add=T)
plot(all.kau$aridity.ann, all.kau$aetrf.ann, col="green", add=T)

#########################################################################################
################## Make pretty figures for publication ##################################
## FIGURE 2: Density plots of mean annual climate ---------
pdf("Fig2_MeanAnnual_v3.pdf", width=9, height=5)
ann.fin<-c(1, 13, 
           2, 9,
           3, 4,
           NA, 6)

names.ann.fin<-c("Elevation (m)",
                  expression("Mean Air Temperature " (degree*C)),
                  "Annual Rainfall (mm)",
                  "Relative Humidity (%)",
                  "Cloud Frequency",
                  expression("Shortwave Radiation " (Wm^-2)),
                  expression("Mean Wind Speed " (ms^-1)), 
                  "Potential ET (mm)")
figlabel<-letters[1:8]

par(mfcol=c(2,4), mar=c(4.5, 4, 1, 0.6) + 0.1)
for (i in 1:length(ann.fin)){
  if(i==7){
    d.koh<-density(raster(all.kohala[[sep[1]]], layer=1), plot=F)
    d.kon<-density(raster(all.kona[[sep[1]]], layer=1), plot=F)
    d.kau<-density(raster(all.kau[[sep[1]]], layer=1), plot=F)
    ylim<-range(d.koh$y, d.kon$y, d.kau$y)
    xlim<-range(d.koh$x, d.kon$x, d.kau$x)
    
    plot(d.koh, xlim=xlim, ylim=ylim, col="#000000", lty=1,
         xlab=names.ann.fin[i], main="", ylab="", cex.axis=0.9)
    lines(d.kon, col="#0072B2", lty=2, lwd=1.5)
    lines(d.kau, col="#E69F00", lty=3, lwd=3)
    mtext(figlabel[i], side=3, line=-1.3, adj=0.04)
  }else{
  d.koh<-density(all.kohala[[ann.fin[i]]], plot=F)
  d.kon<-density(all.kona[[ann.fin[i]]], plot=F)
  d.kau<-density(all.kau[[ann.fin[i]]], plot=F)
  ylim<-range(d.koh$y, d.kon$y, d.kau$y)
  xlim<-range(d.koh$x, d.kon$x, d.kau$x)
  
  plot(d.koh, xlim=xlim, ylim=ylim, col="#000000", lty=1,
       xlab=names.ann.fin[i], main="", ylab="", cex.axis=0.9)
  lines(d.kon, col="#0072B2", lty=2, lwd=1.5)
  lines(d.kau, col="#E69F00", lty=3, lwd=3)
  mtext(figlabel[i], side=3, line=-1.3, adj=0.04)
  if(i==1){
    legend("topright", legend=c("Kohala", "Kona", "Kau"),
           lty=c(1,2,3), lwd=c(1,1.5,2), bty="n",
           col=c("#000000","#0072B2","#E69F00"))
    mtext("Density", side=2, line=2.5, cex=0.7)}
  if(i==2){mtext("Density", side=2, line=2.5, cex=0.7)}
  }
}
dev.off()

## FIGURE 3: Boxplots of seasonality: P, PET, and aridity (PET/P) ----------
pdf("Fig3_AnnualCycle_P_PET_Aridity_fin2.pdf", width=6, height=5)
fig3<-mo[names(mo) %in% c("rf.mo", "pet", "aridity.mo")]
names(all.kohala[["aridity.mo"]])<-month.abb
names(all.kona[["aridity.mo"]])<-month.abb
names(all.kau[["aridity.mo"]])<-month.abb
figlabel<-letters[1:9]
par(mfrow=c(3,3), mar=c(0.2, 0, 0, 0), oma=c(3,5,3,2))
for (i in fig3){
  ylim<-range(minValue(all.kohala[[i]]), 
              minValue(all.kona[[i]]), 
              minValue(all.kau[[i]]), 
              maxValue(all.kohala[[i]]), 
              maxValue(all.kona[[i]]), 
              maxValue(all.kau[[i]]))
  if (i==fig3[1]){
    boxplot(all.kohala[[i]], ylim=ylim, 
            border="#000000", notch=T,
            xaxt="n", las=1)
    mtext("Kohala", side=3, line=1)
    mtext("Rainfall (P, mm)", side=2, line=3.2, cex=0.8)
    mtext("a", side=3, line=-1.3, adj=0.04)
    boxplot(all.kona[[i]], ylim=ylim, 
            border="#0072B2", notch=T,
            xaxt="n", yaxt="n")
    mtext("Kona", side=3, line=1)
    mtext("b", side=3, line=-1.3, adj=0.04)
    boxplot(all.kau[[i]], ylim=ylim,
            border="#E69F00", notch=T, 
            xaxt="n", yaxt="n")
    mtext("Kau", side=3, line=1)
    mtext("c", side=3, line=-1.3, adj=0.04)
  }else if(i==fig3[2]){
    boxplot(all.kohala[[i]], ylim=ylim, 
            border="#000000", notch=T,
            xaxt="n", las=1)
    mtext("PET (mm)", side=2, line=3.2, cex=0.8)
    mtext("d", side=3, line=-1.3, adj=0.04)
    boxplot(all.kona[[i]], ylim=ylim, 
            border="#0072B2", notch=T,
            xaxt="n", yaxt="n")
    mtext("e", side=3, line=-1.3, adj=0.04)
    boxplot(all.kau[[i]], ylim=ylim, 
            border="#E69F00", notch=T, 
            xaxt="n", yaxt="n")
    mtext("f", side=3, line=-1.3, adj=0.04)
  }else{
    boxplot(all.kohala[[i]], ylim=ylim,  
            border="#000000", notch=T,
            cex.axis=0.9, las=1)
    mtext("Aridity (PET/P)", side=2, line=3.2, cex=0.8)
    mtext("g", side=3, line=-1.3, adj=0.04)
    boxplot(all.kona[[i]], ylim=ylim, 
            border="#0072B2", notch=T, 
            cex.axis=0.9, yaxt="n", las=1)
    mtext("h", side=3, line=-1.3, adj=0.04)
    boxplot(all.kau[[i]], ylim=ylim, 
            border="#E69F00", notch=T,
            cex.axis=0.9, yaxt="n", las=1)
    mtext("i", side=3, line=-1.3, adj=0.04)
  }
  print(i)
}
dev.off()

## FIGURE S2: Boxplots of seasonality: Tmax, Tmin, RH, and VPD ----------------
pdf("FigS2_AnnualCycle_T_RH_VPD_fin.pdf", width=6, height=5)
figs2<-mo[names(mo) %in% c("tmax", "tmin", "rh", "vpd")]
figs2<-figs2[c(3,1,2)]

names(all.kohala[[figs2[3]]])<-month.abb
names(all.kona[[figs2[3]]])<-month.abb
names(all.kau[[figs2[3]]])<-month.abb

figlabel<-letters[1:9]
par(mfrow=c(3,3), mar=c(0.2, 0, 0, 0), oma=c(3,5,3,2))
for (i in figs2){   # temperature
  if (i==figs2[1]){
    ylim<-range(minValue(all.kohala[[i+1]]), 
                minValue(all.kona[[i+1]]), 
                minValue(all.kau[[i+1]]), 
                maxValue(all.kohala[[i]]), 
                maxValue(all.kona[[i]]), 
                maxValue(all.kau[[i]]))
    boxplot(all.kohala[[i]], ylim=ylim, 
            border="#000000", notch=T,
            xaxt="n", las=1)
    boxplot(all.kohala[[i+1]], ylim=ylim, 
            border="#000000", notch=T,
            xaxt="n", las=1, add=T)
    mtext("Kohala", side=3, line=1)
    mtext(expression(paste(T[min], " and ", T[max], " ", (degree*C)), sep=""), side=2, line=3.2, cex=0.8)
    mtext("a", side=3, line=-1.3, adj=0.04)
    boxplot(all.kona[[i]], ylim=ylim, 
            border="#0072B2", notch=T,
            xaxt="n", yaxt="n")
    boxplot(all.kona[[i+1]], ylim=ylim, 
            border="#0072B2", notch=T,
            xaxt="n", yaxt="n", add=T)
    mtext("Kona", side=3, line=1)
    mtext("b", side=3, line=-1.3, adj=0.04)
    boxplot(all.kau[[i]], ylim=ylim,
            border="#E69F00", notch=T, 
            xaxt="n", yaxt="n")
    boxplot(all.kau[[i+1]], ylim=ylim,
            border="#E69F00", notch=T, 
            xaxt="n", yaxt="n", add=T)
    mtext("Kau", side=3, line=1)
    mtext("c", side=3, line=-1.3, adj=0.04)
  }else{
    ylim<-range(minValue(all.kohala[[i]]), 
                minValue(all.kona[[i]]), 
                minValue(all.kau[[i]]), 
                maxValue(all.kohala[[i]]), 
                maxValue(all.kona[[i]]), 
                maxValue(all.kau[[i]]))
    if(i==figs2[2]){
      boxplot(all.kohala[[i]], ylim=ylim, 
              border="#000000", notch=T,
              xaxt="n", las=1)
      mtext("RH (%)", side=2, line=3.2, cex=0.8)
      mtext("d", side=3, line=-1.3, adj=0.04)
      boxplot(all.kona[[i]], ylim=ylim, 
              border="#0072B2", notch=T,
              xaxt="n", yaxt="n")
      mtext("e", side=3, line=-1.3, adj=0.04)
      boxplot(all.kau[[i]], ylim=ylim, 
              border="#E69F00", notch=T, 
              xaxt="n", yaxt="n")
      mtext("f", side=3, line=-1.3, adj=0.04)
    }else{
      boxplot(all.kohala[[i]], ylim=ylim,  
              border="#000000", notch=T,
              cex.axis=0.9, las=1)
      mtext("VPD (Pa)", side=2, line=3.2, cex=0.8)
      mtext("g", side=3, line=-1.3, adj=0.04)
      boxplot(all.kona[[i]], ylim=ylim, 
              border="#0072B2", notch=T, 
              cex.axis=0.9, yaxt="n", las=1)
      mtext("h", side=3, line=-1.3, adj=0.04)
      boxplot(all.kau[[i]], ylim=ylim, 
              border="#E69F00", notch=T,
              cex.axis=0.9, yaxt="n", las=1)
      mtext("i", side=3, line=-1.3, adj=0.04)
    }
  }
  print(i)
}
dev.off()




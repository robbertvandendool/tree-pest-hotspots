#description

#load data


#Maps
###Predict with final model
finalpresmod_preds = predictBsDM(data=st_drop_geometry(background_1m[,names(finalpresmod)]), presmod=finalpresmod, backmod=finalbackmod)

background_base2 = background_1m[,names(finalpresmod)]
background_base2$preds = finalpresmod_preds
background_base2$preds_scaled = finalpresmod_preds / sum(finalpresmod_preds, na.rm=T)
background_base2$preds_log = log(finalpresmod_preds)

brks <- unique(quantile(background_base2$preds_scaled, probs = seq(0, 1, 0.01), type=7, na.rm=T))
background_base2$preds_perc <- as.numeric(cut(background_base2$preds_scaled, breaks = brks, labels = 1:c(length(brks)-1), include.lowest = TRUE))

### Maps export.

##figures
# tiff("run7_predmap_scaled.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_base2, aes(col=preds_scaled), cex=0.5, alpha=1)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()
# 
# tiff("run7_predmap_log.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_base2, aes(col=preds_log), cex=0.5, alpha=1)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", midpoint=-15,high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()
# 
# tiff("run7_predmap_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_base2, aes(col=preds_perc), cex=0.5, alpha=0.8)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()

##raster
background_export = background_base2[,"preds_perc"]
st_write(background_export, here("PredictionsPerc.shp"), delete_dsn=T) 

background_points <- as(background_export, "Spatial")
background_raster = rasterFromXYZ(background_points, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
crs(background_raster) = CRS(SRS_string = "EPSG:4326") #3857
raster::writeRaster(background_raster,filename = "run7_percraster", format = "GTiff")

background_exportlog = background_base2[,"preds_log"]
st_write(background_exportlog, here("Predictionslog.shp"), delete_dsn=T) 


background_pointslog <- as(background_exportlog, "Spatial")
background_rasterlog = rasterFromXYZ(background_pointslog, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
crs(background_rasterlog) = CRS(SRS_string = "EPSG:4326") #3857
raster::writeRaster(background_rasterlog,filename = "run7_lograster", format = "GTiff")




background_raster_df = as(background_raster, "SpatialPixelsDataFrame")
background_raster_df <- as.data.frame(background_raster_df)

tiff("run7_rastermap_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
ggplot()+ 
  geom_tile(data = background_raster_df , aes(x = x, y = y, fill = layer)) +
  scale_fill_gradient2(name="",low = "blue", mid="green", midpoint = 42, high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
dev.off() 

#raster log!
background_export = background_base2[,"preds_log"]
background_points <- as(background_export, "Spatial")
background_raster = rasterFromXYZ(background_points, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
background_raster_df = as(background_raster, "SpatialPixelsDataFrame")
background_raster_df <- as.data.frame(background_raster_df)

tiff("run7_rastermap_log.tiff", width = 5, height = 4, units = 'in', res = 600)
ggplot()+ 
  geom_tile(data = background_raster_df , aes(x = x, y = y, fill = layer)) +
  scale_fill_gradient2(name="",low = "blue", mid="green", midpoint = -20, high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
dev.off() 


#
# background_base$frstcv1
# background_base$GDP
# plot(background_base[,"BIO10"])
# plot(background_base[,"frstcv1"])
# subvarnames
# 
# 
# varmap_points <- as(background_100k[,"NrCty_D"], "Spatial")
# varmap_raster = rasterFromXYZ(varmap_points, res=c(NA,NA), crs="EPSG:3035", digits=10)
# crs(varmap_raster) = CRS(SRS_string = "EPSG:4326")
# varmap_raster_df = as(varmap_raster, "SpatialPixelsDataFrame")
# varmap_raster_df <- as.data.frame(varmap_raster_df)
# 
# tiff("run7_varmapraster.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_tile(data = varmap_raster_df , aes(x = x, y = y, fill = layer)) +
#   scale_fill_gradient2(name="",low = "blue", mid="green", midpoint=200,high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off() 
#

#kml
names(background_raster) = "reports"
palette <- rev(RColorBrewer::brewer.pal(10, "Spectral"))
plotKML::kml(background_raster, colour = reports, colour_scale = palette, alpha = 1, file.name = "rep_perc_raster.kml")




####Sampling
#Predict with
sampfinalpresmod_preds = predictBsDM(data=st_drop_geometry(background_1m[,names(sampfinalpresmod)]), presmod=sampfinalpresmod, backmod=sampukde_back)

background_samp = background_1m[,names(sampfinalpresmod_preds)]
background_samp$preds = sampfinalpresmod_preds
background_samp$preds_scaled = sampfinalpresmod_preds / sum(sampfinalpresmod_preds, na.rm=T)
background_samp$preds_log = log(sampfinalpresmod_preds)

brks <- unique(quantile(background_samp$preds_scaled, probs = seq(0, 1, 0.01), type=7, na.rm=T))
background_samp$preds_perc <- as.numeric(cut(background_samp$preds_scaled, breaks = brks, labels = 1:c(length(brks)-1), include.lowest = TRUE))

### Maps export.

##figures
# tiff("run7_sampmap_scaled.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_samp, aes(col=preds_scaled), cex=0.5, alpha=1)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()
# 
# tiff("run7_sampmap_log.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_samp, aes(col=preds_log), cex=0.5, alpha=1)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", midpoint=-15,high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()
# 
# tiff("run7_sampmap_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_samp, aes(col=preds_perc), cex=0.5, alpha=0.8)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()


##raster
background_export2log = background_samp[,"preds_log"]
st_write(background_export2log, here("PredictionsSampLog.shp"), delete_dsn=T) 

background_points2 <- as(background_export2log, "Spatial")
sample_raster = rasterFromXYZ(background_points2, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
names(sample_raster) = "samples"
crs(sample_raster) = CRS(SRS_string = "EPSG:4326") #3857
raster::writeRaster(sample_raster,filename = "run7_samplograster", format = "GTiff", overwrite=TRUE)


background_export2 = background_samp[,"preds_perc"]
st_write(background_export2, here("PredictionsSampPerc.shp"), delete_dsn=T) 

background_points2 <- as(background_export2, "Spatial")
sample_raster = rasterFromXYZ(background_points2, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
names(sample_raster) = "samples"
crs(sample_raster) = CRS(SRS_string = "EPSG:4326") #3857

raster::writeRaster(sample_raster,filename = "run7_samppercraster", format = "GTiff", overwrite=TRUE)


sample_raster_df = as(sample_raster, "SpatialPixelsDataFrame")
sample_raster_df <- as.data.frame(sample_raster_df)

tiff("run7_rastersamplemap_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
ggplot()+ 
  geom_tile(data = sample_raster_df , aes(x = x, y = y, fill = samples)) +
  scale_fill_gradient2(name="",low = "blue", mid="green", midpoint = 50, high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
dev.off() 

#kml
palette <- rev(RColorBrewer::brewer.pal(10, "Spectral"))
plotKML::kml(sample_raster, colour = samples, colour_scale = palette, alpha = 1, file.name = "samp_perc_raster.kml")














#### Sampling bias correction map
### bias corrected map
#predictions
biaspresmod_preds = predictBsDM(data=st_drop_geometry(background_1m[,names(finalpresmod)]), presmod=finalpresmod, backmod=biasbackmodel)



background_bias = background_1m[,names(finalpresmod)]
background_bias$preds = biaspresmod_preds
background_bias$preds_scaled = biaspresmod_preds / sum(biaspresmod_preds, na.rm=T)
background_bias$preds_log = log(biaspresmod_preds)

brks <- unique(quantile(background_bias$preds_scaled, probs = seq(0, 1, 0.01), type=7, na.rm=T))
background_bias$preds_perc <- as.numeric(cut(background_bias$preds_scaled, breaks = brks, labels = 1:c(length(brks)-1), include.lowest = TRUE))

### Maps export.

##figures
# tiff("run7_biasgenera_map_scaled.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_bias, aes(col=preds_scaled), cex=0.5, alpha=1)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()
# 
# tiff("run7_biasgenera_map_log.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_bias, aes(col=preds_log), cex=0.5, alpha=1)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", midpoint=-15,high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()
# 
# tiff("run7_biasgenera_map_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_bias, aes(col=preds_perc), cex=0.5, alpha=0.8)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()

##raster
background_export3 = background_bias[,"preds_perc"]
#st_write(bias_export, here("Data_Intermediate/PredictionsPerc.shp"), delete_dsn=T) #named number 2 for safe testing

background_points3 <- as(background_export3, "Spatial")
background_raster3 = rasterFromXYZ(background_points3, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
names(background_raster3) = "bias"
crs(background_raster3) = CRS(SRS_string = "EPSG:4326") #3857
raster::writeRaster(background_raster3,filename = "run7_biasgenera_percraster", format = "GTiff", overwrite=T)

background_export3log = background_bias[,"preds_log"]
#st_write(bias_export, here("Data_Intermediate/PredictionsPerc.shp"), delete_dsn=T) #named number 2 for safe testing
background_points3log <- as(background_export3log, "Spatial")
background_raster3log = rasterFromXYZ(background_points3log, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
names(background_raster3log) = "bias"
crs(background_raster3log) = CRS(SRS_string = "EPSG:4326") #3857
raster::writeRaster(background_raster3log,filename = "run7_biasgenera_lograster", format = "GTiff", overwrite=T)

bias_raster_df = as(background_raster3, "SpatialPixelsDataFrame")
bias_raster_df <- as.data.frame(bias_raster_df)


tiff("run7_rasterbiasgenera_map_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
ggplot()+ 
  geom_tile(data = bias_raster_df , aes(x = x, y = y, fill = bias)) +
  scale_fill_gradient2(name="",low = "blue", mid="green", midpoint = 42, high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
dev.off() 

#kml
palette <- rev(RColorBrewer::brewer.pal(10, "Spectral"))
plotKML::kml(background_raster3, colour = bias, colour_scale = palette, alpha = 1, file.name = "biasgenera_perc_raster.kml")


###Difference map
##### Percentiles
background_diff = background_1m[,names(finalpresmod)]
background_diff$preds = biaspresmod_preds - finalpresmod_preds
#background_diff$preds_scaled = background_diff$preds / sum(background_diff$preds, na.rm=T)

brks <- unique(quantile(background_diff$preds, probs = seq(0, 1, 0.01), type=7, na.rm=T))
background_diff$preds_perc <- as.numeric(cut(background_diff$preds, breaks = brks, labels = 1:c(length(brks)-1), include.lowest = TRUE))

background_export4 = background_diff[,"preds_perc"]
background_points4 <- as(background_export4, "Spatial")
background_raster4 = rasterFromXYZ(background_points4, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
crs(background_raster4) = CRS(SRS_string = "EPSG:4326") #3857
raster::writeRaster(background_raster4,filename = "run7_diffpercraster", format = "GTiff", overwrite=T)

background_export5 = background_diff[,"preds"]
background_points5 <- as(background_export5, "Spatial")
background_raster5 = rasterFromXYZ(background_points5, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
crs(background_raster5) = CRS(SRS_string = "EPSG:4326") #3857
raster::writeRaster(background_raster5,filename = "run7_diffpredraster", format = "GTiff", overwrite=T)

diff_raster_df = as(background_raster4, "SpatialPixelsDataFrame")
diff_raster_df <- as.data.frame(diff_raster_df)
names(diff_raster_df)[1] = "diff"

tiff("run7_rasterdiffmap_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
ggplot()+ 
  geom_tile(data = diff_raster_df , aes(x = x, y = y, fill = diff)) +
  scale_fill_gradient2(name="",low = "blue", mid="green", midpoint = 42, high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
dev.off() 





### Lepidoptera sampling model

#Predict with
lepidopterafinalpresmod_preds = predictBsDM(data=st_drop_geometry(background_1m[,names(lepidopterafinalpresmod)]), presmod=lepidopterafinalpresmod, backmod=lepidopteraukde_back)

background_lepidoptera = background_1m[,names(lepidopterafinalpresmod_preds)]
background_lepidoptera$preds = lepidopterafinalpresmod_preds
background_lepidoptera$preds_scaled = lepidopterafinalpresmod_preds / sum(lepidopterafinalpresmod_preds, na.rm=T)
background_lepidoptera$preds_log = log(lepidopterafinalpresmod_preds)

brks <- unique(quantile(background_lepidoptera$preds_scaled, probs = seq(0, 1, 0.01), type=7, na.rm=T))
background_lepidoptera$preds_perc <- as.numeric(cut(background_lepidoptera$preds_scaled, breaks = brks, labels = 1:c(length(brks)-1), include.lowest = TRUE))

### Maps export.

##figures
# tiff("run7_lepidopteramap_scaled.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_lepidoptera, aes(col=preds_scaled), cex=0.5, alpha=1)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()
# 
# tiff("run7_lepidopteramap_log.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_lepidoptera, aes(col=preds_log), cex=0.5, alpha=1)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", midpoint=-15,high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()
# 
# tiff("run7_lepidopteramap_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_lepidoptera, aes(col=preds_perc), cex=0.5, alpha=0.8)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()


##raster
background_exportlepidoptera = background_lepidoptera[,"preds_perc"]
st_write(background_exportlepidoptera, here("PredictionslepidopteraPerc.shp"), delete_dsn=T) 


background_pointslepidoptera <- as(background_exportlepidoptera, "Spatial")
lepidoptera_raster = rasterFromXYZ(background_pointslepidoptera, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
names(lepidoptera_raster) = "lepidoptera"
crs(lepidoptera_raster) = CRS(SRS_string = "EPSG:4326") #3857

raster::writeRaster(lepidoptera_raster,filename = "run7_lepidopterapercraster", format = "GTiff")


lepidoptera_raster_df = as(lepidoptera_raster, "SpatialPixelsDataFrame")
lepidoptera_raster_df <- as.data.frame(lepidoptera_raster_df)

tiff("run7_rasterlepidopteramap_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
ggplot()+ 
  geom_tile(data = lepidoptera_raster_df , aes(x = x, y = y, fill = lepidoptera)) +
  scale_fill_gradient2(name="",low = "blue", mid="green", midpoint = 50, high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
dev.off() 

#kml
palette <- rev(RColorBrewer::brewer.pal(10, "Spectral"))
plotKML::kml(lepidoptera_raster, colour = lepidoptera, colour_scale = palette, alpha = 1, file.name = "lepidoptera_perc_raster.kml")









#Lepidoptera bias corrected model
### bias corrected map
#predictions
biaspresmod_preds2 = predictBsDM(data=st_drop_geometry(background_1m[,names(finalpresmod)]), presmod=finalpresmod, backmod=biasbackmodel2)



background_bias2 = background_1m[,names(finalpresmod)]
background_bias2$preds = biaspresmod_preds2
background_bias2$preds_scaled = biaspresmod_preds2 / sum(biaspresmod_preds2, na.rm=T)
background_bias2$preds_log = log(biaspresmod_preds2)

brks <- unique(quantile(background_bias2$preds_scaled, probs = seq(0, 1, 0.01), type=7, na.rm=T))
background_bias2$preds_perc <- as.numeric(cut(background_bias2$preds_scaled, breaks = brks, labels = 1:c(length(brks)-1), include.lowest = TRUE))

### Maps export.

##figures
tiff("run7_biaslepidoptera_map_scaled.tiff", width = 5, height = 4, units = 'in', res = 600)
ggplot()+ 
  geom_sf(data=background_bias2, aes(col=preds_scaled), cex=0.5, alpha=1)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
  scale_colour_gradient2(name="",low = "blue", mid="green", high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
dev.off()

tiff("run7_biaslepidoptera_map_log.tiff", width = 5, height = 4, units = 'in', res = 600)
ggplot()+ 
  geom_sf(data=background_bias2, aes(col=preds_log), cex=0.5, alpha=1)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
  scale_colour_gradient2(name="",low = "blue", mid="green", midpoint=-15,high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
dev.off()

tiff("run7_biaslepidoptera_map_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
ggplot()+ 
  geom_sf(data=background_bias2, aes(col=preds_perc), cex=0.5, alpha=0.8)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
  scale_colour_gradient2(name="",low = "blue", mid="green", high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
dev.off()

##raster
background_exportbias2 = background_bias[,"preds_perc"]
#st_write(bias_export, here("Data_Intermediate/PredictionsPerc.shp"), delete_dsn=T) #named number 2 for safe testing


background_pointsbias2 <- as(background_exportbias2, "Spatial")
background_rasterbias2 = rasterFromXYZ(background_pointsbias2, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
names(background_rasterbias2) = "bias"
crs(background_rasterbias3) = CRS(SRS_string = "EPSG:4326") #3857
raster::writeRaster(background_rasterbias2,filename = "run7_biaslepidoptera_percraster", format = "GTiff", overwrite=T)


lepidoptera2_raster_df = as(background_rasterbias2, "SpatialPixelsDataFrame")
lepidoptera2_raster_df <- as.data.frame(lepidoptera2_raster_df)


tiff("run7_rasterbiasglepidoptera_map_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
ggplot()+ 
  geom_tile(data = lepidoptera2_raster_df , aes(x = x, y = y, fill = bias)) +
  scale_fill_gradient2(name="",low = "blue", mid="green", midpoint = 50, high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
dev.off() 

#kml
#names(background_raster3) = "bias"
palette <- rev(RColorBrewer::brewer.pal(10, "Spectral"))
plotKML::kml(background_rasterbias2, colour = bias, colour_scale = palette, alpha = 1, file.name = "biaslepidoptera_perc_raster.kml")


###Difference map
##### Percentiles
background_diff = background_1m[,names(finalpresmod)]
background_diff$preds = biaspresmod_preds2 - finalpresmod_preds
#background_diff$preds_scaled = background_diff$preds / sum(background_diff$preds, na.rm=T)

brks <- unique(quantile(background_diff$preds, probs = seq(0, 1, 0.01), type=7, na.rm=T))
background_diff$preds_perc <- as.numeric(cut(background_diff$preds, breaks = brks, labels = 1:c(length(brks)-1), include.lowest = TRUE))

background_export4 = background_diff[,"preds_perc"]

background_points4 <- as(background_export4, "Spatial")
background_raster4 = rasterFromXYZ(background_points4, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
crs(background_raster4) = CRS(SRS_string = "EPSG:4326") #3857
raster::writeRaster(background_raster4,filename = "run7_diff2percraster", format = "GTiff")

diff_raster_df = as(background_raster4, "SpatialPixelsDataFrame")
diff_raster_df <- as.data.frame(diff_raster_df)
names(diff_raster_df)[1] = "diff"

tiff("run7_rasterdiffmap2_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
ggplot()+ 
  geom_tile(data = diff_raster_df , aes(x = x, y = y, fill = diff)) +
  scale_fill_gradient2(name="",low = "blue", mid="green", midpoint = 50, high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
dev.off() 





#### Insecta sampling model
#Predict with
insectafinalpresmod_preds = predictBsDM(data=st_drop_geometry(background_1m[,names(insectafinalpresmod)]), presmod=insectafinalpresmod, backmod=insectaukde_back)

background_insecta = background_1m[,names(insectafinalpresmod_preds)]
background_insecta$preds = insectafinalpresmod_preds
background_insecta$preds_scaled = insectafinalpresmod_preds / sum(insectafinalpresmod_preds, na.rm=T)
background_insecta$preds_log = log(insectafinalpresmod_preds)

brks <- unique(quantile(background_insecta$preds_scaled, probs = seq(0, 1, 0.01), type=7, na.rm=T))
background_insecta$preds_perc <- as.numeric(cut(background_insecta$preds_scaled, breaks = brks, labels = 1:c(length(brks)-1), include.lowest = TRUE))

### Maps export.

##figures
# tiff("run7_insectamap_scaled.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_insecta, aes(col=preds_scaled), cex=0.5, alpha=1)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()
# 
# tiff("run7_insectamap_log.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_insecta, aes(col=preds_log), cex=0.5, alpha=1)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", midpoint=-15,high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()
# 
# tiff("run7_insectamap_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_insecta, aes(col=preds_perc), cex=0.5, alpha=0.8)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()


##raster
background_exportinsecta = background_insecta[,"preds_perc"]
st_write(background_exportinsecta, here("PredictionsinsectaPerc.shp"), delete_dsn=T) 


background_pointsinsecta <- as(background_exportinsecta, "Spatial")
insecta_raster = rasterFromXYZ(background_pointsinsecta, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
names(insecta_raster) = "insecta"
crs(insecta_raster) = CRS(SRS_string = "EPSG:4326") #3857

raster::writeRaster(insecta_raster,filename = "run7_insectapercraster", format = "GTiff")


insecta_raster_df = as(insecta_raster, "SpatialPixelsDataFrame")
insecta_raster_df <- as.data.frame(insecta_raster_df)

tiff("run7_rasterinsectamap_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
ggplot()+ 
  geom_tile(data = insecta_raster_df , aes(x = x, y = y, fill = insecta)) +
  scale_fill_gradient2(name="",low = "blue", mid="green", midpoint = 50, high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
dev.off() 

#kml
palette <- rev(RColorBrewer::brewer.pal(10, "Spectral"))
plotKML::kml(insecta_raster, colour = insecta, colour_scale = palette, alpha = 1, file.name = "insecta_perc_raster.kml")






#Insecta bias corrected maps
### bias corrected map
#predictions
biaspresmod_preds3 = predictBsDM(data=st_drop_geometry(background_1m[,names(finalpresmod)]), presmod=finalpresmod, backmod=biasbackmodel3)



background_bias3 = background_1m[,names(finalpresmod)]
background_bias3$preds = biaspresmod_preds3
background_bias3$preds_scaled = biaspresmod_preds3 / sum(biaspresmod_preds3, na.rm=T)
background_bias3$preds_log = log(biaspresmod_preds3)

brks <- unique(quantile(background_bias3$preds_scaled, probs = seq(0, 1, 0.01), type=7, na.rm=T))
background_bias3$preds_perc <- as.numeric(cut(background_bias3$preds_scaled, breaks = brks, labels = 1:c(length(brks)-1), include.lowest = TRUE))

### Maps export.

##figures
# tiff("run7_biasinsecta_map_scaled.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_bias3, aes(col=preds_scaled), cex=0.5, alpha=1)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()
# 
# tiff("run7_biasinsecta_map_log.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_bias3, aes(col=preds_log), cex=0.5, alpha=1)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", midpoint=-15,high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()
# 
# tiff("run7_biasinsecta_map_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
# ggplot()+ 
#   geom_sf(data=background_bias3, aes(col=preds_perc), cex=0.5, alpha=0.8)+ #  geom_sf(data=datatest_sf,aes(col=mb3prob4), lwd = 0)+
#   scale_colour_gradient2(name="",low = "blue", mid="green", high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
# dev.off()

##raster
background_exportbias3 = background_bias3[,"preds_perc"]
#st_write(bias_export, here("Data_Intermediate/PredictionsPerc.shp"), delete_dsn=T) #named number 2 for safe testing


background_pointsbias3 <- as(background_exportbias3, "Spatial")
background_rasterbias3 = rasterFromXYZ(background_pointsbias3, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
names(background_rasterbias3) = "bias"
crs(background_rasterbias3) = CRS(SRS_string = "EPSG:4326") #3857
raster::writeRaster(background_rasterbias3,filename = "run7_biasinsecta_percraster", format = "GTiff")


insecta2_raster_df = as(background_rasterbias3, "SpatialPixelsDataFrame")
insecta2_raster_df <- as.data.frame(insecta2_raster_df)


tiff("run7_rasterbiasginsecta_map_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
ggplot()+ 
  geom_tile(data = insecta2_raster_df , aes(x = x, y = y, fill = bias)) +
  scale_fill_gradient2(name="",low = "blue", mid="green", midpoint = 50, high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
dev.off() 

#kml
#names(background_raster3) = "bias"
palette <- rev(RColorBrewer::brewer.pal(10, "Spectral"))
plotKML::kml(background_rasterbias3, colour = bias, colour_scale = palette, alpha = 1, file.name = "biasinsecta_perc_raster.kml")


###Difference map
##### Percentiles
background_diff2 = background_1m[,names(finalpresmod)]
background_diff2$preds = biaspresmod_preds3 - finalpresmod_preds
#background_diff$preds_scaled = background_diff$preds / sum(background_diff$preds, na.rm=T)

brks <- unique(quantile(background_diff2$preds, probs = seq(0, 1, 0.01), type=7, na.rm=T))
background_diff2$preds_perc <- as.numeric(cut(background_diff2$preds, breaks = brks, labels = 1:c(length(brks)-1), include.lowest = TRUE))

background_export5 = background_diff2[,"preds_perc"]

background_points5 <- as(background_export5, "Spatial")
background_raster5 = rasterFromXYZ(background_points5, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
#crs(background_raster3) = CRS(SRS_string = "EPSG:4326") #3857
raster::writeRaster(background_raster5,filename = "run7_diff2percraster", format = "GTiff")

diff_raster_df2 = as(background_raster5, "SpatialPixelsDataFrame")
diff_raster_df2 <- as.data.frame(diff_raster_df2)
names(diff_raster_df2)[1] = "diff"

tiff("run7_rasterdiffmap3_percentile.tiff", width = 5, height = 4, units = 'in', res = 600)
ggplot()+ 
  geom_tile(data = diff_raster_df2 , aes(x = x, y = y, fill = diff)) +
  scale_fill_gradient2(name="",low = "blue", mid="green", midpoint = 50, high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+
dev.off()


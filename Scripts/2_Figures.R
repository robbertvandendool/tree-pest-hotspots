###### Figures script for the paper 'Introduction hotspots of non-native tree pests and the role of cities' by
###### Robbert T. van den Dool, Alejandro Morales, Wopke van der Werf & J.C. (Bob) Douma
###### Last edited 17 July 2023 - Robbert T. van den Dool

# Used to generate most of the figures shown in the manuscript. 
# run 1_Analysis.R first. 



packages("cowplot", "ggplotify", "grid", "gridExtra", prompt=FALSE)
packages("viridis")

#Namesconvert function 
#varnames 
fullvarnames = c("Tree cover",
                 "Tree cover 1km",
                 "Mean temp.",
                 "Diurnal range",
                 "Isothermality",
                 "Temp seasonality",
                 "Max temp. warm M", 
                 "Min temp. cold M",
                 "Temp. annual range",
                 "Mean temp. wettest Q",
                 "Mean temp. driest Q",
                 "Mean temp. warmest Q",
                 "Mean temp. coldest Q",
                 "Annual prec",
                 "Prec. wettest M",
                 "Prec. driest M",
                 "Prec. seasonality",
                 "Prec. wettest Q",
                 "Prec. driest Q",
                 "Prec. warmest Q",
                 "Prec. coldest Q",
                 "Population density",
                 "All crops density",
                 "Permanent crops density",
                 "Tree nurseries density",
                 "Ornamental plants density", 
                 "Gross domestic product",
                 "GDP per capita",
                 "Maritime freight",
                 "Airports freight",
                 "Nr.City distance",
                 "Nr.Port freight",
                 "Nr.Port distance",
                 "Nr.Airport distance",
                 "Tree Species Richness",
                 "Tourism",
                 "W. product n. companies",
                 "W. product n. employed",
                 "Nr.City distance LOG",
                 "Random variable")


namestable = data.frame(varnames = names(st_drop_geometry(reports_base)), fullnames = fullvarnames)

#convert names
nameconvertfunct = function(name){
  newname = namestable[which(namestable[,1]==name),2]
  return(newname)
}

fullvarnames2 = c("Tree cover",
                  "*Tree cover",
                  "Mean temperature",
                  "Diurnal range",
                  "Isothermality",
                  "Temp. seasonality",
                  "Max temp. warm M", 
                  "Min temp. cold M",
                  "Temp. annual range",
                  "Mean temp. wettest Q",
                  "Mean temp. driest Q",
                  "*Mean temp. warmest Q",
                  "*Mean temp. coldest Q",
                  "Annual precipitation",
                  "Prec. wettest M",
                  "Prec. driest M",
                  "*Prec. seasonality",
                  "Prec. wettest Q",
                  "Prec. driest Q",
                  "Prec. warmest Q",
                  "Prec. coldest Q",
                  "*Population density",
                  "*Crop cover",
                  "*Permanent crop cover",
                  "*Tree nurseries cover",
                  "*Ornamental plants cover", 
                  "*Gross domestic product",
                  "*GDP per capita",
                  "*Maritime freight",
                  "*Air freight",
                  "*Distance to n. city",
                  "Freight unloaded at n. port",
                  "*Distance to n. port",
                  "Distance to n. airport",
                  "*Tree Species Richness",
                  "*Tourism",
                  "*Wood product companies",
                  "Wood product employees",
                  "Nr.City distance LOG",
                  "Random variable")


namestable2 = data.frame(varnames = names(st_drop_geometry(reports_base)), fullnames = fullvarnames2)

nameconvertfunct2 = function(name){
  newname = namestable2[which(namestable2[,1]==name),2]
  return(newname)
}

#Without asterisk
fullvarnames3 = c("Tree cover",
                  "Tree cover",
                  "Mean temperature",
                  "Diurnal range",
                  "Isothermality",
                  "Temp. seasonality",
                  "Max temp. warm M", 
                  "Min temp. cold M",
                  "Temp. annual range",
                  "Mean temp. wettest Q",
                  "Mean temp. driest Q",
                  "Mean temp. warmest Q",
                  "Mean temp. coldest Q",
                  "Annual precipitation",
                  "Prec. wettest M",
                  "Prec. driest M",
                  "Prec. seasonality",
                  "Prec. wettest Q",
                  "Prec. driest Q",
                  "Prec. warmest Q",
                  "Prec. coldest Q",
                  "Population density",
                  "Crop cover",
                  "Permanent crop cover",
                  "Tree nurseries cover",
                  "Ornamental plants cover", 
                  "Gross domestic product",
                  "GDP per capita",
                  "Maritime freight",
                  "Air freight",
                  "Distance to n. city",
                  "Freight unloaded at n. port",
                  "Distance to n. port",
                  "Distance to n. airport",
                  "Tree Species Richness",
                  "Tourism",
                  "Wood product companies",
                  "Wood product employees",
                  "Nr.City distance LOG",
                  "Random variable")


namestable3 = data.frame(varnames = names(st_drop_geometry(reports_base)), fullnames = fullvarnames3)

nameconvertfunct3 = function(name){
  newname = namestable3[which(namestable3[,1]==name),2]
  return(newname)
}

#function for units
unitnames = c("% Tree cover",
                  expression(paste("%")),
                  expression(degree*C),
                  expression(degree*C),
                  expression(paste("%")),
                  expression(paste(degree*C," * ", 10^-2)),
                  expression(degree*C), 
                  expression(degree*C),
                  expression(degree*C),
                  expression(degree*C),
                  expression(degree*C),
                  expression(degree*C),
                  expression(degree*C),
                  expression(paste("mm")),
                  expression(paste("mm")),
                  expression(paste("mm")),
                  expression(paste("%")),
                  expression(paste("mm")),
                  expression(paste("mm")),
                  expression(paste("mm")),
                  expression(paste("mm")),
                  expression(paste("ln(Inhabitants per ", km^2, " + 1)")),
                  expression(paste("%")),
                  expression(paste("ln(Hectares per ", km^2, " * 10000 + 1)")),
                  expression(paste("ln(Hectares per ", km^2, " * 10000 + 1)")),
                  expression(paste("ln(Hectares per ", km^2, " * 10000 + 1)")), 
                  expression(paste("ln(Million euro per ", km^2, " + 1)")),
                  expression(paste("ln(Million euro per capita per ", km^2, " + 1)")),
                  expression(paste("ln(Thousand tonnes ", "* 10000 + 1)")),
                  expression(paste("ln(Thousand tonnes ", "* 10000 + 1)")),
                  expression(paste("km")),
                  expression(paste("ln(Thousand tonnes ", " + 1)")),
                  expression(paste("km")),
                  expression(paste("km")),
                  expression(paste("Number of tree species")),
                  expression(paste("ln(Foreign nightly stays ", year^-1, " ", km^2, " + 1)")), #expression(Foreign ~ nightly ~ stays ~ year^-1 ~ km^2),
                  expression(paste("ln(Companies per ", km^2, " * 1000 + 1)")), 
                  expression(paste("ln(Employees per ", km^2, " * 1000 + 1)")),
                  "Nr.City distance LOG",
                  "Random variable")


unittable = list(varnames = names(st_drop_geometry(reports_base)), units = unitnames)

unitconvertfunct = function(name){
  unitname = unittable[[2]][which(unittable[[1]]==name)]
  return(unitname)
}




####
#### Figure 1
####

### Map of geographic point data 
europe <- ne_countries(continent='europe', scale = "medium", returnclass = "sf")
europe_eu <- st_transform(europe, crs=3035)


NUTS2_2016 <- readOGR("Intermediate/NUTS2_2016_v3.shp")
NUTS2_2016sf <- st_as_sf(NUTS2_2016)
NUTS2_2016sf <- st_transform(NUTS2_2016sf, crs=3035)

samegenera_both_PA_sf = readRDS(file="Intermediate/SG_both.rds")


tiff("Output/Figure1.tiff", width = 5, height = 4, units = 'in', res = 900, compression="lzw")
ggplot()+
  geom_sf(data=europe_eu)+
  theme_minimal()+
  geom_sf(data=NUTS2_2016sf, fill="white")+
  geom_sf(data=samegenera_both_PA_sf, colour=brewer.pal(9,"Set1")[1], size=0.2, alpha = 0.2)+
  geom_sf(data=reports_base, colour=brewer.pal(9,"Set1")[2], size=0.6)+
  coord_sf(xlim=c(2426378.0132,6093974.6215), ylim=c(1318101.2618,5446513.5222), expand=F) #xlim=c(2426378.0132,6293974.6215), ylim=c(1528101.2618,5446513.5222)
dev.off()



####
#### Figure 2
####

#plot1
allresults_fig2 = allresults
#allresults_fig2[!rownames(allresults_fig2) %in% c("frstcvr", "NrCty_DLOG"),]

plotdataframe = as.data.frame(allresults_fig2[!rownames(allresults_fig2) %in% c("frstcvr", "NrCty_DLOG"),])

newrownames = unlist(sapply(rownames(plotdataframe), nameconvertfunct2))
names(newrownames) = NULL
rownames(plotdataframe)[-which(rownames(plotdataframe) %in% c("All variables", "Subset"))] = newrownames


plotdataframe$index = 1:nrow(plotdataframe)
plotdataframe$title = row.names(plotdataframe)

plotdataframe = plotdataframe[order(plotdataframe$AUC_mean, decreasing=T),]
plotdataframe$index = 1:nrow(plotdataframe)

plotdataframe$title = factor(plotdataframe$title, levels = plotdataframe$title)

plot1 <- ggplot(plotdataframe, aes(y = title, x = AUC_mean, color=AUC_mean, height=25)) +
  geom_point(shape = 16, size = 1.5) +  
  geom_errorbarh(aes(xmin = AUC_lower, xmax = AUC_upper), height = 0.5) +
  scale_y_discrete(limits = rev) +
  scale_color_viridis(discrete=F, option="viridis", begin=0, end=1, direction=-1)+
  #scale_color_distiller(palette = "Spectral", direction = - 1, limits = c(0.5,1)) +  #Spectral
  #scale_color_gradient2(name="Percentile",low = brewer.pal(9,"Set1")[2], mid=brewer.pal(9,"Set1")[3], midpoint=0.7,high = brewer.pal(9,"Set1")[1]) +
  xlab("AUC") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8, colour = "black"), #11
        axis.text.x.bottom = element_text(size = 9, colour = "black"), #10
        axis.title.x = element_text(size = 9, colour = "black"), #12
        legend.position = "none") #,plot.margin = unit(c(0,1,1,1), "cm")


#plot2
varnames = subvarnames
presdata = reports[,subvarnames]
backdata = background[,subvarnames]
pmodel = subfinalpresmod
bmodel = subukde_back
repeats = 50
prefix = "run7_sub1_4Dec2022"
origAUC = subvarmodels[[2]][5] #was 1
origNLLp = 1

#Calculate VarImp
VarImp = future_map(varnames,.options = furrr::furrr_options(seed = TRUE),function(x) permutevarFinal(presdata = presdata, backdata = backdata, pmodel = pmodel, bmodel = bmodel, variable = x,repeats = repeats))
names(VarImp) = lapply(names(bmodel), nameconvertfunct3)

#AUC VPI barplot
VarImp_AUC = lapply(VarImp, function(x) myCI(origAUC - x$AUC)) #how much is AUC higher with the variable not shuffled?
VarImp_AUC_min = min(sapply(VarImp_AUC, function(x)x[2]),na.rm = T)
VarImp_AUC_max = max(sapply(VarImp_AUC, function(x)x[2]),na.rm = T)
VarImp_AUC_relative = lapply(VarImp_AUC , function(y) (y - VarImp_AUC_min)/ (VarImp_AUC_max-VarImp_AUC_min))
VarImp_AUC_relativemean = sapply(VarImp_AUC_relative, function(x)x[2])
names(VarImp_AUC_relativemean) = names(VarImp)
VarImp_AUC_relativesort = sort(VarImp_AUC_relativemean)


upper =   sapply(VarImp_AUC_relative, function(x)x[1])
upper_sort = upper[order(VarImp_AUC_relativemean)]

lower =   sapply(VarImp_AUC_relative, function(x)x[3])
lower_sort = lower[order(VarImp_AUC_relativemean)]


name = paste(prefix,"_VPI_AUC",".tiff",sep="")


VarImp_ggplotdf = data.frame(Variable=names(VarImp_AUC_relativesort), value=as.numeric(VarImp_AUC_relativesort), order=1:length(VarImp_AUC_relativesort))

VarImp_ggplotdf$Variable = factor(VarImp_ggplotdf$Variable, levels = VarImp_ggplotdf$Variable)


plot2 = ggplot(VarImp_ggplotdf, aes(x=value, y=variable, fill=value)) +
  geom_col(aes(value, Variable)) + 
  geom_errorbar(aes(y=Variable, xmin=lower_sort, xmax=upper_sort), width=.5,
                position=position_dodge(.9)) + 
  scale_fill_viridis(discrete=F, option="viridis", begin=0, end=1, direction=-1)+
  #scale_fill_distiller(palette = "Spectral", direction = - 1, limits = c(0,1)) +  #Spectral
  #scale_fill_brewer(palette = "Spectral", direction = - 1) +
  #scale_fill_gradient2(name="Percentile",low = brewer.pal(9,"Set1")[2], mid=brewer.pal(9,"Set1")[3], midpoint=0.5,high = brewer.pal(9,"Set1")[1]) + 
  scale_x_continuous(n.breaks=3) +
  ylab("") + 
  theme_bw() +
  xlab("Relative Importance") +
  theme(legend.position = "none", 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x.bottom = element_text(size = 9, colour = "black"), #10
        axis.title.x = element_text(size = 9, colour = "black"), #12
        axis.text.y = element_text(size = 9, colour = "black")) #11


tiff("Output/Figure2.tiff", width = 170, height = 130, units = 'mm', res = 600, compression="lzw") # 11/7 iinch
plot_grid(plot1, plot2, labels = c('(a)', '(b)'), nrow=1, label_size = 12, align = "hv", axis = "b", rel_widths = c(1, 1.3)) 
dev.off()


#AUC VPI barplot
VarImp_AUC = lapply(VarImp, function(x) myCI(origAUC - x$AUC)) #how much is AUC higher with the variable not shuffled?
# VarImp_AUC_min = min(sapply(VarImp_AUC, function(x)x[2]),na.rm = T)
# VarImp_AUC_max = max(sapply(VarImp_AUC, function(x)x[2]),na.rm = T)
# VarImp_AUC_relative = lapply(VarImp_AUC , function(y) (y - VarImp_AUC_min)/ (VarImp_AUC_max-VarImp_AUC_min))
# VarImp_AUC_relativesort = sapply(VarImp_AUC_relative, function(x)x[2])
# names(VarImp_AUC_relativesort) = names(VarImp)
VarImp_AUC_mean = sapply(VarImp_AUC, function(x)x[2])
names(VarImp_AUC_mean) = names(VarImp_AUC)
VarImp_AUC_sort = sort(VarImp_AUC_mean)

upper =   sapply(VarImp_AUC, function(x)x[1])
upper_sort = upper[order(VarImp_AUC_mean)]

lower =   sapply(VarImp_AUC, function(x)x[3])
lower_sort = lower[order(VarImp_AUC_mean)]

name = paste(prefix,"_VPI_AUC",".tiff",sep="")

VarImp_ggplotdf = data.frame(Variable=names(VarImp_AUC_sort), value=as.numeric(VarImp_AUC_sort), order=1:length(VarImp_AUC_sort))

VarImp_ggplotdf$Variable = factor(VarImp_ggplotdf$Variable, levels = VarImp_ggplotdf$Variable)

plot3 = ggplot(VarImp_ggplotdf, aes(x=value, y=variable, fill=value)) +
  geom_col(aes(value, Variable)) + 
  geom_errorbar(aes(y=Variable, xmin=lower_sort, xmax=upper_sort), width=.5,
                position=position_dodge(.9)) + 
  #scale_fill_gradient2(name="Percentile",low = brewer.pal(9,"Set1")[2], mid=brewer.pal(9,"Set1")[3], midpoint=0,high = brewer.pal(9,"Set1")[1]) + 
  scale_fill_viridis(discrete=F, option="viridis", begin=0, end=1, direction=-1)+
  scale_x_continuous(n.breaks=3) +
  ylab("") + 
  theme_bw() +
  xlab("Absolute AUC Importance") +
  theme(legend.position = "none", 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x.bottom = element_text(size = 10, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 11, colour = "black"))


tiff("Output/Figure2b.tiff", width = 11, height = 7, units = 'in', res = 1500, compression="lzw")
plot3
dev.off()



####
#### Figure 3
####

Ch3_densplotgenerator = function(variable, legend=F, customxlim=NA, density=F){
  
  variablemin = mins[variable]
  variablemax = maxs[variable]
  xrange = seq(variablemin, variablemax, length.out=512)
  
  ymax = 1*max(c(finalpresmod[[variable]](xrange),
                 finalbackmod[[variable]](xrange),
                 finalbackmod2[[variable]](xrange),
                 finalbackmod3[[variable]](xrange),
                 finalbackmod4[[variable]](xrange)
  ))
  
  

  xlab = unitconvertfunct(variable)
  xlim = c(variablemin, variablemax)
  
  if(legend==T){
    ymax = 1*ymax #1.5
  }
  
  if(!is.na(customxlim)[1]){
    xlim = customxlim
  }
  
  ylab=""
  
  if(density==T){
    ylab="Density"
  }
  

plotf = function(){
  curve(finalpresmod[[variable]](x), xlim= xlim, ylim=c(0,ymax),ylab=ylab, xlab="", axes=F,frame.plot=TRUE, col=brewer.pal(9,"Set1")[3], lwd=2, lty=2, cex=0.4)
  Axis(side=1, labels=T, line=0, padj=-1.7, cex.axis=0.7, cex=0.7)
  mtext(side=1, text=xlab, line=1.1, cex=0.7)
  curve(finalbackmod[[variable]](x), col="black", lwd=2, lty=1, add=T)
  curve(finalbackmod2[[variable]](x), col=brewer.pal(9,"Set1")[1], lwd=2, lty=3,add=T) #same genera
  curve(finalbackmod3[[variable]](x), col=brewer.pal(9,"Set1")[4], lwd=2, lty=3, add=T) #lepidoptera
  curve(finalbackmod4[[variable]](x), col=brewer.pal(9,"Set1")[5], lwd=2, lty=3, add=T) #insecta

  
  if(legend==T){
    legend(x=65, y=0.055, bty="n", cex=0.6,y.intersp=2.5, legend=c("Establishments", "Background", "Same genera", "Lepidoptera", "Insecta"), col= c(brewer.pal(9,"Set1")[3], "black", brewer.pal(9,"Set1")[1],brewer.pal(9,"Set1")[4], brewer.pal(9,"Set1")[5]), lty=c(2,1,3,3,3), lwd=2) # 
  } #"top"
  #border = "", bg= "",
} 
return(plotf)
}


plot1 = Ch3_densplotgenerator("NrCty_D", legend=T, customxlim=c(0,100))
plot1 = as.grob(plot1) 

plot2 = Ch3_densplotgenerator("BIO11")
plot2 = as.grob(plot2)

plot3 = Ch3_densplotgenerator("WoodUnits")
plot3 = as.grob(plot3)

plot4 = Ch3_densplotgenerator("BIO10")
plot4 = as.grob(plot4)

plot5 = Ch3_densplotgenerator("POPD")
plot5 = as.grob(plot5)

plot6 = Ch3_densplotgenerator("Tourism")
plot6 = as.grob(plot6)

plot7 = Ch3_densplotgenerator("frstcv1")
plot7 = as.grob(plot7)


tiff("Output/Figure3.tiff", width = 170, height = 240, units = 'mm', res = 900, compression="lzw") #14/8/'in'/900/"lzw"
plot = plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, plot7, 
          nrow=4, label_size = 8, axis='l', hjust=0, vjust=2.7, label_x = 0.1, #align = "v", #axis = "b", #rel_widths = c(1), #label_x = .3, scale=0.9
          labels = c(paste('(a)', nameconvertfunct3("NrCty_D")),
                     paste('(b)', nameconvertfunct3("BIO11")),
                     paste('(c)', nameconvertfunct3("WoodUnits")),
                     paste('(d)', nameconvertfunct3("BIO10")),
                     paste('(e)', nameconvertfunct3("POPD")),
                     paste('(f)', nameconvertfunct3("Tourism")),
                     paste('(g)', nameconvertfunct3("frstcv1"))
          )) + theme(plot.margin = unit(c(10,10,10,10), "points"))#+ theme(axis.text.x.bottom = element_text(vjust = 1)) #+ theme(plot.margin = unit(c(70,70,70,70), "points"))  #axis.text.x = element_text(vjust = -2)

grid.arrange(arrangeGrob(plot, left = "Density"))
dev.off()



###
### Figure 4
### 

#Base map
finalpresmod_preds = predictBsDM(data=st_drop_geometry(background_1m[,names(finalpresmod)]), presmod=finalpresmod, backmod=finalbackmod)

background_base2 = background_1m[,names(finalpresmod)]
background_base2$preds = finalpresmod_preds
background_base2$preds_scaled = finalpresmod_preds / sum(finalpresmod_preds, na.rm=T)
finalpresmod_preds_scaled = background_base2$preds_scaled

brks <- unique(quantile(background_base2$preds_scaled, probs = seq(0, 1, 0.01), type=7, na.rm=T))
background_base2$preds_perc <- as.numeric(cut(background_base2$preds_scaled, breaks = brks, labels = 1:c(length(brks)-1), include.lowest = TRUE))

background_points <- as(background_base2[,"preds_perc"], "Spatial")
background_raster = rasterFromXYZ(background_points, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
crs(background_raster) = CRS(SRS_string = "EPSG:4326") #3857

background_raster_df = as(background_raster, "SpatialPixelsDataFrame")
background_raster_df <- as.data.frame(background_raster_df)

 
plot0 = ggplot()+ #plot0 just for the legend
  geom_tile(data = background_raster_df , aes(x = x, y = y, fill = layer)) +
  theme_minimal()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size=6),
        legend.text = element_text(size=6),
        plot.margin=unit(c(0,0.1,0,0.1), "cm"), legend.margin=margin(t = 0, unit='cm'), legend.key.height = unit(1.2, 'cm'))+
  scale_fill_viridis(name="Percentile", discrete=F, option="viridis", begin=0, end=1, direction=-1)
  
  #scale_fill_gradient2(name="Percentile",low = brewer.pal(9,"Set1")[2], mid=brewer.pal(9,"Set1")[3], midpoint = 50, high = brewer.pal(9,"Set1")[1]) #scale_colour_gradient2(name="Percentile",low = brewer.pal(9,"Set1")[2], mid=brewer.pal(9,"Set1")[3], midpoint=50,high = brewer.pal(9,"Set1")[1])+

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(plot0)

plot1 = ggplot()+ 
  geom_tile(data = background_raster_df , aes(x = x, y = y, fill = layer)) +
  theme_minimal()+
  theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size=7), legend.position="none", plot.margin=unit(c(0,0,0,0), "cm"))+
  scale_fill_viridis(name="", discrete=F, option="viridis", begin=0, end=1, direction=-1)
  #scale_fill_gradient2(name="",low = brewer.pal(9,"Set1")[2], mid=brewer.pal(9,"Set1")[3], midpoint = 50, high = brewer.pal(9,"Set1")[1]) #scale_colour_gradient2(name="Percentile",low = brewer.pal(9,"Set1")[2], mid=brewer.pal(9,"Set1")[3], midpoint=50,high = brewer.pal(9,"Set1")[1])+

#bias corrected map
finalpresmod_preds_bias = predictBsDM(data=st_drop_geometry(background_1m[,names(finalpresmod)]), presmod=finalpresmod, backmod=finalbackmod2)

background_base2 = background_1m[,names(finalpresmod)]
background_base2$preds = finalpresmod_preds_bias
background_base2$preds_scaled = finalpresmod_preds_bias / sum(finalpresmod_preds_bias, na.rm=T)
finalpresmod_preds_bias_scaled = background_base2$preds_scaled

brks <- unique(quantile(background_base2$preds_scaled, probs = seq(0, 1, 0.01), type=7, na.rm=T))
background_base2$preds_perc <- as.numeric(cut(background_base2$preds_scaled, breaks = brks, labels = 1:c(length(brks)-1), include.lowest = TRUE))

background_points <- as(background_base2[,"preds_perc"], "Spatial")
background_raster = rasterFromXYZ(background_points, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
crs(background_raster) = CRS(SRS_string = "EPSG:4326") #3857

background_raster_df2 = as(background_raster, "SpatialPixelsDataFrame")
background_raster_df2 <- as.data.frame(background_raster_df2)

plot2 = ggplot()+ 
  geom_tile(data = background_raster_df2 , aes(x = x, y = y, fill = layer)) +
  theme_minimal()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size=7), legend.position="none", axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin=unit(c(0,0,0,0), "cm"))+
  scale_fill_viridis(name="", discrete=F, option="viridis", begin=0, end=1, direction=-1)
  #scale_fill_gradient2(name="",low = brewer.pal(9,"Set1")[2], mid=brewer.pal(9,"Set1")[3], midpoint = 50, high = brewer.pal(9,"Set1")[1]) #scale_colour_gradient2(name="Percentile",low = brewer.pal(9,"Set1")[2], mid=brewer.pal(9,"Set1")[3], midpoint=50,high = brewer.pal(9,"Set1")[1])+

#difference map
background_diff = background_1m[,names(finalpresmod)]
background_diff$preds = finalpresmod_preds_bias - finalpresmod_preds
#background_diff$preds_scaled = background_diff$preds / sum(background_diff$preds, na.rm=T)

brks <- unique(quantile(background_diff$preds, probs = seq(0, 1, 0.01), type=7, na.rm=T))
background_diff$preds_perc <- as.numeric(cut(background_diff$preds, breaks = brks, labels = 1:c(length(brks)-1), include.lowest = TRUE))


background_export4 = background_diff[,"preds_perc"]
background_points4 <- as(background_export4, "Spatial")
background_raster4 = rasterFromXYZ(background_points4, res=c(NA,NA), crs="EPSG:3035", digits=10) #21km2
crs(background_raster4) = CRS(SRS_string = "EPSG:4326") #3857

diff_raster_df = as(background_raster4, "SpatialPixelsDataFrame")
diff_raster_df <- as.data.frame(diff_raster_df)
names(diff_raster_df)[1] = "diff"

plot3 = ggplot()+
  geom_tile(data = diff_raster_df , aes(x = x, y = y, fill = diff)) +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size=8), legend.position="none", plot.margin=unit(c(0,0,0,0), "cm"))+
  scale_fill_viridis(name="", discrete=F, option="viridis", begin=0, end=1, direction=-1)
  #scale_fill_gradient2(name="",low = brewer.pal(9,"Set1")[2], mid=brewer.pal(9,"Set1")[3], midpoint = 50, high = brewer.pal(9,"Set1")[1])  #22th percentile still has negative values.

plot4 = ggplot()+
  geom_tile(data = diff_raster_df , aes(x = x, y = y, fill = diff)) +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size=6),
        legend.text = element_text(size=6),
        text = element_text(size=8), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin=unit(c(0,0,0,0), "cm"))+ #legend.position="none",
  scale_fill_viridis(name="Rel. prob.", discrete=F, option="viridis", begin=0, end=1, direction=-1, breaks=c(0,seq(10,100, by=10)), labels=formatC(brks[c(1, 11,21,31,41,51,61,71,81,91,101)], format = "e", digits = 2))
  #scale_fill_gradient2(name="Rel. Prob.",breaks=c(0,seq(10,100, by=10)), low=brewer.pal(9,"Set1")[2], mid=brewer.pal(9,"Set1")[3], high=brewer.pal(9,"Set1")[1], midpoint = 50, labels=formatC(brks[c(1, 11,21,31,41,51,61,71,81,91,101)], format = "e", digits = 2)  )  #22th percentile still has negative values.

mylegend2<-g_legend(plot4)

#fix widths
plots <- list(plot1, plot2, plot3)
grobs <- list()
widths <- list()

for (i in 1:length(plots)){
  grobs[[i]] <- ggplotGrob(plots[[i]])
  widths[[i]] <- grobs[[i]]$widths[2:5]
}

maxwidth <- do.call(grid::unit.pmax, widths)

for (i in 1:length(grobs)){
  grobs[[i]]$widths[2:5] <- as.list(maxwidth)
}

tiff("Output/Figure4.tiff", width = 210, height = 100, units = 'mm', compression = "lzw", res=900) #width = 12, height = 4,  # res = 800
plot_grid(grobs[[1]], mylegend, grobs[[2]], mylegend, align = "h", nrow = 1, axis="b", 
          rel_widths = c(9/20,1/20,9/20,1/20), rel_heights = c(1/4,1/4,1/4,1/4), label_size = 12,
          labels=c("(a) no bias-correction","", "(b) with bias-correction"),hjust=0, label_x = 0.025)
dev.off()





##### Figure 5 
tiff("Output/Figure5.tiff", width = 112, height = 100, units = 'mm', compression = "lzw", res=900) #width = 12, height = 4,  # res = 800
plot_grid(grobs[[3]], mylegend2, align = "hv", nrow = 1, axis="b", 
          rel_widths = c(0.85,0.15), rel_heights = c(1/4,1/4), label_size = 12,
          labels="")#,hjust=0, label_x = 0.025)
dev.off()




####
#### Figure 6 
####
biasbackmodelVPI = finalbackmod2
biasbackmodelVPI$runif = subukde_back$runif

biasbackmodelVPI2 = finalbackmod3
biasbackmodelVPI2$runif = subukde_back$runif

biasbackmodelVPI3 = finalbackmod4
biasbackmodelVPI3$runif = subukde_back$runif

varnames = subvarnames
presdata = reports[,subvarnames]
backdata = background[,subvarnames]
pmodel = subfinalpresmod
bmodel = subukde_back #no correction
bmodel2 = biasbackmodelVPI #same genera
bmodel3 = biasbackmodelVPI2 #Lepidoptera
bmodel4 = biasbackmodelVPI3 #Insecta

repeats = 50
prefix = "run4_BiasGenera"
origAUC = subvarmodels[[2]][["AUC_mean"]]
origNLLp = subvarmodels[[2]][["NLLp_mean"]]

#Calculate VarImp
set.seed(1)
VarImp = future_map(varnames,.options = furrr::furrr_options(seed = TRUE),function(x) permutevarFinal(presdata = presdata, backdata = backdata, pmodel = pmodel, bmodel = bmodel, variable = x,repeats = repeats))
names(VarImp) = lapply(varnames, nameconvertfunct3)

set.seed(1)
VarImp2 = future_map(varnames,.options = furrr::furrr_options(seed = TRUE),function(x) permutevarFinal(presdata = presdata, backdata = backdata, pmodel = pmodel, bmodel = bmodel2, variable = x,repeats = repeats))
names(VarImp2) = lapply(varnames, nameconvertfunct3)

set.seed(1)
VarImp3 = future_map(varnames,.options = furrr::furrr_options(seed = TRUE),function(x) permutevarFinal(presdata = presdata, backdata = backdata, pmodel = pmodel, bmodel = bmodel3, variable = x,repeats = repeats))
names(VarImp3) = lapply(varnames, nameconvertfunct3)

set.seed(1)
VarImp4 = future_map(varnames,.options = furrr::furrr_options(seed = TRUE),function(x) permutevarFinal(presdata = presdata, backdata = backdata, pmodel = pmodel, bmodel = bmodel4, variable = x,repeats = repeats))
names(VarImp4) = lapply(varnames, nameconvertfunct3)


#No correction
VarImp_AUC = lapply(VarImp, function(x) origAUC - x$AUC) #how much is AUC higher with the variable not shuffled?

VarImp_AUC_Trans = lapply(1:repeats, function(x) sapply(VarImp_AUC, function(y) y[x]))
VarImp_AUC_min = sapply(VarImp_AUC_Trans, min)      
VarImp_AUC_max = sapply(VarImp_AUC_Trans, max)  
VarImp_AUC_Trans_rel = lapply(VarImp_AUC, function(x) sapply(seq_along(x), function(y) (x[y] - VarImp_AUC_min[y])/ (VarImp_AUC_max[y]-VarImp_AUC_min[y]))) 

#Same genera
VarImp2_AUC = lapply(VarImp2, function(x) origAUC - x$AUC) #how much is AUC higher with the variable not shuffled?

VarImp2_AUC_Trans = lapply(1:repeats, function(x) sapply(VarImp2_AUC, function(y) y[x]))
VarImp2_AUC_min = sapply(VarImp2_AUC_Trans, min)      
VarImp2_AUC_max = sapply(VarImp2_AUC_Trans, max)  
VarImp2_AUC_Trans_rel = lapply(VarImp2_AUC, function(x) sapply(seq_along(x), function(y) (x[y] - VarImp2_AUC_min[y])/ (VarImp2_AUC_max[y]-VarImp2_AUC_min[y]))) 

#Lepidoptera
VarImp3_AUC = lapply(VarImp3, function(x) origAUC - x$AUC) #how much is AUC higher with the variable not shuffled?

VarImp3_AUC_Trans = lapply(1:repeats, function(x) sapply(VarImp3_AUC, function(y) y[x]))
VarImp3_AUC_min = sapply(VarImp3_AUC_Trans, min)
VarImp3_AUC_max = sapply(VarImp3_AUC_Trans, max)
VarImp3_AUC_Trans_rel = lapply(VarImp3_AUC, function(x) sapply(seq_along(x), function(y) (x[y] - VarImp3_AUC_min[y])/ (VarImp3_AUC_max[y]-VarImp3_AUC_min[y])))


#Insecta
VarImp4_AUC = lapply(VarImp4, function(x) origAUC - x$AUC) #how much is AUC higher with the variable not shuffled?

VarImp4_AUC_Trans = lapply(1:repeats, function(x) sapply(VarImp4_AUC, function(y) y[x]))
VarImp4_AUC_min = sapply(VarImp4_AUC_Trans, min)
VarImp4_AUC_max = sapply(VarImp4_AUC_Trans, max)
VarImp4_AUC_Trans_rel = lapply(VarImp4_AUC, function(x) sapply(seq_along(x), function(y) (x[y] - VarImp4_AUC_min[y])/ (VarImp4_AUC_max[y]-VarImp4_AUC_min[y])))




#get the difference in relative prob each var each value
#Same genera
VarImpDiff_AUC = lapply(names(VarImp2_AUC), function(x) sapply(seq_along(VarImp2_AUC[[x]]), function(y) (VarImp2_AUC_Trans_rel[[x]][y] - VarImp_AUC_Trans_rel[[x]][y] )))  #x=varnames, y=seed 
names(VarImpDiff_AUC) = names(VarImp)

VarImpDiff_AUC_CI = lapply(VarImpDiff_AUC, function(x) myCI(x))

VarImpDiff_AUC_relativesort = sapply(VarImpDiff_AUC_CI, function(x)x[2])
names(VarImpDiff_AUC_relativesort) = names(VarImpDiff_AUC_CI)

upper =   sapply(VarImpDiff_AUC_CI, function(x)x[1])
lower =   sapply(VarImpDiff_AUC_CI, function(x)x[3])
upper_sort = upper[order(VarImpDiff_AUC_relativesort)]
lower_sort = lower[order(VarImpDiff_AUC_relativesort)]

VarImpDiff_AUC_samegenera = sort(VarImpDiff_AUC_relativesort)

#Lepidoptera
VarImpDiff_AUC = lapply(names(VarImp3_AUC), function(x) sapply(seq_along(VarImp3_AUC[[x]]), function(y) (VarImp3_AUC_Trans_rel[[x]][y] - VarImp_AUC_Trans_rel[[x]][y] )))  #x=varnames, y=seed
names(VarImpDiff_AUC) = names(VarImp)

VarImpDiff_AUC_CI = lapply(VarImpDiff_AUC, function(x) myCI(x))

VarImpDiff_AUC_relativesort = sapply(VarImpDiff_AUC_CI, function(x)x[2])
names(VarImpDiff_AUC_relativesort) = names(VarImpDiff_AUC_CI)

upper =   sapply(VarImpDiff_AUC_CI, function(x)x[1])
lower =   sapply(VarImpDiff_AUC_CI, function(x)x[3])
upper_sort2 = upper[names(upper_sort)]
lower_sort2 = lower[names(lower_sort)]

VarImpDiff_AUC_Lepidoptera = VarImpDiff_AUC_relativesort[names(VarImpDiff_AUC_samegenera)]

#insecta
VarImpDiff_AUC = lapply(names(VarImp4_AUC), function(x) sapply(seq_along(VarImp4_AUC[[x]]), function(y) (VarImp4_AUC_Trans_rel[[x]][y] - VarImp_AUC_Trans_rel[[x]][y] )))  #x=varnames, y=seed
names(VarImpDiff_AUC) = names(VarImp)

VarImpDiff_AUC_CI = lapply(VarImpDiff_AUC, function(x) myCI(x))

VarImpDiff_AUC_relativesort = sapply(VarImpDiff_AUC_CI, function(x)x[2])
names(VarImpDiff_AUC_relativesort) = names(VarImpDiff_AUC_CI)

upper =   sapply(VarImpDiff_AUC_CI, function(x)x[1])
lower =   sapply(VarImpDiff_AUC_CI, function(x)x[3])
upper_sort3 = upper[names(upper_sort)]
lower_sort3 = lower[names(lower_sort)]

VarImpDiff_AUC_Insecta = VarImpDiff_AUC_relativesort[names(VarImpDiff_AUC_samegenera)]

#summary
plotvalues = rbind(VarImpDiff_AUC_samegenera, VarImpDiff_AUC_Lepidoptera, VarImpDiff_AUC_Insecta) #
plotarrows_upper = rbind(upper_sort, upper_sort2, upper_sort3) #
plotarrows_lower = rbind(lower_sort, lower_sort2, lower_sort3) # 

#plot
tiff(paste("Output/Figure6",".tiff",sep=""), width = 100, height = 100, units = 'mm', res = 900, compression='lzw')
par(mgp = c(2,1,0),mar=c(3,8,1.1,1.1))
testplot = barplot(plotvalues, beside=T, horiz=T, las=1, cex.names=0.7, cex.labels=0.7, axes=F, xlim=c(-0.15, 0.4),  main="", xlab= "", xpd=T, col=c(brewer.pal(9,"Set1")[1], brewer.pal(9,"Set1")[4], brewer.pal(9,"Set1")[5])) 
Axis(side=1, labels=T, line=0, padj=-1.7, cex.axis=0.7, cex=0.7)
mtext(side=1, text="Relative importance", line=1.1, cex=0.7)

arrows(plotarrows_upper,testplot, plotarrows_lower, testplot, angle=90, code=3, length=0.035)

legend(x=0.1,y=6, legend=c("Insecta","Lepidoptera", "Same genera"),
       col=c(brewer.pal(9,"Set1")[5], brewer.pal(9,"Set1")[4], brewer.pal(9,"Set1")[1]), lty=1,lwd=2, cex=0.7, box.lty=0)

dev.off()



####
### Appendix S4
####

background_temp = background
names(background_temp) = nameconvertfunct(names(background_temp))

tiff("Output/S4.tiff", width = 10, height = 10, units = 'in', res = 600, compression="lzw")
corplotfunct(background_temp)
dev.off()


####
### Appendix S5 and cdf values for in the discussion. 
####


#### Probmass full models
#all data
finalpresmod_preds = predictBsDM(data=st_drop_geometry(background_1m[,names(finalpresmod)]), presmod=finalpresmod, backmod=finalbackmod)
finalpresmod_preds_samegenera = predictBsDM(data=st_drop_geometry(background_1m[,names(finalpresmod)]), presmod=finalpresmod, backmod=finalbackmod2)
finalpresmod_preds_lepidoptera = predictBsDM(data=st_drop_geometry(background_1m[,names(finalpresmod)]), presmod=finalpresmod, backmod=finalbackmod3)
finalpresmod_preds_insecta = predictBsDM(data=st_drop_geometry(background_1m[,names(finalpresmod)]), presmod=finalpresmod, backmod=finalbackmod4)


CalcProbMass_NrCty_D <- function(data, preds){
  sumt = sum(preds, na.rm=T)
  xvals = seq(min(data,na.rm=T),max(data, na.rm=T),length.out=512)
  yvals = sapply(xvals, function(x) sum( preds[data < x] / sumt,na.rm=T))
  return(function(val){approx(xvals, yvals, val, rule=2, ties="ordered")$y})
}


probmass_full_base = CalcProbMass_NrCty_D(data = background_1m$NrCty_D, preds = finalpresmod_preds)
probmass_full_samegenera = CalcProbMass_NrCty_D(data = background_1m$NrCty_D, preds = finalpresmod_preds_samegenera)
probmass_full_lepidoptera = CalcProbMass_NrCty_D(data = background_1m$NrCty_D, preds = finalpresmod_preds_lepidoptera)
probmass_full_insecta = CalcProbMass_NrCty_D(data = background_1m$NrCty_D, preds = finalpresmod_preds_insecta)

probmass_full_base(10) #0.4455407 #10: 0.8591349
probmass_full_samegenera(10) #0.3406788 #10: 0.6608197
probmass_full_lepidoptera(10) #0.4067643 #10: 0.7730602
probmass_full_insecta(10) #0.3466781 #10: 0.6703323


plot1 = function(){
  curve(probmass_full_base(x),xlim=c(0,100), col=brewer.pal(9,"Set1")[3], lwd=2, lty=2, xlab="km", ylab="Cumulative probability")
  curve(probmass_full_samegenera(x),add=T, col=brewer.pal(9,"Set1")[1], lwd=2, lty=3)
  curve(probmass_full_lepidoptera(x),add=T, col=brewer.pal(9,"Set1")[4], lwd=2, lty=3)
  curve(probmass_full_insecta(x),add=T, col=brewer.pal(9,"Set1")[5], lwd=2, lty=3)
  legend(x=60, y=0.2, legend=c("no correction", "same genera", "Lepidoptera", "Insecta"),
         col=c(brewer.pal(9,"Set1")[3], brewer.pal(9,"Set1")[1], brewer.pal(9,"Set1")[4], brewer.pal(9,"Set1")[5]), lty=c(2,3,3,3),lwd=2, cex=1, box.lty=0)
  abline(v=10, col="darkgrey")
  }  
plot1 = as.grob(plot1) 

plot2 = function(){
plot(ecdf(background$NrCty_D), lwd=2, xlab="km", ylab="Cumulative probability", main="")
abline(v=10, col="darkgrey")  
abline(h=0.05, col="darkgrey")  
}
plot2 = as.grob(plot2)

#integrate(finalbackmod$NrCty_D, lower=min(background$NrCty_D), upper=10)
#0.0635


tiff("Output/S5.tiff", width = 10, height = 5, units = 'in', res = 900, compression="lzw") #
par(mgp = c(1,0.5,0),mar=c(3,3,1.1,1.1))
plot_grid(plot1, plot2, label_size = 12, axis='l',
          labels = c("(a) Probability mass",
                     "(b) Distribution over Europe")) 
dev.off()
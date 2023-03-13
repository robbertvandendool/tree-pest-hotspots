###### Figures script for the paper 'Introduction hotspots of non-native tree pests and the role of cities' by
###### Robbert T. van den Dool, Alejandro Morales, Wopke van der Werf & J.C. (Bob) Douma
###### Last edited 13 March 2023 - Robbert T. van den Dool
# Used to generate the figures shown in the manuscript. 
# run 1_Analysis.R first. 

insectasub = insectaalldata[1:(nrow(insectaalldata)-nrow(background)),]
lepidopterasub = lepidopteraalldata[1:(nrow(lepidopteraalldata)-nrow(background)),]



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
                 "W. products n. companies",
                 "W. products n. employed",
                 "Nr.City distance LOG",
                 "Random variable")


namestable = data.frame(varnames = names(reports), fullnames = fullvarnames)

#convert names
nameconvertfunct = function(name){
  newname = namestable[which(namestable[,1]==name),2]
  return(newname)
}

fullvarnames2 = c("Tree cover",
                  "*Tree cover",
                  "*Mean temperature",
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
                  "Gross domestic product",
                  "*GDP per capita",
                  "Maritime freight",
                  "*Air freight",
                  "*Distance to n. city",
                  "Freight unloaded at n. port",
                  "*Distance to n. port",
                  "Distance to n. airport",
                  "*Tree Species Richness",
                  "*Tourism",
                  "*Wood processing companies",
                  "Wood processing employees",
                  "Nr.City distance LOG",
                  "Random variable")


namestable2 = data.frame(varnames = names(reports), fullnames = fullvarnames2)

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
                  "Wood processing companies",
                  "Wood processing employees",
                  "Nr.City distance LOG",
                  "Random variable")


namestable3 = data.frame(varnames = names(reports), fullnames = fullvarnames3)

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
                  expression(Foreign ~ nightly ~ stays ~ year^-1 ~ km^2),
                  expression(paste("ln(Companies per ", km^2, " * 1000 + 1)")), 
                  expression(paste("ln(Employees per ", km^2, " * 1000 + 1)")),
                  "Nr.City distance LOG",
                  "Random variable")


unittable = list(varnames = names(reports), units = unitnames)

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


NUTS2_2016 <- readOGR(here("C:/Users/robbe/OneDrive - WageningenUR/Ch1 Establishment areas/Data_Intermediate/NUTS2_2016_v3.shp"))
NUTS2_2016sf <- st_as_sf(NUTS2_2016)
NUTS2_2016sf <- st_transform(NUTS2_2016sf, crs=3035)

tiff("Run8_Figure2_pointsmap.tiff", width = 5, height = 4, units = 'in', res = 600)
ggplot()+
  geom_sf(data=europe_eu)+
  theme_minimal()+
  geom_sf(data=NUTS2_2016sf, fill="white")+
  geom_sf(data=samples_base, colour="red", size=0.2, alpha = 0.35)+
  geom_sf(data=reports_base, colour="blue", size=0.6)+
  coord_sf(xlim=c(2426378.0132,6093974.6215), ylim=c(1318101.2618,5446513.5222), expand=F) #xlim=c(2426378.0132,6293974.6215), ylim=c(1528101.2618,5446513.5222)
dev.off()

####
#### Figure 2
####

#Fig 2 performance 
allresults_fig3 = allresults
allresults_fig3[!rownames(allresults_fig3) %in% c("frstcvr", "NrCty_DLOG", "43"),]

plotdataframe = allresults_fig3[!rownames(allresults_fig3) %in% c("frstcvr", "NrCty_DLOG", "43"),]

newrownames = unlist(sapply(rownames(plotdataframe), nameconvertfunct2))
names(newrownames) = NULL
rownames(plotdataframe)[-which(rownames(plotdataframe) %in% c("All variables", "Subset"))] = newrownames


plotdataframe$index = 1:nrow(plotdataframe)
plotdataframe$title = row.names(plotdataframe)

plotdataframe = plotdataframe[order(plotdataframe$AUC_mean, decreasing=T),]
plotdataframe$index = 1:nrow(plotdataframe)

plotdataframe$title = factor(plotdataframe$title, levels = plotdataframe$title)

plot1 <- ggplot(plotdataframe, aes(y = title, x = AUC_mean, color=AUC_mean, height=25)) +
  geom_point(shape = 16, size = 2) +  
  geom_errorbarh(aes(xmin = AUC_lower, xmax = AUC_upper), height = 0.5) +
  scale_y_discrete(limits = rev) +
  scale_color_gradient2(name="Percentile",low = "blue", mid="green", midpoint=0.7,high = "red") +
  xlab("AUC") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 11, colour = "black"),
        axis.text.x.bottom = element_text(size = 10, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"),
        legend.position = "none") #,plot.margin = unit(c(0,1,1,1), "cm")



#plot2
varnames = subvarnames
presdata = reports[,subvarnames]
backdata = background[,subvarnames]
pmodel = subfinalpresmod
bmodel = subukde_back
repeats = 50
prefix = "run7_sub1_4Dec2022"
origAUC = sub_perf[[2]]
origNLLp = sub_perf[[1]]

#Calculate VarImp
VarImp = future_map(varnames,.options = furrr::furrr_options(seed = TRUE),function(x) permutevarFinal(presdata = presdata, backdata = backdata, pmodel = pmodel, bmodel = bmodel, variable = x,repeats = repeats))
names(VarImp) = lapply(names(bmodel), nameconvertfunct3)

#AUC VPI barplot
VarImp_AUC = lapply(VarImp, function(x) myCI(origAUC - x$AUC)) #how much is AUC higher with the variable not shuffled?
VarImp_AUC_min = min(sapply(VarImp_AUC, function(x)x[2]),na.rm = T)
VarImp_AUC_max = max(sapply(VarImp_AUC, function(x)x[2]),na.rm = T)
VarImp_AUC_relative = lapply(VarImp_AUC , function(y) (y - VarImp_AUC_min)/ (VarImp_AUC_max-VarImp_AUC_min))
VarImp_AUC_relativesort = sapply(VarImp_AUC_relative, function(x)x[2])
names(VarImp_AUC_relativesort) = names(VarImp)


upper =   sapply(VarImp_AUC_relative, function(x)x[1])
upper_sort = upper[order(VarImp_AUC_relativesort)]

lower =   sapply(VarImp_AUC_relative, function(x)x[3])
lower_sort = lower[order(VarImp_AUC_relativesort)]

VarImp_AUC_relativesort = sort(VarImp_AUC_relativesort)

name = paste(prefix,"_VPI_AUC",".tiff",sep="")


VarImp_ggplotdf = data.frame(Variable=names(VarImp_AUC_relativesort), value=as.numeric(VarImp_AUC_relativesort), order=1:length(VarImp_AUC_relativesort))

VarImp_ggplotdf$Variable = factor(VarImp_ggplotdf$Variable, levels = VarImp_ggplotdf$Variable)


plot2 = ggplot(VarImp_ggplotdf, aes(x=value, y=variable, fill=value)) +
  geom_col(aes(value, Variable)) + 
  geom_errorbar(aes(y=Variable, xmin=lower_sort, xmax=upper_sort), width=.5,
                position=position_dodge(.9)) + 
  scale_fill_gradient2(name="Percentile",low = "blue", mid="green", midpoint=0.5,high = "red") + 
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
        axis.text.x.bottom = element_text(size = 10, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 11, colour = "black"))


packages("cowplot")
packages("ggplotify")
packages("grid", "gridExtra")


tiff("Run8v2_Figure3_performanceimportance_.tiff", width = 11, height = 7, units = 'in', res = 1500, compression="lzw")
plot_grid(plot1, plot2, labels = c('A)', 'B)'), nrow=1, label_size = 12, align = "hv", axis = "b", rel_widths = c(1, 1.3)) 
dev.off()
#plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, plot7, labels = c('A: bio1', 'B: bio7', 'C: bio12', 'D: bio16', 'E: bio5', 'F: bio6', 'G: bio8'), nrow=2, label_size = 12, align = "hv") 


###
### Figure 3
### #density functions.
#requires new function for plotting. May need to be in two parts. 
#requires all models complete 

#16 plot components


#Fig 3 = selection of most important variables
#appendix -> remaining density plots. 

Ch3_densplotgenerator = function(variable, legend=F, customxlim=NA, density=F){
  
  variablemin = mins[variable]
  variablemax = maxs[variable]
  xrange = seq(variablemin, variablemax, length.out=512)
  
  ymax = 1*max(c(finalpresmod[[variable]](xrange),
                   finalbackmod[[variable]](xrange),
                   biasbackmodel[[variable]](xrange),
                   biasbackmodel2[[variable]](xrange),
                   biasbackmodel3[[variable]](xrange)
  ))
  
  
  #plottitle = nameconvertfunct3(variable)
  xlab = unitconvertfunct(variable)
  xlim = c(variablemin, variablemax)
  
  
  if(legend==T){
    ymax = 1.5*max(c(finalpresmod[[variable]](xrange),
                     finalbackmod[[variable]](xrange),
                     biasbackmodel[[variable]](xrange),
                     biasbackmodel2[[variable]](xrange),
                     biasbackmodel3[[variable]](xrange)
    ))
  }
  
  
  if(!is.na(customxlim)){
    xlim = customxlim
  }
  
  ylab=""
  
  if(density==T){
    ylab="Density"
  }
  
  #eval(parse(text = *))
plotf = function(){
  curve(finalpresmod[[variable]](x), xlim= xlim, ylim=c(0,ymax),ylab=ylab, xlab=xlab, axes=F,frame.plot=TRUE, col="green", lwd=2, lty=2)
  Axis(side=1, labels=T)
  curve(biasbackmodel[[variable]](x), col="red", lwd=2, add=T)
  curve(biasbackmodel2[[variable]](x), col="purple", lwd=2, add=T) #lepidoptera
  curve(biasbackmodel3[[variable]](x), col="orange", lwd=2, add=T) #insecta
  curve(finalbackmod[[variable]](x), col="black", lwd=2, add=T)
  
  if(legend==T){
    legend("topleft",  bty="n", cex=1,y.intersp=1.2, legend=c("Establishments", "Background", "Same genera", "Lepidoptera", "Insecta"), col= c("green", "black", "red", "purple", "orange"), lty=c(2,1,1,1,1), lwd=2)
  }
  #border = "", bg= "",
} 
return(plotf)
}

#Ch3_densplotgenerator("WoodUnits", legend=T)()
#Figure 3
plot1 = Ch3_densplotgenerator("WoodUnits", legend=T, density=F)
plot1 = as.grob(plot1)

plot2 = Ch3_densplotgenerator("NrCty_D", customxlim=c(0,100))
plot2 = as.grob(plot2)

plot3 = Ch3_densplotgenerator("PCrpHa")
plot3 = as.grob(plot3)

plot4 = Ch3_densplotgenerator("NCrpHa")
plot4 = as.grob(plot4)

plot5 = Ch3_densplotgenerator("BIO10", density=F)
plot5 = as.grob(plot5)

plot6 = Ch3_densplotgenerator("Tourism")
plot6 = as.grob(plot6)

plot7 = Ch3_densplotgenerator("frstcv1")
plot7 = as.grob(plot7)

plot8 = Ch3_densplotgenerator("POPD") #in terms of importance should be the 10th plot, not 8th.
plot8 = as.grob(plot8)


tiff("Run8_Figure4_densityplots_.tiff", width = 14, height = 8, units = 'in', res = 900, compression="lzw")
plot = plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, 
          nrow=2, label_size = 12, axis='l', hjust=0, label_x = 0.1, #align = "v", #axis = "b", #rel_widths = c(1), #label_x = .3, 
          labels = c(paste('A)', nameconvertfunct3("WoodUnits")),
                     paste('B)', nameconvertfunct3("NrCty_D")),
                     paste('C)', nameconvertfunct3("PCrpHa")),
                     paste('D)', nameconvertfunct3("NCrpHa")),
                     paste('E)', nameconvertfunct3("BIO10")),
                     paste('F)', nameconvertfunct3("Tourism")),
                     paste('G)', nameconvertfunct3("frstcv1")),
                     paste('H)', nameconvertfunct3("POPD"))
          )) 

y.grob <- textGrob("Density", 
                   gp=gpar(fontface="bold", col="blue", fontsize=12), rot=90)

grid.arrange(arrangeGrob(plot, left = "Density"))
dev.off()

#appendix figure
plot9 = Ch3_densplotgenerator("BIO11", legend=T) 
plot9 = as.grob(plot9)

plot10 = Ch3_densplotgenerator("NrArp_D") 
plot10 = as.grob(plot10)

plot11 = Ch3_densplotgenerator("NrPrt_D") #Distance to n. port
plot11 = as.grob(plot11)

plot12 = Ch3_densplotgenerator("OrPlHa") #Ornamental plants cover
plot12 = as.grob(plot12)

plot13 = Ch3_densplotgenerator("ACrpHa") #Crop cover
plot13 = as.grob(plot13)

plot14 = Ch3_densplotgenerator("SR", legend=T) #Tree Species Richness
plot14 = as.grob(plot14)

plot15 = Ch3_densplotgenerator("GDPp") #GDP per capita
plot15 = as.grob(plot15)

plot16 = Ch3_densplotgenerator("BIO15") #Prec. seasonality
plot16 = as.grob(plot16)

plot17 = Ch3_densplotgenerator("AFrghtU") #Air freight
plot17 = as.grob(plot17)

tiff("Run8_Appendix_densityplots2.tiff", width = 14, height = 8, units = 'in', res = 900, compression="lzw")
plot = plot_grid(plot9, plot10, plot11, plot12, plot13, plot14, plot15, plot16, plot17,
                 nrow=2, label_size = 12, axis='l', hjust=0, label_x = 0.1, #align = "v", #axis = "b", #rel_widths = c(1), #label_x = .3, 
                 labels = c(paste('A)', nameconvertfunct3("BIO11")),
                            paste('B)', nameconvertfunct3("NrArp_D")),
                            paste('C)', nameconvertfunct3("NrPrt_D")),
                            paste('D)', nameconvertfunct3("OrPlHa")),
                            paste('E)', nameconvertfunct3("ACrpHa")),
                            paste('F)', nameconvertfunct3("SR")),
                            paste('G)', nameconvertfunct3("GDPp")),
                            paste('H)', nameconvertfunct3("BIO15")),
                            paste('I)', nameconvertfunct3("AFrghtU"))
                 )) 

y.grob <- textGrob("Density", 
                   gp=gpar(fontface="bold", col="blue", fontsize=12), rot=90)

grid.arrange(arrangeGrob(plot, left = "Density"))
dev.off()




###
### Figure 4
### #prediction maps


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
        plot.margin=unit(c(0,0.1,0,0.1), "cm"), legend.margin=margin(t = 0, unit='cm'), legend.key.height = unit(1.2, 'cm'))+
  scale_fill_gradient2(name="Percentile",low = "blue", mid="green", midpoint = 50, high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+

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
  scale_fill_gradient2(name="",low = "blue", mid="green", midpoint = 50, high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+

#bias corrected map
finalpresmod_preds_bias = predictBsDM(data=st_drop_geometry(background_1m[,names(finalpresmod)]), presmod=finalpresmod, backmod=biasbackmodel)

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
  scale_fill_gradient2(name="",low = "blue", mid="green", midpoint = 50, high = "red") #scale_colour_gradient2(name="Percentile",low = "blue", mid="green", midpoint=50,high = "red")+



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
        text = element_text(size=7), legend.position="none", axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin=unit(c(0,0,0,0), "cm"))+
  scale_fill_gradient2(name="",low = "blue", mid="green", midpoint = 50, high = "red")  #22th percentile still has negative values. 



#tiff("run8_rastermap_percentile.tiff", width = 5, height = 4, units = 'in', res = 900, compression='lzw')
#dev.off() 

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

tiff("Run8_Figure5_legend2.tiff", width = 12, height = 4.0, units = 'in', compression = "lzw", res=800) #width = 12, height = 4,  # res = 800
plot_grid(grobs[[1]], grobs[[2]],grobs[[3]],mylegend, align = "h", nrow = 1, axis="b", 
          rel_widths = c(4/13,4/13,4/13, 1/13), rel_heights = c(1/4,1/4,1/4,1/4), label_size = 12,
          labels=c("A) no bias-correction", "B) with bias-correction", "C) difference"),hjust=0, label_x = 0.025)
dev.off()






####
#### Figure 5 
####
biasbackmodelVPI = biasbackmodel
biasbackmodelVPI$runif = subukde_back$runif

biasbackmodelVPI2 = biasbackmodel2
biasbackmodelVPI2$runif = subukde_back$runif

biasbackmodelVPI3 = biasbackmodel3
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
plotvalues = rbind(VarImpDiff_AUC_samegenera,VarImpDiff_AUC_Lepidoptera, VarImpDiff_AUC_Insecta)
plotarrows_upper = rbind(upper_sort, upper_sort2, upper_sort3)
plotarrows_lower = rbind(lower_sort, lower_sort2, lower_sort3)

#plot
testplot = barplot(VarImpDiff_AUC_samegenera, beside=T, horiz=T, las=1, xlim=c(-1.1, 1.1),  main="AUC", xpd=T, col=c("red", "purple", "orange"))
arrows(upper_sort,testplot, lower_sort, testplot, angle=90, code=3, length=0.05)
legend("bottomright", legend=c("same genera","Lepidoptera", "Insecta"),
       col=c("red", "purple", "orange"), lty=1,lwd=2, cex=0.8, box.lty=0)


tiff(paste("Run8_Figure6_VarImpDifferences","_plot",".tiff",sep=""), width = 9, height = 6, units = 'in', res = 900, compression='lzw')
par(mgp = c(2,0.5,0),mar=c(3,13,1.1,1.1))
testplot = barplot(plotvalues, beside=T, horiz=T, las=1, xlim=c(-0.4, 0.15),  main="", xlab= "Change in relative importance", xpd=T, col=c("red", "purple", "orange"))
arrows(plotarrows_upper,testplot, plotarrows_lower, testplot, angle=90, code=3, length=0.05)
legend("topleft", legend=c("Same genera","Lepidoptera", "Insecta"),
       col=c("red", "purple", "orange"), lty=1,lwd=2, cex=1, box.lty=0)
dev.off()





####
### Appendix S4
####

background_temp = background
names(background_temp) = nameconvertfunct(names(background_temp))

tiff("corplot_background.tiff", width = 10, height = 10, units = 'in', res = 600, compression="lzw")
corplotfunct(background_temp)
dev.off()

####
### Appendix S6 and cdf values for in the discussion. 
####
BSDMapproxPKDE <- function(xnew, densityfunction, min, max){
  
  if(grepl("dnbinom", capture.output(print.function(densityfunction)))[1]==1){
    zx = min:max
    zy = densityfunction(zx)
    total = sum(zy) #creates the mean prediction between x values. and sums. 
    zy = zy/total
    return(sapply(xnew,function(x)sum(zy[1:x])))
  }
  
  zx = seq(from=min, to=max, length.out=1000)
  zy = densityfunction(zx)
  dx = diff(zx)[1] 
  total = sum(rowMeans(cbind(zy[-1], zy[-length(zy)]))*dx) #creates the mean prediction between x values. and sums. 
  cump = cumsum(rowMeans(cbind(zy[-1], zy[-length(zy)]))*dx)/total
  approx(zx, c(0,cump), xnew, rule=2, ties="ordered")$y
}

###Distance to nearest city 
xvals_NrCty_D=seq(mins["NrCty_D"],maxs["NrCty_D"],length.out=500)

#Base model
predfunct_NrCtyD_base = function(x){
  
  finalpresmod$NrCty_D(x) / finalbackmod$NrCty_D(x)
  
}

predfunct_NrCtyD_base_cdf2 = function(x){
  BSDMapproxPKDE(xnew=x, densityfunction=predfunct_NrCtyD_base, min=0, max=450)
}


#same genera model
predfunct_NrCtyD_bias = function(x){
  
  value = finalpresmod$NrCty_D(x) / biasbackmodel$NrCty_D(x)
  value[!is.finite(value)] = 0
  return(value)
}

predfunct_NrCtyD_bias_cdf2 = function(x){
  BSDMapproxPKDE(xnew=x, densityfunction=predfunct_NrCtyD_bias, min=0, max=450)
}

#lepidoptera
predfunct_NrCtyD_bias2 = function(x){
  
  value = finalpresmod$NrCty_D(x) / biasbackmodel2$NrCty_D(x)
  value[!is.finite(value)] = 0
  return(value)
}

predfunct_NrCtyD_bias2_cdf2 = function(x){
  BSDMapproxPKDE(xnew=x, densityfunction=predfunct_NrCtyD_bias2, min=0, max=450)
}

#insecta
predfunct_NrCtyD_bias3 = function(x){
  
  value = finalpresmod$NrCty_D(x) / biasbackmodel3$NrCty_D(x)
  value[!is.finite(value)] = 0
  return(value)
}

predfunct_NrCtyD_bias3_cdf2 = function(x){
  BSDMapproxPKDE(xnew=x, densityfunction=predfunct_NrCtyD_bias3, min=0, max=450)
}


###Population density
xvals_POPD=seq(mins["POPD"],maxs["POPD"],length.out=500)

#Base model
predfunct_POPD_base = function(x){
  
  finalpresmod$POPD(x) / finalbackmod$POPD(x)
  
}

predfunct_POPD_base_cdf2 = function(x){
  BSDMapproxPKDE(xnew=x, densityfunction=predfunct_POPD_base, min=min(xvals_POPD), max=max(xvals_POPD))
}


#same genera model
predfunct_POPD_bias = function(x){
  
  value = finalpresmod$POPD(x) / biasbackmodel$POPD(x)
  value[!is.finite(value)] = 0
  return(value)
}


predfunct_POPD_bias_cdf2 = function(x){
  BSDMapproxPKDE(xnew=x, densityfunction=predfunct_POPD_bias, min=min(xvals_POPD), max=max(xvals_POPD))
}

#lepidoptera
predfunct_POPD_bias2 = function(x){
  
  value = finalpresmod$POPD(x) / biasbackmodel2$POPD(x)
  value[!is.finite(value)] = 0
  return(value)
}

predfunct_POPD_bias2_cdf2 = function(x){
  BSDMapproxPKDE(xnew=x, densityfunction=predfunct_POPD_bias2, min=min(xvals_POPD), max=max(xvals_POPD))
}

#insecta
predfunct_POPD_bias3 = function(x){
  
  value = finalpresmod$POPD(x) / biasbackmodel3$POPD(x)
  value[!is.finite(value)] = 0
  return(value)
}

predfunct_POPD_bias3_cdf2 = function(x){
  BSDMapproxPKDE(xnew=x, densityfunction=predfunct_POPD_bias3, min=min(xvals_POPD), max=max(xvals_POPD))
}


#Figure S7
plot1 = function(){
  curve(predfunct_NrCtyD_base_cdf2(x), xlim=c(min(xvals_NrCty_D),200 ), col="green", lwd=2, lty=2, xlab="km", ylab="Cumulative probability") #max(xvals_NrCty_D)
  curve(predfunct_NrCtyD_bias_cdf2(x),  col="red", lwd=2, add=T)
  curve(predfunct_NrCtyD_bias2_cdf2(x),  col="purple", lwd=2, add=T)
  curve(predfunct_NrCtyD_bias3_cdf2(x),  col="orange", lwd=2, add=T)
  legend(x=120, y=0.2, legend=c("no correction", "same genera", "Lepidoptera", "Insecta"),
       col=c("green", "red", "purple", "orange"), lty=c(2,1,1,1),lwd=2, cex=1, box.lty=0)
}
plot1 = as.grob(plot1)

plot2 = function(){
  curve(predfunct_POPD_base_cdf2(x), xlim=c(min(xvals_POPD),max(xvals_POPD)), col="green", lwd=2, xlab=expression(paste("ln(Inhabitants per ", km^2, " + 1)")), ylab="") #max(xvals_POPD)
  curve(predfunct_POPD_bias_cdf2(x), col="red", lwd=2, add=T)
  curve(predfunct_POPD_bias2_cdf2(x), col="purple", lwd=2, add=T)
  curve(predfunct_POPD_bias3_cdf2(x), col="orange", lwd=2, add=T)
}
plot2 = as.grob(plot2)


tiff("Run8_Appendix_cdfplots.tiff", width = 10, height = 5, units = 'in', res = 900, compression="lzw") #
par(mgp = c(1,0.5,0),mar=c(3,3,1.1,1.1))
plot_grid(plot1, plot2, label_size = 12, axis='l',
                 labels = c(paste('A)', nameconvertfunct3("NrCty_D")),
                            paste('B)', nameconvertfunct3("POPD")))) 
dev.off()


#values
predfunct_NrCtyD_base_cdf2(5) #0.376305
predfunct_NrCtyD_bias_cdf2(5) #0.160753
predfunct_NrCtyD_bias2_cdf2(5) #0.1683589
predfunct_NrCtyD_bias3_cdf2(5) #0.3011999

predfunct_NrCtyD_base_cdf2(50) #0.8745522
predfunct_NrCtyD_bias_cdf2(50) #0.6775515
predfunct_NrCtyD_bias2_cdf2(50) #0.7992625
predfunct_NrCtyD_bias3_cdf2(50) #0.7394912

#values
predfunct_POPD_base_cdf2(6) #0.06407103
predfunct_POPD_bias_cdf2(6) #0.1533739
predfunct_POPD_bias2_cdf2(6) #0.04107063
predfunct_POPD_bias3_cdf2(6) #0.1257292

predfunct_POPD_base_cdf2(8) #0.4844523
predfunct_POPD_bias_cdf2(8) #0.2801856
predfunct_POPD_bias2_cdf2(8) #0.1101975
predfunct_POPD_bias3_cdf2(8) #0.2422395

predfunct_POPD_base_cdf2(9) #0.9171869 #exp(9.305929)-1
predfunct_POPD_bias_cdf2(9) #0.619756
predfunct_POPD_bias2_cdf2(9) #0.4678369
predfunct_POPD_bias3_cdf2(9) #0.5906305







### 
### Miscellaneous figures
###

#VarImp
time = Sys.time()
createVarImpPlot(varnames = subvarnames, presdata = reports[,subvarnames], backdata = background[,subvarnames], pmodel = subfinalpresmod, bmodel = subukde_back, repeats = 50, prefix = "run7_sub1", origAUC = sub_perf[[2]], origNLLp = sub_perf[[1]]) #origAUC = subvarmodels[[2]][["AUC_mean"]], origNLLp = subvarmodels[[2]][["NLLp_mean"]]
Sys.time() - time

time = Sys.time()
createVarImpPlotAbsolute(varnames = subvarnames, presdata = reports[,subvarnames], backdata = background[,subvarnames], pmodel = subfinalpresmod, bmodel = subukde_back, repeats = 100, prefix = "run7_sub1_abs", origAUC = sub_perf[["auc"]], origNLLp = sub_perf[["logloss"]])
Sys.time() - time


###Ecdf vs CDF plots
lapply(names(finalpresmod),function(x) plotAssessFit(data=reportssub, model=finalpresmod, alldata=suballdata, variable=x, prefix="run7_Rep_"))
lapply(names(finalpresmod),function(x) plotAssessFit(data=backgroundsub, model=finalbackmod, alldata=suballdata, variable=x, prefix="run7_Back_"))

###Response plots
lapply(names(finalpresmod),function(x) plotResponseMarg(variable = x, presmod = finalpresmod, backmod = subukde_back, alldata = suballdata, npres=274, prefix="run7_", cex=1))
#plotResponseCond

#assess fits of sampling models
lapply(names(sampfinalpresmod),function(x) plotAssessFit(data=samplessub, model=sampfinalpresmod, alldata=sampalldata, variable=x, prefix="run7_Samp_"))
lapply(names(sampfinalpresmod),function(x) plotResponseMarg(variable = x, presmod = sampfinalpresmod, backmod = sampukde_back, alldata = sampalldata, npres=52237, prefix="run7_Samp", cex=1))

#response
lapply(names(sampukde_back),function(x) plotResponseMarg(variable = x, presmod = finalpresmod, backmod = biasbackmodel, alldata = suballdata, npres=273, prefix="run7_BiasGenera", cex=1))


###Lepidoptera model
#assess fits
lapply(names(lepidopterafinalpresmod),function(x) plotAssessFit(data=lepidopterasub, model=lepidopterafinalpresmod, alldata=lepidopteraalldata, variable=x, prefix="run7_lepidoptera_"))
lapply(names(lepidopterafinalpresmod),function(x) plotResponseMarg(variable = x, presmod = lepidopterafinalpresmod, backmod = lepidopteraukde_back, alldata = lepidopteraalldata, npres=5612600, prefix="run7_lepidoptera", cex=1))

#response
lapply(names(lepidopteraukde_back),function(x) plotResponseMarg(variable = x, presmod = finalpresmod, backmod = biasbackmodel2, alldata = suballdata, npres=273, prefix="run7_Biaslepidoptera", cex=1))




###Insecta model
#assess fits
lapply(names(insectafinalpresmod),function(x) plotAssessFit(data=insectasub, model=insectafinalpresmod, alldata=insectaalldata, variable=x, prefix="run7_insecta_"))
lapply(names(insectafinalpresmod),function(x) plotResponseMarg(variable = x, presmod = insectafinalpresmod, backmod = insectaukde_back, alldata = insectaalldata, npres=5612600, prefix="run7_insecta", cex=1))

#Insecta bias corrected
#response
lapply(names(insectaukde_back),function(x) plotResponseMarg(variable = x, presmod = finalpresmod, backmod = biasbackmodel3, alldata = suballdata, npres=273, prefix="run7_Biasinsecta", cex=1))

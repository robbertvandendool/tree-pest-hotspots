###### Analysis script for the paper 'Introduction hotspots of non-native tree pests and the role of cities' by
###### Robbert T. van den Dool, Alejandro Morales, Wopke van der Werf & J.C. (Bob) Douma
###### Last edited 13 March 2023 - Robbert T. van den Dool


##### Preparation #####
#rm(list=ls(all=TRUE))

#load packages
if(!require("easypackages")) install.packages("easypackages")
library(easypackages)
packages("sf", "caret", "rgdal", "kdevine", "sp", "raster", "rstudioapi", "sgt", "pROC", "MASS", "ggplot2", "rnaturalearth", "rnaturalearthdata", "gridExtra", "furrr", prompt = FALSE)
set.seed(123456789)

#load functions
source("Scripts/Functions.R")

#load data
reports = readRDS(file="Intermediate/reports.rds")
background = readRDS(file="Intermediate/background.rds")
background_1m = readRDS(file="Intermediate/background_maps.rds")
samples = readRDS(file="Intermediate/samegenera.rds")
insecta1 = readRDS(file="Intermediate/insecta_1.rds")
insecta2 = readRDS(file="Intermediate/insecta_2.rds")
insecta3 = readRDS(file="Intermediate/insecta_3.rds")
insecta4 = readRDS(file="Intermediate/insecta_4.rds")
insecta = rbind(insecta1, insecta2, insecta3, insecta4)

lepidoptera1 = readRDS(file="Intermediate/lepidoptera_1.rds")
lepidoptera2 = readRDS(file="Intermediate/lepidoptera_2.rds")
lepidoptera = rbind(lepidoptera1, lepidoptera2)

#setup multicore support
plan(multisession, workers = 8)

#save base sf 
reports_base = reports
background_base = background
samples_base = samples
insecta_base = insecta
lepidoptera_base = lepidoptera

#remove geometry
reports <- st_drop_geometry(reports)
background <- st_drop_geometry(background)
samples <- st_drop_geometry(samples)
insecta <- st_drop_geometry(insecta)
lepidoptera <- st_drop_geometry(lepidoptera)

#determine min & max values of data
alldata <- rbind(reports, background) #samples has slightly different values for POPD and NrCty_D
mins <-  sapply(alldata, function(x) min(x)) #-0.05*min(x) 
maxs <-  sapply(alldata, function(x) max(x)) #+0.05*min(x) 

mins["NrCty_D"] = min(samples$NrCty_D)
##### /End preparation #####







##### Step 1: create models for all variables, assess performance #####

#create background functions
zztime = Sys.time()
ukde_back = fitunivdens(data=background, mins=mins, maxs=maxs, adjust = 1) 
Sys.time() - zztime 

ukde_back$frstcvr = densapproxfactory_old(background[,"frstcvr"], min = mins["frstcvr"], max = maxs["frstcvr"], adjust=1) #fit frstcvr separately using default bandwidth for a better fit. 

#Create holdout validation data 
crossvalidationdata <- createpartitions(data=reports, ptrain=0.8, ptest=0.1, ptune=0.1, times=75) 



###Fit a presence distribution for all variables combined, considering multiple possible distribution functions
zzz = Sys.time()
cv_fits = future_map(names(background), function(x)crossvalidatemarg(data=crossvalidationdata, x, alldata=alldata)) 
names(cv_fits) = names(background) 
Sys.time()-zzz 

#Consider only the best fitting functions per partition for each variable
cvfitssplit = lapply(seq_len(75),function(y){
  temp = lapply(names(cv_fits),function(x){cv_fits[[x]][y,]});
  names(temp) = names(cv_fits);
  return(temp)
})

cvpresmodels = lapply(cvfitssplit, createpresmodel) 

cvpresmodels_norunif = lapply(cvpresmodels, function(x) x[-which(names(x)=="runif")])
crossvalidationdata_norunif = lapply(crossvalidationdata, function(x) lapply(x,function(y) y[-which(names(y)=="runif")])) 
background_norunif = background[,-which(names(background)=="runif")]
ukde_back_norunif = ukde_back[-which(names(ukde_back)=="runif")]

#determine performance
allvarmodels = modelperf(pcvdata = crossvalidationdata_norunif, bdata = background_norunif, presmodels = cvpresmodels_norunif, backmodel = ukde_back_norunif)

allresults = t(data.frame(allvarmodels[[2]])) 
row.names(allresults)[nrow(allresults)] = "All variables"
###/all variables combined

### Fit models for individual variables
zzz = Sys.time()
singlevarmodels = future_map(names(reports),function(x)BSDM1varperf(varname = x, pdata=reports, bdata=background, alldata=alldata, backgroundmodels = ukde_back, ptrain=0.8, ptest=0.1, ptune=0.1, times=75))
Sys.time() - zzz
names(singlevarmodels)=names(reports) 

results_1v = as.data.frame(t(sapply(singlevarmodels,function(x){ x[[2]]})))
allresults = rbind(results_1v, allresults)
###/individual variables
##### /Step 1 #####





##### Step 2: variable selection for the subset model #####
# These code sections contain the final selection of variables for the subset.
# Note that to repeat the complete workflow as described in the paper, you need to add the variables 'GDP' and 'MFrghtU' to the vector 'subvarnames'. Then run the code to get the final models and run code
# for variable permutation importance: either function 'createVarImpPlot' or the code in the '2_Figure.R' script for figure 3. The resulting VPI figure will indicate that these two variables can be dropped.

subvarnames = c("NrCty_D", "Tourism", "WoodUnits", "NrArp_D", "NrPrt_D", "POPD" ,"BIO10", "BIO11", "BIO15", "GDPp", "frstcv1", "SR", "AFrghtU", "OrPlHa",  "ACrpHa",  "PCrpHa", "NCrpHa", "runif")
reportssub = reports[,subvarnames]
backgroundsub = background[,subvarnames]

suballdata = alldata[,subvarnames]
subukde_back = ukde_back[subvarnames]
##### /Step 2 #####





##### Step 3: subset model creation #####
subcrossvalidationdata <- createpartitions(data=reportssub, ptrain=0.8, ptest=0.1, ptune=0.1, times=75) 

zzz = Sys.time()
subcv_fits = future_map(names(backgroundsub), function(x)crossvalidatemarg(data=subcrossvalidationdata, x, alldata=suballdata)) 
names(subcv_fits) = names(backgroundsub) 
Sys.time()-zzz


subcvfitssplit = lapply(seq_len(75),function(y){
  
  temp = lapply(names(subcv_fits),function(x){subcv_fits[[x]][y,]});
  names(temp) = names(subcv_fits);
  return(temp)
  
})

subcvpresmodels = lapply(subcvfitssplit, createpresmodel) 

subcvpresmodels_norunif = lapply(subcvpresmodels, function(x) x[-which(names(x)=="runif")])
subcrossvalidationdata_norunif = lapply(subcrossvalidationdata, function(x) lapply(x,function(y) y[-which(names(y)=="runif")])) 
backgroundsub_norunif = backgroundsub[,-which(names(backgroundsub)=="runif")]
subukde_back_norunif = subukde_back[-which(names(subukde_back)=="runif")]

# Determine subset model performance
subvarmodels = modelperf(pcvdata = subcrossvalidationdata_norunif, bdata = backgroundsub_norunif, presmodels = subcvpresmodels_norunif, backmodel = subukde_back_norunif)

allresults = rbind(allresults, subvarmodels[[2]])
row.names(allresults)[nrow(allresults)] = "Subset"

# Final Model
subfinalpresmod = fitfinalModel(subcv_fits, reportssub, alldata)

# Final Model without random variable
finalpresmod = subfinalpresmod[-which(names(subfinalpresmod)=="runif")] 
finalbackmod = subukde_back[-which(names(subukde_back)=="runif")] 

##### /Step 3 #####





##### Step 4: select variables for bias selection #####
# We selected population density and distance to nearest city: POPD, NrCty_D
# Support for these decisions can be found in the paper and supporting information
##### /Step 4 #####






##### Step 5: Sampling bias correction #####
# For each citizen science dataset (same genera, lepidoptera, insecta) the script follows two steps:
# A) fit presence distributions to the selected variables
# B) adjust the final subset model from step 3 to include bias correction
# The end result is three bias-corrected datasets


### Same genera A: fit functions ###
samplessub = samples[,c("NrCty_D", "POPD")]
sampalldata <- rbind(samplessub, backgroundsub[,c("NrCty_D","POPD")])
sampukde_back = ukde_back[c("NrCty_D","POPD")]

sampcrossvalidationdata <- createpartitions(data=samplessub, ptrain=0.8, ptest=0.1, ptune=0.1, times=50) 

zzz = Sys.time()
sampcv_fits = future_map(names(samplessub), function(x)crossvalidatemarg_simple(data=sampcrossvalidationdata, x, alldata=sampalldata)) #crossvalidatemarg_simple: a simplified version of this function. Here we only consider simpler parametric functions.  
names(sampcv_fits) = names(samplessub) 
Sys.time() - zzz

sampcvfitssplit = lapply(seq_len(50),function(y){
  
  temp = lapply(names(sampcv_fits),function(x){sampcv_fits[[x]][y,]});
  names(temp) = names(sampcv_fits);
  return(temp)
  
})

sampcvpresmodels = lapply(sampcvfitssplit, createpresmodel) 

# determine performance 
sampvarmodels = modelperf(pcvdata = sampcrossvalidationdata, bdata = backgroundsub[,c("NrCty_D","POPD")], presmodels = sampcvpresmodels, backmodel = sampukde_back)

# final sampling model
sampfinalpresmod = fitfinalModel(sampcv_fits, samplessub)
### /Same genera A: fit functions ###


### Same genera B: bias correction ###
#supplant background models with bias correction models. 
biasbackmodel = finalbackmod
biasbackmodel[names(sampukde_back)] = sampfinalpresmod
### /Same genera B: bias correction ###



### lepidoptera A: fit functions ###
lepidopterasub = lepidoptera[,c("NrCty_D", "POPD")]
lepidopteraalldata <- rbind(lepidopterasub, backgroundsub[,c("NrCty_D","POPD")])
lepidopteraukde_back = ukde_back[c("NrCty_D","POPD")]

lepidopteracrossvalidationdata <- createpartitions(data=lepidopterasub, ptrain=0.8, ptest=0.1, ptune=0.1, times=1) 

zzz = Sys.time() 
lepidopteracv_fits = future_map(names(lepidopterasub), function(x)crossvalidatemarg_simple(data=lepidopteracrossvalidationdata, x, alldata=lepidopteraalldata)) 
names(lepidopteracv_fits) = names(lepidopterasub) 
Sys.time() - zzz 

lepidopteracvfitssplit = lapply(seq_len(1),function(y){
  
  temp = lapply(names(lepidopteracv_fits),function(x){lepidopteracv_fits[[x]][y,]});
  names(temp) = names(lepidopteracv_fits);
  return(temp)
  
})

lepidopteracvpresmodels = lapply(lepidopteracvfitssplit, createpresmodel) 

# determine performance 
lepidopteravarmodels = modelperf(pcvdata = lepidopteracrossvalidationdata, bdata = backgroundsub[,c("NrCty_D","POPD")], presmodels = lepidopteracvpresmodels, backmodel = lepidopteraukde_back)

# final sampling model
lepidopterafinalpresmod = fitfinalModel(lepidopteracv_fits, lepidopterasub)
### /lepidoptera A: fit functions ###



### lepidoptera B: bias correction ###
biasbackmodel2 = finalbackmod
biasbackmodel2[names(lepidopteraukde_back)] = lepidopterafinalpresmod
### /lepidoptera B: bias correction ###



### Insecta A: fit functions ###
insectasub = insecta[,c("NrCty_D", "POPD")]
insectaalldata <- rbind(insectasub, backgroundsub[,c("NrCty_D","POPD")])
insectaukde_back = ukde_back[c("NrCty_D","POPD")]

insectacrossvalidationdata <- createpartitions(data=insectasub, ptrain=0.8, ptest=0.1, ptune=0.1, times=1) 

zzz = Sys.time() 
insectacv_fits = future_map(names(insectasub), function(x)crossvalidatemarg_simple(data=insectacrossvalidationdata, x, alldata=insectaalldata)) 
names(insectacv_fits) = names(insectasub) 
Sys.time() - zzz 

insectacvfitssplit = lapply(seq_len(1),function(y){
  
  temp = lapply(names(insectacv_fits),function(x){insectacv_fits[[x]][y,]});
  names(temp) = names(insectacv_fits);
  return(temp)
  
})

insectacvpresmodels = lapply(insectacvfitssplit, createpresmodel) 

# determine performance
insectavarmodels = modelperf(pcvdata = insectacrossvalidationdata, bdata = backgroundsub[,c("NrCty_D","POPD")], presmodels = insectacvpresmodels, backmodel = insectaukde_back)

# determine final sampling model
insectafinalpresmod = fitfinalModel(insectacv_fits, insectasub)
### Insecta A: fit functions ###


### Insecta B: bias correction ###
biasbackmodel3 = finalbackmod
biasbackmodel3[names(insectaukde_back)] = insectafinalpresmod
### /Insecta B: bias correction ###

##### /Step 5: Sampling bias correction #####


# keep environment loaded for script 2_Figures.

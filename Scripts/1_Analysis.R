###### Analysis script for the paper 'Introduction hotspots of non-native tree pests and the role of cities' by
###### Robbert T. van den Dool, Alejandro Morales, Wopke van der Werf & J.C. (Bob) Douma
###### Last edited 17 July 2023 - Robbert T. van den Dool


# This script fits generative models following the routine described in the main text. 


# Note that variable names such as "samegenera_city_PA" and "samegenera_popd_PA" have been included in this script but were not mentioned in the manuscript. 
# If interested in alternative bias corrections based on just the distance to the nearest city (city) or population density (popd) this something that can be attempted adapting the current script.


##### Preparation #####
#rm(list=ls(all=TRUE))

#load packages
if(!require("easypackages")) install.packages("easypackages")

library(easypackages)

packages("sf", "caret", "rgdal", "kdevine", "sp", "raster", "rstudioapi", 
         "sgt", "pROC", "MASS", "ggplot2", "rnaturalearth", "rnaturalearthdata", 
         "gridExtra", "furrr", "cowplot", "ggplotify", "grid", "gridExtra", prompt = FALSE)

set.seed(123456789)

#load functions
source("Scripts/Functions.R")

#load data
reports = readRDS(file="Intermediate/reports.rds")
background = readRDS(file="Intermediate/background.rds")
background_1m = readRDS(file="Intermediate/background_maps.rds")
samples = readRDS(file="Intermediate/samegenera.rds")

samegenera_city_PA = readRDS(file="Intermediate/SG_city.rds")
lepidoptera_city_PA = readRDS(file="Intermediate/L_city.rds")
insecta_city_PA = readRDS(file="Intermediate/I_city.rds")

samegenera_popd_PA = readRDS(file="Intermediate/SG_popd.rds")
lepidoptera_popd_PA = readRDS(file="Intermediate/L_popd.rds")
insecta_popd_PA = readRDS(file="Intermediate/I_popd.rds")


samegenera_both_PA = readRDS(file="Intermediate/SG_both.rds")
lepidoptera_both_PA = readRDS(file="Intermediate/L_both.rds")
insecta_both_PA = readRDS(file="Intermediate/I_both.rds")

#setup multicore support
plan(multisession, workers = 6)

#save base sf 
reports_base = reports
background_base = background

#remove geometry
reports = st_drop_geometry(reports)
background = st_drop_geometry(background)

samegenera_city_PA = st_drop_geometry(samegenera_city_PA)
samegenera_city_PA = samegenera_city_PA[names(samegenera_city_PA) %in% names(background)]
names(samegenera_city_PA) == names(background)
samegenera_city_PA$runif = runif(nrow(samegenera_city_PA))
samegenera_city_PA = samegenera_city_PA[complete.cases(samegenera_city_PA),]

samegenera_popd_PA = st_drop_geometry(samegenera_popd_PA)
samegenera_popd_PA = samegenera_popd_PA[names(samegenera_popd_PA) %in% names(background)]
names(samegenera_popd_PA) == names(background)
samegenera_popd_PA$runif = runif(nrow(samegenera_popd_PA))
samegenera_popd_PA = samegenera_popd_PA[complete.cases(samegenera_popd_PA),]

samegenera_both_PA = st_drop_geometry(samegenera_both_PA)
samegenera_both_PA = samegenera_both_PA[names(samegenera_both_PA) %in% names(background)]
names(samegenera_both_PA) == names(background)
samegenera_both_PA$runif = runif(nrow(samegenera_both_PA))
samegenera_both_PA = samegenera_both_PA[complete.cases(samegenera_both_PA),]

lepidoptera_city_PA = st_drop_geometry(lepidoptera_city_PA)
lepidoptera_city_PA = lepidoptera_city_PA[names(lepidoptera_city_PA) %in% names(background)]
names(lepidoptera_city_PA) == names(background)
lepidoptera_city_PA$runif = runif(nrow(lepidoptera_city_PA))
lepidoptera_city_PA = lepidoptera_city_PA[complete.cases(lepidoptera_city_PA),]

lepidoptera_popd_PA = st_drop_geometry(lepidoptera_popd_PA)
lepidoptera_popd_PA = lepidoptera_popd_PA[names(lepidoptera_popd_PA) %in% names(background)]
names(lepidoptera_popd_PA) == names(background)
lepidoptera_popd_PA$runif = runif(nrow(lepidoptera_popd_PA))
lepidoptera_popd_PA = lepidoptera_popd_PA[complete.cases(lepidoptera_popd_PA),]

lepidoptera_both_PA = st_drop_geometry(lepidoptera_both_PA)
lepidoptera_both_PA = lepidoptera_both_PA[names(lepidoptera_both_PA) %in% names(background)]
names(lepidoptera_both_PA) == names(background)
lepidoptera_both_PA$runif = runif(nrow(lepidoptera_both_PA))
lepidoptera_both_PA = lepidoptera_both_PA[complete.cases(lepidoptera_both_PA),]

insecta_city_PA = st_drop_geometry(insecta_city_PA)
insecta_city_PA = insecta_city_PA[names(insecta_city_PA) %in% names(background)]
names(insecta_city_PA) == names(background)
insecta_city_PA$runif = runif(nrow(insecta_city_PA))
insecta_city_PA = insecta_city_PA[complete.cases(insecta_city_PA),]

insecta_popd_PA = st_drop_geometry(insecta_popd_PA)
insecta_popd_PA = insecta_popd_PA[names(insecta_popd_PA) %in% names(background)]
names(insecta_popd_PA) == names(background)
insecta_popd_PA$runif = runif(nrow(insecta_popd_PA))
insecta_popd_PA = insecta_popd_PA[complete.cases(insecta_popd_PA),]

insecta_both_PA = st_drop_geometry(insecta_both_PA)
insecta_both_PA = insecta_both_PA[names(insecta_both_PA) %in% names(background)]
names(insecta_both_PA) == names(background)
insecta_both_PA$runif = runif(nrow(insecta_both_PA))
insecta_both_PA = insecta_both_PA[complete.cases(insecta_both_PA),]

background_1m_nogeo = st_drop_geometry(background_1m)
background_1m_nogeo = background_1m_nogeo[names(background_1m_nogeo) %in% names(background)]
names(background_1m_nogeo) == names(background)


#remove unused variables
reports = reports[,!names(reports)%in%c("NrCty_DLOG", "frstcvr")] #"NrCty_D", 
background = background[,!names(background)%in%c("NrCty_DLOG", "frstcvr")] 
samegenera_city_PA = samegenera_city_PA[,!names(samegenera_city_PA)%in%c("NrCty_DLOG", "frstcvr")] 
samegenera_popd_PA = samegenera_popd_PA[,!names(samegenera_popd_PA)%in%c("NrCty_DLOG", "frstcvr")]
samegenera_both_PA = samegenera_both_PA[,!names(samegenera_both_PA)%in%c("NrCty_DLOG", "frstcvr")]

lepidoptera_city_PA = lepidoptera_city_PA[,!names(lepidoptera_city_PA)%in%c("NrCty_DLOG", "frstcvr")] 
lepidoptera_popd_PA = lepidoptera_popd_PA[,!names(lepidoptera_popd_PA)%in%c("NrCty_DLOG", "frstcvr")] 
lepidoptera_both_PA = lepidoptera_both_PA[,!names(lepidoptera_both_PA)%in%c("NrCty_DLOG", "frstcvr")] 

insecta_city_PA = insecta_city_PA[,!names(insecta_city_PA)%in%c("NrCty_DLOG", "frstcvr")] 
insecta_popd_PA = insecta_popd_PA[,!names(insecta_popd_PA)%in%c("NrCty_DLOG", "frstcvr")] 
insecta_both_PA = insecta_both_PA[,!names(insecta_both_PA)%in%c("NrCty_DLOG", "frstcvr")] 

background_1m_nogeo = background_1m_nogeo[,!names(background_1m_nogeo)%in%c("NrCty_DLOG", "frstcvr")] 


#determine min & max values of data
alldata <- rbind(reports, background) 
mins <-  sapply(background_1m_nogeo, function(x) as.numeric(min(x, na.rm=T)))
maxs <-  sapply(background_1m_nogeo, function(x) as.numeric(max(x, na.rm=T))) 
mins = c(mins, "runif" = 0)
maxs = c(maxs, "runif" = 1)
##### /End preparation #####





##### Step 1: create models for all variables, assess performance #####

#create background functions
ukde_back = vector(mode="list", length=0)
names(background)

ukde_back$frstcv1 = densapproxfactory_botev_lb(background$frstcv1, min=mins["frstcv1"], max=maxs["frstcv1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO1 = densapproxfactory_botev_lb(background$BIO1, min=mins["BIO1"], max=maxs["BIO1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO2 = densapproxfactory_botev_lb(background$BIO2, min=mins["BIO2"], max=maxs["BIO2"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO3 = densapproxfactory_botev_lb(background$BIO3, min=mins["BIO3"], max=maxs["BIO3"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO4 = densapproxfactory_botev_lb(background$BIO4, min=mins["BIO4"], max=maxs["BIO4"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO5 = densapproxfactory_botev_lb(background$BIO5, min=mins["BIO5"], max=maxs["BIO5"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO6 = densapproxfactory_botev_lb(background$BIO6, min=mins["BIO6"], max=maxs["BIO6"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO7 = densapproxfactory_botev_lb(background$BIO7, min=mins["BIO7"], max=maxs["BIO7"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO8 = densapproxfactory_botev_lb(background$BIO8, min=mins["BIO8"], max=maxs["BIO8"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO9 = densapproxfactory_botev_lb(background$BIO9, min=mins["BIO9"], max=maxs["BIO9"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO10 = densapproxfactory_botev_lb(background$BIO10, min=mins["BIO10"], max=maxs["BIO10"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO11 = densapproxfactory_botev_lb(background$BIO11, min=mins["BIO11"], max=maxs["BIO11"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO12 = densapproxfactory_botev_lb(background$BIO12, min=mins["BIO12"], max=maxs["BIO12"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO13 = densapproxfactory_botev_lb(background$BIO13, min=mins["BIO13"], max=maxs["BIO13"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO14 = densapproxfactory_botev_lb(background$BIO14, min=mins["BIO14"], max=maxs["BIO14"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO15 = densapproxfactory_botev_lb(background$BIO15, min=mins["BIO15"], max=maxs["BIO15"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO16 = densapproxfactory_botev_lb(background$BIO16, min=mins["BIO16"], max=maxs["BIO16"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO17 = densapproxfactory_botev_lb(background$BIO17, min=mins["BIO17"], max=maxs["BIO17"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO18 = densapproxfactory_botev_lb(background$BIO18, min=mins["BIO18"], max=maxs["BIO18"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$BIO19 = densapproxfactory_botev_lb(background$BIO19, min=mins["BIO19"], max=maxs["BIO19"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$POPD = densapproxfactory_botev_lb(background$POPD, min=mins["POPD"], max=maxs["POPD"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$ACrpHa = densapproxfactory_botev_lb(background$ACrpHa, min=mins["ACrpHa"], max=maxs["ACrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_back$PCrpHa = densapproxfactory_botev_lb(background$PCrpHa, min=mins["PCrpHa"], max=maxs["PCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) #worst fit so far, but weird data
ukde_back$NCrpHa = densapproxfactory_botev_lb(background$NCrpHa, min=mins["NCrpHa"], max=maxs["NCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_back$OrPlHa = densapproxfactory_botev_lb(background$OrPlHa, min=mins["OrPlHa"], max=maxs["OrPlHa"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_back$GDP = densapproxfactory_botev_lb(background$GDP, min=mins["GDP"], max=maxs["GDP"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_back$GDPp = densapproxfactory_botev_lb(background$GDPp, min=mins["GDPp"], max=maxs["GDPp"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_back$MFrghtU = densapproxfactory_botev_lb(background$MFrghtU, min=mins["MFrghtU"], max=maxs["MFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.065) #complex fit, data highly irregular with zero inflation.  
ukde_back$AFrghtU = densapproxfactory_botev_lb(background$AFrghtU, min=mins["AFrghtU"], max=maxs["AFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.065)  
ukde_back$NrCty_D = densapproxfactory_botev_lb(background$NrCty_D, min=mins["NrCty_D"], max=maxs["NrCty_D"], useweighted=TRUE, adjust=3, lbmult=0.2)   #was 3, leftbound was not accurate
ukde_back$NrPrt_F = densapproxfactory_botev_lb(background$NrPrt_F, min=mins["NrPrt_F"], max=maxs["NrPrt_F"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_back$NrPrt_D = densapproxfactory_botev_lb(background$NrPrt_D, min=mins["NrPrt_D"], max=maxs["NrPrt_D"], useweighted=TRUE, adjust=3, lbmult=0.2)   #was 3
ukde_back$NrArp_D = densapproxfactory_botev_lb(background$NrArp_D, min=mins["NrArp_D"], max=maxs["NrArp_D"], useweighted=TRUE, adjust=3, lbmult=0.2)   #was 3
ukde_back$SR = densapproxfactory_botev_lb(background$SR, min=mins["SR"], max=maxs["SR"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_back$Tourism = densapproxfactory_botev_lb(background$Tourism, min=mins["Tourism"], max=maxs["Tourism"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_back$WoodUnits = densapproxfactory_botev_lb(background$WoodUnits, min=mins["WoodUnits"], max=maxs["WoodUnits"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_back$WoodEmploy = densapproxfactory_botev_lb(background$WoodEmploy, min=mins["WoodEmploy"], max=maxs["WoodEmploy"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_back$runif = densapproxfactory_botev_lbrb(data=background$runif, min=mins["runif"], max=maxs["runif"], useweighted=T, adjust=1, bmult=0.2)

#Create holdout validation data 
crossvalidationdata <- createpartitions(data=reports, ptrain=0.8, ptest=0.1, ptune=0.1, times=75) 

###Fit presence models 
presmodels = lapply(crossvalidationdata,function(x) fitunivdens_pres(x$train)) #~10 minutes

###Fit a presence distribution for all variables combined, considering multiple possible distribution functions
allvars = modelperf2(pcvdata = crossvalidationdata, bdata = background, presmodels = presmodels, backmodel = ukde_back, variablenames=names(background)[-which(names(background)=="runif")])
allresults = t(data.frame(allvars[[2]])) 
row.names(allresults)[nrow(allresults)] = "All variables"
###/all variables combined


### Fit models for individual variables
singlevars = sapply(names(background), function(x)modelperf2(pcvdata = crossvalidationdata, bdata = background, presmodels = presmodels, backmodel = ukde_back, variablenames=x)[2])
singlevars = do.call(rbind, singlevars)


allresults = rbind(singlevars, allresults)
###/individual variables
##### /Step 1 #####


##### Step 2: variable selection for the subset model #####
# These code sections contain the final selection of variables for the subset.
# Note that to repeat the complete workflow as described in the paper, you need to add the variables listed below and iteratively (one by one) remove variables according to absolute variable importance (Figure 2b)
# For variable permutation importance: either function 'createVarImpPlot' or the code in the '2_Figures.R' script for figure 2b. The resulting VPI figure will indicate the variables to be dropped.
# Variables selected for the initial subset model: "GDP", "MFrghtU", "AFrghtU", "Tourism", "WoodUnits", "NrCty_D", "NrPrt_D", "POPD" ,"BIO10", "BIO11", "BIO15", "GDPp", "frstcv1", "SR", "OrPlHa",  "ACrpHa",  "PCrpHa", "NCrpHa", "runif"

subvarnames = c("Tourism", "WoodUnits", "NrCty_D","POPD","BIO10", "BIO11", "frstcv1", "runif")  
reportssub = reports[,subvarnames]
backgroundsub = background[,subvarnames]

suballdata = alldata[,subvarnames]
subukde_back = ukde_back[subvarnames]
##### /Step 2 #####





##### Step 3: subset model creation #####
subvarmodels = modelperf2(pcvdata = crossvalidationdata, bdata = background, presmodels = presmodels, backmodel = ukde_back, variablenames=subvarnames[-which(subvarnames=="runif")])

allresults = rbind(allresults, subvarmodels[[2]])
row.names(allresults)[nrow(allresults)] = "Subset"

# Final Model
subfinalpresmod_all = fitunivdens_pres(reports)
subfinalpresmod = subfinalpresmod_all[subvarnames]


# Final Model without random variable
finalpresmod = subfinalpresmod[-which(names(subfinalpresmod)=="runif")] 
finalbackmod = subukde_back[-which(names(subukde_back)=="runif")] 


# subukde_samegenera = ukde_samegenera[subvarnames]
# finalbackmod2 = subukde_samegenera[-which(names(subukde_samegenera)=="runif")] 
# 
# subukde_lepidoptera = ukde_lepidoptera[subvarnames]
# finalbackmod3 = subukde_lepidoptera[-which(names(subukde_lepidoptera)=="runif")] 
# 
# subukde_insecta = ukde_insecta[subvarnames]
# finalbackmod4 = subukde_insecta[-which(names(subukde_insecta)=="runif")] 


##### /Step 3 #####

allresults[order(allresults[,"AUC_mean"], decreasing=T),]





#### new step 4 ####
#Fit models for bias correction

###
### Same Genera
### 

###city
#fit samegenera_PA model as replacement for background functions. 
ukde_samegenera_city = vector(mode="list", length=0)
names(samegenera_city_PA)

ukde_samegenera_city$frstcv1 = densapproxfactory_botev_lb(samegenera_city_PA$frstcv1, min=mins["frstcv1"], max=maxs["frstcv1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO1 = densapproxfactory_botev_lb(samegenera_city_PA$BIO1, min=mins["BIO1"], max=maxs["BIO1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO2 = densapproxfactory_botev_lb(samegenera_city_PA$BIO2, min=mins["BIO2"], max=maxs["BIO2"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO3 = densapproxfactory_botev_lb(samegenera_city_PA$BIO3, min=mins["BIO3"], max=maxs["BIO3"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO4 = densapproxfactory_botev_lb(samegenera_city_PA$BIO4, min=mins["BIO4"], max=maxs["BIO4"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO5 = densapproxfactory_botev_lb(samegenera_city_PA$BIO5, min=mins["BIO5"], max=maxs["BIO5"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO6 = densapproxfactory_botev_lb(samegenera_city_PA$BIO6, min=mins["BIO6"], max=maxs["BIO6"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO7 = densapproxfactory_botev_lb(samegenera_city_PA$BIO7, min=mins["BIO7"], max=maxs["BIO7"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO8 = densapproxfactory_botev_lb(samegenera_city_PA$BIO8, min=mins["BIO8"], max=maxs["BIO8"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO9 = densapproxfactory_botev_lb(samegenera_city_PA$BIO9, min=mins["BIO9"], max=maxs["BIO9"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO10 = densapproxfactory_botev_lb(samegenera_city_PA$BIO10, min=mins["BIO10"], max=maxs["BIO10"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO11 = densapproxfactory_botev_lb(samegenera_city_PA$BIO11, min=mins["BIO11"], max=maxs["BIO11"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO12 = densapproxfactory_botev_lb(samegenera_city_PA$BIO12, min=mins["BIO12"], max=maxs["BIO12"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO13 = densapproxfactory_botev_lb(samegenera_city_PA$BIO13, min=mins["BIO13"], max=maxs["BIO13"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO14 = densapproxfactory_botev_lb(samegenera_city_PA$BIO14, min=mins["BIO14"], max=maxs["BIO14"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO15 = densapproxfactory_botev_lb(samegenera_city_PA$BIO15, min=mins["BIO15"], max=maxs["BIO15"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO16 = densapproxfactory_botev_lb(samegenera_city_PA$BIO16, min=mins["BIO16"], max=maxs["BIO16"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO17 = densapproxfactory_botev_lb(samegenera_city_PA$BIO17, min=mins["BIO17"], max=maxs["BIO17"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO18 = densapproxfactory_botev_lb(samegenera_city_PA$BIO18, min=mins["BIO18"], max=maxs["BIO18"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$BIO19 = densapproxfactory_botev_lb(samegenera_city_PA$BIO19, min=mins["BIO19"], max=maxs["BIO19"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$POPD = densapproxfactory_botev_lb(samegenera_city_PA$POPD, min=mins["POPD"], max=maxs["POPD"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$ACrpHa = densapproxfactory_botev_lb(samegenera_city_PA$ACrpHa, min=mins["ACrpHa"], max=maxs["ACrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_city$PCrpHa = densapproxfactory_botev_lb(samegenera_city_PA$PCrpHa, min=mins["PCrpHa"], max=maxs["PCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) #worst fit so far, but weird data
ukde_samegenera_city$NCrpHa = densapproxfactory_botev_lb(samegenera_city_PA$NCrpHa, min=mins["NCrpHa"], max=maxs["NCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_samegenera_city$OrPlHa = densapproxfactory_botev_lb(samegenera_city_PA$OrPlHa, min=mins["OrPlHa"], max=maxs["OrPlHa"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_samegenera_city$GDP = densapproxfactory_botev_lb(samegenera_city_PA$GDP, min=mins["GDP"], max=maxs["GDP"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_samegenera_city$GDPp = densapproxfactory_botev_lb(samegenera_city_PA$GDPp, min=mins["GDPp"], max=maxs["GDPp"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_samegenera_city$MFrghtU = densapproxfactory_botev_lb(samegenera_city_PA$MFrghtU, min=mins["MFrghtU"], max=maxs["MFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4) #complex fit, data highly irregular with zero inflation.  
ukde_samegenera_city$AFrghtU = densapproxfactory_botev_lb(samegenera_city_PA$AFrghtU, min=mins["AFrghtU"], max=maxs["AFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4)  
ukde_samegenera_city$NrCty_D = densapproxfactory_botev_lb(samegenera_city_PA$NrCty_D, min=mins["NrCty_D"], max=maxs["NrCty_D"], useweighted=TRUE, adjust=3, lbmult=0.2)   #was adjust=1 #was 3
ukde_samegenera_city$NrPrt_F = densapproxfactory_botev_lb(samegenera_city_PA$NrPrt_F, min=mins["NrPrt_F"], max=maxs["NrPrt_F"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_samegenera_city$NrPrt_D = densapproxfactory_botev_lb(samegenera_city_PA$NrPrt_D, min=mins["NrPrt_D"], max=maxs["NrPrt_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_samegenera_city$NrArp_D = densapproxfactory_botev_lb(samegenera_city_PA$NrArp_D, min=mins["NrArp_D"], max=maxs["NrArp_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_samegenera_city$SR = densapproxfactory_botev_lb(samegenera_city_PA$SR, min=mins["SR"], max=maxs["SR"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_samegenera_city$Tourism = densapproxfactory_botev_lb(samegenera_city_PA$Tourism, min=mins["Tourism"], max=maxs["Tourism"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_samegenera_city$WoodUnits = densapproxfactory_botev_lb(samegenera_city_PA$WoodUnits, min=mins["WoodUnits"], max=maxs["WoodUnits"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_samegenera_city$WoodEmploy = densapproxfactory_botev_lb(samegenera_city_PA$WoodEmploy, min=mins["WoodEmploy"], max=maxs["WoodEmploy"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_samegenera_city$runif = densapproxfactory_botev_lbrb(data=samegenera_city_PA$runif, min=mins["runif"], max=maxs["runif"], useweighted=T, adjust=1, bmult=0.2)

subukde_samegenera_city = ukde_samegenera_city[subvarnames]
#finalbackmod2 = subukde_samegenera[-which(names(subukde_samegenera)=="runif")] 


###popd
#fit samegenera_PA model as replacement for background functions. 
ukde_samegenera_popd = vector(mode="list", length=0)
names(samegenera_popd_PA)

ukde_samegenera_popd$frstcv1 = densapproxfactory_botev_lb(samegenera_popd_PA$frstcv1, min=mins["frstcv1"], max=maxs["frstcv1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO1 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO1, min=mins["BIO1"], max=maxs["BIO1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO2 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO2, min=mins["BIO2"], max=maxs["BIO2"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO3 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO3, min=mins["BIO3"], max=maxs["BIO3"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO4 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO4, min=mins["BIO4"], max=maxs["BIO4"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO5 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO5, min=mins["BIO5"], max=maxs["BIO5"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO6 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO6, min=mins["BIO6"], max=maxs["BIO6"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO7 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO7, min=mins["BIO7"], max=maxs["BIO7"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO8 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO8, min=mins["BIO8"], max=maxs["BIO8"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO9 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO9, min=mins["BIO9"], max=maxs["BIO9"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO10 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO10, min=mins["BIO10"], max=maxs["BIO10"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO11 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO11, min=mins["BIO11"], max=maxs["BIO11"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO12 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO12, min=mins["BIO12"], max=maxs["BIO12"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO13 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO13, min=mins["BIO13"], max=maxs["BIO13"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO14 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO14, min=mins["BIO14"], max=maxs["BIO14"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO15 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO15, min=mins["BIO15"], max=maxs["BIO15"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO16 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO16, min=mins["BIO16"], max=maxs["BIO16"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO17 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO17, min=mins["BIO17"], max=maxs["BIO17"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO18 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO18, min=mins["BIO18"], max=maxs["BIO18"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$BIO19 = densapproxfactory_botev_lb(samegenera_popd_PA$BIO19, min=mins["BIO19"], max=maxs["BIO19"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$POPD = densapproxfactory_botev_lb(samegenera_popd_PA$POPD, min=mins["POPD"], max=maxs["POPD"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$ACrpHa = densapproxfactory_botev_lb(samegenera_popd_PA$ACrpHa, min=mins["ACrpHa"], max=maxs["ACrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_popd$PCrpHa = densapproxfactory_botev_lb(samegenera_popd_PA$PCrpHa, min=mins["PCrpHa"], max=maxs["PCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) #worst fit so far, but weird data
ukde_samegenera_popd$NCrpHa = densapproxfactory_botev_lb(samegenera_popd_PA$NCrpHa, min=mins["NCrpHa"], max=maxs["NCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_samegenera_popd$OrPlHa = densapproxfactory_botev_lb(samegenera_popd_PA$OrPlHa, min=mins["OrPlHa"], max=maxs["OrPlHa"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_samegenera_popd$GDP = densapproxfactory_botev_lb(samegenera_popd_PA$GDP, min=mins["GDP"], max=maxs["GDP"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_samegenera_popd$GDPp = densapproxfactory_botev_lb(samegenera_popd_PA$GDPp, min=mins["GDPp"], max=maxs["GDPp"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_samegenera_popd$MFrghtU = densapproxfactory_botev_lb(samegenera_popd_PA$MFrghtU, min=mins["MFrghtU"], max=maxs["MFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4) #complex fit, data highly irregular with zero inflation.  
ukde_samegenera_popd$AFrghtU = densapproxfactory_botev_lb(samegenera_popd_PA$AFrghtU, min=mins["AFrghtU"], max=maxs["AFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4)  
ukde_samegenera_popd$NrCty_D = densapproxfactory_botev_lb(samegenera_popd_PA$NrCty_D, min=mins["NrCty_D"], max=maxs["NrCty_D"], useweighted=TRUE, adjust=3, lbmult=0.2)   #was adjust=1 #was 3
ukde_samegenera_popd$NrPrt_F = densapproxfactory_botev_lb(samegenera_popd_PA$NrPrt_F, min=mins["NrPrt_F"], max=maxs["NrPrt_F"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_samegenera_popd$NrPrt_D = densapproxfactory_botev_lb(samegenera_popd_PA$NrPrt_D, min=mins["NrPrt_D"], max=maxs["NrPrt_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_samegenera_popd$NrArp_D = densapproxfactory_botev_lb(samegenera_popd_PA$NrArp_D, min=mins["NrArp_D"], max=maxs["NrArp_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_samegenera_popd$SR = densapproxfactory_botev_lb(samegenera_popd_PA$SR, min=mins["SR"], max=maxs["SR"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_samegenera_popd$Tourism = densapproxfactory_botev_lb(samegenera_popd_PA$Tourism, min=mins["Tourism"], max=maxs["Tourism"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_samegenera_popd$WoodUnits = densapproxfactory_botev_lb(samegenera_popd_PA$WoodUnits, min=mins["WoodUnits"], max=maxs["WoodUnits"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_samegenera_popd$WoodEmploy = densapproxfactory_botev_lb(samegenera_popd_PA$WoodEmploy, min=mins["WoodEmploy"], max=maxs["WoodEmploy"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_samegenera_popd$runif = densapproxfactory_botev_lbrb(data=samegenera_popd_PA$runif, min=mins["runif"], max=maxs["runif"], useweighted=T, adjust=1, bmult=0.2)

subukde_samegenera_popd = ukde_samegenera_popd[subvarnames]
#finalbackmod2 = subukde_samegenera[-which(names(subukde_samegenera)=="runif")] 


###both
#fit samegenera_PA model as replacement for background functions. 
ukde_samegenera_both = vector(mode="list", length=0)
names(samegenera_both_PA)

ukde_samegenera_both$frstcv1 = densapproxfactory_botev_lb(samegenera_both_PA$frstcv1, min=mins["frstcv1"], max=maxs["frstcv1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO1 = densapproxfactory_botev_lb(samegenera_both_PA$BIO1, min=mins["BIO1"], max=maxs["BIO1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO2 = densapproxfactory_botev_lb(samegenera_both_PA$BIO2, min=mins["BIO2"], max=maxs["BIO2"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO3 = densapproxfactory_botev_lb(samegenera_both_PA$BIO3, min=mins["BIO3"], max=maxs["BIO3"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO4 = densapproxfactory_botev_lb(samegenera_both_PA$BIO4, min=mins["BIO4"], max=maxs["BIO4"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO5 = densapproxfactory_botev_lb(samegenera_both_PA$BIO5, min=mins["BIO5"], max=maxs["BIO5"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO6 = densapproxfactory_botev_lb(samegenera_both_PA$BIO6, min=mins["BIO6"], max=maxs["BIO6"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO7 = densapproxfactory_botev_lb(samegenera_both_PA$BIO7, min=mins["BIO7"], max=maxs["BIO7"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO8 = densapproxfactory_botev_lb(samegenera_both_PA$BIO8, min=mins["BIO8"], max=maxs["BIO8"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO9 = densapproxfactory_botev_lb(samegenera_both_PA$BIO9, min=mins["BIO9"], max=maxs["BIO9"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO10 = densapproxfactory_botev_lb(samegenera_both_PA$BIO10, min=mins["BIO10"], max=maxs["BIO10"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO11 = densapproxfactory_botev_lb(samegenera_both_PA$BIO11, min=mins["BIO11"], max=maxs["BIO11"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO12 = densapproxfactory_botev_lb(samegenera_both_PA$BIO12, min=mins["BIO12"], max=maxs["BIO12"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO13 = densapproxfactory_botev_lb(samegenera_both_PA$BIO13, min=mins["BIO13"], max=maxs["BIO13"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO14 = densapproxfactory_botev_lb(samegenera_both_PA$BIO14, min=mins["BIO14"], max=maxs["BIO14"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO15 = densapproxfactory_botev_lb(samegenera_both_PA$BIO15, min=mins["BIO15"], max=maxs["BIO15"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO16 = densapproxfactory_botev_lb(samegenera_both_PA$BIO16, min=mins["BIO16"], max=maxs["BIO16"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO17 = densapproxfactory_botev_lb(samegenera_both_PA$BIO17, min=mins["BIO17"], max=maxs["BIO17"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO18 = densapproxfactory_botev_lb(samegenera_both_PA$BIO18, min=mins["BIO18"], max=maxs["BIO18"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$BIO19 = densapproxfactory_botev_lb(samegenera_both_PA$BIO19, min=mins["BIO19"], max=maxs["BIO19"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$POPD = densapproxfactory_botev_lb(samegenera_both_PA$POPD, min=mins["POPD"], max=maxs["POPD"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$ACrpHa = densapproxfactory_botev_lb(samegenera_both_PA$ACrpHa, min=mins["ACrpHa"], max=maxs["ACrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_samegenera_both$PCrpHa = densapproxfactory_botev_lb(samegenera_both_PA$PCrpHa, min=mins["PCrpHa"], max=maxs["PCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) #worst fit so far, but weird data
ukde_samegenera_both$NCrpHa = densapproxfactory_botev_lb(samegenera_both_PA$NCrpHa, min=mins["NCrpHa"], max=maxs["NCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_samegenera_both$OrPlHa = densapproxfactory_botev_lb(samegenera_both_PA$OrPlHa, min=mins["OrPlHa"], max=maxs["OrPlHa"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_samegenera_both$GDP = densapproxfactory_botev_lb(samegenera_both_PA$GDP, min=mins["GDP"], max=maxs["GDP"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_samegenera_both$GDPp = densapproxfactory_botev_lb(samegenera_both_PA$GDPp, min=mins["GDPp"], max=maxs["GDPp"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_samegenera_both$MFrghtU = densapproxfactory_botev_lb(samegenera_both_PA$MFrghtU, min=mins["MFrghtU"], max=maxs["MFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4) #complex fit, data highly irregular with zero inflation.  
ukde_samegenera_both$AFrghtU = densapproxfactory_botev_lb(samegenera_both_PA$AFrghtU, min=mins["AFrghtU"], max=maxs["AFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4)  
ukde_samegenera_both$NrCty_D = densapproxfactory_botev_lb(samegenera_both_PA$NrCty_D, min=mins["NrCty_D"], max=maxs["NrCty_D"], useweighted=TRUE, adjust=3, lbmult=0.2)   #was adjust=1 #was 3
ukde_samegenera_both$NrPrt_F = densapproxfactory_botev_lb(samegenera_both_PA$NrPrt_F, min=mins["NrPrt_F"], max=maxs["NrPrt_F"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_samegenera_both$NrPrt_D = densapproxfactory_botev_lb(samegenera_both_PA$NrPrt_D, min=mins["NrPrt_D"], max=maxs["NrPrt_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_samegenera_both$NrArp_D = densapproxfactory_botev_lb(samegenera_both_PA$NrArp_D, min=mins["NrArp_D"], max=maxs["NrArp_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_samegenera_both$SR = densapproxfactory_botev_lb(samegenera_both_PA$SR, min=mins["SR"], max=maxs["SR"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_samegenera_both$Tourism = densapproxfactory_botev_lb(samegenera_both_PA$Tourism, min=mins["Tourism"], max=maxs["Tourism"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_samegenera_both$WoodUnits = densapproxfactory_botev_lb(samegenera_both_PA$WoodUnits, min=mins["WoodUnits"], max=maxs["WoodUnits"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_samegenera_both$WoodEmploy = densapproxfactory_botev_lb(samegenera_both_PA$WoodEmploy, min=mins["WoodEmploy"], max=maxs["WoodEmploy"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_samegenera_both$runif = densapproxfactory_botev_lbrb(data=samegenera_both_PA$runif, min=mins["runif"], max=maxs["runif"], useweighted=T, adjust=1, bmult=0.2)

subukde_samegenera_both = ukde_samegenera_both[subvarnames]
finalbackmod2 = subukde_samegenera_both[-which(names(subukde_samegenera_both)=="runif")] 





###
### Lepidoptera
### 

###City
#fit lepidoptera_PA model as replacement for background functions. 
ukde_lepidoptera_city = vector(mode="list", length=0)
names(lepidoptera_city_PA)

ukde_lepidoptera_city$frstcv1 = densapproxfactory_botev_lb(lepidoptera_city_PA$frstcv1, min=mins["frstcv1"], max=maxs["frstcv1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO1 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO1, min=mins["BIO1"], max=maxs["BIO1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO2 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO2, min=mins["BIO2"], max=maxs["BIO2"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO3 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO3, min=mins["BIO3"], max=maxs["BIO3"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO4 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO4, min=mins["BIO4"], max=maxs["BIO4"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO5 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO5, min=mins["BIO5"], max=maxs["BIO5"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO6 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO6, min=mins["BIO6"], max=maxs["BIO6"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO7 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO7, min=mins["BIO7"], max=maxs["BIO7"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO8 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO8, min=mins["BIO8"], max=maxs["BIO8"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO9 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO9, min=mins["BIO9"], max=maxs["BIO9"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO10 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO10, min=mins["BIO10"], max=maxs["BIO10"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO11 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO11, min=mins["BIO11"], max=maxs["BIO11"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO12 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO12, min=mins["BIO12"], max=maxs["BIO12"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO13 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO13, min=mins["BIO13"], max=maxs["BIO13"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO14 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO14, min=mins["BIO14"], max=maxs["BIO14"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO15 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO15, min=mins["BIO15"], max=maxs["BIO15"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO16 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO16, min=mins["BIO16"], max=maxs["BIO16"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO17 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO17, min=mins["BIO17"], max=maxs["BIO17"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO18 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO18, min=mins["BIO18"], max=maxs["BIO18"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$BIO19 = densapproxfactory_botev_lb(lepidoptera_city_PA$BIO19, min=mins["BIO19"], max=maxs["BIO19"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$POPD = densapproxfactory_botev_lb(lepidoptera_city_PA$POPD, min=mins["POPD"], max=maxs["POPD"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$ACrpHa = densapproxfactory_botev_lb(lepidoptera_city_PA$ACrpHa, min=mins["ACrpHa"], max=maxs["ACrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_city$PCrpHa = densapproxfactory_botev_lb(lepidoptera_city_PA$PCrpHa, min=mins["PCrpHa"], max=maxs["PCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) #worst fit so far, but weird data
ukde_lepidoptera_city$NCrpHa = densapproxfactory_botev_lb(lepidoptera_city_PA$NCrpHa, min=mins["NCrpHa"], max=maxs["NCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_lepidoptera_city$OrPlHa = densapproxfactory_botev_lb(lepidoptera_city_PA$OrPlHa, min=mins["OrPlHa"], max=maxs["OrPlHa"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_lepidoptera_city$GDP = densapproxfactory_botev_lb(lepidoptera_city_PA$GDP, min=mins["GDP"], max=maxs["GDP"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_lepidoptera_city$GDPp = densapproxfactory_botev_lb(lepidoptera_city_PA$GDPp, min=mins["GDPp"], max=maxs["GDPp"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_lepidoptera_city$MFrghtU = densapproxfactory_botev_lb(lepidoptera_city_PA$MFrghtU, min=mins["MFrghtU"], max=maxs["MFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4) #complex fit, data highly irregular with zero inflation.  
ukde_lepidoptera_city$AFrghtU = densapproxfactory_botev_lb(lepidoptera_city_PA$AFrghtU, min=mins["AFrghtU"], max=maxs["AFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4)  
ukde_lepidoptera_city$NrCty_D = densapproxfactory_botev_lb(lepidoptera_city_PA$NrCty_D, min=mins["NrCty_D"], max=maxs["NrCty_D"], useweighted=TRUE, adjust=3, lbmult=0.2)   #was adjust=1 #was 3
ukde_lepidoptera_city$NrPrt_F = densapproxfactory_botev_lb(lepidoptera_city_PA$NrPrt_F, min=mins["NrPrt_F"], max=maxs["NrPrt_F"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_lepidoptera_city$NrPrt_D = densapproxfactory_botev_lb(lepidoptera_city_PA$NrPrt_D, min=mins["NrPrt_D"], max=maxs["NrPrt_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_lepidoptera_city$NrArp_D = densapproxfactory_botev_lb(lepidoptera_city_PA$NrArp_D, min=mins["NrArp_D"], max=maxs["NrArp_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_lepidoptera_city$SR = densapproxfactory_botev_lb(lepidoptera_city_PA$SR, min=mins["SR"], max=maxs["SR"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_lepidoptera_city$Tourism = densapproxfactory_botev_lb(lepidoptera_city_PA$Tourism, min=mins["Tourism"], max=maxs["Tourism"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_lepidoptera_city$WoodUnits = densapproxfactory_botev_lb(lepidoptera_city_PA$WoodUnits, min=mins["WoodUnits"], max=maxs["WoodUnits"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_lepidoptera_city$WoodEmploy = densapproxfactory_botev_lb(lepidoptera_city_PA$WoodEmploy, min=mins["WoodEmploy"], max=maxs["WoodEmploy"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_lepidoptera_city$runif = densapproxfactory_botev_lbrb(data=lepidoptera_city_PA$runif, min=mins["runif"], max=maxs["runif"], useweighted=T, adjust=1, bmult=0.2)

subukde_lepidoptera_city = ukde_lepidoptera_city[subvarnames]
#finalbackmod3 = subukde_lepidoptera_city[-which(names(subukde_lepidoptera_city)=="runif")] 

###popd
#fit lepidoptera_PA model as replacement for background functions. 
ukde_lepidoptera_popd = vector(mode="list", length=0)
names(lepidoptera_popd_PA)

ukde_lepidoptera_popd$frstcv1 = densapproxfactory_botev_lb(lepidoptera_popd_PA$frstcv1, min=mins["frstcv1"], max=maxs["frstcv1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO1 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO1, min=mins["BIO1"], max=maxs["BIO1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO2 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO2, min=mins["BIO2"], max=maxs["BIO2"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO3 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO3, min=mins["BIO3"], max=maxs["BIO3"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO4 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO4, min=mins["BIO4"], max=maxs["BIO4"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO5 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO5, min=mins["BIO5"], max=maxs["BIO5"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO6 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO6, min=mins["BIO6"], max=maxs["BIO6"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO7 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO7, min=mins["BIO7"], max=maxs["BIO7"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO8 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO8, min=mins["BIO8"], max=maxs["BIO8"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO9 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO9, min=mins["BIO9"], max=maxs["BIO9"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO10 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO10, min=mins["BIO10"], max=maxs["BIO10"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO11 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO11, min=mins["BIO11"], max=maxs["BIO11"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO12 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO12, min=mins["BIO12"], max=maxs["BIO12"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO13 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO13, min=mins["BIO13"], max=maxs["BIO13"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO14 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO14, min=mins["BIO14"], max=maxs["BIO14"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO15 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO15, min=mins["BIO15"], max=maxs["BIO15"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO16 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO16, min=mins["BIO16"], max=maxs["BIO16"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO17 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO17, min=mins["BIO17"], max=maxs["BIO17"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO18 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO18, min=mins["BIO18"], max=maxs["BIO18"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$BIO19 = densapproxfactory_botev_lb(lepidoptera_popd_PA$BIO19, min=mins["BIO19"], max=maxs["BIO19"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$POPD = densapproxfactory_botev_lb(lepidoptera_popd_PA$POPD, min=mins["POPD"], max=maxs["POPD"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$ACrpHa = densapproxfactory_botev_lb(lepidoptera_popd_PA$ACrpHa, min=mins["ACrpHa"], max=maxs["ACrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_popd$PCrpHa = densapproxfactory_botev_lb(lepidoptera_popd_PA$PCrpHa, min=mins["PCrpHa"], max=maxs["PCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) #worst fit so far, but weird data
ukde_lepidoptera_popd$NCrpHa = densapproxfactory_botev_lb(lepidoptera_popd_PA$NCrpHa, min=mins["NCrpHa"], max=maxs["NCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_lepidoptera_popd$OrPlHa = densapproxfactory_botev_lb(lepidoptera_popd_PA$OrPlHa, min=mins["OrPlHa"], max=maxs["OrPlHa"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_lepidoptera_popd$GDP = densapproxfactory_botev_lb(lepidoptera_popd_PA$GDP, min=mins["GDP"], max=maxs["GDP"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_lepidoptera_popd$GDPp = densapproxfactory_botev_lb(lepidoptera_popd_PA$GDPp, min=mins["GDPp"], max=maxs["GDPp"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_lepidoptera_popd$MFrghtU = densapproxfactory_botev_lb(lepidoptera_popd_PA$MFrghtU, min=mins["MFrghtU"], max=maxs["MFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4) #complex fit, data highly irregular with zero inflation.  
ukde_lepidoptera_popd$AFrghtU = densapproxfactory_botev_lb(lepidoptera_popd_PA$AFrghtU, min=mins["AFrghtU"], max=maxs["AFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4)  
ukde_lepidoptera_popd$NrCty_D = densapproxfactory_botev_lb(lepidoptera_popd_PA$NrCty_D, min=mins["NrCty_D"], max=maxs["NrCty_D"], useweighted=TRUE, adjust=3, lbmult=0.2)   #was adjust=1 #was 3
ukde_lepidoptera_popd$NrPrt_F = densapproxfactory_botev_lb(lepidoptera_popd_PA$NrPrt_F, min=mins["NrPrt_F"], max=maxs["NrPrt_F"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_lepidoptera_popd$NrPrt_D = densapproxfactory_botev_lb(lepidoptera_popd_PA$NrPrt_D, min=mins["NrPrt_D"], max=maxs["NrPrt_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_lepidoptera_popd$NrArp_D = densapproxfactory_botev_lb(lepidoptera_popd_PA$NrArp_D, min=mins["NrArp_D"], max=maxs["NrArp_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_lepidoptera_popd$SR = densapproxfactory_botev_lb(lepidoptera_popd_PA$SR, min=mins["SR"], max=maxs["SR"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_lepidoptera_popd$Tourism = densapproxfactory_botev_lb(lepidoptera_popd_PA$Tourism, min=mins["Tourism"], max=maxs["Tourism"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_lepidoptera_popd$WoodUnits = densapproxfactory_botev_lb(lepidoptera_popd_PA$WoodUnits, min=mins["WoodUnits"], max=maxs["WoodUnits"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_lepidoptera_popd$WoodEmploy = densapproxfactory_botev_lb(lepidoptera_popd_PA$WoodEmploy, min=mins["WoodEmploy"], max=maxs["WoodEmploy"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_lepidoptera_popd$runif = densapproxfactory_botev_lbrb(data=lepidoptera_popd_PA$runif, min=mins["runif"], max=maxs["runif"], useweighted=T, adjust=1, bmult=0.2)

subukde_lepidoptera_popd = ukde_lepidoptera_popd[subvarnames]
#finalbackmod3 = subukde_lepidoptera_popd[-which(names(subukde_lepidoptera_popd)=="runif")] 


###both
#fit lepidoptera_PA model as replacement for background functions. 
ukde_lepidoptera_both = vector(mode="list", length=0)
names(lepidoptera_both_PA)

ukde_lepidoptera_both$frstcv1 = densapproxfactory_botev_lb(lepidoptera_both_PA$frstcv1, min=mins["frstcv1"], max=maxs["frstcv1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO1 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO1, min=mins["BIO1"], max=maxs["BIO1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO2 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO2, min=mins["BIO2"], max=maxs["BIO2"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO3 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO3, min=mins["BIO3"], max=maxs["BIO3"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO4 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO4, min=mins["BIO4"], max=maxs["BIO4"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO5 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO5, min=mins["BIO5"], max=maxs["BIO5"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO6 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO6, min=mins["BIO6"], max=maxs["BIO6"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO7 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO7, min=mins["BIO7"], max=maxs["BIO7"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO8 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO8, min=mins["BIO8"], max=maxs["BIO8"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO9 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO9, min=mins["BIO9"], max=maxs["BIO9"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO10 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO10, min=mins["BIO10"], max=maxs["BIO10"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO11 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO11, min=mins["BIO11"], max=maxs["BIO11"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO12 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO12, min=mins["BIO12"], max=maxs["BIO12"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO13 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO13, min=mins["BIO13"], max=maxs["BIO13"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO14 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO14, min=mins["BIO14"], max=maxs["BIO14"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO15 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO15, min=mins["BIO15"], max=maxs["BIO15"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO16 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO16, min=mins["BIO16"], max=maxs["BIO16"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO17 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO17, min=mins["BIO17"], max=maxs["BIO17"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO18 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO18, min=mins["BIO18"], max=maxs["BIO18"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$BIO19 = densapproxfactory_botev_lb(lepidoptera_both_PA$BIO19, min=mins["BIO19"], max=maxs["BIO19"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$POPD = densapproxfactory_botev_lb(lepidoptera_both_PA$POPD, min=mins["POPD"], max=maxs["POPD"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$ACrpHa = densapproxfactory_botev_lb(lepidoptera_both_PA$ACrpHa, min=mins["ACrpHa"], max=maxs["ACrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_lepidoptera_both$PCrpHa = densapproxfactory_botev_lb(lepidoptera_both_PA$PCrpHa, min=mins["PCrpHa"], max=maxs["PCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) #worst fit so far, but weird data
ukde_lepidoptera_both$NCrpHa = densapproxfactory_botev_lb(lepidoptera_both_PA$NCrpHa, min=mins["NCrpHa"], max=maxs["NCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_lepidoptera_both$OrPlHa = densapproxfactory_botev_lb(lepidoptera_both_PA$OrPlHa, min=mins["OrPlHa"], max=maxs["OrPlHa"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_lepidoptera_both$GDP = densapproxfactory_botev_lb(lepidoptera_both_PA$GDP, min=mins["GDP"], max=maxs["GDP"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_lepidoptera_both$GDPp = densapproxfactory_botev_lb(lepidoptera_both_PA$GDPp, min=mins["GDPp"], max=maxs["GDPp"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_lepidoptera_both$MFrghtU = densapproxfactory_botev_lb(lepidoptera_both_PA$MFrghtU, min=mins["MFrghtU"], max=maxs["MFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4) #complex fit, data highly irregular with zero inflation.  
ukde_lepidoptera_both$AFrghtU = densapproxfactory_botev_lb(lepidoptera_both_PA$AFrghtU, min=mins["AFrghtU"], max=maxs["AFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4)  
ukde_lepidoptera_both$NrCty_D = densapproxfactory_botev_lb(lepidoptera_both_PA$NrCty_D, min=mins["NrCty_D"], max=maxs["NrCty_D"], useweighted=TRUE, adjust=3, lbmult=0.2)   #was adjust=1 #was 3
ukde_lepidoptera_both$NrPrt_F = densapproxfactory_botev_lb(lepidoptera_both_PA$NrPrt_F, min=mins["NrPrt_F"], max=maxs["NrPrt_F"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_lepidoptera_both$NrPrt_D = densapproxfactory_botev_lb(lepidoptera_both_PA$NrPrt_D, min=mins["NrPrt_D"], max=maxs["NrPrt_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_lepidoptera_both$NrArp_D = densapproxfactory_botev_lb(lepidoptera_both_PA$NrArp_D, min=mins["NrArp_D"], max=maxs["NrArp_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_lepidoptera_both$SR = densapproxfactory_botev_lb(lepidoptera_both_PA$SR, min=mins["SR"], max=maxs["SR"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_lepidoptera_both$Tourism = densapproxfactory_botev_lb(lepidoptera_both_PA$Tourism, min=mins["Tourism"], max=maxs["Tourism"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_lepidoptera_both$WoodUnits = densapproxfactory_botev_lb(lepidoptera_both_PA$WoodUnits, min=mins["WoodUnits"], max=maxs["WoodUnits"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_lepidoptera_both$WoodEmploy = densapproxfactory_botev_lb(lepidoptera_both_PA$WoodEmploy, min=mins["WoodEmploy"], max=maxs["WoodEmploy"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_lepidoptera_both$runif = densapproxfactory_botev_lbrb(data=lepidoptera_both_PA$runif, min=mins["runif"], max=maxs["runif"], useweighted=T, adjust=1, bmult=0.2)

subukde_lepidoptera_both = ukde_lepidoptera_both[subvarnames]
finalbackmod3 = subukde_lepidoptera_both[-which(names(subukde_lepidoptera_both)=="runif")] 






###
### Insecta
### 

###City
#fit insecta_PA model as replacement for background functions. 
ukde_insecta_city = vector(mode="list", length=0)
names(insecta_city_PA)

ukde_insecta_city$frstcv1 = densapproxfactory_botev_lb(insecta_city_PA$frstcv1, min=mins["frstcv1"], max=maxs["frstcv1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO1 = densapproxfactory_botev_lb(insecta_city_PA$BIO1, min=mins["BIO1"], max=maxs["BIO1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO2 = densapproxfactory_botev_lb(insecta_city_PA$BIO2, min=mins["BIO2"], max=maxs["BIO2"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO3 = densapproxfactory_botev_lb(insecta_city_PA$BIO3, min=mins["BIO3"], max=maxs["BIO3"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO4 = densapproxfactory_botev_lb(insecta_city_PA$BIO4, min=mins["BIO4"], max=maxs["BIO4"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO5 = densapproxfactory_botev_lb(insecta_city_PA$BIO5, min=mins["BIO5"], max=maxs["BIO5"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO6 = densapproxfactory_botev_lb(insecta_city_PA$BIO6, min=mins["BIO6"], max=maxs["BIO6"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO7 = densapproxfactory_botev_lb(insecta_city_PA$BIO7, min=mins["BIO7"], max=maxs["BIO7"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO8 = densapproxfactory_botev_lb(insecta_city_PA$BIO8, min=mins["BIO8"], max=maxs["BIO8"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO9 = densapproxfactory_botev_lb(insecta_city_PA$BIO9, min=mins["BIO9"], max=maxs["BIO9"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO10 = densapproxfactory_botev_lb(insecta_city_PA$BIO10, min=mins["BIO10"], max=maxs["BIO10"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO11 = densapproxfactory_botev_lb(insecta_city_PA$BIO11, min=mins["BIO11"], max=maxs["BIO11"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO12 = densapproxfactory_botev_lb(insecta_city_PA$BIO12, min=mins["BIO12"], max=maxs["BIO12"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO13 = densapproxfactory_botev_lb(insecta_city_PA$BIO13, min=mins["BIO13"], max=maxs["BIO13"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO14 = densapproxfactory_botev_lb(insecta_city_PA$BIO14, min=mins["BIO14"], max=maxs["BIO14"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO15 = densapproxfactory_botev_lb(insecta_city_PA$BIO15, min=mins["BIO15"], max=maxs["BIO15"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO16 = densapproxfactory_botev_lb(insecta_city_PA$BIO16, min=mins["BIO16"], max=maxs["BIO16"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO17 = densapproxfactory_botev_lb(insecta_city_PA$BIO17, min=mins["BIO17"], max=maxs["BIO17"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO18 = densapproxfactory_botev_lb(insecta_city_PA$BIO18, min=mins["BIO18"], max=maxs["BIO18"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$BIO19 = densapproxfactory_botev_lb(insecta_city_PA$BIO19, min=mins["BIO19"], max=maxs["BIO19"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$POPD = densapproxfactory_botev_lb(insecta_city_PA$POPD, min=mins["POPD"], max=maxs["POPD"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$ACrpHa = densapproxfactory_botev_lb(insecta_city_PA$ACrpHa, min=mins["ACrpHa"], max=maxs["ACrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_city$PCrpHa = densapproxfactory_botev_lb(insecta_city_PA$PCrpHa, min=mins["PCrpHa"], max=maxs["PCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) #worst fit so far, but weird data
ukde_insecta_city$NCrpHa = densapproxfactory_botev_lb(insecta_city_PA$NCrpHa, min=mins["NCrpHa"], max=maxs["NCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_insecta_city$OrPlHa = densapproxfactory_botev_lb(insecta_city_PA$OrPlHa, min=mins["OrPlHa"], max=maxs["OrPlHa"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_insecta_city$GDP = densapproxfactory_botev_lb(insecta_city_PA$GDP, min=mins["GDP"], max=maxs["GDP"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_insecta_city$GDPp = densapproxfactory_botev_lb(insecta_city_PA$GDPp, min=mins["GDPp"], max=maxs["GDPp"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_insecta_city$MFrghtU = densapproxfactory_botev_lb(insecta_city_PA$MFrghtU, min=mins["MFrghtU"], max=maxs["MFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4) #complex fit, data highly irregular with zero inflation.  
ukde_insecta_city$AFrghtU = densapproxfactory_botev_lb(insecta_city_PA$AFrghtU, min=mins["AFrghtU"], max=maxs["AFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4)  
ukde_insecta_city$NrCty_D = densapproxfactory_botev_lb(insecta_city_PA$NrCty_D, min=mins["NrCty_D"], max=maxs["NrCty_D"], useweighted=TRUE, adjust=3, lbmult=0.2)   #was adjust=1 #was 3
ukde_insecta_city$NrPrt_F = densapproxfactory_botev_lb(insecta_city_PA$NrPrt_F, min=mins["NrPrt_F"], max=maxs["NrPrt_F"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_insecta_city$NrPrt_D = densapproxfactory_botev_lb(insecta_city_PA$NrPrt_D, min=mins["NrPrt_D"], max=maxs["NrPrt_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_insecta_city$NrArp_D = densapproxfactory_botev_lb(insecta_city_PA$NrArp_D, min=mins["NrArp_D"], max=maxs["NrArp_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_insecta_city$SR = densapproxfactory_botev_lb(insecta_city_PA$SR, min=mins["SR"], max=maxs["SR"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_insecta_city$Tourism = densapproxfactory_botev_lb(insecta_city_PA$Tourism, min=mins["Tourism"], max=maxs["Tourism"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_insecta_city$WoodUnits = densapproxfactory_botev_lb(insecta_city_PA$WoodUnits, min=mins["WoodUnits"], max=maxs["WoodUnits"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_insecta_city$WoodEmploy = densapproxfactory_botev_lb(insecta_city_PA$WoodEmploy, min=mins["WoodEmploy"], max=maxs["WoodEmploy"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_insecta_city$runif = densapproxfactory_botev_lbrb(data=insecta_city_PA$runif, min=mins["runif"], max=maxs["runif"], useweighted=T, adjust=1, bmult=0.2)

subukde_insecta_city = ukde_insecta_city[subvarnames]
#finalbackmod4 = subukde_insecta_city[-which(names(subukde_insecta_city)=="runif")] 

###popd
#fit insecta_PA model as replacement for background functions. 
ukde_insecta_popd = vector(mode="list", length=0)
names(insecta_popd_PA)

ukde_insecta_popd$frstcv1 = densapproxfactory_botev_lb(insecta_popd_PA$frstcv1, min=mins["frstcv1"], max=maxs["frstcv1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO1 = densapproxfactory_botev_lb(insecta_popd_PA$BIO1, min=mins["BIO1"], max=maxs["BIO1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO2 = densapproxfactory_botev_lb(insecta_popd_PA$BIO2, min=mins["BIO2"], max=maxs["BIO2"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO3 = densapproxfactory_botev_lb(insecta_popd_PA$BIO3, min=mins["BIO3"], max=maxs["BIO3"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO4 = densapproxfactory_botev_lb(insecta_popd_PA$BIO4, min=mins["BIO4"], max=maxs["BIO4"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO5 = densapproxfactory_botev_lb(insecta_popd_PA$BIO5, min=mins["BIO5"], max=maxs["BIO5"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO6 = densapproxfactory_botev_lb(insecta_popd_PA$BIO6, min=mins["BIO6"], max=maxs["BIO6"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO7 = densapproxfactory_botev_lb(insecta_popd_PA$BIO7, min=mins["BIO7"], max=maxs["BIO7"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO8 = densapproxfactory_botev_lb(insecta_popd_PA$BIO8, min=mins["BIO8"], max=maxs["BIO8"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO9 = densapproxfactory_botev_lb(insecta_popd_PA$BIO9, min=mins["BIO9"], max=maxs["BIO9"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO10 = densapproxfactory_botev_lb(insecta_popd_PA$BIO10, min=mins["BIO10"], max=maxs["BIO10"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO11 = densapproxfactory_botev_lb(insecta_popd_PA$BIO11, min=mins["BIO11"], max=maxs["BIO11"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO12 = densapproxfactory_botev_lb(insecta_popd_PA$BIO12, min=mins["BIO12"], max=maxs["BIO12"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO13 = densapproxfactory_botev_lb(insecta_popd_PA$BIO13, min=mins["BIO13"], max=maxs["BIO13"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO14 = densapproxfactory_botev_lb(insecta_popd_PA$BIO14, min=mins["BIO14"], max=maxs["BIO14"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO15 = densapproxfactory_botev_lb(insecta_popd_PA$BIO15, min=mins["BIO15"], max=maxs["BIO15"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO16 = densapproxfactory_botev_lb(insecta_popd_PA$BIO16, min=mins["BIO16"], max=maxs["BIO16"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO17 = densapproxfactory_botev_lb(insecta_popd_PA$BIO17, min=mins["BIO17"], max=maxs["BIO17"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO18 = densapproxfactory_botev_lb(insecta_popd_PA$BIO18, min=mins["BIO18"], max=maxs["BIO18"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$BIO19 = densapproxfactory_botev_lb(insecta_popd_PA$BIO19, min=mins["BIO19"], max=maxs["BIO19"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$POPD = densapproxfactory_botev_lb(insecta_popd_PA$POPD, min=mins["POPD"], max=maxs["POPD"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$ACrpHa = densapproxfactory_botev_lb(insecta_popd_PA$ACrpHa, min=mins["ACrpHa"], max=maxs["ACrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_popd$PCrpHa = densapproxfactory_botev_lb(insecta_popd_PA$PCrpHa, min=mins["PCrpHa"], max=maxs["PCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) #worst fit so far, but weird data
ukde_insecta_popd$NCrpHa = densapproxfactory_botev_lb(insecta_popd_PA$NCrpHa, min=mins["NCrpHa"], max=maxs["NCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_insecta_popd$OrPlHa = densapproxfactory_botev_lb(insecta_popd_PA$OrPlHa, min=mins["OrPlHa"], max=maxs["OrPlHa"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_insecta_popd$GDP = densapproxfactory_botev_lb(insecta_popd_PA$GDP, min=mins["GDP"], max=maxs["GDP"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_insecta_popd$GDPp = densapproxfactory_botev_lb(insecta_popd_PA$GDPp, min=mins["GDPp"], max=maxs["GDPp"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_insecta_popd$MFrghtU = densapproxfactory_botev_lb(insecta_popd_PA$MFrghtU, min=mins["MFrghtU"], max=maxs["MFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4) #complex fit, data highly irregular with zero inflation.  
ukde_insecta_popd$AFrghtU = densapproxfactory_botev_lb(insecta_popd_PA$AFrghtU, min=mins["AFrghtU"], max=maxs["AFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4)  
ukde_insecta_popd$NrCty_D = densapproxfactory_botev_lb(insecta_popd_PA$NrCty_D, min=mins["NrCty_D"], max=maxs["NrCty_D"], useweighted=TRUE, adjust=3, lbmult=0.2)   #was adjust=1 #was 3
ukde_insecta_popd$NrPrt_F = densapproxfactory_botev_lb(insecta_popd_PA$NrPrt_F, min=mins["NrPrt_F"], max=maxs["NrPrt_F"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_insecta_popd$NrPrt_D = densapproxfactory_botev_lb(insecta_popd_PA$NrPrt_D, min=mins["NrPrt_D"], max=maxs["NrPrt_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_insecta_popd$NrArp_D = densapproxfactory_botev_lb(insecta_popd_PA$NrArp_D, min=mins["NrArp_D"], max=maxs["NrArp_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_insecta_popd$SR = densapproxfactory_botev_lb(insecta_popd_PA$SR, min=mins["SR"], max=maxs["SR"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_insecta_popd$Tourism = densapproxfactory_botev_lb(insecta_popd_PA$Tourism, min=mins["Tourism"], max=maxs["Tourism"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_insecta_popd$WoodUnits = densapproxfactory_botev_lb(insecta_popd_PA$WoodUnits, min=mins["WoodUnits"], max=maxs["WoodUnits"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_insecta_popd$WoodEmploy = densapproxfactory_botev_lb(insecta_popd_PA$WoodEmploy, min=mins["WoodEmploy"], max=maxs["WoodEmploy"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_insecta_popd$runif = densapproxfactory_botev_lbrb(data=insecta_popd_PA$runif, min=mins["runif"], max=maxs["runif"], useweighted=T, adjust=1, bmult=0.2)

subukde_insecta_popd = ukde_insecta_popd[subvarnames]
#finalbackmod4 = subukde_insecta_popd[-which(names(subukde_insecta_popd)=="runif")] 

###both
#fit insecta_PA model as replacement for background functions. 
ukde_insecta_both = vector(mode="list", length=0)
names(insecta_both_PA)

ukde_insecta_both$frstcv1 = densapproxfactory_botev_lb(insecta_both_PA$frstcv1, min=mins["frstcv1"], max=maxs["frstcv1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO1 = densapproxfactory_botev_lb(insecta_both_PA$BIO1, min=mins["BIO1"], max=maxs["BIO1"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO2 = densapproxfactory_botev_lb(insecta_both_PA$BIO2, min=mins["BIO2"], max=maxs["BIO2"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO3 = densapproxfactory_botev_lb(insecta_both_PA$BIO3, min=mins["BIO3"], max=maxs["BIO3"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO4 = densapproxfactory_botev_lb(insecta_both_PA$BIO4, min=mins["BIO4"], max=maxs["BIO4"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO5 = densapproxfactory_botev_lb(insecta_both_PA$BIO5, min=mins["BIO5"], max=maxs["BIO5"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO6 = densapproxfactory_botev_lb(insecta_both_PA$BIO6, min=mins["BIO6"], max=maxs["BIO6"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO7 = densapproxfactory_botev_lb(insecta_both_PA$BIO7, min=mins["BIO7"], max=maxs["BIO7"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO8 = densapproxfactory_botev_lb(insecta_both_PA$BIO8, min=mins["BIO8"], max=maxs["BIO8"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO9 = densapproxfactory_botev_lb(insecta_both_PA$BIO9, min=mins["BIO9"], max=maxs["BIO9"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO10 = densapproxfactory_botev_lb(insecta_both_PA$BIO10, min=mins["BIO10"], max=maxs["BIO10"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO11 = densapproxfactory_botev_lb(insecta_both_PA$BIO11, min=mins["BIO11"], max=maxs["BIO11"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO12 = densapproxfactory_botev_lb(insecta_both_PA$BIO12, min=mins["BIO12"], max=maxs["BIO12"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO13 = densapproxfactory_botev_lb(insecta_both_PA$BIO13, min=mins["BIO13"], max=maxs["BIO13"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO14 = densapproxfactory_botev_lb(insecta_both_PA$BIO14, min=mins["BIO14"], max=maxs["BIO14"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO15 = densapproxfactory_botev_lb(insecta_both_PA$BIO15, min=mins["BIO15"], max=maxs["BIO15"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO16 = densapproxfactory_botev_lb(insecta_both_PA$BIO16, min=mins["BIO16"], max=maxs["BIO16"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO17 = densapproxfactory_botev_lb(insecta_both_PA$BIO17, min=mins["BIO17"], max=maxs["BIO17"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO18 = densapproxfactory_botev_lb(insecta_both_PA$BIO18, min=mins["BIO18"], max=maxs["BIO18"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$BIO19 = densapproxfactory_botev_lb(insecta_both_PA$BIO19, min=mins["BIO19"], max=maxs["BIO19"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$POPD = densapproxfactory_botev_lb(insecta_both_PA$POPD, min=mins["POPD"], max=maxs["POPD"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$ACrpHa = densapproxfactory_botev_lb(insecta_both_PA$ACrpHa, min=mins["ACrpHa"], max=maxs["ACrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2)
ukde_insecta_both$PCrpHa = densapproxfactory_botev_lb(insecta_both_PA$PCrpHa, min=mins["PCrpHa"], max=maxs["PCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) #worst fit so far, but weird data
ukde_insecta_both$NCrpHa = densapproxfactory_botev_lb(insecta_both_PA$NCrpHa, min=mins["NCrpHa"], max=maxs["NCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_insecta_both$OrPlHa = densapproxfactory_botev_lb(insecta_both_PA$OrPlHa, min=mins["OrPlHa"], max=maxs["OrPlHa"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_insecta_both$GDP = densapproxfactory_botev_lb(insecta_both_PA$GDP, min=mins["GDP"], max=maxs["GDP"], useweighted=TRUE, adjust=1, lbmult=0.2) 
ukde_insecta_both$GDPp = densapproxfactory_botev_lb(insecta_both_PA$GDPp, min=mins["GDPp"], max=maxs["GDPp"], useweighted=TRUE, adjust=1, lbmult=0.4) 
ukde_insecta_both$MFrghtU = densapproxfactory_botev_lb(insecta_both_PA$MFrghtU, min=mins["MFrghtU"], max=maxs["MFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4) #complex fit, data highly irregular with zero inflation.  
ukde_insecta_both$AFrghtU = densapproxfactory_botev_lb(insecta_both_PA$AFrghtU, min=mins["AFrghtU"], max=maxs["AFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.4)  
ukde_insecta_both$NrCty_D = densapproxfactory_botev_lb(insecta_both_PA$NrCty_D, min=mins["NrCty_D"], max=maxs["NrCty_D"], useweighted=TRUE, adjust=3, lbmult=0.2)   #was adjust=1 #was 3
ukde_insecta_both$NrPrt_F = densapproxfactory_botev_lb(insecta_both_PA$NrPrt_F, min=mins["NrPrt_F"], max=maxs["NrPrt_F"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_insecta_both$NrPrt_D = densapproxfactory_botev_lb(insecta_both_PA$NrPrt_D, min=mins["NrPrt_D"], max=maxs["NrPrt_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_insecta_both$NrArp_D = densapproxfactory_botev_lb(insecta_both_PA$NrArp_D, min=mins["NrArp_D"], max=maxs["NrArp_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
ukde_insecta_both$SR = densapproxfactory_botev_lb(insecta_both_PA$SR, min=mins["SR"], max=maxs["SR"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_insecta_both$Tourism = densapproxfactory_botev_lb(insecta_both_PA$Tourism, min=mins["Tourism"], max=maxs["Tourism"], useweighted=TRUE, adjust=1, lbmult=0.01)  
ukde_insecta_both$WoodUnits = densapproxfactory_botev_lb(insecta_both_PA$WoodUnits, min=mins["WoodUnits"], max=maxs["WoodUnits"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_insecta_both$WoodEmploy = densapproxfactory_botev_lb(insecta_both_PA$WoodEmploy, min=mins["WoodEmploy"], max=maxs["WoodEmploy"], useweighted=TRUE, adjust=1, lbmult=0.2)  
ukde_insecta_both$runif = densapproxfactory_botev_lbrb(data=insecta_both_PA$runif, min=mins["runif"], max=maxs["runif"], useweighted=T, adjust=1, bmult=0.2)

subukde_insecta_both = ukde_insecta_both[subvarnames]
finalbackmod4 = subukde_insecta_both[-which(names(subukde_insecta_both)=="runif")] 



# -> Keep the environment loaded and run 2_Figures.R

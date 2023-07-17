###### Data generation script for the paper 'Introduction hotspots of non-native tree pests and the role of cities' by
###### Robbert T. van den Dool, Alejandro Morales, Wopke van der Werf & J.C. (Bob) Douma
###### Last edited 5 June 2023 - Robbert T. van den Dool

# Script to create new species observation data (PA points) using a model of sampling effort based on different GBIF citizen science datasets.
# This model is only based on two spatial predictors instead of all predictors to mitigate the impact of geographical biases existing in the GBIF database. 

##### Preparation #####
#rm(list=ls(all=TRUE))

#load packages
if(!require("easypackages")) install.packages("easypackages")
library(easypackages)
packages("sf", "caret", "rgdal", "provenance", "kdevine", "sp", "raster", "rstudioapi", "sgt", "pROC", "MASS", "ggplot2", "rnaturalearth", "rnaturalearthdata", "gridExtra", "furrr", prompt = FALSE)
set.seed(123456789)

#load functions
source("Scripts/Functions.R")

#load data
background_1m = readRDS(file="Intermediate/background_maps.rds")
samples = readRDS(file="Intermediate/samegenera.rds")
background = readRDS(file="Intermediate/background.rds")

background_1m_nogeo = st_drop_geometry(background_1m)
samples_nogeo = st_drop_geometry(samples)
background_nogeo =  st_drop_geometry(background)
  
lepidoptera1 = readRDS(file="Intermediate/lepidoptera_1.rds")
lepidoptera2 = readRDS(file="Intermediate/lepidoptera_2.rds")
lepidoptera = rbind(lepidoptera1, lepidoptera2)

lepidoptera_nogeo =  st_drop_geometry(lepidoptera); rm(lepidoptera1, lepidoptera2)
lepidoptera_samp = lepidoptera[sample(nrow(lepidoptera),size=50000),] 
lepidoptera_samp_nogeo = st_drop_geometry(lepidoptera_samp)

insecta1 = readRDS(file="Intermediate/insecta_1.rds")
insecta2 = readRDS(file="Intermediate/insecta_2.rds")
insecta3 = readRDS(file="Intermediate/insecta_3.rds")
insecta4 = readRDS(file="Intermediate/insecta_4.rds")
insecta = rbind(insecta1, insecta2, insecta3, insecta4)

insecta_nogeo =  st_drop_geometry(insecta); rm(insecta1, insecta2, insecta3, insecta4)
insecta_samp = insecta[sample(nrow(insecta),size=50000),] 
insecta_samp_nogeo = st_drop_geometry(insecta_samp)

mins <-  sapply(background_1m_nogeo, function(x) as.numeric(min(x, na.rm=T))) #-0.05*min(x) 
maxs <-  sapply(background_1m_nogeo, function(x) as.numeric(max(x, na.rm=T))) #+0.05*min(x) 


#This script generates 9 datasets by considering 3 GBIF data sources (same genera, lepidoptera, insecta),
#and 3 variable combinations for sampling (distance to city, population density, both variables): 
#SG_city, SG_popd, SG_both, L_city, L_popd, L_both, I_city, I_popd, I_both


###
### Same Genera
###
# fit kernel density functions
samegenera_dens_city = densapproxfactory_botev_lb(data=samples_nogeo$NrCty_D, min=mins["NrCty_D"], max=maxs["NrCty_D"], useweighted=F, adjust=3, lbmult=0.2 )
samegenera_dens_popd = densapproxfactory_botev_lb(data=samples_nogeo$POPD, min=mins["POPD"], max=maxs["POPD"], useweighted=F, adjust=20, lbmult=0.2 )

background_dens_city = densapproxfactory_botev_lb(background$NrCty_D, min=mins["NrCty_D"], max=maxs["NrCty_D"], useweighted=TRUE, adjust=3, lbmult=0.2)  
background_dens_popd = densapproxfactory_botev_lb(background$POPD, min=mins["POPD"], max=maxs["POPD"], useweighted=TRUE, adjust=0.5, lbmult=0.2)  

#cdfs 
samegenera_cdf_city = BSDMapproxPKDE2(samegenera_dens_city, min=mins["NrCty_D"], max=maxs["NrCty_D"])
samegenera_cdf_popd = BSDMapproxPKDE2(samegenera_dens_popd, min=mins["POPD"], max=maxs["POPD"])

background_cdf_city = BSDMapproxPKDE2(background_dens_city, min=mins["NrCty_D"], max=maxs["NrCty_D"])
background_cdf_popd = BSDMapproxPKDE2(background_dens_popd, min=mins["POPD"], max=maxs["POPD"])

#pobs
sgpobs = data.frame(NrCty_D = samegenera_cdf_city(samples_nogeo$NrCty_D), POPD = samegenera_cdf_popd(samples_nogeo$POPD))
bgpobs = data.frame(NrCty_D = background_cdf_city(background_nogeo$NrCty_D), POPD = background_cdf_popd(background_nogeo$POPD))

#non-parametric copula
sgcop = rvinecopulib::vinecop(sgpobs, family_set = "nonparametric", mult=1); sgcop
bgcop = rvinecopulib::vinecop(bgpobs, family_set = "nonparametric", mult=3); bgcop

#prediction function
sg_city_predfunct = function(x){
  pdens = log(samegenera_dens_city(x)) 
  bdens = log(background_dens_city(x)) 
  prob = exp(pdens-bdens)
}

sg_popd_predfunct = function(x){
  pdens = log(samegenera_dens_popd(x))
  bdens = log(background_dens_popd(x)) 
  prob = exp(pdens-bdens)
}

sg_both_predfunct = function(x1,x2){
  pu = data.frame(NrCty_D = samegenera_cdf_city(x1), POPD = samegenera_cdf_popd(x2))
  bu = data.frame(NrCty_D = background_cdf_city(x1), POPD = background_cdf_popd(x2))
  pdens = log(samegenera_dens_city(x1)) + log(samegenera_dens_popd(x2)) + log(rvinecopulib::dvinecop(u = pu, vinecop = sgcop))
  bdens = log(background_dens_city(x1)) + log(background_dens_popd(x2)) + log(rvinecopulib::dvinecop(u = bu, vinecop = bgcop))
  prob = exp(pdens-bdens)
}

#predict 
sg_city_preds = sg_city_predfunct(x = background_1m_nogeo$NrCty_D)  
sg_popd_preds = sg_popd_predfunct(x = background_1m_nogeo$POPD)  
sg_both_preds = sg_both_predfunct(x1 = background_1m_nogeo$NrCty_D, x2 = background_1m_nogeo$POPD) 

#sample
sg_city_data <- background_1m[sample(length(sg_city_preds), size=10000, replace=T, prob=sg_city_preds),]
sg_popd_data <- background_1m[sample(length(sg_popd_preds), size=10000, replace=T, prob=sg_popd_preds),]
sg_both_data <- background_1m[sample(length(sg_both_preds), size=10000, replace=T, prob=sg_both_preds),]

#evaluate

hist(sg_both_data$NrCty_D, n=100, prob=T)
curve(samegenera_dens_city(x),add=T, lwd=2, col="red")
curve(background_dens_city(x), add=T, lwd=2)

hist(sg_both_PAdata$POPD, n=100, prob=T)
curve(samegenera_dens_popd(x),add=T, lwd=2, col="red")
curve(background_dens_popd(x), add=T, lwd=2)

plot(ecdf(sg_both_data$NrCty_D))
curve(samegenera_cdf_city(x), add=T, col="red")

plot(ecdf(sg_city_data$NrCty_D))
curve(samegenera_cdf_city(x), add=T, col="red")



plot(ecdf(samples$NrCty_D))
curve(samegenera_cdf_city(x), add=T, col="red")

plot(ecdf(sg_both_data$POPD))
curve(samegenera_cdf_popd(x), add=T, col="red")

#save PA data
saveRDS(sg_city_data, file="Intermediate/SG_city.rds")
saveRDS(sg_popd_data, file="Intermediate/SG_popd.rds")
saveRDS(sg_both_data, file="Intermediate/SG_both.rds")






### 
### Lepidoptera
###

#dens functions
lepidoptera_dens_city = densapproxfactory_botev_lb(data=lepidoptera_samp_nogeo$NrCty_D, min=mins["NrCty_D"], max=maxs["NrCty_D"], useweighted=F, adjust=3, lbmult=0.2 )
lepidoptera_dens_popd = densapproxfactory_botev_lb(data=lepidoptera_samp_nogeo$POPD, min=mins["POPD"], max=maxs["POPD"], useweighted=F, adjust=20, lbmult=0.2 )

#cdfs 
lepidoptera_cdf_city = BSDMapproxPKDE2(lepidoptera_dens_city, min=mins["NrCty_D"], max=maxs["NrCty_D"])
lepidoptera_cdf_popd = BSDMapproxPKDE2(lepidoptera_dens_popd, min=mins["POPD"], max=maxs["POPD"])

#pobs
lpobs = data.frame(NrCty_D = lepidoptera_cdf_city(lepidoptera_samp_nogeo$NrCty_D), POPD = lepidoptera_cdf_popd(lepidoptera_samp_nogeo$POPD))

#non-parametric copula
lcop = rvinecopulib::vinecop(lpobs, family_set = "nonparametric", mult=1); lcop

#prediction function
l_city_predfunct = function(x){
  pdens = log(lepidoptera_dens_city(x)) 
  bdens = log(background_dens_city(x)) 
  prob = exp(pdens-bdens)
}

l_popd_predfunct = function(x){
  pdens = log(lepidoptera_dens_popd(x))
  bdens = log(background_dens_popd(x)) 
  prob = exp(pdens-bdens)
}

l_both_predfunct = function(x1,x2){
  pu = data.frame(NrCty_D = lepidoptera_cdf_city(x1), POPD = lepidoptera_cdf_popd(x2))
  bu = data.frame(NrCty_D = background_cdf_city(x1), POPD = background_cdf_popd(x2))
  pdens = log(lepidoptera_dens_city(x1)) + log(lepidoptera_dens_popd(x2)) + log(rvinecopulib::dvinecop(u = pu, vinecop = lcop))
  bdens = log(background_dens_city(x1)) + log(background_dens_popd(x2)) + log(rvinecopulib::dvinecop(u = bu, vinecop = bgcop))
  prob = exp(pdens-bdens)
}

#predict 
l_city_preds = l_city_predfunct(x = background_1m_nogeo$NrCty_D)  
l_popd_preds = l_popd_predfunct(x = background_1m_nogeo$POPD)  
l_both_preds = l_both_predfunct(x1 = background_1m_nogeo$NrCty_D, x2 = background_1m_nogeo$POPD) 

#sample
l_city_data <- background_1m[sample(length(l_city_preds), size=10000, replace=T, prob=l_city_preds),]
l_popd_data <- background_1m[sample(length(l_popd_preds), size=10000, replace=T, prob=l_popd_preds),]
l_both_data <- background_1m[sample(length(l_both_preds), size=10000, replace=T, prob=l_both_preds),]

#evaluate
hist(l_both_data$NrCty_D, n=100, prob=T)
curve(lepidoptera_dens_city(x),add=T, lwd=2, col="red")
curve(background_dens_city(x), add=T, lwd=2)

hist(l_both_data$POPD, n=100, prob=T)
curve(lepidoptera_dens_popd(x),add=T, lwd=2, col="red")
curve(background_dens_popd(x), add=T, lwd=2)

plot(ecdf(l_both_data$NrCty_D))
curve(lepidoptera_cdf_city(x), add=T, col="red")

plot(ecdf(l_both_data$POPD))
curve(lepidoptera_cdf_popd(x), add=T, col="red")

#save PA data
saveRDS(l_city_data, file="Intermediate/L_city.rds")
saveRDS(l_popd_data, file="Intermediate/L_popd.rds")
saveRDS(l_both_data, file="Intermediate/L_both.rds")



### 
### Insecta
###

#dens functions
insecta_dens_city = densapproxfactory_botev_lb(data=insecta_samp_nogeo$NrCty_D, min=mins["NrCty_D"], max=maxs["NrCty_D"], useweighted=F, adjust=3, lbmult=0.2 )
insecta_dens_popd = densapproxfactory_botev_lb(data=insecta_samp_nogeo$POPD, min=mins["POPD"], max=maxs["POPD"], useweighted=F, adjust=20, lbmult=0.2 )

#cdfs 
insecta_cdf_city = BSDMapproxPKDE2(insecta_dens_city, min=mins["NrCty_D"], max=maxs["NrCty_D"])
insecta_cdf_popd = BSDMapproxPKDE2(insecta_dens_popd, min=mins["POPD"], max=maxs["POPD"])

#pobs
ipobs = data.frame(NrCty_D = insecta_cdf_city(insecta_samp_nogeo$NrCty_D), POPD = insecta_cdf_popd(insecta_samp_nogeo$POPD))

#non-parametric copula
icop = rvinecopulib::vinecop(ipobs, family_set = "nonparametric", mult=1); icop

#prediction function
i_city_predfunct = function(x){
  pdens = log(insecta_dens_city(x)) 
  bdens = log(background_dens_city(x)) 
  prob = exp(pdens-bdens)
}

i_popd_predfunct = function(x){
  pdens = log(insecta_dens_popd(x))
  bdens = log(background_dens_popd(x)) 
  prob = exp(pdens-bdens)
}

i_both_predfunct = function(x1,x2){
  pu = data.frame(NrCty_D = insecta_cdf_city(x1), POPD = insecta_cdf_popd(x2))
  bu = data.frame(NrCty_D = background_cdf_city(x1), POPD = background_cdf_popd(x2))
  pdens = log(insecta_dens_city(x1)) + log(insecta_dens_popd(x2)) + log(rvinecopulib::dvinecop(u = pu, vinecop = icop))
  bdens = log(background_dens_city(x1)) + log(background_dens_popd(x2)) + log(rvinecopulib::dvinecop(u = bu, vinecop = bgcop))
  prob = exp(pdens-bdens)
}

#predict 
i_city_preds = i_city_predfunct(x = background_1m_nogeo$NrCty_D)  
i_popd_preds = i_popd_predfunct(x = background_1m_nogeo$POPD)  
i_both_preds = i_both_predfunct(x1 = background_1m_nogeo$NrCty_D, x2 = background_1m_nogeo$POPD) 

#sample
i_city_data <- background_1m[sample(length(i_city_preds), size=10000, replace=T, prob=i_city_preds),]
i_popd_data <- background_1m[sample(length(i_popd_preds), size=10000, replace=T, prob=i_popd_preds),]
i_both_data <- background_1m[sample(length(i_both_preds), size=10000, replace=T, prob=i_both_preds),]

#evaluate
hist(i_both_data$NrCty_D, n=100, prob=T)
curve(insecta_dens_city(x),add=T, lwd=2, col="red")
curve(background_dens_city(x), add=T, lwd=2)

hist(i_both_data$POPD, n=100, prob=T)
curve(insecta_dens_popd(x),add=T, lwd=2, col="red")
curve(background_dens_popd(x), add=T, lwd=2)

plot(ecdf(i_both_data$NrCty_D))
curve(insecta_cdf_city(x), add=T, col="red")

plot(ecdf(i_both_data$POPD))
curve(insecta_cdf_popd(x), add=T, col="red")

#save PA data
saveRDS(i_city_data, file="Intermediate/I_city.rds")
saveRDS(i_popd_data, file="Intermediate/I_popd.rds")
saveRDS(i_both_data, file="Intermediate/I_both.rds")


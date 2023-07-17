###### Functions script for the paper 'Introduction hotspots of non-native tree pests and the role of cities' by
###### Robbert T. van den Dool, Alejandro Morales, Wopke van der Werf & J.C. (Bob) Douma
###### Last edited 17 July 2023 - Robbert T. van den Dool

# This script contains all the code for the functions used in the other scripts. 


 
######################## MAIN FUNCTIONS ##########################

# Creates data partitions based on repeated holdout validation. The portions that should go to each type of data can be set as well as 
# the number of repetitions. 
# returns list, eachlistitem has a named list with: traindf, tunedf, testdf
createpartitions <- function(data, ptrain, ptest, times, ptune=0){
  
  outputlist =  vector(mode="list", length=times)
  outputlist = lapply(outputlist, function(x) list(train=NULL, tune=NULL, test=NULL))
  
  dnrow = nrow(data)
  
  for (i in 1:times){
    
    permutation = sample(1:dnrow,size=dnrow, replace=FALSE)
    
    nrtrain = floor(ptrain*dnrow) 
    nrtest =  ceiling(ptest*dnrow) 
    
    trainindex = permutation[1:nrtrain]
    outputlist[[i]]$train = data.frame(data[trainindex,])
    names(outputlist[[i]]$train) = names(data)
    
    testindex = permutation[(nrtrain+1):(nrtest+nrtrain)]
    outputlist[[i]]$test = data.frame(data[testindex,])
    names(outputlist[[i]]$test) = names(data)
    
    if(ptune != 0){
      nrtune = ceiling((ptest+ptune)*dnrow-nrtest) 
      tuneindex = permutation[(nrtest+nrtrain+1):(nrtest+nrtrain+nrtune)]
      outputlist[[i]]$tune = data.frame(data[tuneindex,])
      names(outputlist[[i]]$tune) = names(data)
    }
  }
  
  return(outputlist)  
  
}




# Sub function used in variable permutation importance. Will randomly shuffle data for a variable and determine performance of a model on the new data.
permutevar <- function(cvpresdata, backdata, model, nvars, variable){
  
  output = data.frame("id" = seq_along(cvpresdata), 
                      "NLLp" = rep(0, length(cvpresdata)), 
                      "AUC" = rep(0, length(cvpresdata)))
  for (i in seq_along(cvpresdata)){
    
    presdata = cvpresdata[[i]]$test
    combdata = rbind(presdata, backdata)
    combdata$label = c(rep(1,nrow(presdata)),rep(0,nrow(backdata)))
    combdata[,variable] = sample(combdata[,variable])
    
    prob = model(combdata[,1:nvars]) #currently not flexible to other setups.
    psummed = sum(prob)
    presprob = prob[combdata$label==1] 
    presprobscaled = presprob / psummed
    
    logloss = -sum(log(presprobscaled))
    auc = pROC::auc(response = combdata$label, predictor = prob) 
    output[i,2:3] = c(logloss, auc)
  }
  return(output)
  
}


# approximated high speed density method for kernel density estimation, with correction for unique values. There were many duplicated datapoints because of the regional structure of the data (NUTS2 regions).
# this led to default density functions with too high variance
densapproxfactory <- function(data, min, max, adjust=1){
  
  xvals = seq(min, max, length.out=512)
  bw = density(unique(as.numeric(row.names(table(data)))), bw = "SJ", weights=table((data))/length(data)) 
  kde = kdevine::kde1d(data, xmin = min, xmax = max, bw = bw$bw, mult = adjust)
  kded = kdevine::dkde1d(xvals, kde)
  kded[kded==0] = min(kded[kded>0]) #new line to prevent zero densities
  
  #combined = cbind(data, kded)
  #combined = combined[order(data),]
  
  funct = function(x){
    value = stats::approx(x = xvals, y = kded, xout = x, rule = 2, ties = "ordered")$y #x = combined[,1], y = combined[,2], xout = x, rule = 2, ties = "ordered")$y
    return(value)
  }
  
  return(funct)
  
}

# approximated high speed density method, without correction. Included for the 'treecover' variable, since there is no zero inflated version available
# and with correction the bandwidth is estimated too high for a well fitting density function. 
densapproxfactory_old <- function(data, min, max, adjust=1){
  
  xvals = seq(min, max, length.out=512)
  kde = kdevine::kde1d(data, xmin = min, xmax = max, mult = adjust)
  kded = kdevine::dkde1d(xvals, kde)
  kded[kded==0] = min(kded[kded>0]) #new line to prevent zero densities.
  
  # combined = cbind(data, kded)
  # combined = combined[order(data),]
  
  funct = function(x){
    #combined = combined
    value = stats::approx(x = xvals, y = kded, xout = x, rule = 2, ties = "ordered")$y #x = combined[,1], y = combined[,2]
    return(value)
  }
  
  return(funct)
  
}

#function to fit a kernel density function to each variable in the data
fitunivdens = function(data,mins,maxs, adjust=1){
  ncols = ncol(data)
  list = vector(mode="list", length=ncols)
  
  for (i in seq_len(ncols)){
    list[[i]] = densapproxfactory(data[,i], min = mins[i], max = maxs[i], adjust=adjust)
  }
  
  names(list) = names(data)
  
  return(list)
}

#Makes predictions given data and a model
predictunivdens = function(data,model){ 
  
  nvar = length(model)
  denslist = vector(mode="list", length=nvar)
  
  denslist = lapply(names(data), function(i){ #was:  seq_len(nvar)
    model[[i]](data[,i])}
  )
  
  output = Reduce("*", denslist) 
  
  return(output)
}


#Predicts with the full BayeSDM model, given some data, a presence model and background model
predictBsDM = function(data, presmod, backmod){
  presdens = predictunivdens(data, model=presmod)
  backdens = predictunivdens(data, model=backmod)
  prob = presdens/backdens
  return(prob)
} 


modelperf = function(pcvdata, bdata, presmodels, backmodel){
  
  nrrep = length(pcvdata)  
  
  output1 = data.frame("NLLp" = rep(0,nrrep), "AUC" = rep(0, nrrep))  
  output2 = c("NLLp_upper" = 0, "NLLp_mean" = 0, "NLLp_lower" = 0, "AUC_upper" = 0, "AUC_mean" = 0, "AUC_lower" = 0)
  
  for  (i in seq_along(pcvdata)){
    
    pprob = predictBsDM(data = pcvdata[[i]]$test, presmod = presmodels[[i]], backmod = backmodel)
    bprob = predictBsDM(data = bdata, presmod = presmodels[[i]], backmod = backmodel)
    pprob[is.infinite(pprob)] = NA #check for infinite values sometimes occuring on runif for GB2
    bprob[is.infinite(bprob)] = NA
    
    psummed = sum(pprob, na.rm=T) + sum(bprob, na.rm=T)
    pprobscaled = pprob / psummed
    
    logloss = -sum(log(pprobscaled))
    auc = pROC::auc(response = c(rep(1, length(pprob)), rep(0, length(bprob))), predictor = c(pprob, bprob), quiet=TRUE, na.rm=TRUE) 
    output1[i,] = c(logloss, auc)
  }
  
  output2[1:3] =as.numeric(myCI(output1$NLLp, ci=0.95)) #was mean(output1$NLLp)
  output2[4:6] = as.numeric(myCI(output1$AUC, ci=0.95))
  output = list(output1, output2)
  
  return(output)
  
}

#Do it all function for single variable performance 
BSDM1varperf <- function(varname, pdata, bdata, alldata, backgroundmodels, ptrain, ptest, ptune, times){
  print(paste("working on", varname))
  varname = varname
  pdata = pdata
  bdata = bdata
  ptrain = ptrain
  ptest = ptest
  ptune = ptune
  times = times
  alldata = alldata
  
  pdata_1v = data.frame(pdata[,varname])
  names(pdata_1v) = varname
  
  bdata_1v = data.frame(bdata[,varname])
  names(bdata_1v) = varname
  
  alldata = rbind(pdata_1v, bdata_1v)
  mins =  min(alldata)
  maxs =  max(alldata) 
  
  #background model
  backmodel = backgroundmodels[varname]
  
  #cv data
  cvdata = createpartitions(data=pdata_1v, ptrain=ptrain, ptest=ptest, ptune=ptune, times=times) #271
  
  #Try out models on each split, return NLLtest and parameters
  cvfits = list(crossvalidatemarg(cvdata, varname, alldata=alldata))
  names(cvfits) = varname
  
  #Transform results to different format
  cvfitssplit = lapply(seq_len(times),function(y){
    
    temp = lapply(names(cvfits),function(x){cvfits[[x]][y,]});
    names(temp) = names(cvfits);
    return(temp)
    
  })
  
  #get best fits for each split
  
  cvpresmodels = lapply(cvfitssplit, createpresmodel)
  
  #apply best fits to test data of each split
  
  perf = modelperf(pcvdata = cvdata, bdata = bdata_1v, presmodels = cvpresmodels, backmodel = backmodel)
  names(perf)[1] = varname
  names(perf)[2] = varname
  
  output = append(perf,cvfits)
  
  return(output)
}


myCI = function(x, ci=0.95){
  a <- mean(x, na.rm=T)
  s <- sd(x, na.rm=T)
  n <- length(x[!is.na(x)])
  error <- qt(ci + (1 - ci)/2, df = n - 1) * s/sqrt(n)
  return(c(upper = a + error, mean = a, lower = a - error))
}


#permutation function
permutevarFinal = function(presdata, backdata, pmodel, bmodel, variable, repeats){
  output = data.frame("id" = seq_len(repeats), 
                      "NLLp" = rep(0, repeats), 
                      "AUC" = rep(0, repeats))
  
  combdata = rbind(presdata, backdata)
  combdata$label = c(rep(1,nrow(presdata)),rep(0,nrow(backdata)))
  
  results = furrr::future_map(seq_len(repeats),.options = furrr::furrr_options(seed = TRUE),function(x){
    combdata[,variable] = sample(combdata[,variable])
    
    #added because the function requests it otherwise...
    dGB2 
    dnbinomZI
    dGB2ZI
    #end of added
    
    prob = predictBsDM(data = combdata[,-ncol(combdata)],presmod = pmodel, backmod = bmodel)   
    psummed = sum(prob, na.rm=T)
    presprob = prob[combdata$label==1] 
    presprobscaled = presprob / psummed
    probscaled = prob/psummed
    
    logloss = -sum(log(presprobscaled), na.rm=T)
    auc = pROC::auc(response = combdata$label, predictor = probscaled, quiet=TRUE, na.rm=TRUE) 
    c(logloss, auc)
    
  } )
  
  output[,2:3] = do.call(rbind.data.frame, results)
  return(output)
}



BSDMapproxPKDE <- function(xnew, densityfunction, alldata, min, max ){
  
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



#
### PLOTTING FUNCTIONS ###
#

corplotfunct = function(data){
  
  mydata <- data
  
  cormat <- round(cor(mydata, method="spearman"),2)
  head(cormat)
  
  library(reshape2)
  melted_cormat <- melt(cormat)
  head(melted_cormat)
  
  library(ggplot2)
  ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()
  
  # Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  upper_tri <- get_upper_tri(cormat)
  upper_tri
  
  
  # Melt the correlation matrix
  library(reshape2)
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  # Heatmap
  library(ggplot2)
  ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 10, hjust = 1))+
    coord_fixed()
  
  reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
  }
  
  
  # Reorder the correlation matrix
  cormat <- reorder_cormat(cormat)
  upper_tri <- get_upper_tri(cormat)
  # Melt the correlation matrix
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  # Create a ggheatmap
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Spearman\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 10, hjust = 1))+
    coord_fixed()
  # Print the heatmap
  print(ggheatmap)
  
  highly_correlated <- melted_cormat[melted_cormat[,3] > 0.7 | melted_cormat[,3] < -0.7,]
  
  ggheatmap + 
    geom_text(data=highly_correlated, aes(Var2, Var1, label = value), color = "black", size = 2.2) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  
}

plotAssessFit = function(data, model, alldata, variable, prefix){
  
  data = data[,variable]
  alldataz = alldata[,variable]
  min = min(alldataz)
  max = max(alldataz)
  xlabvar = nameconvertfunct(variable)
  name = paste(prefix,variable,"_fit.tiff",sep="")
  tiff(name, width = 4, height = 5, units = 'in', res = 600)
  plot(ecdf(data), main="", lwd=3,xlab=xlabvar)
  curve(BSDMapproxPKDE(xnew = x, densityfunction = model[[variable]], alldata=alldataz, min = min, max = max), xlim=c(min,max), add=T, lwd=2, col="red")
  dev.off()
}

plotResponseMarg = function(variable, presmod, backmod, npres, alldata, prefix="", cex=1){
  data = alldata[,variable]
  min = min(data)
  max = max(data)
  
  responsefunct = function(x){
    presmod[[variable]](x) / backmod[[variable]](x) 
  }
  
  xlabvar = nameconvertfunct(variable)
  presdata = data[seq_len(npres)]
  backdata = data[npres:length(data)]
  
  name = paste(prefix,variable,"_ResponseM.tiff",sep="")
  tiff(name, width = 4, height = 5, units = 'in', res = 600)
  curve(responsefunct(x), xlim=c(min,max), xlab=xlabvar, cex.lab=cex, cex.axis=cex, ylab="Relative probability")
  rug(backdata, ticksize = 0.03, side = 1, lwd = 0.5, col = par("fg"))
  rug(presdata, ticksize = 0.03, side = 3, lwd = 0.5, col = "green")
  dev.off()
}

plotResponseCond = function(variable, presmod, backmod, alldata, backdata, prefix="", cex=1){
  
  alldataz = alldata
  min = min(alldata[,variable])
  max = max(alldata[,variable])
  
  meddata = backdata[1,]
  
  sapply(names(backdata), function(y){ 
    meddata[1,y] <<- median(backdata[,y])
  })
  
  responsefunct = function(x){
    reps = rep(1, length(x))
    tempdata = meddata[reps,]
    tempdata[,variable] = x
    
    predictBsDM(data = tempdata, presmod = presmod, backmod = backmod)
  } 
  
  name = paste(prefix,variable,"_ResponseC.tiff",sep="")
  tiff(name, width = 4, height = 5, units = 'in', res = 600)
  curve(responsefunct(x), xlim=c(min,max), xlab = variable, cex.lab=cex, cex.axis=cex, ylab="Relative probability")
  dev.off()
}

createVarImpPlot <- function(varnames, presdata, backdata, pmodel, bmodel, repeats, prefix, origAUC, origNLLp, namestable){
  
  #Calculate VarImp
  VarImp = future_map(varnames,.options = furrr::furrr_options(seed = TRUE),function(x) permutevarFinal(presdata = presdata, backdata = backdata, pmodel = pmodel, bmodel = bmodel, variable = x,repeats = repeats))
  names(VarImp) = lapply(varnames, nameconvertfunct)
  
  
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
  
  tiff(name, width = 6, height = 6, units = 'in', res = 300)
  par(mgp = c(3,0.5,0),mar=c(2.1,11.1,1.1,1.1))
  VPIplot = barplot(VarImp_AUC_relativesort, horiz=T, las=1, xlim=c(-0.1, 1.1),  main="AUC", xpd=T)
  arrows(upper_sort,VPIplot, lower_sort, VPIplot, angle=90, code=3, length=0.05)
  
  dev.off()
  
  
  
  #NLLp VPI barplot
  VarImp_NLLp = lapply(VarImp, function(x) myCI(x$NLLp - origNLLp)) #increase in NLLp when variable is shuffled.#higher means more important
  VarImp_NLLp_min = min(sapply(VarImp_NLLp, function(x)x[2]),na.rm = T)
  VarImp_NLLp_max = max(sapply(VarImp_NLLp, function(x)x[2]),na.rm = T)
  VarImp_NLLp_relative = lapply(VarImp_NLLp , function(y) (y - VarImp_NLLp_min)/ (VarImp_NLLp_max-VarImp_NLLp_min)) 
  VarImp_NLLp_relativesort = sapply(VarImp_NLLp_relative, function(x)x[2])
  names(VarImp_NLLp_relativesort) = names(VarImp)
  
  upper =   sapply(VarImp_NLLp_relative, function(x)x[1])
  upper_sort = upper[order(VarImp_NLLp_relativesort)]
  
  lower =   sapply(VarImp_NLLp_relative, function(x)x[3])
  lower_sort = lower[order(VarImp_NLLp_relativesort)]
  
  VarImp_NLLp_relativesort = sort(VarImp_NLLp_relativesort)
  
  name = paste(prefix,"_VPI_NLLp",".tiff",sep="")
  tiff(name, width = 6, height = 6, units = 'in', res = 300)
  par(mgp = c(3,0.5,0),mar=c(2.1,11.1,1.1,1.1))
  VPIplot = barplot(VarImp_NLLp_relativesort, horiz=T, las=1, xlim=c(-0.05, 1.1), main="NLLp", xpd=T)
  arrows(upper_sort,VPIplot, lower_sort, VPIplot, angle=90, code=3, length=0.05)
  dev.off()
}

createVarImpPlotAbsolute <- function(varnames, presdata, backdata, pmodel, bmodel, repeats, prefix, origAUC, origNLLp){
  
  #Calculate VarImp
  VarImp = future_map(varnames,.options = furrr::furrr_options(seed = TRUE),function(x) permutevarFinal(presdata = presdata, backdata = backdata, pmodel = pmodel, bmodel = bmodel, variable = x,repeats = repeats))
  names(VarImp) = lapply(varnames, nameconvertfunct)
  
  #AUC VPI barplot
  VarImp_AUC = lapply(VarImp, function(x) myCI(origAUC - x$AUC)) #how much is AUC higher with the variable not shuffled?
  VarImp_AUC_min = min(sapply(VarImp_AUC, function(x)x[2]),na.rm = T)
  VarImp_AUC_max = max(sapply(VarImp_AUC, function(x)x[2]),na.rm = T)
  
  VarImp_AUC_sort = sapply(VarImp_AUC, function(x)x[2])
  names(VarImp_AUC_sort) = names(VarImp)
  
  upper =   sapply(VarImp_AUC, function(x)x[1])
  upper_sort = upper[order(VarImp_AUC_sort)]
  
  lower =   sapply(VarImp_AUC, function(x)x[3])
  lower_sort = lower[order(VarImp_AUC_sort)]
  
  VarImp_AUC_sort = sort(VarImp_AUC_sort)
  
  name = paste(prefix,"_VPI_AUC",".tiff",sep="")
  tiff(name, width = 4, height = 6, units = 'in', res = 600)
  par(mgp = c(3,0.5,0),mar=c(2.1,11.1,1.1,1.1))
  VPIplot = barplot(VarImp_AUC_sort, horiz=T, las=1, xlim=c(VarImp_AUC_min, VarImp_AUC_max),  main="AUC", xpd=T)
  arrows(upper_sort,VPIplot, lower_sort, VPIplot, angle=90, code=3, length=0.05)
  dev.off()
  
  
  
  #NLLp VPI barplot
  VarImp_NLLp = lapply(VarImp, function(x) myCI(x$NLLp - origNLLp)) #increase in NLLp when variable is shuffled.#higher means more important
  VarImp_NLLp_min = min(sapply(VarImp_NLLp, function(x)x[2]),na.rm = T)
  VarImp_NLLp_max = max(sapply(VarImp_NLLp, function(x)x[2]),na.rm = T)
  
  VarImp_NLLp_sort = sapply(VarImp_NLLp, function(x)x[2])
  names(VarImp_NLLp_sort) = names(VarImp)
  
  upper =   sapply(VarImp_NLLp, function(x)x[1])
  upper_sort = upper[order(VarImp_NLLp_sort)]
  
  lower =   sapply(VarImp_NLLp, function(x)x[3])
  lower_sort = lower[order(VarImp_NLLp_sort)]
  
  VarImp_NLLp_sort = sort(VarImp_NLLp_sort)
  
  name = paste(prefix,"_VPI_NLLp",".tiff",sep="")
  tiff(name, width = 4, height = 6, units = 'in', res = 600)
  par(mgp = c(3,0.5,0),mar=c(2.1,11.1,1.1,1.1))
  VPIplot = barplot(VarImp_NLLp_sort, horiz=T, las=1, xlim=c(VarImp_NLLp_min, VarImp_NLLp_max),main="NLLp", xpd=T)
  arrows(upper_sort,VPIplot, lower_sort, VPIplot, angle=90, code=3, length=0.05)
  dev.off()
}


#
### NEW DISTRIBUTION FUNCTIONS 
#

########### VarImp difference 
createVarImpPlotDifference <- function(varnames, presdata, backdata, pmodel, bmodel, bmodel2, repeats, prefix, origAUC, origNLLp, namestable){
  
  #Calculate VarImp
  VarImp = future_map(varnames,.options = furrr::furrr_options(seed = TRUE),function(x) permutevarFinal(presdata = presdata, backdata = backdata, pmodel = pmodel, bmodel = bmodel, variable = x,repeats = repeats))
  names(VarImp) = lapply(varnames, nameconvertfunct)
  
  VarImp2 = future_map(varnames,.options = furrr::furrr_options(seed = TRUE),function(x) permutevarFinal(presdata = presdata, backdata = backdata, pmodel = pmodel, bmodel = bmodel2, variable = x,repeats = repeats))
  names(VarImp2) = lapply(varnames, nameconvertfunct)
  
  
  #AUC VPI barplot
  VarImp_AUC = lapply(VarImp, function(x) myCI(origAUC - x$AUC)) #how much is AUC higher with the variable not shuffled?
  VarImp2_AUC = lapply(VarImp2, function(x) myCI(origAUC - x$AUC)) #how much is AUC higher with the variable not shuffled?
  
  
  
  VarImp_AUC_min = min(sapply(VarImp_AUC, function(x)x[2]))
  VarImp_AUC_max = max(sapply(VarImp_AUC, function(x)x[2]))
  VarImp_AUC_sort = sapply(VarImp_AUC, function(x)x[2])
  names(VarImp_AUC_sort) = names(VarImp)
  
  upper =   sapply(VarImp_AUC, function(x)x[1])
  upper_sort = upper[order(VarImp_AUC_sort)]
  
  lower =   sapply(VarImp_AUC, function(x)x[3])
  lower_sort = lower[order(VarImp_AUC_sort)]
  
  VarImp_AUC_sort = sort(VarImp_AUC_sort)
  
  name = paste(prefix,"_VPI_AUC",".tiff",sep="")
  
  tiff(name, width = 6, height = 6, units = 'in', res = 300)
  par(mgp = c(3,0.5,0),mar=c(2.1,11.1,1.1,1.1))
  VPIplot = barplot(VarImp_AUC_relativesort, horiz=T, las=1, xlim=c(-0.1, 1.1),  main="AUC", xpd=T)
  arrows(upper_sort,VPIplot, lower_sort, VPIplot, angle=90, code=3, length=0.05)
  dev.off()
  
 
  #NLLp VPI barplot
  VarImp_NLLp = lapply(VarImp, function(x) myCI(x$NLLp - origNLLp)) #increase in NLLp when variable is shuffled.#higher means more important
  VarImp_NLLp_min = min(sapply(VarImp_NLLp, function(x)x[2]))
  VarImp_NLLp_max = max(sapply(VarImp_NLLp, function(x)x[2]))
  VarImp_NLLp_relative = lapply(VarImp_NLLp , function(y) (y - VarImp_NLLp_min)/ (VarImp_NLLp_max-VarImp_NLLp_min)) 
  VarImp_NLLp_relativesort = sapply(VarImp_NLLp_relative, function(x)x[2])
  names(VarImp_NLLp_relativesort) = names(VarImp)
  
  upper =   sapply(VarImp_NLLp_relative, function(x)x[1])
  upper_sort = upper[order(VarImp_NLLp_relativesort)]
  
  lower =   sapply(VarImp_NLLp_relative, function(x)x[3])
  lower_sort = lower[order(VarImp_NLLp_relativesort)]
  
  VarImp_NLLp_relativesort = sort(VarImp_NLLp_relativesort)
  
  name = paste(prefix,"_VPI_NLLp",".tiff",sep="")
  tiff(name, width = 6, height = 6, units = 'in', res = 300)
  par(mgp = c(3,0.5,0),mar=c(2.1,11.1,1.1,1.1))
  VPIplot = barplot(VarImp_NLLp_relativesort, horiz=T, las=1, xlim=c(-0.05, 1.1), main="NLLp", xpd=T)
  arrows(upper_sort,VPIplot, lower_sort, VPIplot, angle=90, code=3, length=0.05)
  dev.off()
}


densapproxfactory_botev =  function(data, min, max, log=FALSE, useweighted=FALSE){
  
  bw=NA
  
  if(useweighted){
    bw = density(unique(as.numeric(row.names(table(data)))), bw = "SJ", weights=table((data))/length(data))$bw 
  } #if useweighted is on, it will no longer use botev2010 to estimate the bandwidth. This will prevent severe overfitting in regional variables. 
  
  xvals = seq(min, max, length.out=512)
  #bw = density(unique(as.numeric(row.names(table(data)))), bw = "SJ", weights=table((data))/length(data)) 
  kde = provenance::KDE(x=data, from = min, to = max, bw=bw, adaptive=T)
  #kded = kdevine::dkde1d(xvals, kde)
  #kded[kded==0] = min(kded[kded>0]) #new line to prevent zero densities
  
  #combined = cbind(data, kded)
  #combined = combined[order(data),]
  
  funct = function(x){
    value = stats::approx(x = kde$x, y = kde$y, xout = x, rule = 2, ties = "ordered")$y #x = combined[,1], y = combined[,2], xout = x, rule = 2, ties = "ordered")$y
    return(value)
  }
  
  return(funct)
  
}

BSDMapproxPKDE2 = function(densityfunction, min, max){
  
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
  return(function(x){approx(zx, c(0,cump), x, rule=2, ties="ordered")$y})
}

densapproxfactory_botev_lb = function(data, min, max, useweighted=FALSE, adjust=1, lbmult=0.2){ #includes mirroring negative bounded kernels.
  
  if(useweighted){
    bw = density(unique(as.numeric(row.names(table(data)))), bw = "SJ", weights=table((data))/length(data), adjust=adjust)$bw 
  }else{bw=provenance::botev(data)*adjust} #if useweighted is on, it will no longer use botev2010 to estimate the bandwidth. This will prevent severe overfitting in regional variables. 
  
  lowbound = min-lbmult*(max-min)
  #upperbound = max+0.1*(max-min)
  
  kde = provenance::KDE(x=data, from = lowbound, to= max , bw=bw, adaptive=T, n=ceiling(512*(1+lbmult))) #614
  
  negx = kde$x[kde$x < min] 
  negy = kde$y[kde$x < min] 
  newx = kde$x[kde$x >= min]
  newy = kde$y[kde$x >= min]
  newy[1:length(negx)] = newy[1:length(negx)] + rev(negy)
  
  dx = diff(kde$x)[1]
  total = sum(rowMeans(cbind(kde$y[-1], kde$y[-length(kde$y)]))*dx)
  newy = newy/total
  
  funct = function(x){
    value = stats::approx(x = newx, y = newy, xout = x, rule = 2, ties = "ordered")$y #x = combined[,1], y = combined[,2], xout = x, rule = 2, ties = "ordered")$y
    return(value)
  }
  
  return(funct)
}

makeplots = function(data, variable, useweighted=FALSE, adjust=1, lbmult=0.2){
  
  dat= data[,variable]
  min= mins[variable]
  max= maxs[variable]
  
  densfunct = densapproxfactory_botev_lb(data=dat, min=min, max=max, useweighted=useweighted, adjust=adjust, lbmult=lbmult)
  densfunct_cdf = BSDMapproxPKDE2(densfunct,min=min, max=max)
  
  dens_plot = function(){
    hist(data[,variable], xlim=c(min,max), main="", xlab=variable, prob=T, n=50)
    curve(densfunct(x),add=T, col="red", lwd=2)
  }
  
  ecdf_plot = function(){
    plot(ecdf(data[,variable]),main="", xlab=variable, col="grey")
    curve(densfunct_cdf,add=T, col="red",lwd=2)
  }
  
  plot1 = as.grob(dens_plot)
  plot2 = as.grob(ecdf_plot)
  
  tiff(paste("Output/Z2_plots_", variable, ".tiff"), width = 10, height = 5, units = 'in', res = 900, compression="lzw")
  plot = plot_grid(plot1, plot2,
                   nrow=1, label_size = 12, axis='l', hjust=0, label_x = 0.1, #align = "v", #axis = "b", #rel_widths = c(1), #label_x = .3, 
                   labels = c(paste('A)', "density"),
                              paste('B)', "cumulative probability")
                   )) 
  
  grid.arrange(arrangeGrob(plot))
  dev.off()
}

densapproxfactory_botev_lbrb = function(data, min, max, useweighted=FALSE, adjust=1, bmult=0.2){ #includes mirroring negative bounded kernels.
  
  if(useweighted){
    bw = density(unique(as.numeric(row.names(table(data)))), bw = "SJ", weights=table((data))/length(data), adjust=adjust)$bw 
  }else{bw=provenance::botev(data)*adjust} #if useweighted is on, it will no longer use botev2010 to estimate the bandwidth. This will prevent severe overfitting in regional variables. 
  
  lowbound = min-bmult*(max-min)
  upperbound = max+bmult*(max-min)
  
  kde = provenance::KDE(x=data, from = lowbound, to= upperbound , bw=bw, adaptive=T, n=ceiling(512*(1+2*bmult))) #614
  
  negx = kde$x[kde$x < min] 
  negy = kde$y[kde$x < min] 
  newx = kde$x[kde$x >= min & kde$x <= max]
  newy = kde$y[kde$x >= min & kde$x <= max]
  newy[1:length(negx)] = newy[1:length(negx)] + rev(negy)
  posx = kde$x[kde$x > max] 
  posy = kde$y[kde$x > max] 
  newy[(length(newx)-length(posx)+1):length(newx)] = rev(newy)[1:length(posx)] + posy
  
  dx = diff(kde$x)[1]
  total = sum(rowMeans(cbind(kde$y[-1], kde$y[-length(kde$y)]))*dx)
  newy = newy/total
  
  funct = function(x){
    value = stats::approx(x = newx, y = newy, xout = x, rule = 2, ties = "ordered")$y #x = combined[,1], y = combined[,2], xout = x, rule = 2, ties = "ordered")$y
    return(value)
  }
  
  return(funct)
}


fitunivdens_pres = function(cvdata){
  
  ncols = ncol(data)
  ukde_pres = vector(mode="list", length=0)
  
  ukde_pres$frstcv1 = densapproxfactory_botev_lb(cvdata$frstcv1, min=mins["frstcv1"], max=maxs["frstcv1"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO1 = densapproxfactory_botev_lb(cvdata$BIO1, min=mins["BIO1"], max=maxs["BIO1"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO2 = densapproxfactory_botev_lb(cvdata$BIO2, min=mins["BIO2"], max=maxs["BIO2"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO3 = densapproxfactory_botev_lb(cvdata$BIO3, min=mins["BIO3"], max=maxs["BIO3"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO4 = densapproxfactory_botev_lb(cvdata$BIO4, min=mins["BIO4"], max=maxs["BIO4"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO5 = densapproxfactory_botev_lb(cvdata$BIO5, min=mins["BIO5"], max=maxs["BIO5"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO6 = densapproxfactory_botev_lb(cvdata$BIO6, min=mins["BIO6"], max=maxs["BIO6"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO7 = densapproxfactory_botev_lb(cvdata$BIO7, min=mins["BIO7"], max=maxs["BIO7"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO8 = densapproxfactory_botev_lb(cvdata$BIO8, min=mins["BIO8"], max=maxs["BIO8"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO9 = densapproxfactory_botev_lb(cvdata$BIO9, min=mins["BIO9"], max=maxs["BIO9"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO10 = densapproxfactory_botev_lb(cvdata$BIO10, min=mins["BIO10"], max=maxs["BIO10"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO11 = densapproxfactory_botev_lb(cvdata$BIO11, min=mins["BIO11"], max=maxs["BIO11"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO12 = densapproxfactory_botev_lb(cvdata$BIO12, min=mins["BIO12"], max=maxs["BIO12"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO13 = densapproxfactory_botev_lb(cvdata$BIO13, min=mins["BIO13"], max=maxs["BIO13"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO14 = densapproxfactory_botev_lb(cvdata$BIO14, min=mins["BIO14"], max=maxs["BIO14"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO15 = densapproxfactory_botev_lb(cvdata$BIO15, min=mins["BIO15"], max=maxs["BIO15"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO16 = densapproxfactory_botev_lb(cvdata$BIO16, min=mins["BIO16"], max=maxs["BIO16"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO17 = densapproxfactory_botev_lb(cvdata$BIO17, min=mins["BIO17"], max=maxs["BIO17"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO18 = densapproxfactory_botev_lb(cvdata$BIO18, min=mins["BIO18"], max=maxs["BIO18"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$BIO19 = densapproxfactory_botev_lb(cvdata$BIO19, min=mins["BIO19"], max=maxs["BIO19"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$POPD = densapproxfactory_botev_lb(cvdata$POPD, min=mins["POPD"], max=maxs["POPD"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$ACrpHa = densapproxfactory_botev_lb(cvdata$ACrpHa, min=mins["ACrpHa"], max=maxs["ACrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2)
  ukde_pres$PCrpHa = densapproxfactory_botev_lb(cvdata$PCrpHa, min=mins["PCrpHa"], max=maxs["PCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) #worst fit so far, but weird data
  ukde_pres$NCrpHa = densapproxfactory_botev_lb(cvdata$NCrpHa, min=mins["NCrpHa"], max=maxs["NCrpHa"], useweighted=TRUE, adjust=1, lbmult=0.2) 
  ukde_pres$OrPlHa = densapproxfactory_botev_lb(cvdata$OrPlHa, min=mins["OrPlHa"], max=maxs["OrPlHa"], useweighted=TRUE, adjust=1, lbmult=0.4) 
  ukde_pres$GDP = densapproxfactory_botev_lb(cvdata$GDP, min=mins["GDP"], max=maxs["GDP"], useweighted=TRUE, adjust=1, lbmult=0.2) 
  ukde_pres$GDPp = densapproxfactory_botev_lb(cvdata$GDPp, min=mins["GDPp"], max=maxs["GDPp"], useweighted=TRUE, adjust=1, lbmult=0.4) 
  ukde_pres$MFrghtU = densapproxfactory_botev_lb(cvdata$MFrghtU, min=mins["MFrghtU"], max=maxs["MFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.065) #complex fit, data highly irregular with zero inflation.  
  ukde_pres$AFrghtU = densapproxfactory_botev_lb(cvdata$AFrghtU, min=mins["AFrghtU"], max=maxs["AFrghtU"], useweighted=TRUE, adjust=1, lbmult=0.065)  
  ukde_pres$NrCty_D = densapproxfactory_botev_lb(cvdata$NrCty_D, min=mins["NrCty_D"], max=maxs["NrCty_D"], useweighted=TRUE, adjust=3, lbmult=0.2)   #was 3, leftbound was not accurate
  ukde_pres$NrPrt_F = densapproxfactory_botev_lb(cvdata$NrPrt_F, min=mins["NrPrt_F"], max=maxs["NrPrt_F"], useweighted=TRUE, adjust=1, lbmult=0.2)  
  ukde_pres$NrPrt_D = densapproxfactory_botev_lb(cvdata$NrPrt_D, min=mins["NrPrt_D"], max=maxs["NrPrt_D"], useweighted=TRUE, adjust=3, lbmult=0.2)   #was 3
  ukde_pres$NrArp_D = densapproxfactory_botev_lb(cvdata$NrArp_D, min=mins["NrArp_D"], max=maxs["NrArp_D"], useweighted=TRUE, adjust=3, lbmult=0.2)   #was 3
  ukde_pres$SR = densapproxfactory_botev_lb(cvdata$SR, min=mins["SR"], max=maxs["SR"], useweighted=TRUE, adjust=1, lbmult=0.01)  
  ukde_pres$Tourism = densapproxfactory_botev_lb(cvdata$Tourism, min=mins["Tourism"], max=maxs["Tourism"], useweighted=TRUE, adjust=1, lbmult=0.01)  
  ukde_pres$WoodUnits = densapproxfactory_botev_lb(cvdata$WoodUnits, min=mins["WoodUnits"], max=maxs["WoodUnits"], useweighted=TRUE, adjust=1, lbmult=0.2)  
  ukde_pres$WoodEmploy = densapproxfactory_botev_lb(cvdata$WoodEmploy, min=mins["WoodEmploy"], max=maxs["WoodEmploy"], useweighted=TRUE, adjust=1, lbmult=0.2)  
  ukde_pres$runif = densapproxfactory_botev_lbrb(data=cvdata$runif, min=mins["runif"], max=maxs["runif"], useweighted=T, adjust=1, bmult=0.2)
  
  return(ukde_pres)
} 
predictBsDM2 = function(data, presmod, backmod){
  presdens = predictunivdens2(data, model=presmod)
  backdens = predictunivdens2(data, model=backmod)
  prob = presdens/backdens
  return(prob)
}

predictunivdens2 = function(data,model){ 
  
  nvar = length(model)
  denslist = vector(mode="list", length=nvar)
  
  denslist = lapply(names(model), function(i){ #was:  seq_len(nvar)
    model[[i]](data[,i])}
  )
  
  output = Reduce("*", denslist) 
  
  return(output)
}

modelperf2 = function(pcvdata, bdata, presmodels, backmodel, variablenames){
  
  nrrep = length(pcvdata)  
  
  output1 = data.frame("NLLp" = rep(0,nrrep), "AUC" = rep(0, nrrep))  
  output2 = c("NLLp_upper" = 0, "NLLp_mean" = 0, "NLLp_lower" = 0, "AUC_upper" = 0, "AUC_mean" = 0, "AUC_lower" = 0)
  
  for  (i in seq_along(pcvdata)){
    
    pmod = presmodels[[i]][variablenames]
    bmod = backmodel[variablenames]
    
    pprob = predictBsDM2(data = pcvdata[[i]]$test, presmod = pmod, backmod = bmod)
    bprob = predictBsDM2(data = bdata, presmod = pmod, backmod = bmod)
    pprob[is.infinite(pprob)] = NA #check for infinite values sometimes occuring on runif for GB2
    bprob[is.infinite(bprob)] = NA
    
    psummed = sum(pprob, na.rm=T) + sum(bprob, na.rm=T)
    pprobscaled = pprob / psummed
    
    logloss = -sum(log(pprobscaled))
    auc = pROC::auc(response = c(rep(1, length(pprob)), rep(0, length(bprob))), predictor = c(pprob, bprob), quiet=TRUE, na.rm=TRUE) 
    output1[i,] = c(logloss, auc)
  }
  
  output2[1:3] =as.numeric(myCI(output1$NLLp, ci=0.95)) #was mean(output1$NLLp)
  output2[4:6] = as.numeric(myCI(output1$AUC, ci=0.95))
  output = list(output1, output2)
  
  return(output)
  
}
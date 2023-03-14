###### Functions script for the paper 'Introduction hotspots of non-native tree pests and the role of cities' by
###### Robbert T. van den Dool, Alejandro Morales, Wopke van der Werf & J.C. (Bob) Douma
###### Last edited 9 March 2023 - Robbert T. van den Dool

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


# Fits distribution functions to variable data using likelihood. The alldata argument is used for the more complex distribution function 'SGT' to make sure that it covers the full domain of the variable.
# Depending on the type of data, different functions are considered suitable.

fitplusbounded <- function(data, alldata){ 
  outputlist = list()
  alldata = alldata
  
  f1 = MASS::fitdistr(x=data, densfun="normal", control = list(maxit = 2000))
  outputlist = append(outputlist, list(normal = f1))
  
  if(sum(alldata < 0) == 0){ #right bounded 
    f2 = MASS::fitdistr(x=data, densfun="exponential", control = list(maxit = 2000))
    outputlist = append(outputlist, list(exponential = f2))
  }
  
  if(sum(alldata == 0) == 0 && sum(alldata < 0) == 0){ #right bounded, above 0
    f3 = MASS::fitdistr(x=data, densfun="weibull", control = list(maxit = 2000))
    f4 = MASS::fitdistr(x=data, densfun="gamma", control = list(maxit = 2000))
    f5 = MASS::fitdistr(x=data, densfun="lognormal", control = list(maxit = 2000))
    outputlist =  append(outputlist, list(weibull=f3, gamma=f4, lognormal=f5))
    
    f6  = fitGB2(data, alldata)
    if(!is.null(f6)){outputlist =  append(outputlist,list(GB2 = f6))}
  }
  
  if(sum(alldata == 0) != 0 && sum(alldata < 0) == 0){ #right bounded, with 0
    f7 = fitGB2ZI(data, alldata)
    if(!is.null(f7)){outputlist =  append(outputlist, list(GB2ZI = f7))}
  }
  
  if(sum(alldata%%1==0)==length(alldata)){ #integer 
    f8 = MASS::fitdistr(x=data, densfun="negative binomial",control = list(maxit = 2000))
    outputlist = append(outputlist, list(negativebinomial = f8))
  }
  
  if(sum(alldata%%1==0)==length(alldata) && sum(alldata==0) != 0){ #integer with 0
    f9 = fitNBZI(data) #
    outputlist = append(outputlist, list(negativebinomialZI = f9))
  }
  
  if(sum(alldata%%1==0)!= length(alldata) ){ #No integer data, not plusbounded #&& sum(alldata < 0) != 0
    f10 = fitSGT(data, alldata)
    outputlist = append(outputlist, list(SGT = f10))
    }
  
  return(outputlist)
}

# This function returns the density depending on the functions fitted in fitplusbounded. It is used to determine predictive likelihood on tune and test sets for both function selection and performance evaluation. 
# It checks which functions have been fitted and will calculate the densities. 
# Returns a list with density of each available function 
dfitplusbounded <-  function(data, functiondata, llog=FALSE) {
  outputlist <- list()
  
  if(exists("normal",where=functiondata)){
    f1 = functiondata$normal$estimate
    p1 = dnorm(data, mean = f1[[1]], sd = f1[[2]] , log=llog)
    outputlist = append(outputlist,list(normal=p1))
  }
  
  if(exists("exponential",where=functiondata)){
    f2 = functiondata$exponential$estimate
    p2 = dexp(data, rate = f2[[1]], log=llog)
    outputlist = append(outputlist,list(exponential=p2))
  }
  
  if(exists("weibull",where=functiondata)){
    f3 = functiondata$weibull$estimate
    p3 = dweibull(data, shape = f3[[1]], scale = f3[[2]] , log=llog)
    outputlist = append(outputlist,list(weibull=p3))
  }
  
  if(exists("gamma",where=functiondata)){
    f4 = functiondata$gamma$estimate
    p4 = dgamma(data, shape = f4[[1]], rate = f4[[2]], log=llog)
    outputlist = append(outputlist,list(gamma=p4))
  }
  
  if(exists("lognormal",where=functiondata)){
    f5 = functiondata$lognormal$estimate
    p5 = dlnorm(data, meanlog = f5[[1]], sdlog = f5[[2]], log=llog)
    outputlist = append(outputlist,list(lognormal=p5))
  }
  
  if(exists("GB2",where=functiondata)){
    f6 = functiondata$GB2$par 
    p6 = dGB2(data, shape1=f6[[1]], scale=f6[[2]], shape2=f6[[3]], shape3=f6[[4]], log=llog)
    outputlist = append(outputlist,list(GB2=p6))
  }
  
  if(exists("GB2ZI",where=functiondata)){
    f7 = functiondata$GB2ZI$par 
    p7 = dGB2ZI(data, shape1=f7[[1]], scale=f7[[2]], shape2=f7[[3]], shape3=f7[[4]], zprob=f7[[5]], log=llog)
    outputlist = append(outputlist,list(GB2ZI=p7))
  }
  
  if(exists("negativebinomial",where=functiondata)){
    f8 = functiondata$negativebinomial$estimate
    p8 = dnbinom(data, size = f8[[1]], mu = f8[[2]] , log=llog)
    outputlist = append(outputlist,list(negativebinomial=p8))
  }
  
  if(exists("negativebinomialZI",where=functiondata)){
    f9 = functiondata$negativebinomialZI$par
    p9 = dnbinomZI(data, size = f9[[1]], mu = f9[[2]] , zprob = f9[[3]], log=llog)
    outputlist = append(outputlist,list(negativebinomialZI=p9))
  }
  
  if(exists("SGT",where=functiondata)){
    f10 = functiondata$SGT$par
    p10 = sgt::dsgt(data, mu = f10[[1]], sigma = f10[[2]], lambda = f10[[3]], p = f10[[4]], q = f10[[5]], log=llog)
    outputlist = append(outputlist,list(SGT=p10))
  }
  return(outputlist)
}


# workhorse function that does a lot of work. runs both fitplusbounded on train data and dfitplusbounded on tune data.
# prepares output for the 'getbest' function which will select the best function out of all fitted functions. 
crossvalidatemarg <- function(data, variable, alldata){ #crossvalidatedata
  
  data = data
  variable = variable
  
  alldata = alldata
  
  
  #fit on train
  trainedm =  lapply(data, function(x){
    fitplusbounded(`[`(`[[`(x,"train"),,variable), alldata = alldata[,variable])
  })

  
  tempnames = unique(unlist(  lapply(1:length(trainedm),function(x){names(unlist(lapply(trainedm[x][[1]], function(y){
    as.data.frame(t(`[[`(y,1)))
  })))})))
  
  templist = lapply(1:length(trainedm),function(x){
    out = data.frame(t(as.data.frame(unlist(lapply(trainedm[x][[1]], function(y){
      as.data.frame(t(`[[`(y,1)))
    })))))
    row.names(out)[1] = 1
    return(out)
  })
  
  params = do.call(rbind,
                   lapply(templist,
                          function(x) data.frame(c(x, sapply(setdiff(tempnames, names(x)),
                                                             function(y) NA)))))
  
  
  #predict on test
  count = 0
  
  testpreds = lapply(data, function(x){
    count <<- count + 1
    dfitplusbounded(data = `[`(`[[`(x,"tune"),,variable), functiondata = trainedm[[count]], llog=TRUE) 
  })
  
  
  NLL = lapply(testpreds, function(x) as.data.frame(t(sapply(x, function(y){-sum(y)})))) 
  
  prednames = unique(unlist(lapply(NLL,names)))
  

  
  NLLresults = do.call(rbind,
                       lapply(NLL,
                              function(x) data.frame(c(x, sapply(setdiff(prednames, names(x)),
                                                                 function(y) NA)))))
  

  output = cbind(data.frame(id=1:length(data)), NLLresults, params)
  
  return(output)
}

# Determines the fitted function that had the best mean performance on all tune sets, thus over all iterations. (from crossvalidatemarg)
# Then, it will return this function and its mean parameters.
getbest = function(cvobj){
  
  NLLs =  as.data.frame(cvobj[,c(FALSE,!grepl(".", names(cvobj[,-1]), fixed = TRUE))]) 
  names(NLLs) = names(cvobj)[c(FALSE,!grepl(".", names(cvobj[,-1]), fixed = TRUE))]
  
  if(is.null(NLLs)){return(0)}
  NLLmeans = colMeans(NLLs)
  minval = min(NLLmeans, na.rm=T)
  best = names(which(NLLmeans == minval))
  
  if(best == "lognormal"){
    meanlog = mean(cvobj[,"lognormal.meanlog"])
    sdlog = mean(cvobj[,"lognormal.sdlog"])
    dfunct <<-   function(x, log=F) dlnorm(x, meanlog = meanlog, sdlog = sdlog, log = log)
    describ <<- c("lognormal"= minval, "meanlog" = meanlog, "sdlog" = sdlog)
  }
  
  if(best == "exponential"){
    rate = mean(cvobj[,"exponential.rate"])
    dfunct <<- function(x, log=F) dexp(x, rate=rate, log = log)
    describ <<- c("exponential"= minval, "rate" = rate)
  }
  
  if(best == "weibull"){
    shape = mean(cvobj[,"weibull.shape"])
    scale = mean(cvobj[,"weibull.scale"])
    dfunct <<- function(x, log=F) dweibull(x, shape=shape, scale=scale, log = log)
    describ <<- c("weibull"= minval, "shape" = shape, "scale" = scale)
  }
  
  if(best == "gamma"){
    shape = mean(cvobj[,"gamma.shape"])
    rate = mean(cvobj[,"gamma.rate"])
    dfunct <<- function(x, log=F) dgamma(x, shape=shape, rate=rate, log = log)
    describ <<- c("gamma"= minval, "shape" = shape, "rate" = rate)
  }
  
  if(best == "negativebinomial"){
    size = mean(cvobj[,"negativebinomial.size"])
    mu = mean(cvobj[,"negativebinomial.mu"])
    dfunct <<- function(x, log=F) dnbinom(x, size=size, mu=mu, log = log)
    describ <<- c("negativebinomial"= minval, "size" = size, "mu" = mu)
  }
  
  if(best == "normal"){
    mean = mean(cvobj[,"normal.mean"])
    sd = mean(cvobj[,"normal.sd"])
    dfunct <<- function(x, log=F) dnorm(x, mean=mean, sd=sd, log = log)
    describ <<- c("normal"= minval, "mean" = mean, "sd" = sd)
  }
  
  if(best == "negativebinomialZI"){
    size = mean(cvobj[,"negativebinomialZI.size"])
    mu = mean(cvobj[,"negativebinomialZI.mu"])
    zprob = mean(cvobj[,"negativebinomialZI.zprob"])
    dfunct <<- function(x, log=F) dnbinomZI(x, size=size, mu=mu, zprob=zprob, log = log)
    describ <<- c("negativebinomialZI"= minval, "size" = size, "mu" = mu, "zprob" = zprob)
  }
  
  if(best == "GB2"){
    shape1 = mean(cvobj[,"GB2.shape1"])
    scale = mean(cvobj[,"GB2.scale"]) 
    shape2 = mean(cvobj[,"GB2.shape2"])
    shape3 = mean(cvobj[,"GB2.shape3"])
    dfunct <<- function(x, log=F) dGB2(x, shape1 = shape1, scale = scale, shape2 = shape2, shape3 = shape3, log = log)
    describ <<- c("GB2"= minval, "shape1" = shape1, "scale" = scale, "shape2" = shape2, "shape3" = shape3)
  }
  
  if(best == "GB2ZI"){
    shape1 = mean(cvobj[,"GB2ZI.shape1"])
    scale = mean(cvobj[,"GB2ZI.scale"]) 
    shape2 = mean(cvobj[,"GB2ZI.shape2"])
    shape3 = mean(cvobj[,"GB2ZI.shape3"])
    zprob = mean(cvobj[,"GB2ZI.zprob"])
    dfunct <<- function(x, log=F) dGB2ZI(x, shape1 = shape1, scale = scale, shape2 = shape2, shape3 = shape3, zprob = zprob, log = log)
    describ <<- c("GB2ZI"= minval, "shape1" = shape1, "scale" = scale, "shape2" = shape2, "shape3" = shape3, "zprob" = zprob)
  }
  
  if(best == "SGT"){
    mu = mean(cvobj[,"SGT.mu"])
    sigma = mean(cvobj[,"SGT.sigma"]) 
    lambda = mean(cvobj[,"SGT.lambda"])
    p = mean(cvobj[,"SGT.p"])
    q = mean(cvobj[,"SGT.q"])
    dfunct <<- function(x, log=F) sgt::dsgt(x, mu = mu, sigma = sigma, lambda = lambda, p = p, q = q, log = log)
    describ <<- c("SGT"= minval, "mu" = mu, "sigma" = sigma, "lambda" = lambda, "p" = p, "q" = q)
  }
  
  return(list(dfunct, describ))
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
  
  bw = density(unique(as.numeric(row.names(table(data)))), bw = "SJ", weights=table((data))/length(data)) 
  kde = kdevine::kde1d(data, xmin = min, xmax = max, bw = bw$bw, mult = adjust)
  kded = kdevine::dkde1d(data, kde)
  
  combined = cbind(data, kded)
  combined = combined[order(data),]
  
  funct = function(x){
    combined = combined
    value = stats::approx(x = combined[,1], y = combined[,2], xout = x, rule = 2, ties = "ordered")$y
    return(value)
  }
  
  return(funct)
  
}

# approximated high speed density method, without correction. Included for the 'treecover' variable, since there is no zero inflated version available
# and with correction the bandwidth is estimated too high for a well fitting density function. 
densapproxfactory_old <- function(data, min, max, adjust=1){
  
  kde = kdevine::kde1d(data, xmin = min, xmax = max, mult = adjust)
  kded = kdevine::dkde1d(data, kde)
  
  combined = cbind(data, kded)
  combined = combined[order(data),]
  
  funct = function(x){
    combined = combined
    value = stats::approx(x = combined[,1], y = combined[,2], xout = x, rule = 2, ties = "ordered")$y
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





#Creates presence models, using getbest on each partition
createpresmodel = function(cvdatapartition){
  model = lapply(cvdatapartition,function(x)getbest(x)[[1]])
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



# function to create a final presence model, based on all holdout validation sets
# here it is applied to a single variable
# the most often best performing variable is selected and fitted to all data.

fitfinalModelVar = function(cvdata1var, pdata1var, alldata){
  
  cvobj = cvdata1var
  data = pdata1var
  
  NLLs =  as.data.frame(cvobj[,c(FALSE,!grepl(".", names(cvobj[,-1]), fixed = TRUE))]) 
  names(NLLs) = names(cvobj)[c(FALSE,!grepl(".", names(cvobj[,-1]), fixed = TRUE))]
  
  
  bests = lapply(1:nrow(cvobj), function(x){names(NLLs) [which(NLLs[x,] == min(NLLs[x,]))]})
  freqs = table(unlist(bests))
  freqmax = names(freqs) [which(freqs == max(freqs))]
  
  if(freqmax == "lognormal"){
    
    fit = MASS::fitdistr(x=data, densfun="lognormal")
    
    meanlog = fit$estimate[1]
    sdlog =  fit$estimate[2]
    
    dfunct =   function(x, log=F) dlnorm(x, meanlog = meanlog, sdlog = sdlog, log = log)
    
  }
  
  if(freqmax == "exponential"){
    
    fit = MASS::fitdistr(x=data, densfun="exponential")
    
    rate = fit$estimate[1]
    dfunct = function(x, log=F) dexp(x, rate=rate, log = log)
    
  }
  
  if(freqmax == "weibull"){
    
    fit = MASS::fitdistr(x=data, densfun="weibull")
    shape = fit$estimate[1]
    scale = fit$estimate[2]
    dfunct = function(x, log=F) dweibull(x, shape=shape, scale=scale, log = log)
    
  }
  
  if(freqmax == "gamma"){
    
    fit = MASS::fitdistr(x=data, densfun="gamma")
    shape = fit$estimate[1]
    rate = fit$estimate[2]
    dfunct = function(x, log=F) dgamma(x, shape=shape, rate=rate, log = log)
    
  }
  
  if(freqmax == "negativebinomial"){
    
    fit = MASS::fitdistr(x=data, densfun="negative binomial")
    size = fit$estimate[1]
    mu = fit$estimate[2]
    dfunct = function(x, log=F) dnbinom(x, size=size, mu=mu, log = log)
  }
  
  if(freqmax == "normal"){
    
    fit = MASS::fitdistr(x=data, densfun="normal")
    mean = fit$estimate[1]
    sd = fit$estimate[2]
    dfunct = function(x, log=F) dnorm(x, mean=mean, sd=sd, log = log)
  } 
  
  if(freqmax == "negativebinomialZI"){
    
    fit = fitNBZI(data)
    size = fit$par[[1]]
    mu = fit$par[[2]]
    zprob = fit$par[[3]]
    dfunct = function(x, log=F) dnbinomZI(x, size=size, mu=mu, zprob=zprob, log = log)
  }
  
  if(freqmax == "GB2"){
    
    fit = fitGB2(data, alldata)
    shape1 = fit$par[[1]]
    scale = fit$par[[2]]
    shape2 = fit$par[[3]]
    shape3 = fit$par[[4]]
    dfunct = function(x, log=F) dGB2(x, shape1=shape1, scale=scale, shape2=shape2, shape3=shape3, log=log)
  }
  
  if(freqmax == "GB2ZI"){
    
    fit = fitGB2ZI(data, alldata)
    shape1 = fit$par[[1]]
    scale = fit$par[[2]]
    shape2 = fit$par[[3]]
    shape3 = fit$par[[4]]
    zprob = fit$par[[5]]
    dfunct = function(x, log=F) dGB2ZI(x, shape1=shape1, scale=scale, shape2=shape2, shape3=shape3, zprob=zprob, log=log)
  }
  
  if(freqmax == "SGT"){
    
    fit = fitSGT(data, alldata)
    mu = fit$par[[1]]
    sigma = fit$par[[2]]
    lambda = fit$par[[3]]
    p = fit$par[[4]]
    q = fit$par[[5]]
    dfunct = function(x, log=F) sgt::dsgt(x, mu = mu, sigma = sigma, lambda = lambda, p = p, q = q, log=log)
  }
  
  
  return(dfunct)
  
}

#Applies fitfinalModelVar to all variables
fitfinalModel <- function(cvdata, pdata, alldata){
  out =  lapply(names(cvdata), function(x){fitfinalModelVar(cvdata1var = cvdata[[x]], pdata1var = pdata[,x], alldata = alldata[,x])})
  names(out) = names(cvdata)
  return(out)
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

#SGT #Updated 28 Sep 2022
fitSGT = function(data, alldata){
  par = c(mu = mean(data), sigma = var(data), lambda = 0, p = 2, q = 100) #defaults: 0, 1, 0, 2, inf

  min = min(alldata)
  max = max(alldata)
  
  data = c(data, min, max)
  
  NLLfunct = function(data, par){
    par1 = par[1]
    par2 = par[2]
    par3 = par[3]
    par4 = par[4]
    par5 = par[5]
    
    #minfit =  sgt::dsgt(min, mu = par1, sigma = par2, lambda = par3, p = par4, q = par5, log=T)
    #maxfit =  sgt::dsgt(max, mu = par1, sigma = par2, lambda = par3, p = par4, q = par5, log=T)
    
    #if(is.na(minfit) || minfit == Inf || is.na(maxfit) || maxfit == Inf){return(NA)}
    
    return(-sum(sgt::dsgt(data, mu = par1, sigma = par2, lambda = par3, p = par4, q = par5, log=T)))
  }
  
  opt_SGT = optim(data = data, fn=NLLfunct, par=par, control=list(maxit=2000)) 
  return(opt_SGT)

} #result includes Positive Log-Likelihood. # -> -result$maximum

# dsgt(x, mu = result1$estimate[1],
#      sigma = result1$estimate[2],
#      lambda = result1$estimate[3],
#      p = result1$estimate[4],
#      q = result1$estimate[5])


#GB2
fitGB2 = function(data, alldata){
  
  min = min(alldata)
  max = max(alldata)
  data = c(data, min, max)
  
  temp = GB2::fisk(data)
  par = c("shape1" = temp[1], "scale" = temp[2], "shape2" = temp[3], "shape3" = temp[4])
  
  NLLfunct = function(data, par){
    par1 = par[1]
    par2 = par[2]
    par3 = par[3]
    par4 = par[4]
    #min = min
    #max = max
    
    #minfit =  log(dGB2(min, shape1 = par1, scale = par2, shape2 = par3, shape3 = par4))
    #maxfit =  log(dGB2(max, shape1 = par1, scale = par2, shape2 = par3, shape3 = par4))
    
    #if(is.na(minfit) || minfit == Inf || is.na(maxfit) || maxfit == Inf){return(NA)}
    
    return(-sum(log(dGB2(data, shape1 = par1, scale = par2, shape2 = par3, shape3 = par4))))
  }
  
  opt_gb2 = optim(data = data, fn=NLLfunct, par=par, control=list(maxit=2000)) 
  return(opt_gb2)
}

dGB2 = function(x, shape1, scale, shape2, shape3, log=FALSE){
    y <- (x/scale)^shape1
    dy_dx <- (shape1/scale) * (x/scale)^(shape1 - 1)
    z <- y/(1 + y)
    z[z == 1] <- 1 - .Machine$double.eps
    dz_dy <- (1 + y)^(-2)
    dens <- dbeta(z, shape2, shape3) * dz_dy * dy_dx
    v <- (x == Inf)
    dens[v] <- 0
    
    if(log==TRUE){
      dens = log(dens)
    }
    
    return(dens)
  }
  
#GB2 ZI

dGB2ZI = function (x, shape1, scale, shape2, shape3, zprob, log=FALSE){
  dens = ifelse(x==0, 
                zprob + (1 - zprob) * GB2::dgb2(x, shape1, scale, shape2, shape3),
                (1 - zprob) * GB2::dgb2(x, shape1, scale, shape2, shape3))
  if(log==T) dens = log(dens)
  return(dens)
}

pGB2ZI = function (q, shape1, scale, shape2, shape3, zprob) {
  prob = zprob + (1-zprob)*GB2::pgb2(q, shape1, scale, shape2, shape3)
  return(prob)
}  

fitGB2ZI = function(data, alldata){
  
  temp = GB2::fisk(data[which(data > 0)])
  zerop = sum(data==0)/length(data)
  if(zerop == 0) zerop=0.0001
  par  = c(temp, zerop)
  names(par) = c("shape1", "scale", "shape2", "shape3", "zprob")
  min = min(alldata)
  max = max(alldata)
  
  
  NLLfunct = function(data, par){
    par1 = par[1]
    par2 = par[2]
    par3 = par[3]
    par4 = par[4]
    par5 = par[5]
    min = min
    max = max
    
    minfit =  dGB2ZI(min, shape1 = par1, scale = par2, shape2 = par3, shape3 = par4, zprob = par5, log=TRUE)
    maxfit =  dGB2ZI(max, shape1 = par1, scale = par2, shape2 = par3, shape3 = par4, zprob = par5, log=TRUE)
    
    if(is.na(minfit) || minfit == Inf || is.na(maxfit) || maxfit == Inf){return(NA)}
    
    return(-sum(dGB2ZI(data, shape1 = par1, scale = par2, shape2 = par3, shape3 = par4, zprob = par5, log=TRUE)))
  }
  
  opt_NBZI = optim(data = data, fn=NLLfunct, par=par, control=list(maxit=5000))
  return(opt_NBZI)
}




#NB-ZI
dnbinomZI <- function (x, size, mu, zprob, log=FALSE){
  dens = ifelse(x==0, 
                zprob + (1 - zprob) * dnbinom(x=x, size=size, mu=mu),
                (1 - zprob) * dnbinom(x, size=size, mu=mu))
  if(log==T) dens = log(dens)
  return(dens)
}


pnbinomZI <- function (q,  size, mu, zprob) {
  prob = zprob + (1-zprob)*pnbinom(q=q, size=size, mu=mu)
  return(prob)
}

initpars_dnbinomZI = function(data){
  zprob = sum(data==0) / length(data)
  m <- mean(data)
  v <- var(data)
  size <- if (v > m) 
    m^2/(v - m)
  else 100
  start <- c(size = size, mu = m, zprob = zprob)
  return(start)
}

fitNBZI = function(data){
  
  par = initpars_dnbinomZI(data)
  
  NLLfunct = function(data, par){
    par1 = par[1]
    par2 = par[2]
    par3 = par[3]
    
    -sum(dnbinomZI(data, size = par1, mu = par2, zprob = par3, log=TRUE))
  }
  
  opt_NBZI = optim(data = data, fn=NLLfunct, par=par, control=list(maxit=5000)) 
  return(opt_NBZI)
  
}
 
###
###add simple version of crossvalidatemarg and fitplusbounded
crossvalidatemarg_simple = function(data, variable, alldata){ #crossvalidatedata
  
  data = data
  variable = variable
  
  alldata = alldata
  
  
  #fit on train
  trainedm =  lapply(data, function(x){
    fitplusbounded_simple(`[`(`[[`(x,"train"),,variable), alldata = alldata[,variable])
  })
  
  # params = as.data.frame(t(sapply(trainedm,function(x){
  #   unlist(lapply(x, function(y){
  #     as.data.frame(t(`[[`(y,1)))
  #   }))
  # })))
  
  
  tempnames = unique(unlist(  lapply(1:length(trainedm),function(x){names(unlist(lapply(trainedm[x][[1]], function(y){
    as.data.frame(t(`[[`(y,1)))
  })))})))
  
  templist = lapply(1:length(trainedm),function(x){
    out = data.frame(t(as.data.frame(unlist(lapply(trainedm[x][[1]], function(y){
      as.data.frame(t(`[[`(y,1)))
    })))))
    row.names(out)[1] = 1
    return(out)
  })
  
  params = do.call(rbind,
                   lapply(templist,
                          function(x) data.frame(c(x, sapply(setdiff(tempnames, names(x)),
                                                             function(y) NA)))))
  
  # params = as.data.frame(t(as.data.frame(sapply(1:length(trainedm),function(x){
  #   unlist(lapply(trainedm[x][[1]], function(y){
  #     as.data.frame(t(`[[`(y,1)))
  #   }))
  # }))))
  
  #row.names(params) = c(1:length(data))
  
  #predict on test
  count = 0
  
  testpreds = lapply(data, function(x){
    count <<- count + 1
    dfitplusbounded(data = `[`(`[[`(x,"test"),,variable), functiondata = trainedm[[count]], llog=TRUE)
  })
  
  
  NLL = lapply(testpreds, function(x) as.data.frame(t(sapply(x, function(y){-sum(y)})))) #this does what I want. 
  
  prednames = unique(unlist(lapply(NLL,names)))
  
  # tempnames = unique(unlist(  lapply(1:length(trainedm),function(x){names(unlist(lapply(trainedm[x][[1]], function(y){
  #   as.data.frame(t(`[[`(y,1)))
  # })))})))
  
  NLLresults = do.call(rbind,
                       lapply(NLL,
                              function(x) data.frame(c(x, sapply(setdiff(prednames, names(x)),
                                                                 function(y) NA)))))
  
  # NLLresults <- as.data.frame(t(sapply(testpreds, sapply, function(x) -sum(x))))
  
  
  output = cbind(data.frame(id=1:length(data)), NLLresults, params)
  
  return(output)
}
     

fitplusbounded_simple = function(data, alldata){ #
  outputlist = list()
  alldata = alldata
  
  f1 = MASS::fitdistr(x=data, densfun="normal", control = list(maxit = 2000))
  outputlist = append(outputlist, list(normal = f1))
  
  if(sum(alldata < 0) == 0){ #right bounded 
    f2 = MASS::fitdistr(x=data, densfun="exponential", control = list(maxit = 2000))
    outputlist = append(outputlist, list(exponential = f2))
  }
  
  if(sum(alldata == 0) == 0 && sum(alldata < 0) == 0){ #right bounded, above 0
    f3 = MASS::fitdistr(x=data, densfun="weibull", control = list(maxit = 2000))
    f4 = MASS::fitdistr(x=data, densfun="gamma", control = list(maxit = 2000))
    f5 = MASS::fitdistr(x=data, densfun="lognormal", control = list(maxit = 2000))
    outputlist =  append(outputlist, list(weibull=f3, gamma=f4, lognormal=f5))
    
    # f6  = fitGB2(data, alldata)
    # if(!is.null(f6)){outputlist =  append(outputlist,list(GB2 = f6))}
  }
  
  # if(sum(alldata == 0) != 0 && sum(alldata < 0) == 0){ #right bounded, with 0
  #   f7 = fitGB2ZI(data, alldata)
  #   if(!is.null(f7)){outputlist =  append(outputlist, list(GB2ZI = f7))}
  # }
  
  if(sum(alldata%%1==0)==length(alldata)){ #integer 
    f8 = MASS::fitdistr(x=data, densfun="negative binomial",control = list(maxit = 2000))
    outputlist = append(outputlist, list(negativebinomial = f8))
  }
  
  # if(sum(alldata%%1==0)==length(alldata) && sum(alldata==0) != 0){ #integer with 0
  #   f9 = fitNBZI(data) #
  #   outputlist = append(outputlist, list(negativebinomialZI = f9))
  # }
  
  # if(sum(alldata%%1==0)!= length(alldata) ){ #No integer data, not plusbounded #&& sum(alldata < 0) != 0
  #   f10 = fitSGT(data, alldata)
  #   outputlist = append(outputlist, list(SGT = f10))
  # }
  
  return(outputlist)
}    



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



# #  
# VarImp_AUC = lapply(VarImp, function(x) myCI(origAUC - x$AUC)) #how much is AUC higher with the variable not shuffled?
# VarImp_AUC_min = min(sapply(VarImp_AUC, function(x)x[2]))
# VarImp_AUC_max = max(sapply(VarImp_AUC, function(x)x[2]))
# 
# VarImp_AUC_sort = sapply(VarImp_AUC, function(x)x[2])
# names(VarImp_AUC_sort) = names(VarImp)
# 
# upper =   sapply(VarImp_AUC, function(x)x[1])
# upper_sort = upper[order(VarImp_AUC_sort)]
# 
# lower =   sapply(VarImp_AUC, function(x)x[3])
# lower_sort = lower[order(VarImp_AUC_sort)]
# 
# VarImp_AUC_sort = sort(VarImp_AUC_sort)
# 
# name = paste(prefix,"_VPI_AUC",".tiff",sep="")
# tiff(name, width = 4, height = 6, units = 'in', res = 600)
# par(mgp = c(3,0.5,0),mar=c(2.1,11.1,1.1,1.1))
# VPIplot = barplot(VarImp_AUC_sort, horiz=T, las=1, xlim=c(VarImp_AUC_min, VarImp_AUC_max),  main="AUC", xpd=T)
# arrows(upper_sort,VPIplot, lower_sort, VPIplot, angle=90, code=3, length=0.05)
# dev.off()



getparfunction = function(y){
  parnames = c("rate", "mu", "sigma", "lambda", "p", "q", "meanlog", "sdlog", "scale", "shape", "mean", "sd", "shape1", "shape2", "shape3", "zprob", "size")
  sum(parnames %in%ls(environment(y)))
}

nparBayeSDM = function(presmodel){
  sum(sapply(presmodel, function(x){getparfunction(x)}))
}




######################################################
#rollpred.rf is a function that return the prediction# 
#for random forest that conducts 1 Month prediction  #
######################################################
#wind: is the window length (i.e. 25 window 50 window etc)
#df: is the dataframe to conduct analysis

rollpred.rf = function(wind,df){
  test = df[(wind+1):nrow(df),]
  len = nrow(df) - wind
  
  preds = c()
  for (i in 1:len){
    train = final[i:(i+wind-1),]
    rf = randomForest(nasdaqpctchg~.-Date-cpi-ppi-dji-ftse-snp-nasdaq-nasdaqvol,
                      data = train,ntree=5000,maxnodes = 10,mtry=(length(colnames(final))-9)/3)
    result = predict(rf,newdata = test[i,])
    preds = c(preds,result)
    print(paste('Iter',i,result))
  }
  return(preds)
}

#####################################################
#hpred.rf is a function that return the prediction  # 
#for random forest that conducts h Months prediction#
#####################################################
#wind: is the window length (i.e. 25 window 50 window etc)
#df: is the dataframe to conduct analysis
#h: step of prediction (i.e. Predict 3 Months, 6 Months etc)

hpred.rf = function(wind,df,h){
  test = df[(wind+h):nrow(df),]
  len = nrow(df) - wind - h + 1
  
  preds = c()
  for (i in 1:len){
    train = final[i:(i+wind-1),]
    rf = randomForest(nasdaqpctchg~.-Date-cpi-ppi-dji-ftse-snp-nasdaq-nasdaqvol,
                      data = train,ntree=5000,maxnodes = 10,mtry=(length(colnames(final))-9)/3)
    result = predict(rf,newdata = test[i,])
    preds = c(preds,result)
    print(paste('Iter',i))
  }
  return(preds)
}

#################################################################
#msecalc is a function that returns a dataframe with the        # 
#square error for each prediction and corresponding actual value#
#################################################################
#name: name of the corresponding dataframe to be produced
#wind: is the window length (i.e. 25 window 50 window etc)
#df: is the dataframe to conduct analysis
#h: step of prediction (i.e. Predict 3 Months, 6 Months etc)
#res: is the resulted prediction for each model

msecalc = function(name,wind,h,res){
  
  name = data.frame(Date = final$Date[(wind+h):nrow(final)],pred = res,val = y[(wind+h):nrow(final)])
  
  mse = c()
  for (i in 1:nrow(name)){
    mse = c(mse,(name$val[i]-name$pred[i])**2)
  }
  name = cbind(name,square_error = mse)
  return(name)
}

########################################################
#hanndf is a function that returns a dataframe with the# 
#actual result and corresponding ann prediction results#
########################################################
#pred: is the resulted prediction for ann model
#wind: is the window length (i.e. 25 window 50 window etc)
#h: step of prediction (i.e. Predict 3 Months, 6 Months etc)

hanndf = function(pred,wind,h){
  df = cbind(Date = final$Date[(wind+h):nrow(final)],val = y[(wind+h):nrow(final)],pred)
  return(df)
}

###################################################
#ann is a function that returns a dataframe with  # 
#the predicted ann results using different lambdas#
###################################################
#winds: is the window length (i.e. 25 window 50 window etc)
#df: is the dataframe to conduct analysis

ann = function(winds,df){
  
  prednn = data.frame(one=numeric(0),two=numeric(0),thr=numeric(0),four=numeric(0),fve=numeric(0),
                        six=numeric(0),svn=numeric(0),eight=numeric(0),nine=numeric(0),ten=numeric(0))
  
  dec=c(0,0.001,0.003,0.01,0.03,0.1,0.3,1,3,10)
  
  nd=length(dec) #no of lambdas, will determine no of iterations in a loop
  
  len = nrow(df) - winds
  
  for (i in 1:len){
 
    train = data[i:(i+winds-1),]
    test = data[i+winds,]
    
    maxs = apply(train, 2, max) #find maxima by columns (i.e., for each variable)
    mins = apply(train, 2, min) #find minima by columns (i.e., for each variable)
    
    train = as.data.frame(scale(train, center = mins, scale = maxs - mins))
    test = as.data.frame(scale(test, center = mins, scale = maxs - mins))
    
    k=which(colnames(test)=="nasdaqpctchg")

    for (j in 1:length(dec)){
      nn=nnet(nasdaqpctchg~.-cpi-ppi-dji-ftse-snp-nasdaq-nasdaqvol, data=train, size=10,  
              maxit=1000, decay=dec[j], linout = TRUE, trace=FALSE,MaxNWts=1271)
      yhat = predict(nn, test)*(maxs[k[1]]-mins[k])+mins[k[1]]
      prednn[i,j] = yhat[1]
      print(yhat)
      print(paste('Iter',i,'dec',j))
    }
  } 
  return(prednn)
}

################################################################
#hann is a function that returns a dataframe with the predicted# 
#ann results using different lambdas for h-Month prediction    #
################################################################
#winds: is the window length (i.e. 25 window 50 window etc)
#h: step of prediction (i.e. Predict 3 Months, 6 Months etc)
#df: is the dataframe to conduct analysis
###NB: MaxNWts could produce an error and the number given in the error 
###must be changed accordingly

hann = function(winds,df,h){
  
  prednn = data.frame(one=numeric(0),two=numeric(0),thr=numeric(0),four=numeric(0),fve=numeric(0),
                      six=numeric(0),svn=numeric(0),eight=numeric(0),nine=numeric(0),ten=numeric(0))
  
  dec=c(0,0.001,0.003,0.01,0.03,0.1,0.3,1,3,10)
  
  len = nrow(df) - winds - h + 1
  
  for (i in 1:len){
    
    train = data[i:(i+winds-1),]
    test = data[i+winds+h-1,]
    
    maxs = apply(train, 2, max) #find maxima by columns (i.e., for each variable)
    mins = apply(train, 2, min) #find minima by columns (i.e., for each variable)
    
    train = as.data.frame(scale(train, center = mins, scale = maxs - mins))
    test = as.data.frame(scale(test, center = mins, scale = maxs - mins))
    
    k=which(colnames(test)=="nasdaqpctchg")
    
    for (j in 1:length(dec)){
      nn=nnet(nasdaqpctchg~.-cpi-ppi-dji-ftse-snp-nasdaq-nasdaqvol, data=train, size=10,
              maxit=1000, decay=dec[j], linout = TRUE, trace=FALSE,MaxNWts=1271)
      yhat = predict(nn, test)*(maxs[k[1]]-mins[k])+mins[k[1]]
      prednn[i,j] = yhat[1]
      print(yhat)
      print(paste('Iter',i,'dec',j))
    }
  } 
  return(prednn)
}

#####################################################
#rollrlasso is a function that returns a vector with# 
#the predicted rlasso results for 1 Month prediction#
#####################################################
#wind: is the window length (i.e. 25 window 50 window etc)
#df: is the dataframe to conduct analysis
rollrlasso = function(wind,df){
  predslasso = c()
  test = df[(wind+1):nrow(df),]
  
  len = nrow(df) - wind

  for (i in 1:len){
    train = final[i:(i+wind-1),]
    rlasso = rlasso(nasdaqpctchg~.-Date-cpi-ppi-dji-ftse-snp-nasdaq-nasdaqvol,
                    data = train,penalty=list(X.dependent.lambda=TRUE, homoscedastic=FALSE),  post=TRUE)
    result = predict(rlasso,newdata = test[i,])
    predslasso = c(predslasso,result)
    print(predslasso)
    print(paste('Iter',i))
  }

  # rlassodf = data.frame(Date = as.yearmon(df[['Date']][(wind+1):nrow(df)]),pred = predslasso,val = y[wind+1:nrow(df)])
  return(predslasso)
}

######################################################
#hrlasso is a function that returns a vector with the# 
#predicted rlasso results for h Month prediction     #
######################################################
#wind: is the window length (i.e. 25 window 50 window etc)
#df: is the dataframe to conduct analysis
#h: step of prediction (i.e. Predict 3 Months, 6 Months etc)
hrlasso = function(wind,df,h){
  test = df[(wind+h):nrow(df),]
  len = nrow(df) - wind - h + 1
  
  predslasso = c()
  
  for (i in 1:len){
    train = final[i:(i+wind-1),]
    rlasso = rlasso(nasdaqpctchg~.-Date-cpi-ppi-dji-ftse-snp-nasdaq-nasdaqvol,
                    data = train,penalty=list(X.dependent.lambda=TRUE, homoscedastic=FALSE),  post=TRUE)
    result = predict(rlasso,newdata = test[i,])
    predslasso = c(predslasso,result)
    print(predslasso)
    print(paste('Iter',i))
  }
  
  # rlassodf = data.frame(Date = as.yearmon(df[['Date']][(wind+1):nrow(df)]),pred = predslasso,val = y[wind+1:nrow(df)])
  return(predslasso)
}




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
    print(paste('Iter',i))
  }
  return(preds)
}

msecalc = function(name,wind,pred){
  
  name = data.frame(Date = final$Date[(wind+1):nrow(final)],pred,val = y[(wind+1):nrow(final)])
  
  mse = c()
  for (i in 1:nrow(name)){
    mse = c(mse,(name$val[i]-name$pred[i])**2)
  }
  name = cbind(name,square_error = mse)
  return(name)
}

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
              maxit=1000, decay=dec[j], linout = TRUE, trace=FALSE,MaxNWts=1041)
      yhat = predict(nn, test)*(maxs[k[1]]-mins[k])+mins[k[1]]
      prednn[i,j] = yhat[1]
      print(yhat)
      print(paste('Iter',i,'dec',j))
    }
  } 
  return(prednn)
}

rollrlasso = function(wind,df,y){
  predslasso = c()
  test = df[(wind+1):nrow(df),]
  
  len = nrow(df) - wind
  
  # for (i in 1:len){
  #   train = final[i:(i+wind-1),]
  #   rlasso = rlasso(nasdaqpctchg~.-Date-cpi-ppi-dji-ftse-snp-nasdaq-nasdaqvol,
  #                   data = train,penalty=list(X.dependent.lambda=TRUE, homoscedastic=FALSE),  post=TRUE)
  #   result = predict(rlasso,newdata = test[i,])
  #   predslasso = c(predslasso,result)
  #   print(paste('Iter',i))
  # }
  # 
  # rlassodf = data.frame(Date = as.yearmon(df[['Date']][(wind+1):nrow(df)]),pred = predslasso,val = y[wind+1:nrow(final)])
  return(test)
}

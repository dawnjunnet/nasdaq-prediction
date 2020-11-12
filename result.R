rm(list = ls())
setwd('/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final')
library(zoo) #to change the dates to yearmon
library(dplyr) #to use inner_join
library(randomForest)
library(hdm)
source('roll func.R') #to used functions created for this model

winds = c(25,50,75,100) #rolling window used

final = read.csv('final.csv')
y = final$nasdaqpctchg

##################
# Random Forest #
################
###########
# 1 Month #
###########

set.seed(785903475)
rfls = list() #Save all 1 Month random forest prediction

rfls[[1]] = read.csv('rf1mth25.csv') #25 Months rolling window
rfls[[2]] = read.csv('rf1mth50.csv') #50 Months rolling window
rfls[[3]] = read.csv('rf1mth75.csv') #75 Months rolling window
rfls[[4]] = read.csv('rf1mth100.csv') #100 Months rolling window

#to show that importance weight changes as time passes
train = final[1:25,]
rf = randomForest(nasdaqpctchg~.-Date-cpi-ppi-dji-ftse-snp-nasdaq-nasdaqvol,
                  data = train,ntree=5000,maxnodes = 10,mtry=(length(colnames(final))-9)/3,importance=TRUE)
as.data.frame(rf$importance[order(rf$importance[,'IncNodePurity'],decreasing = TRUE),-1])

#to show that importance weight changes as time passes
train = final[219:244,]
rf = randomForest(nasdaqpctchg~.-Date-cpi-ppi-dji-ftse-snp-nasdaq-nasdaqvol,
                  data = train,ntree=5000,maxnodes = 10,mtry=(length(colnames(final))-9)/3,importance=TRUE)
rf$importance[order(rf$importance[,'IncNodePurity'],decreasing = TRUE),]
predict(rf,newdata = final[244,])

for(i in 1:4){
  rfls[[i]] = subset(rfls[[i]],select = -c(X)) #take out 'X' column
  rfls[[i]][['Date']] = as.yearmon(rfls[[i]][['Date']]) #change Date to yearmon
}
#calculate RMSE for each rolling window
lapply(rfls, function(x) mean((x[['pred']]-x[['val']])**2)**.5)
#calculate percentage of wrong sign predictions against actual sign of value
lapply(rfls, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))

#Save each rmse for each rolling window that predicts 1 Month
for(i in 1:4){
  name = paste('rf1mth',winds[i],'rmse',sep = '')
  assign(name,lapply(rfls, function(x) mean((x[['pred']]-x[['val']])**2)**.5)[[i]])
}

##Dataframe of RMSE for rf model
rf1mthrmsedf = data.frame('type' = 'random forest', 'window25' = rf1mth25rmse, 'window50' = rf1mth50rmse, 'window75' = rf1mth75rmse,
                          'window100' = rf1mth100rmse)

###########
# 3 Month #
###########
rf3mth = list() #Save all 3 Months random forest prediction
rf3mth[[1]] = read.csv('rf3mth1.csv') #25 Months rolling window
rf3mth[[2]] = read.csv('rf3mth2.csv') #50 Months rolling window
rf3mth[[3]] = read.csv('rf3mth3.csv') #75 Months rolling window
rf3mth[[4]] = read.csv('rf3mth4.csv') #100 Months rolling window

for(i in 1:4){
  rf3mth[[i]] = subset(rf3mth[[i]],select = -c(X)) #take out 'X' column
  rf3mth[[i]][['Date']] = as.yearmon(rf3mth[[i]][['Date']]) #change Date to yearmon
}

#calculate RMSE for each rolling window
lapply(rf3mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)
#calculate percentage of wrong sign predictions against actual sign of value
lapply(rf3mth, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))

#Save each rmse for each rolling window that predicts 1 Month
for(i in 1:4){
  name = paste('rf3mth',winds[i],'rmse',sep = '')
  assign(name,lapply(rf3mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)[[i]])
}

##Dataframe of RMSE for rf model
rf3mthrmsedf = data.frame('type' = 'random forest', 'window25' = rf3mth25rmse, 'window50' = rf3mth50rmse, 'window75' = rf3mth75rmse,
                          'window100' = rf3mth100rmse)

###########
# 6 Month #
###########
rf6mth = list() #Save all 6 Months random forest prediction
rf6mth[[1]] = read.csv('rf6mth25.csv') #25 Months rolling window
rf6mth[[2]] = read.csv('rf6mth50.csv') #50 Months rolling window
rf6mth[[3]] = read.csv('rf6mth75.csv') #75 Months rolling window
rf6mth[[4]] = read.csv('rf6mth100.csv') #100 Months rolling window

for(i in 1:4){
  rf6mth[[i]] = subset(rf6mth[[i]],select = -c(X)) #take out 'X' column
  rf6mth[[i]][['Date']] = as.yearmon(rf6mth[[i]][['Date']]) #change Date to yearmon
}

#calculate RMSE for each rolling window
lapply(rf6mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)
#calculate percentage of wrong sign predictions against actual sign of value
lapply(rf6mth, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))

#Save each rmse for each rolling window that predicts 1 Month
for(i in 1:4){
  name = paste('rf6mth',winds[i],'rmse',sep = '')
  assign(name,lapply(rf6mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)[[i]])
}

##Dataframe of RMSE for rf model
rf6mthrmsedf = data.frame('type' = 'random forest', 'window25' = rf6mth25rmse, 'window50' = rf6mth50rmse, 'window75' = rf6mth75rmse,
                          'window100' = rf6mth100rmse)

############
# 12 Month #
############
rf12mth = list() #Save all 12 Months random forest prediction
rf12mth[[1]] = read.csv('rf12mth25.csv') #25 Months rolling window
rf12mth[[2]] = read.csv('rf12mth50.csv') #50 Months rolling window
rf12mth[[3]] = read.csv('rf12mth75.csv') #75 Months rolling window
rf12mth[[4]] = read.csv('rf12mth100.csv') #100 Months rolling window

for(i in 1:4){
  rf12mth[[i]] = subset(rf12mth[[i]],select = -c(X)) #take out 'X' column
  rf12mth[[i]][['Date']] = as.yearmon(rf12mth[[i]][['Date']]) #change Date to yearmon
}

#calculate RMSE for each rolling window
lapply(rf12mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)
#calculate percentage of wrong sign predictions against actual sign of value
lapply(rf12mth, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))

#Save each rmse for each rolling window that predicts 1 Month
for(i in 1:4){
  name = paste('rf12mth',winds[i],'rmse',sep = '')
  assign(name,lapply(rf12mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)[[i]])
}

##Dataframe of RMSE for rf model
rf12mthrmsedf = data.frame('type' = 'random forest', 'window25' = rf12mth25rmse, 'window50' = rf12mth50rmse, 'window75' = rf12mth75rmse,
                          'window100' = rf12mth100rmse)

##############
# POST LASSO #
#############
###########
# 1 Month #
###########

rlassolst = list() #Save all 1 Month rlasso (post) prediction
rlassolst[[1]] = read.csv('rlass1mth25.csv') #25 Months rolling window
rlassolst[[2]] = read.csv('rlass1mth50.csv') #50 Months rolling window
rlassolst[[3]] = read.csv('rlass1mth75.csv') #75 Months rolling window
rlassolst[[4]] = read.csv('rlass1mth100.csv') #100 Months rolling window

for(i in 1:4){
  rlassolst[[i]] = subset(rlassolst[[i]],select = -c(X)) #take out 'X' column
  rlassolst[[i]][['Date']] = as.yearmon(rlassolst[[i]][['Date']]) #change Date to yearmon
}

#show that coefficient that is shrunk/kept changes as time passes
train = final[1:25,]
rlasso = rlasso(nasdaqpctchg~.-Date-cpi-ppi-dji-ftse-snp-nasdaq-nasdaqvol,
                data = train,penalty=list(X.dependent.lambda=TRUE, homoscedastic=FALSE),  post=TRUE)

for (i in 1:length(rlasso$coefficients)){
  if (rlasso$coefficients[i] != 0){
    print(rlasso$coefficients[i])
  }
}

train = final[219:243,]
rlasso = rlasso(nasdaqpctchg~.-Date-cpi-ppi-dji-ftse-snp-nasdaq-nasdaqvol,
                data = train,penalty=list(X.dependent.lambda=TRUE, homoscedastic=FALSE),  post=TRUE)

for (i in 1:length(rlasso$coefficients)){
  if (rlasso$coefficients[i] != 0){
    print(rlasso$coefficients[i])
  }
}

#calculate RMSE for each rolling window
lapply(rlassolst, function(x) mean((x[['pred']]-x[['val']])**2)**.5)
#calculate percentage of wrong sign predictions against actual sign of value
lapply(rlassolst, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))

#Save each rmse for each rolling window that predicts 1 Month
for(i in 1:4){
  name = paste('rlass1mth',winds[i],'rmse',sep = '')
  assign(name,lapply(rlassolst, function(x) mean((x[['pred']]-x[['val']])**2)**.5)[[i]])
}
bestdf = data.frame('horizon'='1 Month','window25' = rlass1mth25rmse, 'window50' = rlass1mth50rmse, 'window75' = rlass1mth75rmse,
                          'window100' = rlass1mth100rmse)

##Dataframe of RMSE for post LASSO model
lass1mthrmsedf = data.frame('type' = 'post-LASSO', 'window25' = rlass1mth25rmse, 'window50' = rlass1mth50rmse, 
                           'window75' = rlass1mth75rmse,'window100' = rlass1mth100rmse)

#Save each wrong sign percentage in each rolling window
wrong1mth = c()
for (i in 1:4){
  wrong1mth = c(wrong1mth,round(lapply(rlassolst, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))[[i]],4)*100)
}
#Dataframe of wrong sign percentage
wrongdf = data.frame('horizon'='1 Month','window25' = wrong1mth[1], 'window50' = wrong1mth[2], 
                     'window75' = wrong1mth[3],'window100' = wrong1mth[4])

############
# 3 Months #
############

lass3mth = list() #Save all 3 Months rlasso (post) prediction
lass3mth[[1]] = read.csv('rlass3mth25.csv') #25 Months rolling window
lass3mth[[2]] = read.csv('rlass3mth50.csv') #50 Months rolling window
lass3mth[[3]] = read.csv('rlass3mth75.csv') #75 Months rolling window
lass3mth[[4]] = read.csv('rlass3mth100.csv') #100 Months rolling window

for(i in 1:4){
  lass3mth[[i]] = subset(lass3mth[[i]],select = -c(X)) #take out 'X' column
  lass3mth[[i]][['Date']] = as.yearmon(lass3mth[[i]][['Date']]) #change Date to yearmon
}

#calculate RMSE for each rolling window
lapply(lass3mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)
#calculate percentage of wrong sign predictions against actual sign of value
lapply(lass3mth, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))

#Save each rmse for each rolling window that predicts 3 Months
for(i in 1:4){
  name = paste('rlass3mth',winds[i],'rmse',sep = '')
  assign(name,lapply(lass3mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)[[i]])
}
##Dataframe of RMSE for post LASSO model
lass3mthrmsedf = data.frame('type' = 'post-LASSO', 'window25' = rlass3mth25rmse, 'window50' = rlass3mth50rmse, 
                            'window75' = rlass3mth75rmse,'window100' = rlass3mth100rmse)

#Save each wrong sign percentage in each rolling window
wrong3mth = c()
for (i in 1:4){
  wrong3mth = c(wrong3mth,round(lapply(lass3mth, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))[[i]],4)*100)
}

#Dataframe of wrong sign percentage in 3 Month prediction
wrong3mthdf = data.frame('horizon'='3 Month','window25' = wrong3mth[1], 'window50' = wrong3mth[2], 
                         'window75' = wrong3mth[3],'window100' = wrong3mth[4])

#Combine the wrong sign percentage to wrongdf dataframe to collate for each horizon prediction
wrongdf = rbind(wrongdf,wrong3mthdf)

#Dataframe of RMSE for best 3 Month prediction model
lass3mthrmse = data.frame('horizon'='3 Month','window25' = rlass3mth25rmse, 'window50' = rlass3mth50rmse, 'window75' = rlass3mth75rmse,
                          'window100' = rlass3mth100rmse)
#Combine the RMSE 3 Month dataframe to bestdf to collate RMSE for each horizon prediction
bestdf = rbind(bestdf,lass3mthrmse)

############
# 6 Months #
############

lass6mth = list() #Save all 6 Months rlasso (post) prediction
lass6mth[[1]] = read.csv('rlass6mth25.csv') #25 Months rolling window
lass6mth[[2]] = read.csv('rlass6mth50.csv') #50 Months rolling window
lass6mth[[3]] = read.csv('rlass6mth75.csv') #75 Months rolling window
lass6mth[[4]] = read.csv('rlass6mth100.csv') #100 Months rolling window

for(i in 1:4){
  lass6mth[[i]] = subset(lass6mth[[i]],select = -c(X)) #take out 'X' column
  lass6mth[[i]][['Date']] = as.yearmon(lass6mth[[i]][['Date']]) #change Date to yearmon
}

#Save each rmse for each rolling window that predicts 6 Months
for(i in 1:4){
  name = paste('rlass6mth',winds[i],'rmse',sep = '')
  assign(name,lapply(lass6mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)[[i]])
}

lass6mthrmsedf =  data.frame('type' = 'post-LASSO', 'window25' = rlass6mth25rmse, 'window50' = rlass6mth50rmse, 
                             'window75' = rlass6mth75rmse,'window100' = rlass6mth100rmse)

#calculate RMSE for each rolling window
lapply(lass6mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)
#calculate percentage of wrong sign predictions against actual sign of value
lapply(lass6mth, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))

#Dataframe of RMSE for best 6 Month prediction model
lass6mthrmse = data.frame('horizon'='6 Month','window25' = rlass6mth25rmse, 'window50' = rlass6mth50rmse, 'window75' = rlass6mth75rmse,
                          'window100' = rlass6mth100rmse)
#Combine the RMSE 6 Month dataframe to bestdf to collate RMSE for each horizon prediction
bestdf = rbind(bestdf,lass6mthrmse)

#Save each wrong sign percentage in each rolling window
wrong6mth = c()
for (i in 1:4){
  wrong6mth = c(wrong6mth,round(lapply(lass6mth, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))[[i]],4)*100)
}

#Dataframe of wrong sign percentage in 6 Month prediction
wrong6mthdf = data.frame('horizon'='6 Month','window25' = wrong6mth[1], 'window50' = wrong6mth[2], 
                         'window75' = wrong6mth[3],'window100' = wrong6mth[4])

#Combine the wrong sign percentage to wrongdf dataframe to collate for each horizon prediction
wrongdf = rbind(wrongdf,wrong6mthdf)

#############
# 12 Months #
#############

lass12mth = list() #Save all 12 Months rlasso (post) prediction
lass12mth[[1]] = read.csv('rlass12mth25.csv') #25 Months rolling window
lass12mth[[2]] = read.csv('rlass12mth50.csv') #50 Months rolling window
lass12mth[[3]] = read.csv('rlass12mth75.csv') #75 Months rolling window
lass12mth[[4]] = read.csv('rlass12mth100.csv') #100 Months rolling window

for(i in 1:4){
  lass12mth[[i]] = subset(lass12mth[[i]],select = -c(X)) #take out 'X' column
  lass12mth[[i]][['Date']] = as.yearmon(lass12mth[[i]][['Date']]) #change Date to yearmon
}

#Save each rmse for each rolling window that predicts 12 Months
for(i in 1:4){
  name = paste('rlass12mth',winds[i],'rmse',sep = '')
  assign(name,lapply(lass12mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)[[i]])
}

#calculate RMSE for each rolling window
lapply(lass12mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)
#calculate percentage of wrong sign predictions against actual sign of value
lapply(lass12mth, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))

lass12mthrmsedf =  data.frame('type' = 'post-LASSO', 'window25' = rlass12mth25rmse, 'window50' = rlass12mth50rmse, 
                             'window75' = rlass12mth75rmse,'window100' = rlass12mth100rmse)

#Dataframe of RMSE for best 12 Months prediction model
lass12mthrmse = data.frame('horizon'='12 Month','window25' = rlass12mth25rmse, 'window50' = rlass12mth50rmse, 'window75' = rlass12mth75rmse,
                          'window100' = rlass12mth100rmse)

#Combine the RMSE 12 Months dataframe to bestdf to collate RMSE for each horizon prediction
bestdf = rbind(bestdf,lass12mthrmse)
# write.csv(bestdf,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/bestrmse.csv')

#Save each wrong sign percentage in each rolling window
wrong12mth = c()
for (i in 1:4){
  wrong12mth = c(wrong12mth,round(lapply(lass12mth, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))[[i]],4)*100)
}

#Dataframe of wrong sign percentage in 12 Month prediction
wrong12mthdf = data.frame('horizon'='12 Month','window25' = wrong12mth[1], 'window50' = wrong12mth[2], 
                         'window75' = wrong12mth[3],'window100' = wrong12mth[4])

#Combine the wrong sign percentage to wrongdf dataframe to collate for each horizon prediction
wrongdf = rbind(wrongdf,wrong12mthdf)

# write.csv(wrongdf,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/wrongsignlasso.csv')

#######
# ANN #
#######
###########
# 1 Month #
###########
nnlst = list() #Save all 1 Month ANN prediction for all lambdas
nnlst[[1]] = read.csv('ann 1mth 25.csv') #25 Months rolling window
nnlst[[2]] = read.csv('ann 1mth 50.csv') #50 Months rolling window
nnlst[[3]] = read.csv('ann 1mth 75.csv') #75 Months rolling window
nnlst[[4]] = read.csv('ann 1mth 100.csv') #100 Months rolling window

hanndf(nnlst[[1]],25,1)
for(i in 1:4){
  nnlst[[i]] = subset(nnlst[[i]],select = -c(X)) #take out 'X' column
}

#include corresponding date and actual values for each window and each predictions
for (i in 1:4){
  nnlst[[i]] = hanndf(nnlst[[i]],winds[i],1)
}

msenn = c() #Save the lambda position that minimises MSE for each rolling window
for (i in 1:length(nnlst)){ #i = rolling window (i.e. 25,50 etc)
  xx = c()
  for (j in 1:10){ #j = lambda column
    xx = c(xx,summary(lm((nnlst[[i]][,j+2]-nnlst[[i]][['val']])^2~1))$coef[1]) #calculate MSE
  }
  msenn=c(msenn,which.min(xx)) #save the lambda position that minimises MSE
}
msenn

for (i in 1:length(nnlst)){
  nnlst[[i]] = nnlst[[i]][,c(1,2,msenn[i]+2)] #extract the predictions from optimal lambda for each rolling window
  colnames(nnlst[[i]])[length(colnames(nnlst[[i]]))] = 'pred' #change prediciton column name to 'pred
}

for (i in 1:length(nnlst)){
  nnlst[[i]][['Date']] = as.yearmon(nnlst[[i]][['Date']]) #change Date to yearmon
}

#calculate RMSE for each rolling window
lapply(nnlst, function(x) mean((x[['pred']]-x[['val']])**2)**.5)
#calculate percentage of wrong sign predictions against actual sign of value
lapply(nnlst, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))

for(i in 1:4){
  name = paste('nn1mth',winds[i],'rmse',sep = '')
  assign(name,lapply(nnlst, function(x) mean((x[['pred']]-x[['val']])**2)**.5)[[i]])
}

nn1mthrmsedf = data.frame('type' = 'ANN', 'window25' = nn1mth25rmse, 'window50' = nn1mth50rmse, 'window75' = nn1mth75rmse,
                          'window100' = nn1mth100rmse)


###########
# 3 Month #
###########

nn3mth = list() #Save all 3 Months ANN prediction for all lambdas
nn3mth[[1]] = read.csv('ann 3mth 25.csv') #25 Months rolling window
nn3mth[[2]] = read.csv('ann 3mth 50.csv') #50 Months rolling window
nn3mth[[3]] = read.csv('ann 3mth 75.csv') #75 Months rolling window
nn3mth[[4]] = read.csv('ann 3mth 100.csv') #100 Months rolling window

for(i in 1:4){
  nn3mth[[i]] = subset(nn3mth[[i]],select = -c(X))  #take out 'X' column
}

#include corresponding date and actual values for each window and each predictions
for (i in 1:4){
  nn3mth[[i]] = hanndf(nn3mth[[i]],winds[i],3)
}
msenn = c() #Save the lambda position that minimises MSE for each rolling window
for (i in 1:length(nn3mth)){ #i = rolling window (i.e. 25,50 etc)
  xx = c()
  for (j in 1:10){ #j = lambda column
    xx = c(xx,summary(lm((nn3mth[[i]][,j+2]-nn3mth[[i]][['val']])^2~1))$coef[1]) #calculate MSE
  }
  msenn=c(msenn,which.min(xx)) #save the lambda position that minimises MSE
}
msenn

for (i in 1:length(nn3mth)){
  nn3mth[[i]] = nn3mth[[i]][,c(1,2,msenn[i]+2)] #extract the predictions from optimal lambda for each rolling window
  colnames(nn3mth[[i]])[length(colnames(nn3mth[[i]]))] = 'pred' #change prediciton column name to 'pred'
}

for (i in 1:length(nn3mth)){
  nn3mth[[i]][['Date']] = as.yearmon(nn3mth[[i]][['Date']]) #change Date to yearmon
}

#calculate RMSE for each rolling window
lapply(nn3mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)
#calculate percentage of wrong sign predictions against actual sign of value
lapply(nn3mth, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))

for(i in 1:4){
  name = paste('nn3mth',winds[i],'rmse',sep = '')
  assign(name,lapply(nn3mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)[[i]])
}

nn3mthrmsedf = data.frame('type' = 'ANN', 'window25' = nn3mth25rmse, 'window50' = nn3mth50rmse, 'window75' = nn3mth75rmse,
                          'window100' = nn3mth100rmse)

###########
# 6 Month #
###########

nn6mth = list() #Save all 3 Months ANN prediction for all lambdas
nn6mth[[1]] = read.csv('ann 6mth 25.csv') #25 Months rolling window
nn6mth[[2]] = read.csv('ann 6mth 50.csv') #50 Months rolling window
nn6mth[[3]] = read.csv('ann 6mth 75.csv') #75 Months rolling window
nn6mth[[4]] = read.csv('ann 6mth 100.csv') #100 Months rolling window

for(i in 1:4){
  nn6mth[[i]] = subset(nn6mth[[i]],select = -c(X)) #take out 'X' column
}

#include corresponding date and actual values for each window and each predictions
for (i in 1:4){
  nn6mth[[i]] = hanndf(nn6mth[[i]],winds[i],6)
}

msenn = c() #Save the lambda position that minimises MSE for each rolling window
for (i in 1:length(nn6mth)){ #i = rolling window (i.e. 25,50 etc)
  xx = c()
  for (j in 1:10){ #j = lambda column
    xx = c(xx,summary(lm((nn6mth[[i]][,j+2]-nn6mth[[i]][['val']])^2~1))$coef[1]) #calculate MSE
  }
  msenn=c(msenn,which.min(xx)) #save the lambda position that minimises MSE
}
msenn

for (i in 1:length(nn6mth)){
  nn6mth[[i]] = nn6mth[[i]][,c(1,2,msenn[i]+2)] #extract the predictions from optimal lambda for each rolling window
  colnames(nn6mth[[i]])[length(colnames(nn6mth[[i]]))] = 'pred' #change prediciton column name to 'pred'
}

for (i in 1:length(nn6mth)){
  nn6mth[[i]][['Date']] = as.yearmon(nn6mth[[i]][['Date']]) #change Date to yearmon
}

#calculate RMSE for each rolling window
lapply(nn6mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)
#calculate percentage of wrong sign predictions against actual sign of value
lapply(nn6mth, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))

for(i in 1:4){
  name = paste('nn6mth',winds[i],'rmse',sep = '')
  assign(name,lapply(nn6mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)[[i]])
}

nn6mthrmsedf = data.frame('type' = 'ANN', 'window25' = nn6mth25rmse, 'window50' = nn6mth50rmse, 'window75' = nn6mth75rmse,
                          'window100' = nn6mth100rmse)


############
# 12 Month #
############

nn12mth = list() #Save all 3 Months ANN prediction for all lambdas
nn12mth[[1]] = read.csv('ann 12mth 25.csv') #25 Months rolling window
nn12mth[[2]] = read.csv('ann 12mth 50.csv') #50 Months rolling window
nn12mth[[3]] = read.csv('ann 12mth 75.csv') #75 Months rolling window
nn12mth[[4]] = read.csv('ann 12mth 100.csv') #100 Months rolling window

for(i in 1:4){
  nn12mth[[i]] = subset(nn12mth[[i]],select = -c(X)) #take out 'X' column
}

#include corresponding date and actual values for each window and each predictions
for (i in 1:4){
  nn12mth[[i]] = hanndf(nn12mth[[i]],winds[i],12)
}

msenn = c() #Save the lambda position that minimises MSE for each rolling window
for (i in 1:length(nn12mth)){ #i = rolling window (i.e. 25,50 etc)
  xx = c()
  for (j in 1:10){ #j = lambda column
    xx = c(xx,summary(lm((nn12mth[[i]][,j+2]-nn12mth[[i]][['val']])^2~1))$coef[1]) #Calculate MSE
  }
  msenn=c(msenn,which.min(xx)) #save the lambda position that minimises MSE
}
msenn

for (i in 1:length(nn12mth)){
  nn12mth[[i]] = nn12mth[[i]][,c(1,2,msenn[i]+2)] #extract the predictions from optimal lambda for each rolling window
  colnames(nn12mth[[i]])[length(colnames(nn12mth[[i]]))] = 'pred' #change prediciton column name to 'pred'
}

for (i in 1:length(nn12mth)){
  nn12mth[[i]][['Date']] = as.yearmon(nn12mth[[i]][['Date']]) #change Date to yearmon
}

#calculate RMSE for each rolling window
lapply(nn12mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)
#calculate percentage of wrong sign predictions against actual sign of value
lapply(nn12mth, function(x) sum(sign(x[['val']]) != sign(x[['pred']]))/nrow(x))


for(i in 1:4){
  name = paste('nn12mth',winds[i],'rmse',sep = '')
  assign(name,lapply(nn12mth, function(x) mean((x[['pred']]-x[['val']])**2)**.5)[[i]])
}

nn12mthrmsedf = data.frame('type' = 'ANN', 'window25' = nn12mth25rmse, 'window50' = nn12mth50rmse, 'window75' = nn12mth75rmse,
                          'window100' = nn12mth100rmse)


####################
# Goyal and Welch #
###################

# 1 Month #
exp1mth = list() #Save all 1 month prediction using expanding mean for each rolling window
expmean = read.csv('expanding mean.csv') 
expmean = subset(expmean,select = -c(X)) #take out 'X' columns
expmean$Date = as.yearmon(expmean$Date) #change Date to yearmon

# Extract the corresponding Date and actual value for each prediction using expanding mean
for (i in 1:4){
  exp1mth[[i]] = inner_join(rfls[[i]][c('Date','val')],expmean,by='Date')
}
#calculate RMSE for each rolling window
lapply(exp1mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)
#calculate percentage of wrong sign predictions against actual sign of value
lapply(exp1mth, function(x) sum(sign(x[['expmean']]) != sign(x[['val']]))/nrow(x))

#Save rmse for 1Month prediction for each rolling window
for(i in 1:4){
  name = paste('gw1mth',winds[i],'rmse',sep = '') 
  assign(name,lapply(exp1mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)[[i]])
}

#Data frame for 1 Month prediction RMSE
gw1mthdf = data.frame('type' = 'goyal and welch model', 'window25' = gw1mth25rmse,'window50' = gw1mth50rmse,
                      'window75' = gw1mth75rmse,'window100' = gw1mth100rmse)

#Save all wrong sign prediction percentage for 1 Month prediction for each rolling window
exp1mthwrong = c()
for (i in 1:4){
  exp1mthwrong = c(exp1mthwrong,round(lapply(exp1mth, function(x) sum(sign(x[['val']]) != sign(x[['expmean']]))/nrow(x))[[i]],4)*100)
}

#Data frame for wrong prediction
expwrongdf = data.frame('horizon'='1 Month','window25' = exp1mthwrong[1], 
                        'window50' = exp1mthwrong[2],'window75' = exp1mthwrong[3],'window100' = exp1mthwrong[4])

# 3 Months #
exp3mth = list() #Save all 3 month prediction using expanding mean for each rolling window

# Extract the corresponding Date and actual value for each prediction using expanding mean
for (i in 1:4){
  exp3mth[[i]] = inner_join(rf3mth[[i]][c('Date','val')],expmean,by='Date')
}

#calculate RMSE for each rolling window
lapply(exp3mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)

#Save rmse for 3Month prediction for each rolling window
for(i in 1:4){
  name = paste('gw3mth',winds[i],'rmse',sep = '')
  assign(name,lapply(exp3mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)[[i]])
}

#Data frame for 3 Month prediction RMSE
gw3mthdf = data.frame('type' = 'goyal and welch model', 'window25' = gw3mth25rmse,'window50' = gw3mth50rmse,
                      'window75' = gw3mth75rmse,'window100' = gw3mth100rmse)

#Save all wrong sign prediction percentage for 3 Month prediction for each rolling window
exp3mthwrong = c()
for (i in 1:4){
  exp3mthwrong = c(exp3mthwrong,round(lapply(exp3mth, function(x) sum(sign(x[['val']]) != sign(x[['expmean']]))/nrow(x))[[i]],4)*100)
}

#Data frame of wrong sign 3 Month prediction for benchmark model
exp3mthwrongdf = data.frame('horizon'='3 Month','window25' = exp3mthwrong[1], 
                        'window50' = exp3mthwrong[2],'window75' = exp3mthwrong[3],
                        'window100' = exp3mthwrong[4])


#Combine the wrong sign percentage for benchmark model for each horizon prediction
expwrongdf = rbind(expwrongdf,exp3mthwrongdf)

# 6 Months # 
exp6mth = list() #Save all 6 month prediction using expanding mean for each rolling window

# Extract the corresponding Date and actual value for each prediction using expanding mean
for (i in 1:4){
  exp6mth[[i]] = inner_join(rf6mth[[i]][c('Date','val')],expmean,by='Date')
}

#calculate RMSE for each rolling window
lapply(exp6mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)

#Save rmse for 6Month prediction for each rolling window
for(i in 1:4){
  name = paste('gw6mth',winds[i],'rmse',sep = '')
  assign(name,lapply(exp6mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)[[i]])
}

#Data frame for 6 Month prediction RMSE
gw6mthdf = data.frame('type' = 'goyal and welch model', 'window25' = gw6mth25rmse,'window50' = gw6mth50rmse,
                      'window75' = gw6mth75rmse,'window100' = gw6mth100rmse)

#Save all wrong sign prediction percentage for 6 Month prediction for each rolling window
exp6mthwrong = c()
for (i in 1:4){
  exp6mthwrong = c(exp6mthwrong,round(lapply(exp6mth, function(x) sum(sign(x[['val']]) != sign(x[['expmean']]))/nrow(x))[[i]],4)*100)
}

#Data frame of wrong sign 6 Month prediction for benchmark model
exp6mthwrongdf = data.frame('horizon'='6 Month','window25' = exp6mthwrong[1], 
                            'window50' = exp6mthwrong[2],'window75' = exp6mthwrong[3],
                            'window100' = exp6mthwrong[4])

#Combine the wrong sign percentage for benchmark model for each horizon prediction
expwrongdf = rbind(expwrongdf,exp6mthwrongdf)

# 12 Months #
exp12mth = list() #Save all 12 month prediction using expanding mean for each rolling window

# Extract the corresponding Date and actual value for each prediction using expanding mean
for (i in 1:4){
  exp12mth[[i]] = inner_join(rf12mth[[i]][c('Date','val')],expmean,by='Date')
}

#calculate RMSE for each rolling window
lapply(exp12mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)

#Save rmse for 12Month prediction for each rolling window
for(i in 1:4){
  name = paste('gw12mth',winds[i],'rmse',sep = '')
  assign(name,lapply(exp12mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)[[i]])
}

#Data frame for 12 Month prediction RMSE
gw12mthdf = data.frame('type' = 'goyal and welch model', 'window25' = gw12mth25rmse,'window50' = gw12mth50rmse,
                      'window75' = gw12mth75rmse,'window100' = gw12mth100rmse)

#Save all wrong sign prediction percentage for 12 Month prediction for each rolling window
exp12mthwrong = c()
for (i in 1:4){
  exp12mthwrong = c(exp12mthwrong,round(lapply(exp12mth, function(x) sum(sign(x[['val']]) != sign(x[['expmean']]))/nrow(x))[[i]],4)*100)
}

#Data frame of wrong sign 12 Month prediction for benchmark model
exp12mthwrongdf = data.frame('horizon'='12 Month','window25' = exp12mthwrong[1], 
                            'window50' = exp12mthwrong[2],'window75' = exp12mthwrong[3],
                            'window100' = exp12mthwrong[4])

#Combine the wrong sign percentage for benchmark model for each horizon prediction
expwrongdf = rbind(expwrongdf,exp12mthwrongdf)
# write.csv(expwrongdf,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/expwrongsign.csv')

###########
# RMSE df #
###########
###########
# 1 Month #
###########
rmse1mth = rbind(rf1mthrmsedf,lass1mthrmsedf,nn1mthrmsedf,gw1mthdf)

############
# 3 Months #
############
rmse3mth = rbind(rf3mthrmsedf,lass3mthrmsedf,nn3mthrmsedf,gw3mthdf)

############
# 6 Months #
############
rmse6mth = rbind(rf6mthrmsedf,lass6mthrmsedf,nn6mthrmsedf,gw6mthdf)

#############
# 12 Months #
#############
rmse12mth = rbind(rf12mthrmsedf,lass12mthrmsedf,nn12mthrmsedf,gw12mthdf)

# Best Model (post-LASSO)#
bestdf

##########
# Graphs #
##########
###############
#Random Forest#
###############
# 1 Month Prediction #
for (i in 1:4){
  plot(rfls[[i]][['Date']],rfls[[i]][['val']],type="l",col="red",
       main = paste('Random Forest (1 Month prediction) using',winds[i],'Months training window'),xlab = 'Date',ylab = 'nasdaqpctchg')
  lines(rfls[[i]][['Date']],rfls[[i]][['pred']],col="green")
  lines(exp1mth[[i]][['Date']],exp1mth[[i]][['expmean']],col="black")
  legend('bottomright', legend = c('Actual values','Random Forest','benchmark model'),
         col=c("red", "green",'black'),lty=1,cex = .75)
  abline(h = 0,lty=2)
}

# 3 Months Prediction #
for (i in 1:4){
  plot(rf3mth[[i]][['Date']],rf3mth[[i]][['val']],type="l",col="red",
       main = paste('Random Forest (3 Month prediction) using',winds[i],'Months training window'),xlab = 'Date',ylab = 'nasdaqpctchg')
  lines(rf3mth[[i]][['Date']],rf3mth[[i]][['pred']],col="green")
  lines(exp3mth[[i]][['Date']],exp3mth[[i]][['expmean']],col="black")
  legend('bottomright', legend = c('Actual values','Random Forest','benchmark model'),
         col=c("red", "green",'black'),lty=1,cex = .75)
  abline(h = 0,lty=2)
}

# 6 Month Prediction #
for (i in 1:4){
  plot(rf6mth[[i]][['Date']],rf6mth[[i]][['val']],type="l",col="red",
       main = paste('Random Forest (6 Month prediction) using',winds[i],'Months training window'),xlab = 'Date',ylab = 'nasdaqpctchg')
  lines(rf6mth[[i]][['Date']],rf6mth[[i]][['pred']],col="green")
  lines(exp6mth[[i]][['Date']],exp6mth[[i]][['expmean']],col="black")
  legend('bottomright', legend = c('Actual values','Random Forest','benchmark model'),
         col=c("red", "green",'black'),lty=1,cex = .75)
  abline(h = 0,lty=2)
}

# 12 Month Prediction #
for (i in 1:4){
  plot(rf12mth[[i]][['Date']],rf12mth[[i]][['val']],type="l",col="red",
       main = paste('Random Forest (12 Month prediction) using',winds[i],'Months training window'),xlab = 'Date',ylab = 'nasdaqpctchg')
  lines(rf12mth[[i]][['Date']],rf12mth[[i]][['pred']],col="green")
  lines(exp12mth[[i]][['Date']],exp12mth[[i]][['expmean']],col="black")
  legend('bottomright', legend = c('Actual values','Random Forest','benchmark model'),
         col=c("red", "green",'black'),lty=1,cex = .75)
  abline(h = 0,lty=2)
}

#########################
#post-LASSO (best model)#
#########################
# 1 Month Prediction #
for (i in 1:4){
  plot(rlassolst[[i]][['Date']],rlassolst[[i]][['val']],type="l",col="red",
       main = paste('post-LASSO (1 Month prediction) using',winds[i],'Months training window'),xlab = 'Date',ylab = 'nasdaqpctchg')
  lines(rlassolst[[i]][['Date']],rlassolst[[i]][['pred']],col="green")
  lines(exp1mth[[i]][['Date']],exp1mth[[i]][['expmean']],col="black")
  legend('bottomright', legend = c('Actual values','rlasso (post)','benchmark model'),
         col=c("red", "green",'black'),lty=1,cex = .75)
  abline(h = 0,lty=2)
}

# 3 Month Prediction #
for (i in 1:4){
  plot(lass3mth[[i]][['Date']],lass3mth[[i]][['val']],type="l",col="red",
       main = paste('post-LASSO (1 Month prediction) using',winds[i],'Months trainig window'),xlab = 'Date',ylab = 'nasdaqpctchg')
  lines(lass3mth[[i]][['Date']],lass3mth[[i]][['pred']],col="green")
  lines(exp3mth[[i]][['Date']],exp3mth[[i]][['expmean']],col="black")
  legend('bottomright', legend = c('Actual values','rlasso (post)','benchmark model'),
         col=c("red", "green",'black'),lty=1,cex = .75)
  abline(h = 0,lty=2)
}

# 6 Month Prediction #
for (i in 1:4){
  plot(lass6mth[[i]][['Date']],lass6mth[[i]][['val']],type="l",col="red",
       main = paste('post-LASSO (6 Month prediction) using',winds[i],'Months training window'),xlab = 'Date',ylab = 'nasdaqpctchg')
  lines(lass6mth[[i]][['Date']],lass6mth[[i]][['pred']],col="green")
  lines(exp6mth[[i]][['Date']],exp6mth[[i]][['expmean']],col="black")
  legend('bottomright', legend = c('Actual values','rlasso (post)','benchmark model'),
         col=c("red", "green",'black'),lty=1,cex = .75)
  abline(h = 0,lty=2)
}

# 12 Month Prediction #
for (i in 1:4){
  plot(lass12mth[[i]][['Date']],lass12mth[[i]][['val']],type="l",col="red",
       main = paste('post-LASSO (12 Month prediction) using',winds[i],'Months training window'),xlab = 'Date',ylab = 'nasdaqpctchg')
  lines(lass12mth[[i]][['Date']],lass12mth[[i]][['pred']],col="green")
  lines(exp12mth[[i]][['Date']],exp12mth[[i]][['expmean']],col="black")
  legend('bottomright', legend = c('Actual values','rlasso (post)','benchmark model'),
         col=c("red", "green",'black'),lty=1,cex = .75)
  abline(h = 0,lty=2)
}

#####
#ANN#
#####
# 1 Month Prediction #
for (i in 1:4){
  plot(nnlst[[i]][['Date']],nnlst[[i]][['val']],type="l",col="red",
       main = paste('ANN (1 Month prediction) using',winds[i],'Months trainig window'),xlab = 'Date',ylab = 'nasdaqpctchg')
  lines(nnlst[[i]][['Date']],nnlst[[i]][['pred']],col="green")
  lines(exp1mth[[i]][['Date']],exp1mth[[i]][['expmean']],col="black")
  legend('bottomright', legend = c('Actual values','ANN','benchmark model'),
         col=c("red", "green",'black'),lty=1,cex = .75)
  abline(h = 0,lty=2)
}

# 3 Month Prediction #
for (i in 1:4){
  plot(nn3mth[[i]][['Date']],nn3mth[[i]][['val']],type="l",col="red",
       main = paste('ANN (3 Month prediction) using',winds[i],'Months trainig window'),xlab = 'Date',ylab = 'nasdaqpctchg')
  lines(nn3mth[[i]][['Date']],nn3mth[[i]][['pred']],col="green")
  lines(exp3mth[[i]][['Date']],exp3mth[[i]][['expmean']],col="black")
  legend('bottomright', legend = c('Actual values','ANN','benchmark model'),
         col=c("red", "green",'black'),lty=1,cex = .75)
  abline(h = 0,lty=2)
}

# 6 Month Prediction #
for (i in 1:4){
  plot(nn6mth[[i]][['Date']],nn6mth[[i]][['val']],type="l",col="red",
       main = paste('ANN (6 Month prediction) using',winds[i],'Months trainig window'),xlab = 'Date',ylab = 'nasdaqpctchg')
  lines(nn6mth[[i]][['Date']],nn6mth[[i]][['pred']],col="green")
  lines(exp6mth[[i]][['Date']],exp6mth[[i]][['expmean']],col="black")
  legend('bottomright', legend = c('Actual values','ANN','benchmark model'),
         col=c("red", "green",'black'),lty=1,cex = .75)
  abline(h = 0,lty=2)
}
# 12 Month Prediction #
for (i in 1:4){
  plot(nn12mth[[i]][['Date']],nn12mth[[i]][['val']],type="l",col="red",
       main = paste('ANN (12 Month prediction) using',winds[i],'Months training window'),xlab = 'Date',ylab = 'nasdaqpctchg')
  lines(nn12mth[[i]][['Date']],nn12mth[[i]][['pred']],col="green")
  lines(exp12mth[[i]][['Date']],exp12mth[[i]][['expmean']],col="black")
  legend('bottomright', legend = c('Actual values','ANN','benchmark model'),
         col=c("red", "green",'black'),lty=1,cex = .75)
  abline(h = 0,lty=2)
}



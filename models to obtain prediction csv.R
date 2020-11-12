rm(list = ls())
setwd('/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final')
library(randomForest)
library(nnet)
library(hdm)
library(zoo)
source('roll func.R')

final = read.csv('final.csv')
final = subset(final, select = -c(X))
final$Date = as.yearmon(final$Date)
y = final$nasdaqpctchg

winds = c(25,50,75,100) #rolling window used
rfls = list() #save all prediction for 1 Month prediction using different window

set.seed(785903475) #seed for replication
##################
# Random Forest #
################

### window = 25, 1 month prediction ###
res = rollpred.rf(25,final)
rfls[[1]] = msecalc(rf25,25,1,res)
rf25rmse = mean(rfls[[1]][,'square_error']) ** .5

### window = 50, 1 month prediction ###
res = rollpred.rf(50,final)
rfls[[2]] = msecalc(rf50,50,1,res)
rf50rmse = mean(rfls[[2]][,'square_error']) ** .5

### window = 75, 1 month prediction  ###
res = rollpred.rf(75,final)
rfls[[3]] = msecalc(rf75,75,1,res)
rf75rmse = mean(rfls[[3]][,'square_error']) ** .5

### window = 100, 1 month prediction  ###
res = rollpred.rf(100,final)
rfls[[4]] = msecalc(rf100,100,1,res)
rf100rmse = mean(rfls[[4]][,'square_error']) ** .5

# To obtain prediction csv run a loop
# for(i in 1:4){
#   name = paste('rf1mth',i,'.csv',sep = '')
#   filename = paste('/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/',
#         name,sep = '')
#   write.csv(rfls[[i]],filename)
# }


###########
# 3 Month #
###########
rf3mth = list() #save all prediction for 3 Month prediction using different window

### window = 25, 3 month prediction  ###
res3mth = hpred.rf(25,final,3)
rf3mth[[1]] = msecalc(rf25,25,3,res3mth)
rf3mth25rmse = mean(rf3mth[[1]][,'square_error']) ** .5
rf3mth25rmse

### window = 50, 3 month prediction  ###
res3mth50 = hpred.rf(50,final,3)
rf3mth[[2]] = msecalc(rf50,50,3,res3mth50)
rf3mth50rmse = mean(rf3mth[[2]][,'square_error']) ** .5
rf3mth50rmse 

### window = 75, 3 month prediction ###
res3mth75 = hpred.rf(75,final,3)
rf3mth[[3]] = msecalc(rf75,75,3,res3mth75)
rf3mth75rmse = mean(rf3mth[[3]][,'square_error']) ** .5
rf3mth75rmse

### window = 100, 3 month prediction ###
res3mth100 = hpred.rf(100,final,3)
rf3mth[[4]] = msecalc(rf100,100,3,res3mth100)
rf3mth100rmse = mean(rf3mth[[4]][,'square_error']) ** .5
rf3mth100rmse

rf3mthdf = data.frame('type' = 'random forest' ,'window25' = rf3mth25rmse,'window50' = rf3mth50rmse,
                    'window75' = rf3mth75rmse,'window100' = rf3mth100rmse)

# To obtain prediction csv run a loop
# for(i in 1:4){
#   name = paste('rf3mth',i,'.csv',sep = '')
#   filename = paste('/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/',
#         name,sep = '')
#   write.csv(rf3mth[[i]],filename)
# }

###########
# 6 Month #
###########
rf6mth = list() #save all prediction for 6 Month prediction using different window

### window = 25, 6 month prediction  ###
res6mth = hpred.rf(25,final,6)
rf6mth[[1]] = msecalc(rf25,25,6,res6mth)
rf6mth25rmse = mean(rf6mth[[1]][,'square_error']) ** .5
rf6mth25rmse

### window = 50, 6 month prediction  ###
res6mth50 = hpred.rf(50,final,6)
rf6mth[[2]] = msecalc(rf50,50,6,res6mth50)
rf6mth50rmse = mean(rf6mth[[2]][,'square_error']) ** .5
rf6mth50rmse

### window = 75, 6 month prediction ###
res6mth75 = hpred.rf(75,final,6)
rf6mth[[3]] = msecalc(rf75,75,6,res6mth75)
rf6mth75rmse = mean(rf6mth[[3]][,'square_error']) ** .5
rf6mth75rmse

### window = 100, 6 month prediction ###
res6mth100 = hpred.rf(100,final,6)
rf6mth[[4]] = msecalc(rf100,100,6,res6mth100)
rf6mth100rmse = mean(rf6mth[[4]][,'square_error']) ** .5
rf6mth100rmse

rf6mthdf = data.frame('type' = 'random forest','window25' = rf6mth25rmse,'window50' = rf6mth50rmse,
                        'window75' = rf6mth75rmse,'window100' = rf6mth100rmse)

# To obtain prediction csv run a loop
# for(i in 1:4){
#   name = paste('rf6mth',winds[i],'.csv',sep = '')
#   filename = paste('/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/',
#         name,sep = '')
#   write.csv(rf6mth[[i]],filename)
# }

############
# 12 Month #
############
rf12mth = list() #save all prediction for 12 Month prediction using different window

### window = 25, 12 month prediction  ###
res12mth = hpred.rf(25,final,12)
rf12mth[[1]] = msecalc(rf25,25,12,res12mth)
rf12mth25rmse = mean(rf12mth[[1]][,'square_error']) ** .5
rf12mth25rmse

### window = 50, 12 month prediction  ###
res12mth50 = hpred.rf(50,final,12)
rf12mth[[2]] = msecalc(rf50,50,12,res12mth50)
rf12mth50rmse = mean(rf12mth[[2]][,'square_error']) ** .5
rf12mth50rmse

### window = 75, 12 month prediction ###
res12mth75 = hpred.rf(75,final,12)
rf12mth[[3]] = msecalc(rf75,75,12,res12mth75)
rf12mth75rmse = mean(rf12mth[[3]][,'square_error']) ** .5
rf12mth75rmse

### window = 100, 12 month prediction ###
res12mth100 = hpred.rf(100,final,12)
rf12mth[[4]] = msecalc(rf100,100,12,res12mth100)
rf12mth100rmse = mean(rf12mth[[4]][,'square_error']) ** .5
rf12mth100rmse

rf12mthdf = data.frame('type' = 'random forest','window25' = rf12mth25rmse,'window50' = rf12mth50rmse,
                        'window75' = rf12mth75rmse,'window100' = rf12mth100rmse)

# To obtain prediction csv run a loop
# for(i in 1:4){
#   name = paste('rf12mth',winds[i],'.csv',sep = '')
#   filename = paste('/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/',
#         name,sep = '')
#   write.csv(rf12mth[[i]],filename)
# }

set.seed(785903475) #seed for replication

dec=c(0,0.001,0.003,0.01,0.03,0.1,0.3,1,3,10) #lambdas used for regularisation

Y  =  model.matrix(~nasdaqpctchg-1, data=final)   # note (-1) in the formula omits the constant regressor
X  =  model.matrix(~.-1-Date, data=final)   # note (-1) in the formula omits the constant regressor          # X matrix for neural and forest

data = as.data.frame(cbind(Y,X)) #dataframe to be fed to the ann function

prednn25 = data.frame(one=numeric(0),two=numeric(0),thr=numeric(0),four=numeric(0),fve=numeric(0),
                       six=numeric(0),svn=numeric(0),eight=numeric(0),nine=numeric(0),ten=numeric(0))

prednn25 = ann(25,data)
View(prednn25)
# write.csv(prednn25,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 25.csv')

# prednn50 = ann(50,data)
View(prednn50)
# write.csv(prednn50,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 50.csv')

prednn50 = read.csv('ann 50.csv')
View(prednn50)
prednn50 = prednn50[,-1]

prednn75 = ann(75,data)
View(prednn75)
# write.csv(prednn75,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 75.csv' )

prednn100 = ann(100,data)
View(prednn100)
# write.csv(prednn100,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 100.csv')

### Code to find optimal lambda is in result.R script ###

#######################
# 3 months prediction #
#######################
nn3mth = list() #list to save all 3 Month ANN prediction with corresponding date and actual value
dec=c(0,0.001,0.003,0.01,0.03,0.1,0.3,1,3,10)

prednn3mth25 = hann(25,data,3)
View(prednn3mth25)
# write.csv(prednn3mth25,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 3mth 25.csv')

prednn3mth50 = hann(50,data,3)
View(prednn3mth50)
# write.csv(prednn3mth50,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 3mth 50.csv')

prednn3mth75 = hann(75,data,3)
View(prednn3mth75)
# write.csv(prednn3mth75,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 3mth 75.csv' )

prednn3mth100 = hann(100,data,3)
View(prednn3mth100)
# write.csv(prednn3mth100,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 3mth 100.csv')

### Code to find optimal lambda is in result.R script ###

#######################
# 6 months prediction #
#######################
nn6mth = list() #list to save all 6 Month ANN prediction with corresponding date and actual value
dec=c(0,0.001,0.003,0.01,0.03,0.1,0.3,1,3,10)

prednn6mth25 = hann(25,data,6)
View(prednn6mth25)
# write.csv(prednn6mth25,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 6mth 25.csv')

prednn6mth50 = hann(50,data,6)
View(prednn6mth50)
# write.csv(prednn6mth50,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 6mth 50.csv')

prednn6mth75 = hann(75,data,6)
View(prednn6mth75)
# write.csv(prednn6mth75,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 6mth 75.csv' )

prednn6mth100 = hann(100,data,6)
View(prednn6mth100)
# write.csv(prednn6mth100,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 6mth 100.csv')

### Code to find optimal lambda is in result.R script ###

#######################
# 12 months prediction #
#######################
nn12mth = list() #list to save all 12 Month ANN prediction with corresponding date and actual value
dec=c(0,0.001,0.003,0.01,0.03,0.1,0.3,1,3,10)

prednn12mth25 = hann(25,data,12)
View(prednn12mth25)
# write.csv(prednn12mth25,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 12mth 25.csv')

prednn12mth50 = hann(50,data,12)
View(prednn12mth50)
# write.csv(prednn12mth50,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 12mth 50.csv')

prednn12mth75 = hann(75,data,12)
View(prednn12mth75)
# write.csv(prednn12mth75,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 12mth 75.csv' )

prednn12mth100 = hann(100,data,12)
View(prednn12mth100)
# write.csv(prednn12mth100,'/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/ann 12mth 100.csv')

### Code to find optimal lambda is in result.R script ###

### RLASSO ###

#######################
# 1 month prediction #
#######################
rlassolst = list() #list to save all 1 Month rlasso prediction with different training windows

# 25 window #
lasso25 = rollrlasso(25,final)
rlassolst[[1]] = msecalc(las25,25,1,lasso25)

# 50 window #
lasso50 = rollrlasso(50,final)
rlassolst[[2]] = msecalc(las25,50,1,lasso50)

# 75 window #
lasso75 = rollrlasso(75,final)
rlassolst[[3]] = msecalc(las25,75,1,lasso75)

# 100 window #
lasso100 = rollrlasso(100,final)
rlassolst[[4]] = msecalc(las25,100,1,lasso100)

#######################
# 3 months prediction #
#######################
lasslst = list() #Save all 3 Months prediction that uses different training windows

### window = 25 ###
lass3mth25 = hrlasso(25,final,3)
lasslst[[1]] = msecalc(rf25,25,3,lass3mth25)

### window = 50 ###
las3mth50 = hrlasso(50,final,3)
lasslst[[2]] = msecalc(rf25,50,3,las3mth50)

### window = 75 ###
las3mth75 = hrlasso(75,final,3)
lasslst[[3]] = msecalc(rf25,75,3,las3mth75)

### window = 100 ###
las3mth100 = hrlasso(100,final,3)
lasslst[[4]] = msecalc(rf25,100,3,las3mth100)

# for(i in 1:4){
#   name = paste('lass3mth',i,'.csv',sep = '')
#   filename = paste('/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/',
#         name,sep = '')
#   write.csv(lasslst[[i]],filename)
# }

#######################
# 6 months prediction #
#######################
lass6mth = list() #Save all 6 Months prediction that uses different training windows

### window = 25 ###
lass6mth25 = hrlasso(25,final,6)
lass6mth[[1]] = msecalc(rf25,25,6,lass6mth25)

### window = 50 ###
lass6mth50 = hrlasso(50,final,6)
lass6mth[[2]] = msecalc(rf25,50,6,lass6mth50)

### window = 75 ###
las6mth75 = hrlasso(75,final,6)
lasslst[[3]] = msecalc(rf25,75,6,lass6mth75)

### window = 100 ###
las6mth100 = hrlasso(100,final,6)
lasslst[[4]] = msecalc(rf25,100,6,lass6mth100)

# for(i in 1:4){
#   name = paste('lass6mth',i,'.csv',sep = '')
#   filename = paste('/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/',
#         name,sep = '')
#   write.csv(lasslst[[i]],filename)
# }

#######################
# 12 months prediction #
#######################
lass12mth = list() #Save all 12 Months prediction using different rolling window

### window = 25 ###
lass12mth25 = hrlasso(25,final,12)
lass12mth[[1]] = msecalc(rf25,25,12,lass12mth25)
lass12mth25rmse = mean(lass12mth[[1]][,'square_error']) ** .5
lass12mth25rmse 

### window = 50 ###
lass12mth50 = hrlasso(50,final,12)
lass12mth[[2]] = msecalc(rf25,50,12,las12mth50)
lass12mth50rmse = mean(lass12mth[[2]][,'square_error']) ** .5
lass12mth50rmse

### window = 75 ###
las12mth75 = hrlasso(75,final,12)
lasslst[[3]] = msecalc(rf25,75,12,las12mth75)
lass12mth75rmse = mean(lasslst[[3]][,'square_error']) ** .5
lass12mth75rmse

### window = 100 ###
las12mth100 = hrlasso(100,final,12)
lasslst[[4]] = msecalc(rf25,100,12,las6mth100)
lass12mth100rmse = mean(lasslst[[4]][,'square_error']) ** .5
lass12mth100rmse

# for(i in 1:4){
#   name = paste('lass12mth',i,'.csv',sep = '')
#   filename = paste('/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final/',
#         name,sep = '')
#   write.csv(lasslst[[i]],filename)
# }

####################
# Goyal and Welch #
###################

# 1 Month #
exp1mth = list()
expmean = read.csv('expanding mean.csv')
expmean = subset(expmean,select = -c(X))
expmean$Date = as.yearmon(expmean$Date)
for (i in 1:4){
  exp1mth[[i]] = inner_join(rfls[[i]][c('Date','val')],expmean,by='Date')
}
lapply(exp1mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)

#Benchmark model rmse for each window in 1 Month prediction
for(i in 1:4){
  name = paste('gw1mth',winds[i],'rmse',sep = '')
  assign(name,lapply(exp1mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)[[i]])
}

gw1mthdf = data.frame('type' = 'goyal and welch model', 'window25' = gw1mth25rmse,'window50' = gw1mth50rmse,
                  'window75' = gw1mth75rmse,'window100' = gw1mth100rmse)

# 3 Months #
exp3mth = list()
for (i in 1:4){
  exp3mth[[i]] = inner_join(rf3mth[[i]][c('Date','val')],expmean,by='Date')
}
lapply(exp3mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)

#Benchmark model rmse for each window in 3 Month prediction
for(i in 1:4){
  name = paste('gw3mth',winds[i],'rmse',sep = '')
  assign(name,lapply(exp3mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)[[i]])
}

gw3mthdf = data.frame('type' = 'goyal and welch model', 'window25' = gw3mth25rmse,'window50' = gw3mth50rmse,
                      'window75' = gw3mth75rmse,'window100' = gw3mth100rmse)

# 6 Months #
exp6mth = list()
for (i in 1:4){
  exp6mth[[i]] = inner_join(rf6mth[[i]][c('Date','val')],expmean,by='Date')
}
lapply(exp6mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)

#Benchmark model rmse for each window in 6 Month prediction
for(i in 1:4){
  name = paste('gw6mth',winds[i],'rmse',sep = '')
  assign(name,lapply(exp6mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)[[i]])
}

gw6mthdf = data.frame('type' = 'goyal and welch model', 'window25' = gw6mth25rmse,'window50' = gw6mth50rmse,
                      'window75' = gw6mth75rmse,'window100' = gw6mth100rmse)

# 12 Months #
exp12mth = list()
for (i in 1:4){
  exp12mth[[i]] = inner_join(rf12mth[[i]][c('Date','val')],expmean,by='Date')
}
lapply(exp12mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)

#Benchmark model rmse for each window in 12 Month prediction
for(i in 1:4){
  name = paste('gw12mth',winds[i],'rmse',sep = '')
  assign(name,lapply(exp12mth, function(x) mean((x[['expmean']]-x[['val']])**2)**.5)[[i]])
}

gw12mthdf = data.frame('type' = 'goyal and welch model', 'window25' = gw12mth25rmse,'window50' = gw12mth50rmse,
                      'window75' = gw12mth75rmse,'window100' = gw12mth100rmse)




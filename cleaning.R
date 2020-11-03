rm(list = ls())
setwd('/Users/dawnstaana/Documents/NUS/Year 4/Sem 1/EC4308/Project/Dataset final')
#install packages as necessary
library(readr)
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(lubridate)
library(zoo)
library(Hmisc)
library(randomForest)
library(hdm)
library(glmnet)
MSE <- function(pred, truth){ #start and end body of the function by { } - same as a loop 
  return(mean((truth - pred)^2)) #end function with a return(output) statement. Here we can go straight to return because the object of interest is a simple function of inputs
}

# lst is a list of dataframe
# ff is a vector of countries
# name = name of measure (i.e bci,cci) 
# name is a string that can be concatenated to country in ff
ci_clean = function(lst,ff,name){
  
  for (i in ff){
    lst[[i]][['TIME']] = as.yearmon(lst[[i]][['TIME']])
  }
  lst = lapply(lst, function(x) subset(x, select=c(TIME,Value)))
  for (i in ff){
    lst[[i]][['TIME']] = as.yearmon(lst[[i]][['TIME']])
  }
  for (i in ff){
    colnames(lst[[i]])[1] = 'Date'
    colnames(lst[[i]])[2] = paste(i,name,sep = '')
  }
  return(lst)
}
bci = read.csv('BCI.csv')
ff = c("EA19",'G-7','OECD','OECDE','CHN','GBR','JPN','RUS','USA','DEU','FRA')
resbci = list()
for (i in ff){
  resbci[[i]] = bci[bci$LOCATION==i,]
}

resbci = ci_clean(resbci,ff,'bci')

final = resbci$EA19

for (i in 2:length(resbci)){
  final = inner_join(final,resbci[[i]],by='Date')
}

cci = read.csv('CCI.csv')
# View(cci)
head(cci)
rescci = list()
for (i in ff){
  rescci[[i]] = cci[cci$LOCATION==i,]
}

rescci = ci_clean(rescci,ff,'cci')

for (i in ff){
  final = inner_join(final,rescci[[i]],by='Date')
}

View(final)

ffr = read.csv('Monthly FFR (Avg of Daily).csv')
colnames(ffr)[1] = 'Date'
colnames(ffr)[2] = 'ffr'
ffr$Date = as.yearmon(ffr$Date)
final = inner_join(final,ffr,by='Date')

vix = read.csv('VIX ave.csv')
vix%>%tail()
# View(vix)
colnames(vix)[1] = 'Date'
colnames(vix)[2] = 'vix'
vix$Date = as.yearmon(vix$Date)
final = inner_join(final,vix,by='Date')

#### End of absolute values variables ####

cpi = read.csv('CPI (adjusted).csv')
head(cpi)
pct("cpipctchg",cpi,"CPIAUCSL")
cpipctchg = c(NA)
for (i in 1:length(cpi$CPIAUCSL)){
  result = (cpi$CPIAUCSL[i+1]-cpi$CPIAUCS[i])/cpi$CPIAUCS[i]
  cpipctchg = c(cpipctchg,result)
}

cpi$cpipctchg = cpipctchg[-length(cpipctchg)]
# df is a dataframe
# col is is the column to apply the lags
# name = name of index or stock

pct = function(df,col,name){
  colnames(df)[1] = 'Date'
  colnames(df)[2] = name
  df[['Date']] = as.yearmon(df[['Date']])
  for (i in 1:12){
    result = Lag(df[[col]],+i)
    df = cbind(df,result)
    colnames(df)[length(df)] = paste(col,i,sep = '')
  }
  return(df)
}

cpi = pct(cpi,'cpipctchg','cpi')

final = inner_join(final,cpi,by='Date')

ppi = read.csv('PPIACO.csv')
ppipct = c(NA)
for (i in 1:length(ppi$PPIACO)){
  result = (ppi$PPIACO[i+1]-ppi$PPIACO[i])/ppi$PPIACO[i]
  ppipct = c(ppipct,result)
}
ppi$ppipctchg = ppipct[-length(ppipct)]

ppi = pct(ppi,'ppipctchg','ppi')

final = inner_join(final,ppi,by='Date')
# View(ppi)

### stocks datasets ###
dji = read.csv('DJI 1985 to 2020.csv')
dji %>% tail()
dji = dji[,c('Date','Close')]
djipctchg = c(NA)
for (i in 1:length(dji$Close)){
  result = (dji$Close[i+1]-dji$Close[i])/dji$Close[i]
  djipctchg = c(djipctchg,result)
}
dji$djipctchg = djipctchg[-length(djipctchg)]

dji = pct(dji,'djipctchg','dji')

final = inner_join(final,dji,by='Date')

ftse = read.csv('FTSE 95 to 2020.csv')
ftse %>% tail()
ftse = ftse[,c('Date','Close')]
ftsepctchg = c(NA)
for (i in 1:length(ftse$Close)){
  result = (ftse$Close[i+1]-ftse$Close[i])/ftse$Close[i]
  ftsepctchg = c(ftsepctchg,result)
}

ftse$ftsepctchg = ftsepctchg[-length(ftsepctchg)]

ftse = pct(ftse,'ftsepctchg','ftse')

final = inner_join(final,ftse,by='Date')

snp = read.csv('S&P 500 1927 to Octo 2020.csv')
snp %>% tail()
snp = snp[,c('Date','Close')]
snppctchg = c(NA)
for (i in 1:length(snp$Close)){
  result = (snp$Close[i+1]-snp$Close[i])/snp$Close[i]
  snppctchg = c(snppctchg,result)
}

snp$snppctchg = snppctchg[-length(snppctchg)]

snp = pct(snp,'snppctchg','snp')

final = inner_join(final,snp,by='Date')


# Y var #
nasdaq = read.csv('IXIC Returns and Volume.csv')
nasdaq = nasdaq[,c('Date','Close','Volume')]

nasdaqpctchg = c(NA)
for (i in 1:length(nasdaq$Close)){
  result = (nasdaq$Close[i+1]-nasdaq$Close[i])/nasdaq$Close[i]
  nasdaqpctchg = c(nasdaqpctchg,result)
}

nasdaq$nasdaqpctchg = nasdaqpctchg[-length(nasdaqpctchg)]

nasdaq = pct(nasdaq,'nasdaqpctchg','nasdaq')
colnames(nasdaq)[3] = 'nasdaq_vol'

final = inner_join(final,nasdaq,by='Date')

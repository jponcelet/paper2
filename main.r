intDate <- 2001
#Use that to check out the other time period
#intYear <- 1946
library(data.table)
library(ggplot2)
library(lubridate)
library(vars)
library(reshape2)
library(zoo)

monthly  <- read.csv2("D:\\Mes Documents\\Dropbox\\1 - University\\Data\\Bad Beta Good Beta\\Goyal Data monthly.csv", sep =";", dec =".", 
                     na.strings=c("NaN", "NA"), stringsAsFactors=FALSE)

monthly <- as.data.table(monthly)

#Transform the variable yyyymm in a date R can understand
monthly <- monthly[, Date := as.character(yyyymm)]
#Need to separate years from months
monthly <- monthly[, Date := paste(substr(Date,1,4),substr(Date,5,6), sep ="-")]
#Ask to interpret those as dates (will take 1st of the month)
monthly <- monthly[, Date := as.Date(as.yearmon(Date))]

# Need to adjust for the fact that there are thousand separators in the data, uses regular expression subs
monthly$Index<- as.numeric(gsub(',','',monthly$Index))

monthly <- monthly[, IndexDiv := Index + D12]
monthly <- monthly[, dp := log(D12) - log(Index)]
monthly <- monthly[, ep := log(E12) - log(Index)]
vec_dy <- c(NA, monthly[2:nrow(monthly), log(D12)] - monthly[1:(nrow(monthly)-1), log(Index)])
monthly <- monthly[, dy := vec_dy]
monthly <- monthly[, logret   :=c(NA,diff(log(Index)))]
vec_logretdiv <- c(NA, monthly[2:nrow(monthly), log(IndexDiv)] - monthly[1:(nrow(monthly)-1), log(Index)])
monthly <- monthly[, logretdiv:=vec_logretdiv]
monthly <- monthly[, logRfree := log(Rfree + 1)]
monthly <- monthly[, rp_div   := logretdiv - logRfree]
monthly <- monthly[, div_growth := c(NA, diff(log(D12)))]
vec_state <- monthly[Date >= intDate, list(logretdiv, div_growth, dp, eqis)]

var_est <- VAR(vec_state,   p=1, type="const")
summary(var_est)

library(dyn)
summary(dyn$lm(eqis ~ lag(logretdiv, -1) + 
                 lag(div_growth, -1) + 
                 lag(dp, -1) + 
                 lag(eqis, -1), 
               data=ts(vec_state)))

rho <- 0.96
Gamma <- t(sapply(coef(var_est), FUN=function(df) {df[1:4, 1]}))
lambda <- (rho * Gamma) %*% solve(diag(4) - rho * Gamma)

u <- resid(var_est)[, 1]
DR_news <- as.vector(c(1,0,0,0) %*% lambda %*% t(resid(var_est)))
CF_news <- as.vector(c(0,1,0,0) %*% (diag(4) + lambda) %*% t(resid(var_est)))
#Alternatively, backed out
CF_news_backed <- as.vector((c(1,0,0,0) + c(1,0,0,0) %*% lambda) %*% t(resid(var_est)))
#Other ways of writing that
#CF_news_backed <- as.vector(c(1,0,0,0) %*% t(resid(var_est)) + c(1,0,0,0) %*% lambda %*% t(resid(var_est)))
#CF_news_backed <- u + DR_news
#Regression coefficients as reported in table 4 of Chen/Da/Zhao (2013)
summary(lm(DR_news ~ u))$coef[2, 1]*-1
## [1] 0.5612
summary(lm(CF_news_backed ~ u))$coef[2, 1]
## [1] 0.4388
summary(lm(CF_news ~ u))$coef[2, 1]
## [1] 0.4038
#Variance decomposition; terms have to add to 1
var(DR_news)/var(u)
## [1] 0.586
var(CF_news_backed)/var(u)
## [1] 0.4637
-2*cov(CF_news_backed, DR_news)/var(u)
## [1] -0.0497

ChangePointEstimator <- function(data = NULL, peptide, L = 1, U = 5, metric, normalization = TRUE,
                                 ytitle = "Change Point Plot - mean", type = "mean", selectMean = NULL,
                                 selectSD = NULL) {
  if(is.null(data))
    return()
  if(!is.data.frame(data)){
    stop(data)
  }
  metricData <- getMetricData(data, peptide, L, U, metric, normalization, selectMean, selectSD)
  precursor.data <- data[data$Precursor==peptide,]
  ## Create variables
  plot.data <- CP.data.prepare(metricData, type)
  

library(forecast)
library(MASS)
library(nlme)
##################################################################

# I have a data of 3 colums: date, number of patients and holiday
# I will add some more columns to the data
  
resp_table_child[,4:10] <- cbind( rep (c(1,0,0,0,0,0,0),352), rep(c(0,1,0,0,0,0,0),352), rep(c(0,0,1,0,0,0,0),352), 
                                 rep(c(0,0,0,1,0,0,0),352), rep(c(0,0,0,0,1,0,0),352), rep(c(0,0,0,0,0,1,0),352),
                                 rep(c(0,0,0,0,0,0,1),352) )
  

  dow <- function(x) format(as.Date(x), "%A")
  DF$day <- dow(DF$date)
  
  
  # day of the week as numeric (Monday is 1)
  dat$weekday1 = as.numeric(format(dat$Date, format = "%u"))
  
  # abbreviated weekday name
  dat$weekday2 = format(dat$Date, format = "%a")
  
  # full weekday name
  dat$weekday3 = format(dat$Date, format = "%A")
  
  
  weekdaydual <- table(sequence(length(DF$day)), DF$day)
  DF$Mondaydual <- data.frame(weekdaydual)$Monday
  DF$Tuesdaydual <- data.frame(weekdaydual)$Tuesday
  
#binary matrix icin:
  within(dat,{ 
    C1_A =ifelse(C1=='A',1,0)
    C1_B =ifelse(C1=='B',1,0)})
  
   
resp_table_child[,27:33] <- resp_table_child[,2]*resp_table_child[,3:9] 
holidays<- factor(resp_table_child [,22]) #
monthofyear <- factor(resp_table_child[,23])
weekofyear <- resp_table_child[,24]
dayofyear <- c(267:365,1:366,1:365,1:365,1:365,1:366,1:365,1:173)
day_dummy <- resp_table_child [,3:9] #hangi gun ise 1 digerleri 0
month_dummy <- resp_table_child [,10:21] #hangi gun ise 1 digerleri 0
day <- c(rep("Mon",352),rep("Tue",352),rep("Wed",352),rep("Thu",352),rep("Fri",352),rep("Sat",352),rep("Sun",352))
# month <- c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
# weekno <- resp_table_child[,20] #toplam week sayisi kadar
# dayofyear <- resp_table_child[,21]
# weekofyear <- resp_table_child[,19]
resp_child<- resp_table_child[,2]
###########
#352 x 7'lik matris yapti. haftanin gonlerinde kac hasta var seklinde
dow_resp_child <- data.frame(
  resp_table_child[,27][which(resp_table_child[,27]!=0)]
  , resp_table_child[,28][which(resp_table_child[,28]!=0)]
  , resp_table_child[,29][which(resp_table_child[,29]!=0)]
  , resp_table_child[,30][which(resp_table_child[,30]!=0)]
  , resp_table_child[,31][which(resp_table_child[,31]!=0)]
  , resp_table_child[,32][which(resp_table_child[,32]!=0)]
  , resp_table_child[,33][which(resp_table_child[,33]!=0)])
colnames(dow_resp_child) <- c("mon","tue","wed","thu","fri","sat","sun")
#######
#once pazartesileri sonra salilari sonra.... gunlerini birlestirdi vector yapti
count_child<- c(
  resp_table_child[,27][which(resp_table_child[,27]!=0)]
  , resp_table_child[,28][which(resp_table_child[,28]!=0)]
  , resp_table_child[,29][which(resp_table_child[,29]!=0)]
  , resp_table_child[,30][which(resp_table_child[,30]!=0)]
  , resp_table_child[,31][which(resp_table_child[,31]!=0)]
  , resp_table_child[,32][which(resp_table_child[,32]!=0)]
  , resp_table_child[,33][which(resp_table_child[,33]!=0)])
#-------------------------------------------------------------------------------------------
####ANOVA###### haftanin gunu etkisi var mi bakiyoruz
#
stacked_dow_resp_child <- data.frame(count_child,day)
par(mfrow=c(1,1))
boxplot(dow_resp_child)
output_resp_child=aov(count_child~day, data=stacked_dow_resp_child)
summary(output_resp_child)
TukeyHSD(output_resp_child, conf.level =0.95)
mean.daily.residuals=apply((dow_resp_child-mean(count_child)),2,mean)
sd.daily.residuals=apply((dow_resp_child-mean(count_child)),2,sd)
par(mfrow=c(1,2))
plot(mean.daily.residuals,pch=19,main='Respiratory Cases: Mean Comparison')
abline(h=0)
plot(sd.daily.residuals,pch=19, main='Respiratory Cases: Dispersion Comparison')
abline(h=mean(sd.daily.residuals))
########################################################################
# GRAFIKLER
#
par(mfrow=c(1,1))
par(mfrow=c(2,4))
#
plot(resp_child)
hist(resp_child)
plot(log(resp_child))
hist(log(resp_child))
plot(diff(resp_child))
hist(diff(resp_child))
plot(diff(log(resp_child)))
hist(diff(log(resp_child)))
abline(v=367, col='red')
abline(v=732, col='red')
abline(v=1097, col='red')
abline(v=1462, col='red')
#
#lengthresp=length(resp_child)
##------------------------------------------------------------------------------------------------------
b <- 1   # yillara gore: 07-08: 1, 08-09: 367, 09-10:732,  10-11:1097, 11-12: 1462, 12-13: 1828, 13-14: 2193
T <- 366 # yillara gore: 07-08:366, 08-09: 731, 09-10:1096, 10-11:1461, 11-12: 1827, 12-13: 2192, 13-14: 2464
#
#----------------------------------------------------------------------------------------------------------
#             1- DEFINE Box-Cox Coeff.  && DEFINE RESPONSE &INPUT VARIABLES --  
# ---------------------------------------------------------------------------------------------------------
#
#trend <- c(1:length(resp_child))
input_bc <- cbind(day_dummy[,1], day_dummy[,2], day_dummy[,4],
                  day_dummy[,5], day_dummy[,6],day_dummy[,7],month_dummy[,1], month_dummy[,2],
                  month_dummy[,3], month_dummy[,4],month_dummy[,5], 
                  month_dummy[,7], month_dummy[,8],month_dummy[,9], month_dummy[,10],
                  month_dummy[,11], month_dummy[,12], holidays, dayofyear, #dayofyear=trend
                  sin((2*pi*dayofyear)/365.25),
                  cos((2*pi*dayofyear)/365.25), sin((4*pi*dayofyear)/365.25),
                  cos((4*pi*dayofyear)/365.25))
#
data_bc<-ts(cbind(resp_child[b:T],input_bc[b:T,]),frequency=(T-b+1),start=c(2007,9))
#
#data_bc[,1] ==> gelen hasta sayisi
#data_bc[,2,3,4,5,6,7] ==> cars. harinc gunler
#data_bc[,8:18] ==> haziran haric aylar
#data_bc[,19] ==> holiday
#data_bc[,20] ==> dayofyear
#data_bc[,21:24] ==> sin-cos        # 1 response, 23 input variables
#
modelo_bc<-lm(data_bc[,1]~data_bc[,2]+data_bc[,3]+data_bc[,4]+data_bc[,5]+data_bc[,6]+
                data_bc[,7]+data_bc[,8]+data_bc[,9]+data_bc[,10]+data_bc[,11]+
                data_bc[,12]+data_bc[,13]+data_bc[,14]+data_bc[,15]+data_bc[,16]+
                data_bc[,17]+data_bc[,18]+data_bc[,19]+data_bc[,20]+data_bc[,21]+
                data_bc[,22]+data_bc[,23] +data_bc[,24] )
par(mfrow=c(1,1))
bc <- boxcox(modelo_bc) 
#bccoef <- -0.2
y_bc <- NULL
data_bc<- NULL
modelo_bc<- NULL
#
y_bc <- log(resp_child)   #BOX-COX transformation
data_bc<-ts(cbind(y_bc[b:T],input_bc[b:T,]),frequency=(T-b+1),start=c(2007,9))
dim(data_bc) #366,  24
#
#--------------------------------------------------------------------
#                     2- OLS MODEL --  BOX-COX
# -------------------------------------------------------------------
#
modelo_bc<-lm(data_bc[,1]~data_bc[,2]+data_bc[,3]+data_bc[,4]+data_bc[,5]+data_bc[,6]+
                data_bc[,7]+data_bc[,8]+data_bc[,9]+data_bc[,10]+data_bc[,11]+
                data_bc[,12]+data_bc[,13]+data_bc[,14]+data_bc[,15]+data_bc[,16]+
                data_bc[,17]+data_bc[,18]+data_bc[,19]+data_bc[,20]+data_bc[,21]+
                data_bc[,22]+data_bc[,23]+data_bc[,24]     )
summary(modelo_bc) 
#shapiro.test(residuals(modelo))
#qqnorm(residuals(modelo))
plot(residuals(modelo_bc))   # ERRORLAR STATIONARY DEGIL
tsdisplay(residuals(modelo_bc), main="ARIMA errors")
auto.arima(residuals(modelo_bc))$coef # 2009 y_dif icin: arima(5,0,3)-- 2010 y_boxcox icin arima(3,0,0)
#
#--------------------------------------------------------------------
#                     3- GLS FIT WITH ARMA ERRORS (use nmle)  -- BOX-COX
# -------------------------------------------------------------------
#
gls_fit_bc <- gls(data_bc[,1]~data_bc[,2]+data_bc[,3]+data_bc[,4]+data_bc[,5]+data_bc[,6]+
                    data_bc[,7]+data_bc[,8]+data_bc[,9]+data_bc[,10]+data_bc[,11]+
                    data_bc[,12]+data_bc[,13]+data_bc[,14]+data_bc[,15]+data_bc[,16]+
                    data_bc[,17]+data_bc[,18]+data_bc[,19]+data_bc[,20]+data_bc[,21]+
                    data_bc[,22]+data_bc[,23]+data_bc[,24], correlation = corARMA(p =2, q = 0),
                  method = "ML")
#
save(gls_fit_bc, file="gls_fit_bc") #48 veri icin save ettim. 143363 satirlik verim var.
#
residuals(gls_fit_bc)  #gives estimates of zt, the ARMA process.
residuals(gls_fit_bc, type = "normalized")  #to  get wt
#shapiro.test(residuals(gls_fit_bc))
#qqnorm(residuals(gls_fit_bc))
#
arima_fit_bc <- arima(data_bc[,1], order=c(2,0,0),
                      xreg=data.frame(cbind(data_bc[,2]+data_bc[,3]+data_bc[,4]+data_bc[,5]+data_bc[,6]+
                                              data_bc[,7]+data_bc[,8]+data_bc[,9]+data_bc[,10]+data_bc[,11]+
                                              data_bc[,12]+data_bc[,13]+data_bc[,14]+data_bc[,15]+data_bc[,16]+
                                              data_bc[,17]+data_bc[,18]+data_bc[,19]+data_bc[,20]+data_bc[,21]+
                                              data_bc[,22]+data_bc[,23]+data_bc[,24])))
residuals(arima_fit_bc) # gives estimates of wt, the white noise.
#
intervals(gls_fit_bc)$coef
#
tsdisplay(residuals(gls_fit_bc, type = "normalized"), main="ARIMA errors")
#
#--------------------------------------------------------------------
#          4- PREDICTIONS FOR SEP 2008- JUME 2014  --  
# -------------------------------------------------------------------
#
gls_predict_bc <-  c(  predict(gls_fit_bc, as.data.frame(input_bc[(367 :731),]))[1:365], #08-09
                       predict(gls_fit_bc, as.data.frame(input_bc[(732 :1096),]))[1:365], #09-10
                       predict(gls_fit_bc, as.data.frame(input_bc[(1097:1461),]))[1:365], #10-11 
                       predict(gls_fit_bc, as.data.frame(input_bc[(1462:1827),]))[1:366], #11-12
                       predict(gls_fit_bc, as.data.frame(input_bc[(1828:2192),]))[1:365], #12-13
                       predict(gls_fit_bc, as.data.frame(input_bc[(2193:2464),]))[1:272]) #13-14
# -------------------------------------------------------------------

save(y_bc, file="y_bc")
save(resp_child, file="resp_child")
save(gls_predict_bc, file="gls_predict_bc")

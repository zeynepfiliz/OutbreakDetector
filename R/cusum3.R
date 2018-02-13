#
#setwd('C:\\Users\\ZFED\\Documents\\My Dropbox\\8. RESIDUAL CUSUM for RESPIRATORY DATA\\R')
setwd('C:\\Users\\eralp.dogu\\Dropbox\\8. RESIDUAL CUSUM for RESPIRATORY DATA\\R')
load("gls_predict_bc")
load("y_bc")
load("resp_child")
#
#------------ AGGRESSIVE   win=7 ------------------
#
kk=0.5 ;    # agressive:0.5  ---  moderate:1     ----  routine:1
H=0.365;  # agressive:0.365 --- moderate:0.695 ----  routine:1.2
win=7; #sliding window size

cusum <- function (data, kk, H, win) {
# kk: agressive:0.5  ---  moderate:1     ----  routine:1
# H: agressive:0.365 --- moderate:0.695 ----  routine:1.2
# win: sliding window size
  
  #If there are some missing arguments:
  
  if (missing(kk)){
    kk <- 1
  }
  
  if (missing(H)){
    H <- 0.695
  }
  
  if (missing(win)){
    win <- 14
  }
  
cusum_df <- data.frame()

while (test_startdate <= test_enddate) {
  
  # elimizdeki test data sini yillik yillik cekiyoruz, taa ki test_startdate >= test_enddate olana kadar
  # test data her sene icin yeniden olusturulacak
  
  # test data list oldugu icin onu dataNum diye bir numeric'e donusturup islemlere devam ettim
  
  testdata <-data[which(data[,"Date"] >= test_startdate & data[,"Date"]< test_startdate %m+%years(1)),
                  -which(names(data)==c("Date","Number"))]
  dataNum <- matrix(data = NA, nrow = dim(testdata)[1], ncol = dim(testdata)[2])
  colnames(dataNum) <- colnames(testdata)
  for (i in 1:dim(testdata)[2]) {
    dataNum[,i] <- as.numeric(testdata[[i]])
  }
  
  gls_predict <-  c(gls_predict, predict(gls_fit_ts,as.data.frame(dataNum))[1:dim(dataNum)[1]])
  
  test_startdate <- test_startdate %m+% years(1)
  rm(testdata)
  rm(dataNum)
}

for(j in 2008:2013){
  #   
  if (j == 2008) { pb=367 
  pT=731
  year=c(rep("2009-2010",365))}
  if (j == 2009)  { pb=732 
  pT=1096
  year=c(rep("2010-2011",365))}
  if (j == 2010) { pb=1097 
  pT=1461
  year=c(rep("2011-2012",365))}
  if (j == 2011) { pb=1462 
  pT=1827
  year=c(rep("2012-2013",366))}
  if (j == 2012) { pb=1828 
  pT=2192
  year=c(rep("2013-2014",365))}
  if (j == 2013) {pb=2193 
  pT=2464
  year=c(rep("2014-2015",272))}
  # -------------------------------------------------------------------
  pred_id <- c(pb:pT)
  day <- c(1:(pT-pb+1))
  log_count <- y_bc[pb:pT]
  count <- resp_child[pb:pT]
  resid_bc <- exp(y_bc[pred_id])-exp(gls_predict_bc[pred_id-366])
  resid <- scale(resid_bc)
  s <- rep (win,(pT-pb+1))
  ll <- length(resid)
  #  CUSUM CHARTS
  Cp=NULL
  Cp[1]=0;
  for(i in 2:ll){Cp[i]=max(0,resid[i]-kk+Cp[i-1])}
  # -------------------------------------------------------------------
  #for(i in 2:ll){Cp[i]=max(0,resid[i]-kk+Cp[i-1])
  #                 if(Cp[i]<Cp[i-1]){Cp[i]=0}}
  # -------------------------------------------------------------------
#   for(i in 2:ll){Cp[i]=max(0,resid[i]-kk+Cp[i-1])}
  for(i in (win+1):ll){
  mn=(i-win):i
  if(Cp[i]>H){mod=lm(Cp[(i-win):i]~mn)
  if(mod$coef[2]<0){Cp[i]=0}}}
    Cp2 <- Cp
  for(i in 2:ll){if(Cp[i]>H) {Cp2[i]=4} }
  #  RESULTS
  tmp <- data.frame(cbind(s,year,pred_id,day,count,log_count,as.numeric(resid),as.numeric(Cp),as.numeric(Cp2)))
  colnames(tmp) <- c("s","Year","CumDay","Day","Daily_logs","log_Daily_logs","Residuals" ,"Cp","Cp2")
  cusum_aggressive_s7 <- rbind(cusum_aggressive_s7,tmp)
  tmp <- NULL
  pred_id <- NULL
  resid_bc <- NULL
  resid <- NULL
}
#
save(cusum_aggressive_s7, file="cusum_aggressive_s7")
#
 cusum_routine_s21 <- rbind(cusum_routine_s21,tmp)
  tmp <- NULL
  pred_id <- NULL
  resid_bc <- NULL
  resid <- NULL
}
#

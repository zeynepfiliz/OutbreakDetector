setwd('/Users/ZFED/Dropbox/RESIDUAL CUSUM for RESPIRATORY DATA/R')
resp_table_child<- read.csv("resp_child_07_14.csv", header=TRUE)
rm(data)
data <- resp_table_child[,c(1,2,22)]
colnames(data) <- c("Date","Number","Holiday")

data[,"Date"] <- as.Date(parse_date_time(x = data[,"Date"], 
                                         orders = c("%d-%b-%y", "%d %b %Y", "%d-%m-%Y", "%m/%d/%y", "%d.%m.%y", "%d.%m.%Y")))

datarow <- data

train_startdate <- as.Date(c("2007-09-24"))
test_startdate <- as.Date(c("2008-09-24"))
numyears_train <- 1

test_startdate <- train_startdate %m+% years(numyears_train)
test_enddate <- data[dim(data)[1],"Date"]

data[,"Dayofyear"]  <- yday(data[,"Date"])

data[,"Mon_dual"] <- ifelse( weekdays(data[,"Date"]) == "Monday", 1,
                             ifelse( weekdays(data[,"Date"]) != "Monday", 0,
                                     NA)) 

data[,"Tues_dual"] <- ifelse( weekdays(data[,"Date"]) == "Tuesday", 1,
                              ifelse( weekdays(data[,"Date"]) != "Tuesday", 0,
                                      NA)) 

data[,"Wed_dual"] <- ifelse( weekdays(data[,"Date"]) == "Wednesday", 1,
                             ifelse( weekdays(data[,"Date"]) != "Wednesday", 0,
                                     NA)) 

data[,"Thu_dual"] <- ifelse( weekdays(data[,"Date"]) == "Thursday", 1,
                             ifelse( weekdays(data[,"Date"]) != "Thursday", 0,
                                     NA)) 

data[,"Fri_dual"] <- ifelse( weekdays(data[,"Date"]) == "Friday", 1,
                             ifelse( weekdays(data[,"Date"]) != "Friday", 0,
                                     NA)) 

data[,"Sat_dual"] <- ifelse( weekdays(data[,"Date"]) == "Saturday", 1,
                             ifelse( weekdays(data[,"Date"]) != "Saturday", 0,
                                     NA)) 

data[,"Sun_dual"] <- ifelse( weekdays(data[,"Date"]) == "Sunday", 1,
                             ifelse( weekdays(data[,"Date"]) != "Sunday", 0,
                                     NA)) 

data[,c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")] <- 
  data[,"Number"]*data[,c("Mon_dual", "Tues_dual", "Wed_dual", "Thu_dual", "Fri_dual", "Sat_dual", "Sun_dual")] 

### same for months
data[,"Month"] <- month(data[,"Date"])

data[,"Jan_dual"] <- ifelse( month(data[,"Month"]) == 1, 1,
                             ifelse( month(data[,"Month"]) != 1, 0,
                                     NA)) 

data[,"Feb_dual"] <- ifelse( month(data[,"Month"]) == 2, 1,
                             ifelse( month(data[,"Month"]) != 2, 0,
                                     NA)) 

data[,"Mar_dual"] <- ifelse( month(data[,"Month"]) == 3, 1,
                             ifelse( month(data[,"Month"]) != 3, 0,
                                     NA)) 

data[,"Apr_dual"] <- ifelse( month(data[,"Month"]) == 4, 1,
                             ifelse( month(data[,"Month"]) != 4, 0,
                                     NA)) 

data[,"May_dual"] <- ifelse( month(data[,"Month"]) == 5, 1,
                             ifelse( month(data[,"Month"]) != 5, 0,
                                     NA)) 

data[,"Jun_dual"] <- ifelse( month(data[,"Month"]) == 6, 1,
                             ifelse( month(data[,"Month"]) != 6, 0,
                                     NA)) 

data[,"Jul_dual"] <- ifelse( month(data[,"Month"]) == 7, 1,
                             ifelse( month(data[,"Month"]) != 7, 0,
                                     NA)) 

data[,"Aug_dual"] <- ifelse( month(data[,"Month"]) == 8, 1,
                             ifelse( month(data[,"Month"]) != 8, 0,
                                     NA)) 
data[,"Sep_dual"] <- ifelse( month(data[,"Month"]) == 9, 1,
                             ifelse( month(data[,"Month"]) != 9, 0,
                                     NA)) 
data[,"Oct_dual"] <- ifelse( month(data[,"Month"]) == 10, 1,
                             ifelse( month(data[,"Month"]) != 10, 0,
                                     NA)) 
data[,"Nov_dual"] <- ifelse( month(data[,"Month"]) == 11, 1,
                             ifelse( month(data[,"Month"]) != 11, 0,
                                     NA)) 
data[,"Dec_dual"] <- ifelse( month(data[,"Month"]) == 12, 1,
                             ifelse( month(data[,"Month"]) != 12, 0,
                                     NA)) 
###### end for months
# We need to extract some variables:
train_enddate <- as.Date(train_startdate %m+% years(numyears_train)-days(1))

b <- as.numeric(which(data[,"Date"] == train_startdate))
T <- as.numeric(which(data[,"Date"] == train_enddate))


colnames <- c("Date","Number","Holiday", "Dayofyear","Mon_dual", "Tues_dual", "Thu_dual","Fri_dual", "Sat_dual", "Sun_dual",
              "Jan_dual", "Feb_dual", "Mar_dual", "Apr_dual", "May_dual", "Jul_dual","Aug_dual", "Sep_dual", 
              "Oct_dual", "Nov_dual", "Dec_dual")
data <- cbind( data[ ,names(data) %in% colnames],
               sin((2*pi*data[,"Dayofyear"])/365.25),cos((2*pi*data[,"Dayofyear"])/365.25),
               sin((4*pi*data[,"Dayofyear"])/365.25),cos((4*pi*data[,"Dayofyear"])/365.25) )
colnames(data) <- c(colnames, c("sin2","cos2","sin4","cos4"))

#--------------------------------------------------------------------
#                     2- OLS MODEL --  BOX-COX
# -------------------------------------------------------------------

traindata <- data[b:T,-which(names(data)=="Date")]
data_ts <- ts (traindata, frequency = (T-b+1), start = c (year(train_startdate),month(train_startdate)) )
model_ts<-lm( Number ~., data=data_ts ) 
plot(residuals(model_ts))   # ERRORLAR STATIONARY DEGIL

# Implement the Box-Cox transformation
# if we set lambda2 to zero, it becomes the one parameter transformation

bc <- boxcox(Number ~., data=data_ts) 
lambda <- bc$x[which.max(bc$y)]
data[ ,"Transformednumber"] <-  c()


if (lambda <= 0.2 && lambda >= -0.2) {
  data[ ,"Transformednumber"] <- log(data[ ,"Number"])
} else {
  data[ ,"Transformednumber"] <- ((data[ ,"Number"]^lambda )-1)/lambda
}


# We can clear the Number column now and create new traindata and new data_ts with transformed values
rm(traindata)

traindata <- data[b:T,-which(names(data) %in% c("Date","Number"))]

data_ts <- ts (traindata, frequency = (T-b+1), start = c (year(train_startdate),month(train_startdate)) )

model_ts<-lm(Transformednumber~., data=data_ts )

summary(model_ts) 
plot(residuals(model_ts))   # ERRORLAR STATIONARY DEGIL
tsdisplay(residuals(model_ts), main="ARIMA errors")
auto.arima(residuals(model_ts))$coef 

# Extract AR and MA coefficient outputs from the auto.arima function and call them as ar_coef and ma_coef


coefficients_arima <- as.data.frame(strsplit(rownames( data.frame(auto.arima(residuals(model_ts))$coef)), 
                                             "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE))

if ( length (coefficients_arima[2,][coefficients_arima[1,]=="ar"] ) == 0 ) {
  ar_coef <- 0
} else {
  ar_coef <-as.numeric(max(coefficients_arima[2,][coefficients_arima[1,]=="ar"]))
}

if (length(coefficients_arima[2,][coefficients_arima[1,]=="ma"] ) == 0 ) {
  ma_coef <- 0
} else {
  ma_coef <-as.numeric(max(coefficients_arima[2,][coefficients_arima[1,]=="ma"]))
}
ar_coef 
ma_coef
#--------------------------------------------------------------------
#                     3- GLS FIT WITH ARMA ERRORS (use nmle)  -- BOX-COX
# -------------------------------------------------------------------

gls_fit_ts <- gls(Transformednumber ~ ., data=data_ts, correlation = corARMA(p=ar_coef,q=ma_coef),
                  method="ML")

tsdisplay(residuals(gls_fit_ts, type="normalized"), main="ARIMA errors")

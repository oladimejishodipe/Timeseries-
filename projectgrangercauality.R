

Enerdata<-ts.combine(lWTI, lHHG, lREP)
#Lag selection
lagselect<-VARselect(Enerdata, lag.max = 3, type="const")
lagselect$selection



Enerdata<-ts.combine(dlWTI, dlHHG, lREP)

#Model for VAR
EnerModel2 <- function(t) {
  Enermodelint <- window(Enerdata, end=t)
  fitmodel2 <- VAR(Enermodelint, p=3)
  return(getVarForecast(fitmodel2, "lREP", 1))
}

#Model for AR
EnerModel3 <- function(t) {
  Enermodelint2 <- window(Enerdata, end=t)
  fitmodel3 <- arima(Enermodelint2[, "lREP"],
                     order=c(3,0,0))
  pred <- predict(fitmodel3, 1)
  return(pred$pred)
}         


#Date setting
dates <- make.dates(c(2016,12), c(2019,12),
                    12)


#Forecast Function
Make.Fcst.EnerModel <- function(f, endDates, 
                                firstForecast) {
  return(ts(unlist(lapply(endDates, f)),
            start=firstForecast, 
            frequency=frequency(Enerdata)))
}

# Forecast Values and Actual Data
F.var <- Make.Fcst.EnerModel(EnerModel2, dates, c(2016,1))         
F.ar <- Make.Fcst.EnerModel(EnerModel3, dates, c(2016,1))   
Actual <- window(lREP, start=c(2016,1),
                 end=c(2019,12))

#Forecast Error for VAR and AR
E.var <- Actual - F.var
E.ar <- Actual - F.ar  



#Enc-New Statistics - We reject the null- WTIP granger cause resUS Electric Price, 
#Even Enc-New ends table ends at 10 lags, the statistics is high enough for rejection.
Num <- sum(E.ar^2 - E.ar*E.var)
Den <- sum(E.var^2)  
Enc.New <- 48*Num/Den  
Enc.New


#Only dlWTI as additional variable
Enerdata<-ts.combine(dlWTI, lREP)
#Model for VAR
EnerModel2 <- function(t) {
  Enermodelint <- window(Enerdata, end=t)
  fitmodel2 <- VAR(Enermodelint, p=3)
  return(getVarForecast(fitmodel2, "lREP", 1))
}

#Model for AR
EnerModel3 <- function(t) {
  Enermodelint2 <- window(Enerdata, end=t)
  fitmodel3 <- arima(Enermodelint2[, "lREP"],
                     order=c(3,0,0))
  pred <- predict(fitmodel3, 1)
  return(pred$pred)
}         


#Date setting
dates <- make.dates(c(2016,12), c(2019,12),
                    12)


#Forecast Function
Make.Fcst.EnerModel <- function(f, endDates, 
                                firstForecast) {
  return(ts(unlist(lapply(endDates, f)),
            start=firstForecast, 
            frequency=frequency(Enerdata)))
}

# Forecast Values and Actual Data
F.var <- Make.Fcst.EnerModel(EnerModel2, dates, c(2016,1))         
F.ar <- Make.Fcst.EnerModel(EnerModel3, dates, c(2016,1))   
Actual <- window(lREP, start=c(2016,1),
                 end=c(2019,12))

#Forecast Error for VAR and AR
E.var <- Actual - F.var
E.ar <- Actual - F.ar  



#Enc-New Statistics - We reject the null- WTIP granger cause resUS Electric Price, 
#Even Enc-New ends table ends at 10 lags, the statistics is high enough for rejection.
Num <- sum(E.ar^2 - E.ar*E.var)
Den <- sum(E.var^2)  
Enc.New <- 48*Num/Den  
Enc.New



#Only HHG as additional variable
Enerdata<-ts.combine(dlHHG, lREP)
#Model for VAR
EnerModel2 <- function(t) {
  Enermodelint <- window(Enerdata, end=t)
  fitmodel2 <- VAR(Enermodelint, p=3)
  return(getVarForecast(fitmodel2, "lREP", 1))
}

#Model for AR
EnerModel3 <- function(t) {
  Enermodelint2 <- window(Enerdata, end=t)
  fitmodel3 <- arima(Enermodelint2[, "lREP"],
                     order=c(3,0,0))
  pred <- predict(fitmodel3, 1)
  return(pred$pred)
}         


#Date setting
dates <- make.dates(c(2016,12), c(2019,12),
                    12)


#Forecast Function
Make.Fcst.EnerModel <- function(f, endDates, 
                                firstForecast) {
  return(ts(unlist(lapply(endDates, f)),
            start=firstForecast, 
            frequency=frequency(Enerdata)))
}

# Forecast Values and Actual Data
F.var <- Make.Fcst.EnerModel(EnerModel2, dates, c(2016,1))         
F.ar <- Make.Fcst.EnerModel(EnerModel3, dates, c(2016,1))   
Actual <- window(lREP, start=c(2016,1),
                 end=c(2019,12))

#Forecast Error for VAR and AR
E.var <- Actual - F.var
E.ar <- Actual - F.ar  


#Enc-New Statistics - We reject the null- WTIP granger cause resUS Electric Price, 
#Even Enc-New ends table ends at 10 lags, the statistics is high enough for rejection.
Num <- sum(E.ar^2 - E.ar*E.var)
Den <- sum(E.var^2)  
Enc.New <- 48*Num/Den  
Enc.New

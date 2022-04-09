install.packages('tstoolS')
install.packages('tstool')
#install.packages("rio")
library(openxlsx)
install.packages("devtools")
library(devtools)
install_bitbucket("bachmeil/tstools")

library(tseries)
library(tstools)

#library(dplyr)
#library(rio)
#install.packages('extrafont')
#install.packages("ggplot2")
#library(extrafont)
#library("ggplot2")
#library(dplyr)
#font_import()
#install.packages('forecast', dependencies = TRUE)
#install.packages("tidyverse")
library(rugarch)
library(fGarch)
#library(forecast)
#library(tidyverse)
#library(tstools)
#library(broom)
#library(extrafont)
#loadfonts()
#font_install("fontcm")
l#ibrary(fpp2)

#Local Projection
#install.packages("lpirfs")
#library(lpirfs)
#install.packages("factoextra")
#library(factoextra)  
#install.packages("urca")
#install.packages("vars")
#install.packages("ggplot2")
#install.packages('forecast', dependencies = TRUE)
#install.packages("tidyverse")
#install.packages("mFilter")

#library("ggplot2")
library("urca")
library("vars")
library('forecast')
#library("tidyverse")
#library("mFilter")
library(vars)

library(readxl)
Finalelectricdatares <- read_excel("~/Dissertation/Residential/Finalelectricdata.xlsx",
                                   sheet = "Residential")

which(is.na(Finalelectricdatares))


REP<- ts(Finalelectricdatares[,5], start=c(2001,1),
         frequency=12)

WTI<- ts(Finalelectricdatares[,6], start=c(2001,1),
         frequency=12)

HHG<- ts(Finalelectricdatares[,7], start=c(2001,1),
         frequency=12)


REP1<- window(REP, end=c(2019,12))
WTI1<- window(WTI, end=c(2019,12))
HHG1<- window(HHG, end=c(2019,12))


#Log of the series
lREP<-log(REP1)
lWTI<-log(WTI1)
lHHG<-log(HHG1)


## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)
## Plot first set of data and draw its axis
op <- par(family = "Times New Roman") 
plot(WTI1, pch=16, axes=TRUE, ylim=c(0,140), xlab="", ylab="", 
     type="l",col="black")
#axis(2, ylim=c(0,120),col="black",las=1)  ## las=1 makes horizontal labels
mtext("WTI crude oil price ($/barrel)",side=2,line=2.5)
mtext("Fig. 1.  Fossil Fuel Prices, 2001:1 - 2019:12",side=1,line=2.5)
box()
op <- par(family = "Times New Roman") 
## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(HHG1, pch=15,  xlab="", ylab="", ylim=c(2,15), 
     axes=FALSE, type="l", lty=2, col="2")
## a little farther out (line=4) to make room for labels
mtext("HHG spot price ($/million btu)",side=4,col="black",line=2.5) 
axis(4, ylim=c(2,15), col="black",col.axis="black",las=1)

## Draw the time axis
#axis(1,pretty(range(time),10))
#mtext("Time (Hours)",side=1,col="black",line=2.5)  

## Add Legend
legend("topright",legend=c("WTI oil price","HHG price"),
       cex=0.7, text.col=c("black","black"),pch=c(0,0),col=c("black","2"))


adflREP<-ur.df(y=lREP, type="none", selectlags="AIC")
adflREP
adflREP<-ur.df(y=lREP, type="drift", selectlags="AIC")
adflREP
adflREP<-ur.df(y=lREP, type="trend", selectlags="AIC")
adflREP

adflreWIP<-ur.df(y=lWTI, type="none", selectlags="AIC")
adflreWIP
adflreWIP<-ur.df(y=lWTI, type="drift", selectlags="AIC")
adflreWIP
adflreWIP

adflHHG<-ur.df(y=lHHG, type="none", selectlags="AIC")
adflHHG
adflHHG<-ur.df(y=lHHG, type="drift", selectlags="AIC")
adflHHG
adflHHG<-ur.df(y=lHHG, type="trend", selectlags="AIC")
adflHHG


#Differencing

dlWTI<-diff(lWTI)
dadflreWIP<-ur.df(y=dlWTI, type="none", selectlags="AIC")
dadflreWIP
dadflreWIP<-ur.df(y=dlWTI, type="drift", selectlags="AIC")
dadflreWIP
dadflreWIP<-ur.df(y=dlWTI, type="trend", selectlags="AIC")
dadflreWIP

dlHHG<-diff(lHHG)
dadflHHG<-ur.df(y=dlHHG, type="none", selectlags="AIC")
dadflHHG
dadflHHG<-ur.df(y=dlHHG, type="drift", selectlags="AIC")
dadflHHG
dadflHHG<-ur.df(y=dlHHG, type="trend", selectlags="AIC")
dadflHHG




#Plot of the log of the variables
op <- par(family = "Times New Roman") 
plot(lREP, col="black",
     #sub="Fig.1.1",
     xlab="Fig. 2. Series, 2001:12 - 2019:12",
     ylab="lREP",
     col.axis = "black",
     family = "Times New Roman",
     cex.main=0.95, cex.lab=0.75, cex.axis=0.75)
op <- par(family = "Times New Roman") 
mtext("Log Residential Electric Price",side=3,line=1)

op <- par(family = "Times New Roman") 
plot(dlWTI, col="black",
     #sub="Fig.1.1",
     xlab="Fig. 3. Series, 2001:12 - 2019:12",
     ylab=expression(paste(Delta, "lWTI")),
     col.axis = "black",
     family = "Times New Roman",
     cex.main=0.95, cex.lab=0.75, cex.axis=0.75)
op <- par(family = "Times New Roman") 
mtext("Differenced Log of WTI Crude Oil Price",side=3,line=1)

op <- par(family = "Times New Roman") 
plot(dlHHG, col="black",
     #sub="Fig.1.1",
     xlab="Fig. 4. Series, 2001:12 - 2019:12",
     ylab=expression(paste(Delta, "lHHG")),
     col.axis = "black",
     family = "Times New Roman",
     cex.main=0.95, cex.lab=0.75, cex.axis=0.75)
op <- par(family = "Times New Roman") 
mtext("Differenced Log of HHG Gas Price",side=3,line=1)


# Reduced Form VAR : # Estimate reduced form VAR, the IRF is orthogonal
Enerdata<-ts.combine(dlWTI, dlHHG, lREP)
Enermodel<- VAR(Enerdata, p=3)
Enermodel

# This IRF is the same as the structural VAR because it is orthogonal IRF:
#Usually, researchers address this by using orthogonal impulse responses, 
#where the correlation between the errors is obtained from the
#(lower) Cholesky decomposition of the error covariance matrix

irf <- irf(Enermodel, impulse="dlWTI", response="dlWTI", boot=TRUE, n.ahead=24, ci=0.90)
irf_df1 <- data.frame(Time=1:25, 
                     actual=irf$irf,
                     lower=irf$Lower,
                     upper=irf$Upper)
colnames(irf_df1) <- c('Time','actual','lower1','upper1')
irf <- irf(Enermodel, impulse="dlWTI", response="dlWTI", boot=TRUE, n.ahead=24, ci=0.95)
irf_df2 <- data.frame(Time=1:25, 
                     actual=irf$irf,
                     lower=irf$Lower,
                     upper=irf$Upper)
colnames(irf_df2) <- c('Time','actual','lower2','upper2')

op <- par(family = "Times New Roman") 
plot(irf_df1$Time, irf_df1$actual, type = "l", col = 1, xlab="", ylab="")
lines(irf_df1$Time, irf_df1$lower1, type = "l", lty=2, lwd= 2,col = 2)
lines(irf_df1$Time, irf_df1$upper1, type = "l", lty=2, lwd= 2,col = 2)
lines(irf_df2$Time, irf_df2$lower2, type = "l", lty=2,lwd= 2, col = 3)
lines(irf_df2$Time, irf_df2$upper2, type = "l", lty=2,lwd= 2, col =3)
abline(h=0, col="black")
mtext("WTI crude oil price",side=2,line=2.5)
mtext("Oil market shock",side=3,line=0.5)
legend("topright",legend=c("one s.e band","two s.e band"),
       cex=0.7, text.col=c("black","black"),pch=c(0,0),col=c("2","3"))











#Cummulative IRF
irf <- irf(Enermodel, cumulative =TRUE, impulse="dlWTI", boot=TRUE, n.ahead=12)
str(irf)
plot(irf)

?irf
fevd(varfit, n.ahead=inf)


#___________________________  Impact Effect
#Resid<-(residuals(Enermodel))
#cova<-cov(Resid)
#Shock<-t(chol(cova))
#Shock


#  (recovering inverse contemporaneous matrix). This is similar to same GMM estimation for matrix 
# when we normalise and restrict the upperdiagonal. 
b <- diag(1, 3)
b[lower.tri(b)] <- NA
# Estimate
svar_est_b <- SVAR(Enermodel, Bmat = b, max.iter = 1000)
# Show result
svar_est_b
#(recovering the contemporanous matrix at the left hand side)
a <- diag(1, 3)
a[lower.tri(a)] <- NA
svar_est_a <- SVAR(Enermodel, Amat = a, max.iter = 1000)
svar_est_a



# Convert a vector of parameters
# into the deviation from the moment
# conditions

# Now do the moment restrictions of the SVAR
objfnc2 <- function(par) {
        k11<-par[1]
        k12<-par[2]
        k13<-par[3]
        k21<-par[4]
        k22<-par[5]
        k23<-par[6]
        k31<-par[7]
        k32<-par[8]
        k33<-par[9]
        k10<-par[10]
        a11<-par[11]
        a12<-par[12]
        a13<-par[13]
        a21<-par[14]
        a22<-par[15]
        a23<-par[16]
        a31<-par[17]
        a32<-par[18]
        a33<-par[19]  
        a10<-par[20]
        p11<-par[21]
        p12<-par[22]
        p13<-par[23]
        p21<-par[24]
        p22<-par[25]
        p23<-par[26]
        p31<-par[27]
        p32<-par[28]
        p33<-par[29]  
        p10<-par[30]
        d<-par[31]
        g<-par[32]  
        m<-par[33]
        wti.var <- par[34]
        hhg.var <- par[35]
        rep.var <- par[36]
        b<- 0
        c<- 0
        f<- 0
        res.wti <- dlWTI - k10 - k11*lag(dlWTI,1) - k12*lag(dlWTI,2) - k13*lag(dlWTI,3) 
                        - k21*lag(dlHHG,1) - k22*lag(dlHHG,2) - k23*lag(dlHHG,3) 
                        - k31*lag(dlHHG,1) - k32*lag(dlHHG,2) - k33*lag(dlHHG,3) 
        res.hhg <- dlHHG - a10 - a11*lag(dlWTI,1) - a12*lag(dlWTI,2) - a13*lag(dlWTI,3) 
        - a21*lag(dlHHG,1) - a22*lag(dlHHG,2) - a23*lag(dlHHG,3) 
        - a31*lag(dlHHG,1) - a32*lag(dlHHG,2) - a33*lag(dlHHG,3) 
        res.rep <- lREP - p10 - p11*lag(dlWTI,1) - p12*lag(dlWTI,2) - p13*lag(dlWTI,3) 
        - p21*lag(dlHHG,1) - p22*lag(dlHHG,2) - p23*lag(dlHHG,3) 
        - p31*lag(dlHHG,1) - p32*lag(dlHHG,2) - p33*lag(dlHHG,3) 
        rfvar.wti <- mean(res.wti^2)
        rfvar.hhg<- mean(res.hhg^2)
        rfvar.rep<- mean(res.rep^2)
        rfcovwtihhg <- mean(res.wti*res.hhg)
        rfcovwtirep <- mean(res.wti*res.rep)
        rfcovhhgrep <- mean(res.hhg*res.rep)
        dev1 <-  rfvar.wti - wti.var - b^2*hhg.var - c^2*rep.var
        dev2 <-  rfvar.hhg - d^2*wti.var - hhg.var - f^2*rep.var
        dev3 <-  rfvar.rep - g^2*wti.var - m^2*hhg.var - rep.var
        dev4 <-  rfcovwtihhg - d*wti.var- b*hhg.var  - c*f*rep.var
        dev5 <-  rfcovwtirep - g*wti.var- b*m*hhg.var  - c*rep.var
        dev6 <-  rfcovhhgrep - g*d*wti.var- m*hhg.var  - f*rep.var
        return(dev1^2 + dev2^2 + dev3^2 + dev1^4 + dev2^5 + dev3^6 )
}


#objfnc2(c(rep(0.1, 33), 0.1, 0.1,0.1))  

K<-optim(c(rep(0.01, 33), 0.3, 0.3, 0.3), objfnc2,
      control=list(maxit=10000))  



B<- matrix(c(K$par[1], K$par[11],K$par[21],
             K$par[2],K$par[12],K$par[22],
             K$par[3],K$par[13],K$par[23],
             K$par[4],K$par[14],K$par[24],
             K$par[7],K$par[17],K$par[27],
             K$par[5],K$par[15],K$par[25],
             K$par[6],K$par[16],K$par[26],
             K$par[8],K$par[18],K$par[28],
             K$par[9],K$par[19],K$par[29]), ncol=9)


#This matrix is the same as the matrix A  in B-Model below = b[lower.tri(b)] <- NA
D<- (matrix(c(K$par[34],0,0,
              0,K$par[35],0,
              0,0,K$par[36]), ncol=3)^.5)

Ainv<-matrix(c(1,K$par[31],K$par[32],
               0,1,K$par[33],
               0,0,1), ncol=3)

Shock<- Ainv%*%D
Shock




#Combining the stationary series
Enerdata<-ts.combine(dlWTI, dlHHG, lREP)

# Structural VAR Estimation
Enermodel1<-tsreg(dlWTI, ts.combine(lags(dlWTI,1:3), lags(lHHG,1:3), lags(lREP,1:3)))
Enermodel1

Enermodel1
Enermodel2<-tsreg(dlHHG, ts.combine(lags(dlWTI,0:3), lags(dlHHG,1:3),lags(lREP,1:3)))
Enermodel2

Enermodel3<-tsreg(lREP, ts.combine(lags(dlWTI,0:3), lags(dlHHG,0:3), lags(lREP,1:3)))
Enermodel3










# Structural VAR Residual Matrix
Resid<-matrix(c(Enermodel1$residuals, Enermodel2$residuals, Enermodel3$residuals),ncol=3)

# Structural VAR Covariance Matrix
CV<-cov(Resid)
CV

# Contemporaneous Effect Matrix
ConD<-matrix(c(0,Enermodel2$coefficients[2],Enermodel3$coefficients[2],0,0,Enermodel3$coefficients[4],0,0,0), ncol=3)
ConD

# The Inverse of Reduced Form Contemporaneos matrix
ImP<- diag(3) - ConD
ImP
InverseImp<-solve(ImP)
InverseImp


# Impact Effect Matrix from Structural Shock
ImpactShock<-t(chol(InverseImp%*%CV%*%t(InverseImp)))
ImpactShock




#CoeMatrix<-matrix(c(Enermodel1$coefficients[2],Enermodel2$coefficients[3],Enermodel3$coefficients[3],
#                   Enermodel1$coefficients[3],Enermodel2$coefficients[4],Enermodel3$coefficients[5],
#                  Enermodel1$coefficients[4],Enermodel2$coefficients[5],Enermodel3$coefficients[6]), ncol=3)






























# This use matrix on the lags and the convariance matrix similar to what I estimated to generate the 
#series and later reestimate the model, and it is very close to that parameter.
# Generate series BModel - This exactly I want to do but I am still having many lags parameters
# Reset random number generator for reproducibility
set.seed(24579)

tt <- 500 # Number of time series observations

# Coefficient matrix
A_1 <- matrix(c(0.3, 0, 0.24,
                0.12, 0.3, 0.24,
                0.69, 0.48, 0.3), 3)

# Structural coefficients
B <- diag(1, 3)
B[lower.tri(B)] <- c(-0.14, -0.06, 0.39)

# Generate series
series <- matrix(rnorm(3, 0, 1), 3, tt + 1) # Raw series with zeros
for (i in 2:(tt + 1)){
  series[, i] <- A_1 %*% series[, i - 1] +  B %*% rnorm(3, 0, 1)
}

series <- ts(t(series)) # Convert to time series object
dimnames(series)[[2]] <- c("S1", "S2", "S3") # Rename variables

# Plot the series
plot.ts(series, main = "Artificial time series")
library(vars)

# Estimate reduced form VAR
var_est <- VAR(series, p = 1, type = "none")
var_est



A- Model 
The A-model requires to specify a matrix Amat, which contains the K(K−1)/2K(K−1)/2 restrictions. 
In the following example, we create a diagonal matrix with ones as diagonal elements and zeros 
in its upper triangle. The lower triangular elements are set to NA, which indicates that 
they should be estimated.
# Estimate structural coefficients
a <- diag(1, 3)
a[lower.tri(a)] <- NA

svar_est_a <- SVAR(var_est, Amat = a, max.iter = 1000)

svar_est_a

## 
## SVAR Estimation Results:
## ======================== 
## 
## 
## Estimated A matrix:
##         S1      S2 S3
## S1 1.00000  0.0000  0
## S2 0.18177  1.0000  0
## S3 0.05078 -0.3132  1

The result is not equal to matrix B,
because we estimated an A-model. In order to translate it into the
structural coefficients of the B-model, we only have to obtain the inverse 
of the matrix:

solve(svar_est_a$A)

B-model

B-modes are estimated in a similar way as A-models by specifying a matrix Bmat, 
which contains restrictions on the structural matrix B
B. In the following example B is equal to Amat above.

# Create structural matrix with restrictions

b <- diag(1, 3)
b[lower.tri(b)] <- NA

# Estimate
svar_est_b <- SVAR(var_est, Bmat = b)

# Show result
svar_est_b

## 
## SVAR Estimation Results:
## ======================== 
## 
## 
## Estimated B matrix:
##         S1     S2 S3
## S1  1.0000 0.0000  0
## S2 -0.1818 1.0000  0
## S3 -0.1077 0.3132  1



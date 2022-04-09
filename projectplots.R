Enerdata<-ts.combine(dlWTI, dlHHG, lREP)
Enermodel<- VAR(Enerdata, p=3)
Enermodel

#WTI response to oil shock
irf <- irf(Enermodel, impulse="dlWTI", response="dlWTI", boot=TRUE, n.ahead=24, ci=0.95)
irf_df1 <- data.frame(Time=1:25, 
                      actual=irf$irf,
                      lower=irf$Lower,
                      upper=irf$Upper)
colnames(irf_df1) <- c('Time','actual','lower1','upper1')
op <- par(family = "Times New Roman") 
plot(irf_df1$Time, irf_df1$actual, type = "l", ylim=c(-0.015,0.09), col = 1, xlab="", ylab="")
lines(irf_df1$Time, irf_df1$lower1, type = "l", lty=2, lwd= 2,col = 2)
lines(irf_df1$Time, irf_df1$upper1, type = "l", lty=2, lwd= 2,col = 2)
abline(h=0, col="black")
mtext("WTI crude oil price",side=2,line=2.5)
mtext("Oil market shock",side=3,line=0.5)
legend("topright",legend=c("mean effect","two s.e. band"),
       cex=0.7, text.col=c("black","black"),pch=c(0,0),col=c("1","2"))


#WTI response to gas shock
irf <- irf(Enermodel, impulse="dlHHG", response="dlWTI", boot=TRUE, n.ahead=24, ci=0.95)
irf_df1 <- data.frame(Time=1:25, 
                      actual=irf$irf,
                      lower=irf$Lower,
                      upper=irf$Upper)
colnames(irf_df1) <- c('Time','actual','lower1','upper1')
op <- par(family = "Times New Roman") 
plot(irf_df1$Time, irf_df1$actual, type = "l", ylim=c(-0.012,0.020), col = 1, xlab="", ylab="")
lines(irf_df1$Time, irf_df1$lower1, type = "l", lty=2, lwd= 2,col = 2)
lines(irf_df1$Time, irf_df1$upper1, type = "l", lty=2, lwd= 2,col = 2)
abline(h=0, col="black")
mtext("WTI crude oil price",side=2,line=2.5)
mtext("Natural Gas shock",side=3,line=0.5)
legend("topright",legend=c("mean effect","two s.e. band"),
       cex=0.7, text.col=c("black","black"),pch=c(0,0),col=c("1","2"))

#WTI response to electric shock
irf <- irf(Enermodel, impulse="lREP", response="dlWTI", boot=TRUE, n.ahead=24, ci=0.95)
irf_df1 <- data.frame(Time=1:25, 
                      actual=irf$irf,
                      lower=irf$Lower,
                      upper=irf$Upper)
colnames(irf_df1) <- c('Time','actual','lower1','upper1')
op <- par(family = "Times New Roman") 
plot(irf_df1$Time, irf_df1$actual, type = "l", ylim=c(-0.012,0.015), col = 1, xlab="", ylab="")
lines(irf_df1$Time, irf_df1$lower1, type = "l", lty=2, lwd= 2,col = 2)
lines(irf_df1$Time, irf_df1$upper1, type = "l", lty=2, lwd= 2,col = 2)
abline(h=0, col="black")
mtext("WTI crude oil price",side=2,line=2.5)
mtext("Electric-specific shock",side=3,line=0.5)
legend("topright",legend=c("mean effect","two s.e. band"),
       cex=0.7, text.col=c("black","black"),pch=c(0,0),col=c("1","2"))



#gas response to oil shock
irf <- irf(Enermodel, impulse="dlWTI", response="dlHHG", boot=TRUE, n.ahead=24, ci=0.95)
irf_df1 <- data.frame(Time=1:25, 
                      actual=irf$irf,
                      lower=irf$Lower,
                      upper=irf$Upper)
colnames(irf_df1) <- c('Time','actual','lower1','upper1')
op <- par(family = "Times New Roman") 
plot(irf_df1$Time, irf_df1$actual, type = "l", ylim=c(-0.025,0.058), col = 1, xlab="", ylab="")
lines(irf_df1$Time, irf_df1$lower1, type = "l", lty=2, lwd= 2,col = 2)
lines(irf_df1$Time, irf_df1$upper1, type = "l", lty=2, lwd= 2,col = 2)
abline(h=0, col="black")
mtext("HHG gas price",side=2,line=2.5)
mtext("Oil market shock",side=3,line=0.5)
legend("topright",legend=c("mean effect","two s.e band"),
       cex=0.7, text.col=c("black","black"),pch=c(0,0),col=c("1","2"))


#gas response to gas shock
irf <- irf(Enermodel, impulse="dlHHG", response="dlHHG", boot=TRUE, n.ahead=24, ci=0.95)
irf_df1 <- data.frame(Time=1:25, 
                      actual=irf$irf,
                      lower=irf$Lower,
                      upper=irf$Upper)
colnames(irf_df1) <- c('Time','actual','lower1','upper1')
op <- par(family = "Times New Roman") 
plot(irf_df1$Time, irf_df1$actual, type = "l", ylim=c(-0.025,0.16), col = 1, xlab="", ylab="")
lines(irf_df1$Time, irf_df1$lower1, type = "l", lty=2, lwd= 2,col = 2)
lines(irf_df1$Time, irf_df1$upper1, type = "l", lty=2, lwd= 2,col = 2)
abline(h=0, col="black")
mtext("HHG gas price",side=2,line=2.5)
mtext("Natural gas shock",side=3,line=0.5)
legend("topright",legend=c("mean effect","two s.e. band"),
       cex=0.7, text.col=c("black","black"),pch=c(0,0),col=c("1","2"))


#gas response to electric shock
irf <- irf(Enermodel, impulse="lREP", response="dlHHG", boot=TRUE, n.ahead=24, ci=0.95)
irf_df1 <- data.frame(Time=1:25, 
                      actual=irf$irf,
                      lower=irf$Lower,
                      upper=irf$Upper)
colnames(irf_df1) <- c('Time','actual','lower1','upper1')
op <- par(family = "Times New Roman") 
plot(irf_df1$Time, irf_df1$actual, type = "l", ylim=c(-0.025,0.015), col = 1, xlab="", ylab="")
lines(irf_df1$Time, irf_df1$lower1, type = "l", lty=2, lwd= 2,col = 2)
lines(irf_df1$Time, irf_df1$upper1, type = "l", lty=2, lwd= 2,col = 2)
abline(h=0, col="black")
mtext("HHG gas price",side=2,line=2.5)
mtext("Electric-specific shock",side=3,line=0.5)
legend("topright",legend=c("mean effect","two s.e. band"),
       cex=0.7, text.col=c("black","black"),pch=c(0,0),col=c("1","2"))




#electric response to oil shock
irf <- irf(Enermodel, impulse="dlWTI", response="lREP", boot=TRUE, n.ahead=24, ci=0.95)
irf_df1 <- data.frame(Time=1:25, 
                      actual=irf$irf,
                      lower=irf$Lower,
                      upper=irf$Upper)
colnames(irf_df1) <- c('Time','actual','lower1','upper1')
op <- par(family = "Times New Roman") 
plot(irf_df1$Time, irf_df1$actual, type = "l", ylim=c(-0.005,0.02), col = 1, xlab="", ylab="")
lines(irf_df1$Time, irf_df1$lower1, type = "l", lty=2, lwd= 2,col = 2)
lines(irf_df1$Time, irf_df1$upper1, type = "l", lty=2, lwd= 2,col = 2)
abline(h=0, col="black")
mtext("Electric price",side=2,line=2.5)
mtext("Oil market shock",side=3,line=0.5)
legend("topright",legend=c("mean effect","two s.e band"),
       cex=0.7, text.col=c("black","black"),pch=c(0,0),col=c("1","2"))


#electric response to gas shock
irf <- irf(Enermodel, impulse="dlHHG", response="lREP", boot=TRUE, n.ahead=24, ci=0.95)
irf_df1 <- data.frame(Time=1:25, 
                      actual=irf$irf,
                      lower=irf$Lower,
                      upper=irf$Upper)
colnames(irf_df1) <- c('Time','actual','lower1','upper1')
op <- par(family = "Times New Roman") 
plot(irf_df1$Time, irf_df1$actual, type = "l", ylim=c(-0.014,0.016), col = 1, xlab="", ylab="")
lines(irf_df1$Time, irf_df1$lower1, type = "l", lty=2, lwd= 2,col = 2)
lines(irf_df1$Time, irf_df1$upper1, type = "l", lty=2, lwd= 2,col = 2)
abline(h=0, col="black")
mtext("Electric price",side=2,line=2.5)
mtext("Natural gas shock",side=3,line=0.5)
legend("topright",legend=c("mean effect","two s.e. band"),
       cex=0.7, text.col=c("black","black"),pch=c(0,0),col=c("1","2"))


#electric response to electric shock
irf <- irf(Enermodel, impulse="lREP", response="lREP", boot=TRUE, n.ahead=24, ci=0.95)
irf_df1 <- data.frame(Time=1:25, 
                      actual=irf$irf,
                      lower=irf$Lower,
                      upper=irf$Upper)
colnames(irf_df1) <- c('Time','actual','lower1','upper1')
op <- par(family = "Times New Roman") 
plot(irf_df1$Time, irf_df1$actual, type = "l", ylim=c(-0.000,0.07), col = 1, xlab="", ylab="")
lines(irf_df1$Time, irf_df1$lower1, type = "l", lty=2, lwd= 2,col = 2)
lines(irf_df1$Time, irf_df1$upper1, type = "l", lty=2, lwd= 2,col = 2)
abline(h=0, col="black")
mtext("Electric price",side=2,line=2.5)
mtext("Electric-specific shock",side=3,line=0.5)
legend("topright",legend=c("mean effect","two s.e. band"),
       cex=0.7, text.col=c("black","black"),pch=c(0,0),col=c("1","2"))



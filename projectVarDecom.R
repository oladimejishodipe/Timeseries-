Enerdata<-ts.combine(dlWTI, dlHHG, lREP)
Enermodel<- VAR(Enerdata, p=3)
Enermodel

# Only pick for the electric prices
fevd(Enermodel, n.ahead=1)
fevd(Enermodel, n.ahead=2)
fevd(Enermodel, n.ahead=5)
fevd(Enermodel, n.ahead=10)
fevd(Enermodel, n.ahead=20)
fevd(Enermodel, n.ahead=200)






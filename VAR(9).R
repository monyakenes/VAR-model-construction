#Packages
library(quantmod) 
library(fBasics)
install.packages("MTS")
require(MTS)
install.packages("readxl")
require(readxl)

require(fUnitRoots)

#Data
setwd("C:/Users/monya/Desktop/ch2")
da= read_excel("sp500_data.xlsx")
com_ = da[,2]
con_ = da[,3]
fin_ = da[,4]
inf_ = da[,5]


com = na.omit(as.numeric(unlist(com_[[1]])))
con = na.omit(as.numeric(unlist(con_[[1]])))
fin = na.omit(as.numeric(unlist(fin_[[1]])))
inf = na.omit(as.numeric(unlist(inf_[[1]])))

z = cbind(com, con, fin, inf)
zt = diffM(log(z,2))
zt= zt*100

dim(zt)
basicStats(zt)
par(mfcol=c(2,1))
acf(zt, lag= 12)
pacf(zt, lag = 12)

# VAR order specification
z1=zt/100
m2=VARorder(z1)
names(m2)

# Use the VAR command in MTS
a1=MTS::VAR(zt, 1)

# Recall VAR estimation
names(m1)
resi=m1$residuals
mq(resi,adj=18)

### Chi-square test for parameter constraints
m3=VARchi(zt,p=1)

m3=VARchi(zt,p=9,thres=1.96)

### Model simplification
a2=refVAR(a1,thres=1.96)

### Model checking
MTSdiag(m2, adj =12)

##### Prediction
VARpred(m2,20)  ## Using unconstrained model
colMeans(zt)
sqrt(apply(zt,2, var))

### Impulse response functions
Phi=a2$Phi
Sig=a2$Sigma
VARirf(Phi,Sig)
VARirf(Phi,Sig,orth=F)

### Forecast error variance decomposition
m2=refVAR(m1)  ## using default threshold

names(m2)
Phi=m2$Phi
Sig=m2$Sigma
Theta=NULL
FEVdec(Phi,Theta,Sig,lag=5)

#Stationarity Test
apply(zt,2,adfTest)

#Lag selection
lags = VARselect(zt, lag.max = 10, type = "const")
lags$selection

#Model Estimation
modelvar = VAR(zt, p=9, type = "const", season = NULL, exog = NULL)
summary(modelvar)

#Tests
Serial1 = serial.test(modelvar, lags.pt = 12, type = "PT.asymptotic")
Serial1
Arch1 = arch.test(modelvar, lags.multi = 12, multivariate.only = TRUE)
Arch1
Norm1 = normality.test(modelvar, multivariate.only = TRUE)
Norm1
Stability1 = stability(modelvar, type = "OLS-CUSUM")
plot(Stability1)

#Granger Causality
Grangersc = causality(modelvar, cause = "com")
Grangersc
Grangerpet = causality(modelvar, cause = "adj_cls2")
Grangerpet
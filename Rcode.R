
getSymbols("PEP",src="yahoo")
getSymbols("KO",src="yahoo")



KO.df = read.csv('KO.csv')
PEP.df = read.csv('PEP.csv')

KO.df['dReturns'] = (KO.df$Open-KO.df$Adj.Close)/KO.df$Adj.Close
PEP.df['dReturns'] = (PEP.df$Open-PEP.df$Adj.Close)/PEP.df$Adj.Close


candleChart(PEP, multi.col = T, theme='white')

candleChart(KO, multi.col = T, theme='white')

#Convert to xts

KO.df$Date = as.POSIXct(as.character(KO.df$Date, format = '%Y/%m/%d'))
PEP.df$Date = as.POSIXct(as.character(PEP.df$Date, format = '%Y/%m/%d'))

KO.xts = xts(KO.df$dReturns, KO.df$Date)
PEP.xts = xts(PEP.df$dReturns, PEP.df$Date)

KO.2019 = KO.xts['2019']
PEP.2019 = PEP.xts['2019']

data.xts = merge(KO.2019, PEP.2019)

#plot the returns

plot(data.xts, ylim=c(-0.01,0.08), main='Returns for KO and PEP', col=c('black', 'red'), lty = c(1,2))
addLegend('topright', lty= c(1,2), lwd=c(2,2), col = c('black', 'red'), bg='white', bty='o')


#is the Spread mean reverting?

Diff.2019 = KO.2019-PEP.2019

mean = mean(Diff.2019)
mean.df = cbind(rep(0, 1259), as.character(KO.df$Date))
mean.df = as.data.frame(mean.df)
colnames(mean.df) = c('mean', 'Date')
mean.df$mean = mean
mean.df$Date = as.POSIXct(as.character(mean.df$Date, format = '%Y/%m/%d'))
mean.xts = xts(mean.df$mean, mean.df$Date)
mean.2019 = mean.xts['2019']

data2.xts = merge(Diff.2019, mean.2019)

#plot Spread

plot(data2.xts, main='')

par(mfrow=c(1,2))
par(mar=c(5,4,4,2))
acf(Diff.2019, main= 'ACF of the Spread')
pacf(Diff.2019, main= 'PACF of the Spread')


#Test for Cointegration

tseries::adf.test(Diff.2019, alternative = 's')



Small p-value rejects the null hypothesis of non-stationarity.
Yes, the spread is likely mean reverting.


###Data Preparation for Training and Testing

train.2019 = Diff.2019['2019-01/2019-11']
test.2019 = Diff.2019['2019-12']



### ARIMA model

final.aic = Inf
final.order = c(0,0,0,0)

for (p in 1:6) for (d in 0:1) for (q in 1:6) for (s in 0:1){
  current.aic = AIC(Arima(train.2019, order=c(p,d,q), seasonal = list(order=c(0,s,0)), method='ML'))
  if (current.aic < final.aic){
    final.aic = current.aic
    final.order = c(p,d,q,s)
  }
}



Lowest AIC is ARIMA(1,0,1) model for the 2019 returns difference.



model.arima = Arima(Diff.2019, order=c(1,0,1), method='ML')


#Residual Analysis

par(mfrow=c(1,1)) 
plot(resid(model.arima), ylab='Residuals',type='o',main="Residual Plot") 
abline(h=0) 
acf(resid(model.arima),main="ACF: Residuals")
hist(resid(model.arima),xlab='Residuals',main='Histogram: Residuals') 
qqnorm(resid(model.arima),ylab="Sample Q",xlab="Theoretical Q") 
qqline(resid(model.arima))



#The residual plot shows the variance is constant across time. Histogram of the Residuals shows unimodal normal distribution. From the Q-Q Plot, the data looks normally distributed with fat tails. There's no residuals that fall outside 0.05 in the acf plot.


Box.test(model.arima$residuals, lag = 6, type='Box-Pierce', fitdf = 4)

Box.test(model.arima$residuals, lag = 6, type='Ljung-Box', fitdf = 4)


High p-values indicate independance in the residuals.


#Test

fore = forecast::forecast(model.arima, h=21)
fore = as.data.frame(fore)


point.fore = cbind(rep(0,21), as.character(index(test.2019)))
point.fore = as.data.frame(point.fore)
colnames(point.fore)= c('Point Estimate', 'Date')
point.fore$`Point Estimate` = fore$`Point Forecast`
point.fore$Date = as.POSIXct(as.character(point.fore$Date, format = '%Y/%m/%d'))
point.fore.xts = xts(point.fore$`Point Estimate`, point.fore$Date)

lo.fore = cbind(rep(0,21), as.character(index(test.2019)))
lo.fore = as.data.frame(lo.fore)
colnames(lo.fore)= c('Low Estimate', 'Date')
lo.fore$`Low Estimate` = fore$`Lo 95`
lo.fore$Date = as.POSIXct(as.character(lo.fore$Date, format = '%Y/%m/%d'))
lo.fore.xts = xts(lo.fore$`Low Estimate`, lo.fore$Date)

hi.fore = cbind(rep(0,21), as.character(index(test.2019)))
hi.fore = as.data.frame(hi.fore)
colnames(hi.fore)= c('High Estimate', 'Date')
hi.fore$`High Estimate` = fore$`Hi 95`
hi.fore$Date = as.POSIXct(as.character(hi.fore$Date, format = '%Y/%m/%d'))
hi.fore.xts = xts(hi.fore$`High Estimate`, hi.fore$Date)

fore.merge = merge(hi.fore.xts, point.fore.xts, lo.fore.xts, test.2019)

plot(fore.merge, ylim=c(-0.05,0.05), main='Spread Forecast', col = c('red', 'blue', 'red', 'black'), lty= c(2,4,2,1))
addLegend('topright', lty= c(2,4,2,1), lwd=c(2,2,2,2), col = c('red', 'blue', 'red', 'black'), bg='white', bty='o')





#VAR Model

KO.PEP.2019 = merge(KO.2019, PEP.2019)

KO.PEP.Diff = diff(KO.PEP.2019)
KO.PEP.Diff = na.omit(KO.PEP.Diff)

plot(KO.PEP.Diff)




model.var = VAR(KO.PEP.Diff)
summary(model.var)


#Restricted VAR
model.var.restrict = restrict(model.var)
summary(model.var.restrict)
### Corona virus CR Forecast based on time series and stochastic processes, 6th May 21

#libraries
library(astsa)
library(forecast)

#loading data
ruta<-#"local path file location\\covid_cr_data.csv"
covid_cr<-read.csv(ruta)
covid_cr<-ts(covid_cr)
class(covid_cr)
head(covid_cr)

#exploring full data
length(covid_cr)
plot(covid_cr, xlab= "Index Days", ylab = "Cases", main= "Covid Cases CR - 6th May 2021")
acf(covid_cr, lag.max = 50, main='ACF of Original data Covid CR')
pacf(covid_cr, lag.max = 50,  main='PACF of Original data Covid CR')


#Looking for the best transformation, keeping whole data
t1<-log(covid_cr)
plot(t1) #is not WN

t2<-diff(log(covid_cr),7) 
plot(t2) #is not WN

t3<-diff(diff(log(covid_cr),7))
plot(t3) #looks like WN but first 100 days are different
Box.test(t3, lag=log(length(t3)))

#Looking for the best transformation, cutting data
plot.ts(covid_cr[100:425], xlab= "Index Days", ylab = "Cases", main= "Covid Cases CR (100 days cut) - 6th May 2021")
zoom_covid_cr<-ts(covid_cr[100:425])
length(zoom_covid_cr)


par(mfrow=c(2,1))
t4<-diff(diff(log(zoom_covid_cr),6)) #still having seasonality
plot(t4)

t<-diff(diff(log(zoom_covid_cr),7))
plot(t, xlab= "Index Days", ylab = "Transformation Data", main= "White Noise transformation data - 6th May 2021") #this is the most similar to WN
Box.test(t, lag=log(length(t))) #we get a very very low p-value according to theory
par(mfrow=c(1,1))
acf(zoom_covid_cr, lag.max = 50, main='ACF of Transformed data Covid CR')
pacf(zoom_covid_cr, lag.max = 50,  main='PACF of Transformed data Covid CR')


#analysis:
data_t<-diff(diff(log(zoom_covid_cr),7))
Box.test(data_t, lag=log(length(data_t))) #we get a very very low p-value according to theory
acf(data_t, lag.max = 50, main='ACF of Transformed data')
pacf(data_t, lag.max = 50,  main='PACF of Transformed data')


#test and choose sarima order
#Testing models
d=1
DD=1
per=7

for(p in 1:5){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(zoom_covid_cr), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

#Residuals Analysis:
sarima(log(zoom_covid_cr), 0,1,1,0,1,1,7) #we get the best residuals behavior with this model
sarima(log(zoom_covid_cr), 0,1,1,1,1,1,7)
sarima(log(zoom_covid_cr), 0,1,1,1,1,0,7)


#model and forecast
model<- arima(x=log(zoom_covid_cr), order = c(0,1,1), seasonal = list(order=c(0,1,1), period=7))
par(mfrow=c(1,1))
plot(forecast(model,12),xlab= "Index Days", ylab = "Transformed Cases", main= "Forecast Covid Cases CR - 6th May")
forecast(model,12)








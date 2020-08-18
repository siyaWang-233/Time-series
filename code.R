library(tseries)
#读取数据
da<-read.csv("C:\\Users\\46693\\Desktop\\ts_data.csv",header=T)
data<-(da$铁路客运量)
da.ts<-ts(data,frequency = 12,start = c(2010,1))
plot(da.ts,ylab="铁路客运量",xlab="时间")
da.decom <- decompose(da.ts, type = "mult") 
plot(da.decom) 
data1<-data.frame(da.ts)
#1阶差分
plot(diff(da.ts))
#12步差分
da1<-diff(da.ts)
plot(diff(da1,12))
acf(data.frame(diff(da1,12)))
pacf(data.frame(diff(da1,12)))
#平稳性检验
adf.test(diff(da1,12))
#白噪声检验
Box.test(diff(da1,12),type="Ljung-Box",lag=12)
#画图
plot(log(da.ts))
plot(diff(log(da.ts))) 
acf(data1)
pacf(data1)
#定阶
get.best.arima <- function(x.ts, maxord = c(1,1,1,1,1,1))
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
    for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p,d,q),
                   seas = list(order = c(P,D,Q),
                               frequency(x.ts)), method = "CSS")
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
      if (fit.aic < best.aic)
      {
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p,d,q,P,D,Q)
      } }
  list(best.aic, best.fit, best.model)
}
best.arima.da <- get.best.arima( log(da.ts),
                                   maxord = c(2,2,2,2,2,2))
best.fit.da <- best.arima.da[[2]] ##[[]]
best.arima.da
#残差检验
r<-resid(best.fit.da)
re<-data.frame(r)
acf(re)
pacf(re)
Box.test(re,type="Ljung-Box",lag=12)
acf(re^2)
pacf(re^2)
Box.test(re^2,type="Ljung-Box",lag=12)
#预测
ts.plot(cbind( window(da.ts,start = 2010),
               exp(predict(best.fit.da,36)$pred) ), lty = 1:2)
da.arima.pred<-exp(predict(best.fit.da,36)$pred)
da.arima.pred







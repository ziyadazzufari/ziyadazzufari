library(forecast)
library(TSA)
library(tseries)
library(readxl)
library(ggplot2)
data_produksi<-read_excel("E:/kp/project/final/Final.xlsx")
produksi_soft_heavy=data_produksi[2]
produksi_soft_heavy
produksi_soft_heavy.ts=ts(data_produksi[2],start=c(2019,1),frequency = 12)
produksi_soft_heavy.ts
plot(produksi_soft_heavy.ts,xlab='Tahun',ylab='Jumlah Produksi (ton)',main='DATA PRODUKSI SOFT HEAYV')
abline(reg=lm(produksi_soft_heavy.ts~time(produksi_soft_heavy.ts)))
adf.test(produksi_soft_heavy.ts)

produksi_HS=diff(produksi_soft_heavy.ts,differencing =1 )
produksi_HS
plot(produksi_HS,xlab='Tahun',ylab='Jumlah Produksi (ton)',main='DATA PRODUKSI HEAVY SOFT')
abline(reg=lm(produksi_HS~time(produksi_HS)))
adf.test(produksi_HS)

produksi_HS=diff(produksi_HS,differencing =1)
produksi_HS
plot(produksi_HS,xlab='Tahun',ylab='Jumlah Produksi (ton)',main='DATA PRODUKSI HEAVY SOFT')
abline(reg=lm(produksi_HS~time(produksi_HS)))
adf.test(produksi_HS)
acf(produksi_HS)
pacf(produksi_HS)


model1=Arima(produksi_soft_heavy.ts, order=c(0,2,0),lambda=0)
model1$fitted
accuracy(model1)

model2=Arima(produksi_soft_heavy.ts, order=c(0,2,1),lambda=0)
model2$fitted
accuracy(model2)

model3=Arima(produksi_soft_heavy.ts, order=c(0,2,4),lambda=0)
model3$fitted
accuracy(model3)

model4=Arima(produksi_soft_heavy.ts, order=c(0,2,5),lambda=0)
model4$fitted
accuracy(model4)

model5=Arima(produksi_soft_heavy.ts, order=c(1,2,0),lambda=0)
model5$fitted
accuracy(model5)

model6=Arima(produksi_soft_heavy.ts, order=c(1,2,1),lambda=0)
model6$fitted
accuracy(model6)

model7=Arima(produksi_soft_heavy.ts, order=c(1,2,4),lambda=0)
model7$fitted
accuracy(model7)

model8=Arima(produksi_soft_heavy.ts, order=c(1,2,5),lambda=0)
model8$fitted
accuracy(model8)

model9=Arima(produksi_soft_heavy.ts, order=c(2,2,0),lambda=0)
model9$fitted
accuracy(model9)

model10=Arima(produksi_soft_heavy.ts, order=c(2,2,1),lambda=0)
model10$fitted
accuracy(model10)

model11=Arima(produksi_soft_heavy.ts, order=c(2,2,4),lambda=0)
model11$fitted
accuracy(model11)

model12=Arima(produksi_soft_heavy.ts, order=c(2,2,5),lambda=0)
model12$fitted
accuracy(model12)

model13=Arima(produksi_soft_heavy.ts, order=c(3,2,0),lambda=0)
model13$fitted
accuracy(model13)

model14=Arima(produksi_soft_heavy.ts, order=c(3,2,1),lambda=0)
model14$fitted
accuracy(model14)

model15=Arima(produksi_soft_heavy.ts, order=c(3,2,4),lambda=0)
model15$fitted
accuracy(model15)

model16=Arima(produksi_soft_heavy.ts, order=c(3,2,5),lambda=0)
model16$fitted
accuracy(model16)

MAD_1=mean(abs(produksi_soft_heavy.ts-model1$fitted))
MAD_1
MAPE_1=mean(abs(produksi_soft_heavy.ts-model1$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_1
MSE_1=mean(abs(produksi_soft_heavy.ts-model1$fitted)^2)
MSE_1
perbandingan_1=c(MAD_1,MAPE_1,MSE_1)
perbandingan_1

MAD_2=mean(abs(produksi_soft_heavy.ts-model2$fitted))
MAD_2
MAPE_2=mean(abs(produksi_soft_heavy.ts-model2$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_2
MSE_2=mean(abs(produksi_soft_heavy.ts-model2$fitted)^2)
MSE_2
perbandingan_2=c(MAD_2,MAPE_2,MSE_2)
perbandingan_2

MAD_3=mean(abs(produksi_soft_heavy.ts-model3$fitted))
MAD_3
MAPE_3=mean(abs(produksi_soft_heavy.ts-model3$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_3
MSE_3=mean(abs(produksi_soft_heavy.ts-model3$fitted)^2)
MSE_3
perbandingan_3=c(MAD_3,MAPE_3,MSE_3)
perbandingan_3

MAD_4=mean(abs(produksi_soft_heavy.ts-model4$fitted))
MAD_4
MAPE_4=mean(abs(produksi_soft_heavy.ts-model4$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_4
MSE_4=mean(abs(produksi_soft_heavy.ts-model4$fitted)^2)
MSE_4
perbandingan_4=c(MAD_4,MAPE_4,MSE_4)
perbandingan_4

MAD_5=mean(abs(produksi_soft_heavy.ts-model5$fitted))
MAD_5
MAPE_5=mean(abs(produksi_soft_heavy.ts-model5$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_5
MSE_5=mean(abs(produksi_soft_heavy.ts-model5$fitted)^2)
MSE_5
perbandingan_5=c(MAD_5,MAPE_5,MSE_5)
perbandingan_5

MAD_6=mean(abs(produksi_soft_heavy.ts-model6$fitted))
MAD_6
MAPE_6=mean(abs(produksi_soft_heavy.ts-model6$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_6
MSE_6=mean(abs(produksi_soft_heavy.ts-model6$fitted)^2)
MSE_6
perbandingan_6=c(MAD_6,MAPE_6,MSE_6)
perbandingan_6

MAD_7=mean(abs(produksi_soft_heavy.ts-model7$fitted))
MAD_7
MAPE_7=mean(abs(produksi_soft_heavy.ts-model7$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_7
MSE_7=mean(abs(produksi_soft_heavy.ts-model7$fitted)^2)
MSE_7
perbandingan_7=c(MAD_7,MAPE_7,MSE_7)
perbandingan_7

MAD_8=mean(abs(produksi_soft_heavy.ts-model8$fitted))
MAD_8
MAPE_8=mean(abs(produksi_soft_heavy.ts-model8$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_8
MSE_8=mean(abs(produksi_soft_heavy.ts-model8$fitted)^2)
MSE_8
perbandingan_8=c(MAD_8,MAPE_8,MSE_8)
perbandingan_8

MAD_9=mean(abs(produksi_soft_heavy.ts-model9$fitted))
MAD_9
MAPE_9=mean(abs(produksi_soft_heavy.ts-model9$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_9
MSE_9=mean(abs(produksi_soft_heavy.ts-model9$fitted)^2)
MSE_9
perbandingan_9=c(MAD_9,MAPE_9,MSE_9)
perbandingan_9

MAD_10=mean(abs(produksi_soft_heavy.ts-model10$fitted))
MAD_10
MAPE_10=mean(abs(produksi_soft_heavy.ts-model10$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_10
MSE_10=mean(abs(produksi_soft_heavy.ts-model10$fitted)^2)
MSE_10
perbandingan_10=c(MAD_10,MAPE_10,MSE_10)
perbandingan_10

MAD_11=mean(abs(produksi_soft_heavy.ts-model11$fitted))
MAD_11
MAPE_11=mean(abs(produksi_soft_heavy.ts-model11$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_11
MSE_11=mean(abs(produksi_soft_heavy.ts-model11$fitted)^2)
MSE_11
perbandingan_11=c(MAD_11,MAPE_11,MSE_11)
perbandingan_11

MAD_12=mean(abs(produksi_soft_heavy.ts-model12$fitted))
MAD_12
MAPE_12=mean(abs(produksi_soft_heavy.ts-model12$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_12
MSE_12=mean(abs(produksi_soft_heavy.ts-model12$fitted)^2)
MSE_12
perbandingan_12=c(MAD_12,MAPE_12,MSE_12)
perbandingan_12

MAD_13=mean(abs(produksi_soft_heavy.ts-model13$fitted))
MAD_13
MAPE_13=mean(abs(produksi_soft_heavy.ts-model13$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_13
MSE_13=mean(abs(produksi_soft_heavy.ts-model13$fitted)^2)
MSE_13
perbandingan_13=c(MAD_13,MAPE_13,MSE_13)
perbandingan_13

MAD_14=mean(abs(produksi_soft_heavy.ts-model14$fitted))
MAD_14
MAPE_14=mean(abs(produksi_soft_heavy.ts-model14$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_14
MSE_14=mean(abs(produksi_soft_heavy.ts-model14$fitted)^2)
MSE_14
perbandingan_14=c(MAD_14,MAPE_14,MSE_14)
perbandingan_14

MAD_15=mean(abs(produksi_soft_heavy.ts-model15$fitted))
MAD_15
MAPE_15=mean(abs(produksi_soft_heavy.ts-model15$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_15
MSE_15=mean(abs(produksi_soft_heavy.ts-model15$fitted)^2)
MSE_15
perbandingan_15=c(MAD_15,MAPE_15,MSE_15)
perbandingan_15

MAD_16=mean(abs(produksi_soft_heavy.ts-model16$fitted))
MAD_16
MAPE_16=mean(abs(produksi_soft_heavy.ts-model16$fitted)/produksi_soft_heavy.ts,na.rm=TRUE)
MAPE_16
MSE_16=mean(abs(produksi_soft_heavy.ts-model16$fitted)^2)
MSE_16
perbandingan_16=c(MAD_16,MAPE_16,MSE_16)
perbandingan_16

perbandingantotal=rbind(perbandingan_1,perbandingan_2,perbandingan_3,perbandingan_4,perbandingan_5,perbandingan_6,perbandingan_7,perbandingan_8,perbandingan_9,perbandingan_10,perbandingan_11,perbandingan_12,perbandingan_13,perbandingan_14,perbandingan_15,perbandingan_16)
perbandingantotal

Box.test(produksi_soft_heavy.ts-model8$fitted,type = "Ljung")
ks.test(produksi_soft_heavy.ts-model8$fitted,"pnorm",mean(produksi_soft_heavy.ts-model8$fitted),sd(produksi_soft_heavy.ts-model8$fitted))

Box.test(produksi_soft_heavy.ts-model12$fitted,type = "Ljung")
ks.test(produksi_soft_heavy.ts-model12$fitted,"pnorm",mean(produksi_soft_heavy.ts-model12$fitted),sd(produksi_soft_heavy.ts-model12$fitted))

# Forecast plot with integer y-axis labels

ramalan_1 <- forecast::forecast(model12, h = 6)
autoplot(ramalan_1) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ylab("Produksi Soft Heavy (ton)") + 
  xlab("Tahun")
ramalan_1

one_step_forecasts=fitted(model12)

g=ggplot() +
  geom_line(
    aes(
      x = as.numeric(time(produksi_soft_heavy.ts)),
      y = as.numeric(one_step_forecasts)
    ),
    col = "black"
  ) +
  geom_line(
    aes(
      x = as.numeric(time(produksi_soft_heavy.ts)),
      y = as.numeric(produksi_soft_heavy.ts)
    ),
    col = "red"
  )
g <- g + ylab("Produksi Soft Heavy") + xlab("Waktu")
g

#Peramalan_produksi_Soft_medium
data_produksi<-read_excel("E:/kp/project/final/Final.xlsx")
produksi_soft_medium=data_produksi[4]
produksi_soft_medium
produksi_soft_medium.ts=ts(data_produksi[4],start=c(2019,1),frequency = 12)
produksi_soft_medium.ts
plot(produksi_soft_medium.ts,xlab='Tahun',ylab='Jumlah Produksi (ton)',main='DATA PRODUKSI MEDIUM SOFT')
abline(reg=lm(produksi_soft_medium.ts~time(produksi_soft_medium.ts)))

adf.test(produksi_soft_medium.ts)
box_cox1=sqrt(produksi_soft_medium.ts)
box_cox1
box_cox2=sqrt(box_cox1)
box_cox2

produksi_MS=diff(box_cox2,differencing =1 )
produksi_MS
plot(produksi_MS,xlab='Tahun',ylab='Jumlah Produksi (ton)',main='DATA PRODUKSI MEDIUM SOFT')
abline(reg=lm(produksi_MS~time(produksi_MS)))
adf.test(produksi_MS)

produksi_MS2=diff(produksi_MS,differencing=1)
produksi_MS2

adf.test(produksi_MS2)

acf(produksi_MS2)
pacf(produksi_MS2)

model1=Arima(produksi_soft_medium.ts, order=c(0,2,0),lambda = 0)
model1
model1$fitted
accuracy(model1)

model2=Arima(produksi_soft_medium.ts, order=c(0,2,1),lambda = 0)
model2
model2$fitted
accuracy(model2)

model3=Arima(produksi_soft_medium.ts, order=c(0,2,16),lambda = 0)
model3
model3$fitted
accuracy(model3)

model4=Arima(produksi_soft_medium.ts, order=c(1,2,0),lambda = 0)
model4
model4$fitted
accuracy(model4)

model5=Arima(produksi_soft_medium.ts, order=c(1,2,1),lambda = 0)
model5
model5$fitted
accuracy(model5)

model6=Arima(produksi_soft_medium.ts, order=c(1,2,16),lambda = 0)
model6
model6$fitted
accuracy(model6)

model7=Arima(produksi_soft_medium.ts, order=c(2,2,0),lambda = 0)
model7
model7$fitted
accuracy(model7)

model8=Arima(produksi_soft_medium.ts, order=c(2,2,1),lambda = 0)
model8
model8$fitted
accuracy(model8)

model9=Arima(produksi_soft_medium.ts, order=c(2,2,16),lambda = 0)
model9
model9$fitted
accuracy(model9)

MAD_1=mean(abs(produksi_soft_medium.ts-model1$fitted))
MAD_1
MAPE_1=mean(abs(produksi_soft_medium.ts-model1$fitted)/produksi_soft_medium.ts,na.rm=TRUE)
MAPE_1
MSE_1=mean(abs(produksi_soft_medium.ts-model1$fitted)^2)
MSE_1
perbandingan_1=c(MAD_1,MAPE_1,MSE_1)
perbandingan_1

MAD_2=mean(abs(produksi_soft_medium.ts-model2$fitted))
MAD_2
MAPE_2=mean(abs(produksi_soft_medium.ts-model2$fitted)/produksi_soft_medium.ts,na.rm=TRUE)
MAPE_2
MSE_2=mean(abs(produksi_soft_medium.ts-model2$fitted)^2)
MSE_2
perbandingan_2=c(MAD_2,MAPE_2,MSE_2)
perbandingan_2

MAD_3=mean(abs(produksi_soft_medium.ts-model3$fitted))
MAD_3
MAPE_3=mean(abs(produksi_soft_medium.ts-model3$fitted)/produksi_soft_medium.ts,na.rm=TRUE)
MAPE_3
MSE_3=mean(abs(produksi_soft_medium.ts-model3$fitted)^2)
MSE_3
perbandingan_3=c(MAD_3,MAPE_3,MSE_3)
perbandingan_3

MAD_4=mean(abs(produksi_soft_medium.ts-model4$fitted))
MAD_4
MAPE_4=mean(abs(produksi_soft_medium.ts-model4$fitted)/produksi_soft_medium.ts,na.rm=TRUE)
MAPE_4
MSE_4=mean(abs(produksi_soft_medium.ts-model4$fitted)^2)
MSE_4
perbandingan_4=c(MAD_4,MAPE_4,MSE_4)
perbandingan_4

MAD_5=mean(abs(produksi_soft_medium.ts-model5$fitted))
MAD_5
MAPE_5=mean(abs(produksi_soft_medium.ts-model5$fitted)/produksi_soft_medium.ts,na.rm=TRUE)
MAPE_5
MSE_5=mean(abs(produksi_soft_medium.ts-model5$fitted)^2)
MSE_5
perbandingan_5=c(MAD_5,MAPE_5,MSE_5)
perbandingan_5

MAD_6=mean(abs(produksi_soft_medium.ts-model6$fitted))
MAD_6
MAPE_6=mean(abs(produksi_soft_medium.ts-model6$fitted)/produksi_soft_medium.ts,na.rm=TRUE)
MAPE_6
MSE_6=mean(abs(produksi_soft_medium.ts-model6$fitted)^2)
MSE_6
perbandingan_6=c(MAD_6,MAPE_6,MSE_6)
perbandingan_6

MAD_7=mean(abs(produksi_soft_medium.ts-model7$fitted))
MAD_7
MAPE_7=mean(abs(produksi_soft_medium.ts-model7$fitted)/produksi_soft_medium.ts,na.rm=TRUE)
MAPE_7
MSE_7=mean(abs(produksi_soft_medium.ts-model7$fitted)^2)
MSE_7
perbandingan_7=c(MAD_7,MAPE_7,MSE_7)
perbandingan_7

MAD_8=mean(abs(produksi_soft_medium.ts-model8$fitted))
MAD_8
MAPE_8=mean(abs(produksi_soft_medium.ts-model8$fitted)/produksi_soft_medium.ts,na.rm=TRUE)
MAPE_8
MSE_8=mean(abs(produksi_soft_medium.ts-model8$fitted)^2)
MSE_8
perbandingan_8=c(MAD_8,MAPE_8,MSE_8)
perbandingan_8

MAD_9=mean(abs(produksi_soft_medium.ts-model9$fitted))
MAD_9
MAPE_9=mean(abs(produksi_soft_medium.ts-model9$fitted)/produksi_soft_medium.ts,na.rm=TRUE)
MAPE_9
MSE_9=mean(abs(produksi_soft_medium.ts-model9$fitted)^2)
MSE_9
perbandingan_9=c(MAD_9,MAPE_9,MSE_9)
perbandingan_9

perbandingantotal=rbind(perbandingan_1,perbandingan_2,perbandingan_3,perbandingan_4,perbandingan_5,perbandingan_6,perbandingan_7,perbandingan_8,perbandingan_9)
perbandingantotal

Box.test(produksi_soft_medium.ts-model3$fitted,type = "Ljung")
ks.test(produksi_soft_medium.ts-model3$fitted,"pnorm",mean(produksi_soft_medium.ts-model3$fitted),sd(produksi_soft_medium.ts-model3$fitted))

ramalan_3=forecast::forecast(model3,h=6)
ramalan_3
summary(ramalan_3)
plot(ramalan_3)

one_step_forecasts=fitted(model3)

g=ggplot() +
  geom_line(
    aes(
      x = as.numeric(time(produksi_soft_medium.ts)),
      y = as.numeric(one_step_forecasts)
    ),
    col = "black"
  ) +
  geom_line(
    aes(
      x = as.numeric(time(produksi_soft_medium.ts)),
      y = as.numeric(produksi_soft_medium.ts)
    ),
    col = "red"
  )
g <- g + ylab("Produksi Soft Medium") + xlab("Waktu")
g

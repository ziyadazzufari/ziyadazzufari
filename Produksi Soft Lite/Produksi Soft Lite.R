data_produksi<-read_excel("E:/kp/project/final/Final.xlsx")
produksi_soft_lite=data_produksi[3]
produksi_soft_lite
produksi_soft_lite.ts=ts(data_produksi[3],start=c(2019,1),frequency = 12)
produksi_soft_lite.ts
plot(produksi_soft_lite.ts,xlab='Tahun',ylab='Jumlah Produksi (ton)',main='DATA PRODUKSI SOFT LITE')
abline(reg=lm(produksi_soft_lite.ts~time(produksi_soft_lite.ts)))
adf.test(produksi_soft_lite.ts)

transformasi_data=log(produksi_soft_lite.ts+1)

produksi_SL=diff(transformasi_data,differencing =1 )
produksi_SL
plot(produksi_SL,xlab='Tahun',ylab='Jumlah Produksi (ton)',main='DATA PRODUKSI Soft Lite')
abline(reg=lm(produksi_SL~time(produksi_SL)))
adf.test(produksi_SL)

acf(produksi_SL)
pacf(produksi_SL)

model1=Arima(transformasi_data, order=c(0,1,0))
model1
model1$fitted


model2=Arima(transformasi_data, order=c(0,1,1))
model2
model2$fitted
model2$fitted=exp(model2$fitted)-1
model2$fitted

model3=Arima(transformasi_data, order=c(1,1,0))
model3
model3$fitted
model3$fitted=exp(model3$fitted)-1
model3$fitted

model4=Arima(transformasi_data, order=c(1,1,1))
model4
model4$fitted
model4$fitted=exp(model4$fitted)-1
model4$fitted

model5=Arima(transformasi_data, order=c(4,1,0))
model5
model5$fitted
model5$fitted=exp(model5$fitted)-1
model5$fitted

model6=Arima(transformasi_data, order=c(4,1,1))
model6
model6$fitted
model6$fitted=exp(model6$fitted)-1
model6$fitted

MAD_1=mean(abs(produksi_soft_lite.ts-model1$fitted))
MAD_1
MAPE_1=mean(abs(produksi_soft_lite.ts-model1$fitted)/produksi_soft_lite.ts,na.rm=TRUE)
MAPE_1
MSE_1=mean(abs(produksi_soft_lite.ts-model1$fitted)^2)
MSE_1
perbandingan_1=c(MAD_1,MAPE_1,MSE_1)
perbandingan_1

MAD_2=mean(abs(produksi_soft_lite.ts-model2$fitted))
MAD_2
MAPE_2=mean(abs(produksi_soft_lite.ts-model2$fitted)/produksi_soft_lite.ts,na.rm=TRUE)
MAPE_2
MSE_2=mean(abs(produksi_soft_lite.ts-model2$fitted)^2)
MSE_2
perbandingan_2=c(MAD_2,MAPE_2,MSE_2)
perbandingan_2

MAD_3=mean(abs(produksi_soft_lite.ts-model3$fitted))
MAD_3
MAPE_3=mean(abs(produksi_soft_lite.ts-model3$fitted)/produksi_soft_lite.ts,na.rm=TRUE)
MAPE_3
MSE_3=mean(abs(produksi_soft_lite.ts-model3$fitted)^2)
MSE_3
perbandingan_3=c(MAD_3,MAPE_3,MSE_3)
perbandingan_3

MAD_4=mean(abs(produksi_soft_lite.ts-model4$fitted))
MAD_4
MAPE_4=mean(abs(produksi_soft_lite.ts-model4$fitted)/produksi_soft_lite.ts,na.rm=TRUE)
MAPE_4
MSE_4=mean(abs(produksi_soft_lite.ts-model4$fitted)^2)
MSE_4
perbandingan_4=c(MAD_4,MAPE_4,MSE_4)
perbandingan_4

MAD_5=mean(abs(produksi_soft_lite.ts-model5$fitted))
MAD_5
MAPE_5=mean(abs(produksi_soft_lite.ts-model5$fitted)/produksi_soft_lite.ts,na.rm=TRUE)
MAPE_5
MSE_5=mean(abs(produksi_soft_lite.ts-model5$fitted)^2)
MSE_5
perbandingan_5=c(MAD_5,MAPE_5,MSE_5)
perbandingan_5

MAD_6=mean(abs(produksi_soft_lite.ts-model6$fitted))
MAD_6
MAPE_6=mean(abs(produksi_soft_lite.ts-model6$fitted)/produksi_soft_lite.ts,na.rm=TRUE)
MAPE_6
MSE_6=mean(abs(produksi_soft_lite.ts-model6$fitted)^2)
MSE_6
perbandingan_6=c(MAD_6,MAPE_6,MSE_6)
perbandingan_6

perbandingantotal=rbind(perbandingan_1,perbandingan_2,perbandingan_3,perbandingan_4,perbandingan_5,perbandingan_6)
perbandingantotal

Box.test(produksi_soft_lite.ts-model5$fitted,type = "Ljung")
ks.test(produksi_soft_lite.ts-model5$fitted,"pnorm",mean(produksi_soft_lite.ts-model5$fitted),sd(produksi_soft_lite.ts-model5$fitted))

Box.test(produksi_soft_lite.ts-model6$fitted,type = "Ljung")
ks.test(produksi_soft_lite.ts-model6$fitted,"pnorm",mean(produksi_soft_lite.ts-model6$fitted),sd(produksi_soft_lite.ts-model6$fitted))

Box.test(produksi_soft_lite.ts-model4$fitted,type = "Ljung")
ks.test(produksi_soft_lite.ts-model4$fitted,"pnorm",mean(produksi_soft_lite.ts-model4$fitted),sd(produksi_soft_lite.ts-model4$fitted))

Box.test(produksi_soft_lite.ts-model2$fitted,type = "Ljung")
ks.test(produksi_soft_lite.ts-model2$fitted,"pnorm",mean(produksi_soft_lite.ts-model2$fitted),sd(produksi_soft_lite.ts-model2$fitted))

Box.test(produksi_soft_lite.ts-model3$fitted,type = "Ljung")
ks.test(produksi_soft_lite.ts-model3$fitted,"pnorm",mean(produksi_soft_lite.ts-model3$fitted),sd(produksi_soft_lite.ts-model3$fitted))

ramalan_2=forecast::forecast(model3,h=6)
ramalan_2$lower=exp(ramalan_2$lower)-1
ramalan_2$upper=exp(ramalan_2$upper)-1
ramalan_2$mean=exp(ramalan_2$mean)-1
ramalan_2
plot(ramalan_2)

one_step_forecasts=model3$fitted

g=ggplot() +
  geom_line(
    aes(
      x = as.numeric(time(produksi_soft_lite.ts)),
      y = as.numeric(one_step_forecasts)
    ),
    col = "black"
  ) +
  geom_line(
    aes(
      x = as.numeric(time(produksi_soft_lite.ts)),
      y = as.numeric(produksi_soft_lite.ts)
    ),
    col = "red"
  )
g <- g + ylab("Produksi Soft Lite") + xlab("Waktu")
g

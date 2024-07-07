#Peramalan_produksi_Lite_Hard
data_produksi<-read_excel("E:/kp/project/final/Final.xlsx")
produksi_hard_lite=data_produksi[6]
produksi_hard_lite
produksi_hard_lite.ts=ts(data_produksi[6],start=c(2019,1),frequency = 12)
produksi_hard_lite.ts
plot(produksi_hard_lite.ts,xlab='Tahun',ylab='Jumlah Produksi (ton)',main='DATA PRODUKSI HARD LITE')
abline(reg=lm(produksi_hard_lite.ts~time(produksi_hard_lite.ts)))
adf.test(produksi_hard_lite.ts)

transformasi_data=log(produksi_hard_lite.ts+1)
transformasi_data
adf.test(transformasi_data)

pacf(transformasi_data)

transformasi_data1=log(transformasi_data+1)
pacf(transformasi_data1)

adf.test(transformasi_data1)

produksi_HL=diff(transformasi_data1,differencing =1 )
produksi_HL
plot(produksi_HL,xlab='Tahun',ylab='Jumlah Produksi (ton)',main='DATA PRODUKSI Hard Lite')
abline(reg=lm(produksi_HL~time(produksi_HL)))
adf.test(produksi_HL)

acf(produksi_HL)
pacf(produksi_HL)

model1=Arima(transformasi_data, order=c(0,1,0))
model1
model1$fitted
model1$fitted=exp(model1$fitted)-1
model1$fitted
accuracy(model1)

model2=Arima(transformasi_data, order=c(0,1,1))
model2
model2$fitted
model2$fitted=exp(model2$fitted)-1
model2$fitted
accuracy(model2)

model3=Arima(transformasi_data, order=c(1,1,0))
model3
model3$fitted
model3$fitted=exp(model3$fitted)-1
model3$fitted
accuracy(model3)

model4=Arima(transformasi_data, order=c(1,1,1))
model4
model4$fitted
model4$fitted=exp(model4$fitted)-1
model4$fitted
accuracy(model4)

MAD_1=mean(abs(produksi_hard_lite.ts-model1$fitted))
MAD_1
MAPE_1=mean(abs(produksi_hard_lite.ts-model1$fitted)/produksi_hard_lite.ts,na.rm=TRUE)
MAPE_1
MSE_1=mean(abs(produksi_hard_lite.ts-model1$fitted)^2)
MSE_1
perbandingan_1=c(MAD_1,MAPE_1,MSE_1)
perbandingan_1

MAD_2=mean(abs(produksi_hard_lite.ts-model2$fitted))
MAD_2
MAPE_2=mean(abs(produksi_hard_lite.ts-model2$fitted)/produksi_hard_lite.ts,na.rm=TRUE)
MAPE_2
MSE_2=mean(abs(produksi_hard_lite.ts-model2$fitted)^2)
MSE_2
perbandingan_2=c(MAD_2,MAPE_2,MSE_2)
perbandingan_2

MAD_3=mean(abs(produksi_hard_lite.ts-model3$fitted))
MAD_3
MAPE_3=mean(abs(produksi_hard_lite.ts-model3$fitted)/produksi_hard_lite.ts,na.rm=TRUE)
MAPE_3
MSE_3=mean(abs(produksi_hard_lite.ts-model3$fitted)^2)
MSE_3
perbandingan_3=c(MAD_3,MAPE_3,MSE_3)
perbandingan_3

MAD_4=mean(abs(produksi_hard_lite.ts-model4$fitted))
MAD_4
MAPE_4=mean(abs(produksi_hard_lite.ts-model4$fitted)/produksi_hard_lite.ts,na.rm=TRUE)
MAPE_4
MSE_4=mean(abs(produksi_hard_lite.ts-model4$fitted)^2)
MSE_4
perbandingan_4=c(MAD_4,MAPE_4,MSE_4)
perbandingan_4

perbandingantotal=rbind(perbandingan_1,perbandingan_2,perbandingan_3,perbandingan_4)
perbandingantotal

Box.test(produksi_hard_lite.ts-model4$fitted,type = "Ljung")
ks.test(produksi_hard_lite.ts-model4$fitted,"pnorm",mean(produksi_hard_lite.ts-model4$fitted),sd(produksi_hard_lite.ts-model4$fitted))

Box.test(produksi_hard_lite.ts-model2$fitted,type = "Ljung")
ks.test(produksi_hard_lite.ts-model2$fitted,"pnorm",mean(produksi_hard_lite.ts-model2$fitted),sd(produksi_hard_lite.ts-model2$fitted))

Box.test(produksi_hard_lite.ts-model1$fitted,type = "Ljung")
ks.test(produksi_hard_lite.ts-model1$fitted,"pnorm",mean(produksi_hard_lite.ts-model1$fitted),sd(produksi_hard_lite.ts-model1$fitted))

Box.test(produksi_hard_lite.ts-model3$fitted,type = "Ljung")
ks.test(produksi_hard_lite.ts-model3$fitted,"pnorm",mean(produksi_hard_lite.ts-model3$fitted),sd(produksi_hard_lite.ts-model3$fitted))

ramalan_5=forecast::forecast(model3,h=6)
ramalan_5
ramalan_5$lower=exp(ramalan_5$lower)-1
ramalan_5$upper=exp(ramalan_5$upper)-1
ramalan_5$mean=exp(ramalan_5$mean)-1
ramalan_5

one_step_forecasts=model3$fitted

g=ggplot() +
  geom_line(
    aes(
      x = as.numeric(time(produksi_hard_lite.ts)),
      y = as.numeric(one_step_forecasts)
    ),
    col = "black"
  ) +
  geom_line(
    aes(
      x = as.numeric(time(produksi_hard_lite.ts)),
      y = as.numeric(produksi_hard_lite.ts)
    ),
    col = "red"
  )
g <- g + ylab("Produksi Lite Hard") + xlab("Waktu")
g

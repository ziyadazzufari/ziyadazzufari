#Peramalan_produksi_hard_medium
data_produksi<-read_excel("D:/kp/project/final/Final.xlsx")
produksi_hard_medium=data_produksi[7]
produksi_hard_medium
produksi_hard_medium.ts=ts(data_produksi[7],start=c(2019,1),frequency = 12)
produksi_hard_medium.ts
plot(produksi_hard_medium.ts,xlab='Tahun',ylab='Jumlah Produksi (ton)',main='DATA PRODUKSI MEDIUM HARD')
abline(reg=lm(produksi_hard_medium.ts~time(produksi_hard_medium.ts)))
adf.test(produksi_hard_medium.ts)
box_cox_HM=sqrt(produksi_hard_medium.ts)

produksi_MH=diff(box_cox_HM,differencing =1 )
produksi_MH
plot(produksi_MH,xlab='Tahun',ylab='Jumlah Produksi (ton)',main='DATA PRODUKSI HARD MEDIUM')
abline(reg=lm(produksi_MH~time(produksi_MH)))
adf.test(produksi_MH)
acf(produksi_MH)
pacf(produksi_MH)

model1=Arima(produksi_hard_medium.ts, order=c(0,1,0),lambda=0)
model1
model1$fitted
accuracy(model1)

model2=Arima(produksi_hard_medium.ts, order=c(0,1,3),lambda=0)
model2
model2$fitted
accuracy(model3)

model3=Arima(produksi_hard_medium.ts, order=c(3,1,0),lambda=0)
model3
model3$fitted
accuracy(model3)

model4=Arima(produksi_hard_medium.ts, order=c(3,1,3),lambda=0)
model4
model4$fitted
accuracy(model4)

MAD_1=mean(abs(produksi_hard_medium.ts-model1$fitted))
MAD_1
MAPE_1=mean(abs(produksi_hard_medium.ts-model1$fitted)/produksi_hard_medium.ts,na.rm=TRUE)
MAPE_1
MSE_1=mean(abs(produksi_hard_medium.ts-model1$fitted)^2)
MSE_1
perbandingan_1=c(MAD_1,MAPE_1,MSE_1)
perbandingan_1

MAD_2=mean(abs(produksi_hard_medium.ts-model2$fitted))
MAD_2
MAPE_2=mean(abs(produksi_hard_medium.ts-model2$fitted)/produksi_hard_medium.ts,na.rm=TRUE)
MAPE_2
MSE_2=mean(abs(produksi_hard_medium.ts-model2$fitted)^2)
MSE_2
perbandingan_2=c(MAD_2,MAPE_2,MSE_2)
perbandingan_2

MAD_3=mean(abs(produksi_hard_medium.ts-model3$fitted))
MAD_3
MAPE_3=mean(abs(produksi_hard_medium.ts-model3$fitted)/produksi_hard_medium.ts,na.rm=TRUE)
MAPE_3
MSE_3=mean(abs(produksi_hard_medium.ts-model3$fitted)^2)
MSE_3
perbandingan_3=c(MAD_3,MAPE_3,MSE_3)
perbandingan_3

MAD_4=mean(abs(produksi_hard_medium.ts-model4$fitted))
MAD_4
MAPE_4=mean(abs(produksi_hard_medium.ts-model4$fitted)/produksi_hard_medium.ts,na.rm=TRUE)
MAPE_4
MSE_4=mean(abs(produksi_hard_medium.ts-model4$fitted)^2)
MSE_4
perbandingan_4=c(MAD_4,MAPE_4,MSE_4)
perbandingan_4

perbandingantotal=rbind(perbandingan_1,perbandingan_2,perbandingan_3,perbandingan_4)
perbandingantotal

Box.test(produksi_hard_medium.ts-model4$fitted,type = "Ljung")
ks.test(produksi_hard_medium.ts-model4$fitted,"pnorm",mean(produksi_hard_medium.ts-model4$fitted),sd(produksi_hard_medium.ts-model4$fitted))
ramalan_6=forecast::forecast(model4,h=6)
ramalan_6
plot(ramalan_6)

one_step_forecasts=fitted(model4)

g=ggplot() +
  geom_line(
    aes(
      x = as.numeric(time(produksi_hard_medium.ts)),
      y = as.numeric(one_step_forecasts)
    ),
    col = "black"
  ) +
  geom_line(
    aes(
      x = as.numeric(time(produksi_hard_medium.ts)),
      y = as.numeric(produksi_hard_medium.ts)
    ),
    col = "red"
  )
g <- g + ylab("Produksi Medium Hard") + xlab("Waktu")
g

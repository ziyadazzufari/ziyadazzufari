#Peramalan_produksi_hard_heavy
data_produksi<-read_excel("D:/kp/project/final/Final.xlsx")
produksi_hard_heavy=data_produksi[5]
produksi_hard_heavy
produksi_hard_heavy.ts=ts(data_produksi[5],start=c(2019,1),frequency = 12)
produksi_hard_heavy.ts
plot(produksi_hard_heavy.ts,xlab='Tahun',ylab='Jumlah Produksi (ton)',main='DATA PRODUKSI HARD HEAVY')
abline(reg=lm(produksi_hard_heavy.ts~time(produksi_hard_heavy.ts)))

adf.test(produksi_hard_heavy.ts)

box_cox_HH=sqrt(produksi_hard_heavy.ts)
adf.test(box_cox_HH)
produksi_HH=diff(box_cox_HH,differencing =1 )
produksi_HH
adf.test(produksi_HH)

acf(produksi_HH,lag.max = 60)
pacf(produksi_HH,lag.max = 60)

produksi_HH=diff(produksi_hard_heavy.ts,differencing=1)
produksi_HH
plot(produksi_HH,xlab='Tahun',ylab='Jumlah Produksi (ton)',main='DATA PRODUKSI HARD HEAVY')
abline(reg=lm(produksi_HH~time(produksi_HH)))
adf.test(produksi_HH)

acf(produksi_HH,lag.max = 60)
pacf(produksi_HH,lag.max = 60)

model1=Arima(produksi_hard_heavy.ts, order=c(0,1,0),lambda=0)
model1
model1$fitted
accuracy(model1)

model2=Arima(produksi_hard_heavy.ts, order=c(0,1,1),lambda=0)
model2
model2$fitted
accuracy(model2)

model3=Arima(produksi_hard_heavy.ts, order=c(1,1,0),lambda=0)
model3
model3$fitted
accuracy(model3)

model4=Arima(produksi_hard_heavy.ts, order=c(1,1,1),lambda=0)
model4
model4$fitted
accuracy(model4)

MAD_1=mean(abs(produksi_hard_heavy.ts-model1$fitted))
MAD_1
MAPE_1=mean(abs(produksi_hard_heavy.ts-model1$fitted)/produksi_hard_heavy.ts,na.rm=TRUE)
MAPE_1
MSE_1=mean(abs(produksi_hard_heavy.ts-model1$fitted)^2)
MSE_1
perbandingan_1=c(MAD_1,MAPE_1,MSE_1)
perbandingan_1

MAD_2=mean(abs(produksi_hard_heavy.ts-model2$fitted))
MAD_2
MAPE_2=mean(abs(produksi_hard_heavy.ts-model2$fitted)/produksi_hard_heavy.ts,na.rm=TRUE)
MAPE_2
MSE_2=mean(abs(produksi_hard_heavy.ts-model2$fitted)^2)
MSE_2
perbandingan_2=c(MAD_2,MAPE_2,MSE_2)
perbandingan_2

MAD_3=mean(abs(produksi_hard_heavy.ts-model3$fitted))
MAD_3
MAPE_3=mean(abs(produksi_hard_heavy.ts-model3$fitted)/produksi_hard_heavy.ts,na.rm=TRUE)
MAPE_3
MSE_3=mean(abs(produksi_hard_heavy.ts-model3$fitted)^2)
MSE_3
perbandingan_3=c(MAD_3,MAPE_3,MSE_3)
perbandingan_3

MAD_4=mean(abs(produksi_hard_heavy.ts-model4$fitted))
MAD_4
MAPE_4=mean(abs(produksi_hard_heavy.ts-model4$fitted)/produksi_hard_heavy.ts,na.rm=TRUE)
MAPE_4
MSE_4=mean(abs(produksi_hard_heavy.ts-model4$fitted)^2)
MSE_4
perbandingan_4=c(MAD_4,MAPE_4,MSE_4)
perbandingan_4

perbandingantotal=rbind(perbandingan_1,perbandingan_2,perbandingan_3,perbandingan_4)
perbandingantotal

Box.test(produksi_hard_heavy.ts-model3$fitted,type = "Ljung")
ks.test(produksi_hard_heavy.ts-model3$fitted,"pnorm",mean(produksi_hard_heavy.ts-model3$fitted),sd(produksi_hard_heavy.ts-model3$fitted))

ramalan_4=forecast::forecast(model3,h=6)
ramalan_4
plot(ramalan_4)

one_step_forecasts=fitted(model3)

g=ggplot() +
  geom_line(
    aes(
      x = as.numeric(time(produksi_hard_heavy.ts)),
      y = as.numeric(one_step_forecasts)
    ),
    col = "black"
  ) +
  geom_line(
    aes(
      x = as.numeric(time(produksi_hard_heavy.ts)),
      y = as.numeric(produksi_hard_heavy.ts)
    ),
    col = "red"
  )
g <- g + ylab("Produksi Heavy Hard") + xlab("Waktu")
g

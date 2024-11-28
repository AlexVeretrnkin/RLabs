if(!require(forecast)) install.packages("forecast")
if(!require(tseries)) install.packages("tseries")
if(!require(healthyR.ts)) install.packages("healthyR.ts")

library(tseries)
library(forecast)
library(healthyR.ts)

#------ 1. Часовий ряд AirPassengers ------

# Завантажуємо вбудований датасет AirPassengers
data("AirPassengers")

# Відображаємо часовий ряд
plot(AirPassengers,
     main="Кількість авіапасажирів (1949-1960)",
     ylab="Кількість пасажирів",
     xlab="Рік")



#------ 2. Згладжування часового ряду ------

# Згладжування з кроком 5
movingAverageWithOrder5 <- ma(AirPassengers, order=5)

# Згладжування з кроком 12
movingAverageWithOrder12 <- ma(AirPassengers, order=12)

# Відображаємо результати
plot(AirPassengers,
     main="Згладжування методом рухомого середнього",
     ylab="Кількість пасажирів",
     xlab="Рік")

lines(movingAverageWithOrder5, col="yellow")
lines(movingAverageWithOrder12, col="blue")

legend("topleft",
       legend=c(
         "Оригінал",
         "Згладжування з кроком 5",
         "Згладжування з кроком 12"),
       col=c(
         "black",
         "yellow",
         "blue"
         ),
       lty=1)



#------ 3. Декомпозиція часового ряду ------

# Декомпозиція ряду
decomposedAirPassengers <- decompose(AirPassengers, type="multiplicative")

# Відображаємо результати декомпозиції:

# Встановлюємо 4 графіки в одному вікні
par(mfrow = c(4, 1))

# Оригінальний ряд
plot(AirPassengers, main = "Оригінальний часовий ряд", ylab = "Кількість пасажирів", xlab = "Рік")

# Тренд
plot(decomposedAirPassengers$trend, main = "Тренд", ylab = "Кількість пасажирів", xlab = "Рік")

# Сезонність
plot(decomposedAirPassengers$seasonal, main = "Сезонна компонента", ylab = "Кількість пасажирів", xlab = "Рік")

# Залишки
plot(decomposedAirPassengers$random, main = "Залишки", ylab = "Кількість пасажирів", xlab = "Рік")

par(mfrow = c(1, 1)) 



#------ 4. Побудова корелограми та часткової корелограми ------

acf(AirPassengers, main="Корелограма (ACF) часового ряду")
pacf(AirPassengers, main="Часткова корелограма (PACF) часового ряду")



#------ 5. Трансформація часового ряду для досягнення стаціонарності ------

# Тест Дікі-Фуллера для перевірки стаціонарності
# Якщо p-value < 0.05, відхиляємо H₀, і ряд вважається стаціонарним.
adf.test(AirPassengers, alternative="stationary", k = frequency(AirPassengers))

# Якщо ряд не стаціонарний, можна застосувати логарифмування та диференціювання
logAirPassengers <- log(AirPassengers)
diffAirPassengers <- diff(AirPassengers)
difLogfAirPassengers <- diff(logAirPassengers)

# Повторний тест
adf.test(diffAirPassengers, alternative="stationary", k = frequency(AirPassengers))


#------ 6. Вибір моделі та прогнозування ------

# Модель Хольта-Вінтерса
hwModel <- HoltWinters(logAirPassengers)

# Прогноз на 24 місяці вперед
hwForecast <- forecast(hwModel, h=24)

# Відображаємо прогноз
plot(hwForecast, main="Прогноз методом Хольта-Вінтерса", ylab="Логарифм кількості пасажирів", xlab="Рік")

# Автоматичний підбір моделі ARIMA
autoArimaModel <- auto.arima(logAirPassengers)

# Параметри моделі
autoArimaModel

# Прогноз на 24 місяці вперед
arimaForecast <- forecast(autoArimaModel, h=24)

# Відображаємо прогноз
plot(arimaForecast, main="Прогноз моделлю ARIMA", ylab="Логарифм кількості пасажирів", xlab="Рік")



#------ 7. Аналіз залишків та оцінка якості прогнозу ------

# Залишки моделі ARIMA
arimaResiduals <- residuals(autoArimaModel)

# Корелограма залишків
acf(arimaResiduals, main="ACF залишків моделі ARIMA")
pacf(arimaResiduals, main="pacf залишків моделі ARIMA")

# Тест Льюнга-Бокса:
# P-value менше 0.05 вказує на наявність автокореляції в залишках,
# що може свідчити про недосконалість моделі.
Box.test(arimaResiduals, type="Ljung-Box", lag = 12)

# Залишки моделі Хольта-Вінтерса
hwResiduals <- residuals(hwModel)

# Корелограма залишків
acf(hwResiduals, main="ACF залишків моделі Хольта-Вінтерса")
pacf(hwResiduals, main="pacf залишків моделі Хольта-Вінтерса")

# Тест Льюнга-Бокса
Box.test(hwResiduals, type="Ljung-Box", lag = 12)

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(hwResiduals)
plotForecastErrors(arimaResiduals)



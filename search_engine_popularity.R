if(!require(forecast)) install.packages("forecast")
if(!require(tseries)) install.packages("tseries")
if(!require(healthyR.ts)) install.packages("healthyR.ts")

library(tseries)
library(forecast)
library(healthyR.ts)
library(readr)
library(ggplot2)
library(xts)


# Зчитування файлу
data <- read_csv("search_engine_data.csv")


#------ 1. Часовий ряд з використання Google як пошукового рушія ------
google_data <- head(data$Google, -10)

google_ts <- ts(google_data, start = c(2009, 1), frequency = 12)''

# Конвертація в датафрейм для ggplot2
df <- data.frame(Date = time(google_ts), Google = as.numeric(google_ts))

ggplot(data.frame(Date = time(google_ts), Google = as.numeric(google_ts)), aes(x = Date, y = Google)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2009, 2024, by = 1)) +  # Деталізація підписів
  labs(
      title = "Використання Google як пошукового рушія", 
      x = "Рік",
      y = "Використання Google (%)"
    ) +
  theme_minimal()


#------ 2. Згладжування часового ряду ------

# Згладжування з кроком 5
moving_avarate_with_order_5 <- ma(google_ts, order = 5)

# Згладжування з кроком 12
moving_avarate_with_order_12 <- ma(google_ts, order = 12)

# Відображаємо результати
plot(google_ts,
     main="Згладжування методом рухомого середнього",
     ylab="Викиристання Googleв",
     xlab="Рік")

lines(moving_avarate_with_order_5, col="yellow")
lines(moving_avarate_with_order_12, col="blue")

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


if(!require(forecast))
  install.packages("forecast")
if (!require(tseries))
  install.packages("tseries")
if (!require(healthyR.ts))
  install.packages("healthyR.ts")

library(tseries)
library(forecast)
library(healthyR.ts)
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork)
library(xts)


# Зчитування файлу
data <- read_csv("search_engine_data.csv")


#------ 1. Часовий ряд з використання Google як пошукового рушія ------
google_data <- head(data$Google, -10)

google_ts <- ts(google_data, start = c(2009, 1), frequency = 12)

plot(ts(data$Google, start = c(2009, 1), frequency = 12))

# Конвертація в датафрейм для ggplot2
df <- data.frame(Date = time(google_ts), Google = as.numeric(google_ts))

ggplot(data.frame(Date = time(google_ts), Google = as.numeric(google_ts)),
       aes(x = Date, y = Google)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2009, 2024, by = 1)) +  # Деталізація підписів
  labs(title = "Використання Google як пошукового рушія", x = "Рік", y = "Використання Google (%)") +
  theme_minimal()


#------ 2. Згладжування часового ряду ------

# Згладжування з кроком 5
moving_avarate_with_order_5 <- ma(google_ts, order = 5)

# Згладжування з кроком 12
moving_avarate_with_order_12 <- ma(google_ts, order = 12)

# Відображаємо результати
ggplot() +
  geom_line(
    data = data.frame(Date = time(google_ts), Google = as.numeric(google_ts)),
    aes(x = Date, y = Google, colour = 'Оригінальний графік', ),
    linewidth = 1
  ) +
  geom_line(
    data = data.frame(
      Date = time(moving_avarate_with_order_5),
      Google = as.numeric(moving_avarate_with_order_5)
    ),
    aes(x = Date, y = Google, colour = 'Рухоме середнє 5'),
    linewidth = 1
  ) +
  geom_line(
    data = data.frame(
      Date = time(moving_avarate_with_order_12),
      Google = as.numeric(moving_avarate_with_order_12)
    ),
    aes(x = Date, y = Google, colour = 'Рухоме середнє 12'),
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "Оригінальний графік" = 'black',
      "Рухоме середнє 5" = "blue",
      "Рухоме середнє 12" = "orange"
    )
  ) +
  scale_x_continuous(breaks = seq(2009, 2024, by = 1)) +  # Деталізація підписів
  labs(
    title = "Використання Google як пошукового рушія",
    x = "Рік",
    y = "Використання Google (%)",
    colour = 'Графіки'
  ) +
  theme_minimal()


#------ 3. Декомпозиція часового ряду ------

decomposed_google_ts <- decompose(google_ts, type = "additive")

plot(decomposed_google_ts)

data <- google_ts

# Використання decompose
decomposed <- decomposed_google_ts

# Перетворення результатів у data.frame для ggplot
decomposed_df <- data.frame(
  Time = time(google_ts),
  Observed = as.numeric(decomposed_google_ts$x),
  Trend = as.numeric(decomposed_google_ts$trend),
  Seasonal = as.numeric(decomposed_google_ts$seasonal),
  Random = as.numeric(decomposed_google_ts$random)
) %>%
  pivot_longer(cols = -Time, names_to = "Component", values_to = "Value")

# Побудова графіка
ggplot(decomposed_df, aes(x = Time, y = Value, color = Component)) +
  geom_line() +
  facet_wrap(~ Component, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Декомпозиція",
    x = "Рік",
    y = "Значення",
    color = "Component"
  )


#------ 4. Побудова корелограми та часткової корелограми ------

acf(google_ts, main="Корелограма (ACF) часового ряду")
pacf(google_ts, main="Часткова корелограма (PACF) часового ряду")


#------ 5. Трансформація часового ряду для досягнення стаціонарності ------

# Тест Дікі-Фуллера для перевірки стаціонарності
# Якщо p-value < 0.05, відхиляємо H₀, і ряд вважається стаціонарним.
ts_frequency <- frequency(google_ts)

adf.test(google_ts, alternative="stationary", k = ts_frequency)

# Якщо ряд не стаціонарний, можна застосувати логарифмування та диференціювання
log_google_ts <- log(google_ts)
diff_google_ts <- diff(google_ts)
diff_log_google_ts <- diff(log_google_ts)

# Повторний тест
adf.test(diff_log_google_ts, alternative="stationary", k = ts_frequency)


#------ 6. Вибір моделі та прогнозування ------

# Модель Хольта-Вінтерса
hw_model <- HoltWinters(diff_log_google_ts)

# Прогноз на 24 місяці вперед
hw_forecast <- forecast(hw_model, h=24)

# Відображаємо прогноз
plot(hw_forecast, main="Прогноз методом Хольта-Вінтерса", ylab="diff_google_ts", xlab="Рік")

# Автоматичний підбір моделі ARIMA
auto_arima_model <- auto.arima(diff_log_google_ts)

# Параметри моделі
auto_arima_model

# Прогноз на 24 місяці вперед
arima_forecast <- forecast(auto_arima_model, h=24)

# Відображаємо прогноз
plot(arima_forecast, main="Прогноз моделлю ARIMA", ylab="diff_google_ts", xlab="Рік")


#------ 7. Аналіз залишків та оцінка якості прогнозу ------

# Залишки моделі ARIMA
arima_residuals <- residuals(auto_arima_model)

# Корелограма залишків
acf(arima_residuals, main="ACF залишків моделі ARIMA")

# Тест Льюнга-Бокса:
# P-value менше 0.05 вказує на наявність автокореляції в залишках,
# що може свідчити про недосконалість моделі.
Box.test(arima_residuals, type="Ljung-Box", lag = 12)

# Залишки моделі Хольта-Вінтерса
hw_residuals <- residuals(hw_model)

# Корелограма залишків
acf(hw_residuals, main="ACF залишків моделі Хольта-Вінтерса")

# Тест Льюнга-Бокса
Box.test(hw_residuals, type="Ljung-Box", lag = 12)



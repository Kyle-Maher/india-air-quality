
library(fpp3)
library(forecast)  # Box-Cox Transformation
library(fable.prophet)  # Prophet Model

df <- readRDS("data/loaded/delhi_filter.rds") %>%
  select(PM2.5)
# df_daily <- readRDS("data/loaded/delhi_daily.rds") %>%
#   select(PM2.5)

df_monthly <- df %>%
  index_by(month = yearmonth(datetime)) %>%
  summarise(PM2.5 = median(PM2.5, na.rm = TRUE))

df_monthly %>%
  autoplot(PM2.5)

df_weekly <- df %>%
  index_by(week = yearweek(datetime)) %>%
  summarise(PM2.5 = median(PM2.5, na.rm = TRUE))

df_weekly %>%
  autoplot(PM2.5)

df_daily <- df %>%
  index_by(date = as.Date(datetime)) %>%
  summarise(PM2.5 = median(PM2.5, na.rm = TRUE))

df_daily %>%
  autoplot(PM2.5)

# Variance Stablization
################################################################################

# Box-Cox Lambda Values
lambda_hourly_PM2.5 <- BoxCox.lambda(df$PM2.5)
lambda_daily_PM2.5 <- BoxCox.lambda(df_daily$PM2.5)
lambda_weekly_PM2.5 <- BoxCox.lambda(df_weekly$PM2.5)
lambda_monthly_PM2.5 <- BoxCox.lambda(df_monthly$PM2.5)


################################################################################
# Monthly
################################################################################

# ARIMA
################################################################################
# ARIMA(1,0,0)(0,1,1)[12]
fit <- df_monthly %>%
  model(ARIMA(box_cox(PM2.5, lambda_monthly_PM2.5)))

fc <- forecast(fit, h = 30)
autoplot(fc, df_monthly)

# ARIMA with Fourier
################################################################################
# ARIMA(0,1,1)(1,0,0)[12]
fit <- df_monthly %>%
  model(ARIMA(box_cox(PM2.5, lambda_monthly_PM2.5) ~ fourier(K = 6)))

fc <- forecast(fit, h = 30)
autoplot(fc, df_monthly)

# Network
################################################################################
# NNAR(1,1,2)[12]
fit <- df_monthly %>%
  model(NNETAR(box_cox(PM2.5, lambda_monthly_PM2.5)))

fc <- readRDS("models/monthly_fc_NNAR(1,1,2)[12]")
# fc <- forecast(fit, h = 30)

autoplot(fc, df_monthly)


# STL NAIVE
################################################################################
fit <- df_monthly %>%
  model(
    decomposition_model(
      STL(box_cox(PM2.5, lambda_monthly_PM2.5) ~ trend(window = 72), robust = TRUE),
      NAIVE(season_adjust)
    )
  )

fc <- forecast(fit, h = 30)
autoplot(fc, df_monthly)




################################################################################
# Weekly
################################################################################

# ARIMA
################################################################################
# ARIMA(4,0,0) w/ mean

fit <- df_weekly %>%
  model(ARIMA(box_cox(PM2.5, lambda_weekly_PM2.5)))

fc <- forecast(fit, h = 104)
autoplot(fc, df_weekly)

# ARIMA with Fourier
################################################################################
# LM w/ ARIMA(0,0,0)(1,0,0)[52]
fit <- df_weekly %>%
  model(ARIMA(box_cox(PM2.5, lambda_weekly_PM2.5) ~ fourier(K = 6)))

# K_candidates <- 1:22
# results <- data.frame(K = numeric(), AICc = numeric())

# for (K in K_candidates) {
#   fit <- df_weekly %>%
#     model(ARIMA(box_cox(PM2.5, lambda_weekly_PM2.5) ~ fourier(K = K)))
#   data <- data.frame(K = K, AICc = glance(fit)$AICc)
#   results <- bind_rows(results, data)
# }
# results %>%
#   filter(AICc == min(results$AICc))

# K = 7 was best with an AICc of 230.221
readRDS("models/weekly_fit_ARIMA(4,0,0)(1,0,0)[52].rds")
# fit <- df_weekly %>%
#   model(ARIMA(box_cox(PM2.5, lambda_weekly_PM2.5) ~ fourier(K = 7)))


fc <- forecast(fit, h = 104)
autoplot(fc, df_weekly)

df_weekly %>%
  autoplot(PM2.5) +
  autolayer(fitted(fit), .fitted, color = "red")

# Interpolate
########################################

df_weekly_filled <- interpolate(fit, df_weekly)

df_weekly_filled %>%
  autoplot(PM2.5)

# STL Decomposition
########################################
dc <- df_weekly_filled %>%
  model(STL(PM2.5 ~ trend(window = 104) + season(window = "periodic")))

components(dc) %>%
  autoplot()

# Network
################################################################################
# NNAR(13,1,8)[52]
fit <- df_weekly %>%
  model(NNETAR(box_cox(PM2.5, lambda_weekly_PM2.5)))

fc <- readRDS("models/weekly_fc_NNAR(13,1,8)[52]")
# fc <- forecast(fit, h = 104)

autoplot(fc, df_weekly)




################################################################################
# Hourly
################################################################################

# Dummy Variables for Diwali and Burning Season
################################################################################
diwali_dates <- c(
  seq(as.Date("2017-10-16"), as.Date("2017-10-20"), by = "day"),
  seq(as.Date("2018-11-05"), as.Date("2018-11-09"), by = "day"),
  seq(as.Date("2019-10-25"), as.Date("2019-10-29"), by = "day"),
  seq(as.Date("2020-11-12"), as.Date("2020-11-16"), by = "day"),
  seq(as.Date("2021-11-02"), as.Date("2021-11-06"), by = "day"),
  seq(as.Date("2022-11-22"), as.Date("2022-11-26"), by = "day"),
  seq(as.Date("2023-11-10"), as.Date("2023-11-14"), by = "day"),
  seq(as.Date("2024-10-30"), as.Date("2024-11-03"), by = "day"),
  seq(as.Date("2025-10-18"), as.Date("2025-10-23"), by = "day")
)

burning_dates <- c(
  seq(as.Date("2017-9-15"), as.Date("2017-11-30"), by = "day"),
  seq(as.Date("2018-9-15"), as.Date("2018-11-30"), by = "day"),
  seq(as.Date("2019-9-15"), as.Date("2019-11-30"), by = "day"),
  seq(as.Date("2020-9-15"), as.Date("2020-11-30"), by = "day"),
  seq(as.Date("2021-9-15"), as.Date("2021-11-30"), by = "day"),
  seq(as.Date("2022-9-15"), as.Date("2022-11-30"), by = "day"),
  seq(as.Date("2023-9-15"), as.Date("2023-11-30"), by = "day"),
  seq(as.Date("2024-9-15"), as.Date("2024-11-30"), by = "day"),
  seq(as.Date("2025-9-15"), as.Date("2025-11-30"), by = "day")
)

df <- df %>%
  mutate(
    is_diwali = as.integer(as.Date(datetime) %in% diwali_dates),
    is_burning_season = as.integer(as.Date(datetime) %in% burning_dates)
  )

df %>%
  autoplot(PM2.5) +
  autolayer(filter(df, as.Date(datetime) %in% diwali_dates), color = "red")

df %>%
  autoplot(PM2.5) +
  autolayer(filter(df, as.Date(datetime) %in% burning_dates), color = "red")



# Prophet
################################################################################
fit <- readRDS("models/daily_fit_prophet.rds")
# fit <- df %>%
#   model(
#     prophet(PM2.5 ~ is_diwali + is_burning_season +
#         season(period = "day", order = 10) +
#         season(period = "week", order = 5) +
#         season(period = "month", order = 3) +
#         season(period = "year", order = 3)
#     )
#   )

fit2 <- readRDS("models/daily_fit_prophet_log.rds")
# fit2 <- df %>%
#   model(
#     prophet(log(PM2.5) ~ is_diwali + is_burning_season +
#         season(period = "day", order = 10) +
#         season(period = "week", order = 5) +
#         season(period = "month", order = 3) +
#         season(period = "year", order = 3)
#     )
#   )

fit3 <- readRDS("models/daily_fit_prophet_box-cox.rds")
# fit3 <- df %>%
#   model(
#     prophet(box_cox(PM2.5, lambda_hourly_PM2.5) ~
#         is_diwali +
#         is_burning_season +
#         season(period = "day", order = 10) +
#         season(period = "week", order = 5) +
#         season(period = "month", order = 3) +
#         season(period = "year", order = 3)
#     )
#   )

new_data <- df %>%
  new_data(n = 24 * 30 * 12) %>%
  mutate(
    is_diwali = as.integer(as.Date(datetime) %in% diwali_dates),
    is_burning_season = as.integer(as.Date(datetime) %in% burning_dates)
  )

fc <- forecast(fit, new_data = new_data)
autoplot(fc, filter(df, as.Date(datetime) > as.Date("2023-01-01")))

fc2 <- forecast(fit2, new_data = new_data)
autoplot(fc2, filter(df, as.Date(datetime) > as.Date("2023-01-01")))

fc3 <- forecast(fit3, new_data = new_data)
autoplot(fc3, filter(df, as.Date(datetime) > as.Date("2022-06-01")))

autoplot(df, PM2.5)




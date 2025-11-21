
library(fpp3)
library(forecast)

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



# Dummy Variable for Diwali
################################################################################
diwali_dates <- c(
  seq(as.Date("2017-10-16"), as.Date("2017-10-20"), by = "day"),
  seq(as.Date("2018-11-05"), as.Date("2018-11-09"), by = "day"),
  seq(as.Date("2019-10-25"), as.Date("2019-10-29"), by = "day"),
  seq(as.Date("2020-11-12"), as.Date("2020-11-16"), by = "day"),
  seq(as.Date("2021-11-02"), as.Date("2021-11-06"), by = "day"),
  seq(as.Date("2022-11-22"), as.Date("2022-11-26"), by = "day"),
  seq(as.Date("2023-11-10"), as.Date("2023-11-14"), by = "day")
)

df <- df %>%
  mutate(
    is_diwali = as.integer(as.Date(datetime) %in% diwali_dates)
  )

df %>% View()


# Prophet
################################################################################


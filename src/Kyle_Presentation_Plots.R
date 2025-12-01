

library(fpp3) # Time Series Plots and Forecasting
library(corrplot) # Correlation Plot
library(forecast) # BoxCox Transformation
library(scales) # Log Scale Tick Labels
library(fable.prophet) # Prophet Model
set.seed(280)  # Reproducibility


data <- readRDS("../data/loaded/delhi_filter.rds")

df_monthly <- data %>%
  select(PM2.5) %>%
  index_by(month = yearmonth(datetime)) %>%
  summarise(PM2.5 = median(PM2.5, na.rm = TRUE))

df_weekly <- data %>%
  select(PM2.5) %>%
  index_by(week = yearweek(datetime)) %>%
  summarise(PM2.5 = median(PM2.5, na.rm = TRUE))

df_daily <- data %>%
  select(PM2.5) %>%
  index_by(date = as.Date(datetime)) %>%
  summarise(PM2.5 = median(PM2.5, na.rm = TRUE))

# Hourly
df <- data %>%
  select(PM2.5)




df_daily %>%
  autoplot(PM2.5) +
  geom_hline(mapping = aes(yintercept = 5, color = "Yearly Avg."), linetype = "dashed") +
  geom_hline(mapping = aes(yintercept = 15, color = "Daily Avg."), linetype = "dashed")+
  labs(x = "Date", title = "Comparison of Air Quality to WHO Recommendations")+
  scale_color_manual(name = "WHO Recommendations",
                     values = c("Yearly Avg." = "blue", "Daily Avg." = "red"))+
  theme(legend.position = "bottom")



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
  autolayer(filter(df, as.Date(datetime) %in% diwali_dates), color = "red")+
  labs(x = "Date", title = "Highlighted Diwali Season")

df %>%
  autoplot(PM2.5) +
  autolayer(filter(df, as.Date(datetime) %in% burning_dates), color = "red")+
  labs(x = "Date", title = "Highlighted Burning Season")



fit <- df %>%
  model(ARIMA(box_cox(PM2.5, BoxCox.lambda(df$PM2.5))))
fc <- forecast(fit, h = 24*7*4*12*1)
autoplot(fc, df) +
  labs(x = "Date-time",
       title = "Arima Model 1 Year Hourly Forecast")

fit


library(fpp3)
library(forecast)

df <- readRDS("data/loaded/delhi_filter.rds")
daily_df <- readRDS("data/loaded/delhi_daily.rds")

df$logPM25 <- log(df$PM2.5) %>%
    difference(24)

# Box-Cox Transformation
BoxCox.lambda(df$PM2.5)
df$BCPM25 <- box_cox(df$PM2.5, BoxCox.lambda(df$PM2.5)) %>%
    difference(24)

df %>% autoplot(logPM25)
df %>% autoplot(BCPM25)

# Try dynamic harmonic first
delhi_dynamo <- df |>
    model(
        season = ARIMA(
            log(PM2.5) ~ PDQ(0,1,0, period = 24),
            ),
        fourier24 = ARIMA(
            log(PM2.5) ~ PDQ(0,0,0) + fourier(K=12)
            )
        )
report(delhi_dynamo$fourier24[[1]])

dynamo_fc <- delhi_dynamo$fourier24[[1]] |>
    forecast(h = "1 month")

dynamo_fc |> autoplot(df[45000:48936,])

# STL decomp
# Doesn't accept missing values
df_stl <- df |>
    mutate(PM2.5 = case_when(is.nan(PM2.5) ~ NA,
                             TRUE ~ PM2.5),
           PM2.5 = case_when(is.na(PM2.5) ~ mean(PM2.5, na.rm = TRUE),
                             TRUE ~ PM2.5))


delhi_decomp <- decomposition_model(
    STL(PM2.5 ~ season(window = Inf)),
    ETS(season_adjust ~ season("N"))
)

delhi_stl <- df_stl |>
    model(
        stl = delhi_decomp
    )
report(delhi_stl)

stl_fc <- delhi_stl |>
    forecast(h = "1 year")

stl_fc |> autoplot(df_stl,level = 95)

# Daily Modeling
#=================================================================

daily_df |> ACF(PM2.5) |>
    autoplot()

daily_df |> autoplot(PM2.5)
daily_df |> autoplot(log(PM2.5) |> difference(1))

lambda <- BoxCox.lambda(daily_df$PM2.5)
daily_df |> autoplot(box_cox(PM2.5, lambda) |> difference(1))


daily_dynamo <- daily_df |>
    model(
        delhi_dynamo = ARIMA(box_cox(PM2.5, BoxCox.lambda(PM2.5)) ~ pdq(d = 1) + PDQ(0,0,0) + fourier(K = 2))
    )
report(daily_dynamo)
accuracy(daily_dynamo)

daily_fc <- daily_dynamo |>
    forecast(h = "6 month")

daily_aug <- daily_dynamo |>
    augment()

daily_fc |> autoplot(daily_df)+
    geom_line(data = daily_aug,
              mapping = aes(x = date, y = .fitted), color = "red")

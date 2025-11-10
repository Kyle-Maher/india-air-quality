
library(fpp3)

df <- readRDS("C:/Users/aidan/OneDrive/Documents/GitHub/india-air-quality/data/loaded/delhi.rds")

delhi2017 <- df %>%
    filter(file_name == "DL008.csv") %>%
    filter(year(`From Date`) > 2017) %>%
    as_tsibble(index = `From Date`)

delhi2017 |> autoplot(`NO2 (ug/m3)`)

# Filter out columns that are too empty
coldata <- data.frame(colname = colnames(delhi2017))
coldata <- coldata |>
    mutate(na_count = sapply(delhi2017, function(x) sum(is.na(x))),
           na_ratio = na_count / nrow(delhi2017),
           col_keep = na_ratio <= 0.15)

# Keep columns that have 10% or less missing
delhi2017 <- delhi2017[,coldata$col_keep]

delhi2017 |> autoplot(`CO (mg/m3)`)

delhi2017 |> autoplot(`PM2.5 (ug/m3)`)

delhi2017 |> autoplot(`Toluene (ug/m3)`)

delhi2017 |> autoplot(`PM10 (ug/m3)`)

delhi2017 |> autoplot(`NOx (ppb)`)

delhi2017 |> autoplot(`Benzene (ug/m3)`)

gg_season(delhi2017,`PM10 (ug/m3)`,
          period = "1 year")

# Several holidays here. Diwali (Festival of Lights) in late October/early November,
# and Guru Nanak Jayanti (Sikh festival) on November 5th
gg_season(delhi2017,`PM2.5 (ug/m3)`,
          period = "1 year")+
    geom_vline(xintercept = ymd("1973-11-05"),
               linewidth = 1.25,
               linetype = 2)

gg_season(delhi2017,`PM2.5 (ug/m3)`,
          period = "1 month")

# Appears to be daily seasonality present. Very general, and not super strong
# when considering individual years
gg_season(delhi2017,`PM2.5 (ug/m3)`,
          period = "1 week")
gg_season(delhi2017,`PM2.5 (ug/m3)`,
          period = "1 day")

# Correlation between different pollutants, only using complete observations
# Could give insight on which pollutants arise at the same time or opposite times
# Correlated pollutants could have the same cause, like traffic emissions
library(corrplot)
M <- cor(x = delhi2017[,c(3:9,11:13)],
         y = delhi2017[,c(3:9,11:13)],
         use = "na.or.complete")
corrplot(M)

delhi_long <- delhi2017 |>
    pivot_longer(c(3:9,11:13))


# plot of each of the time series on top of each other
# helps visualize common patterns between each pollutant
# RECOMMENDED: Make this plot larger in order to see these patterns
delhi_long |> autoplot(value)+
    facet_grid(rows = vars(name), scales = "free_y")

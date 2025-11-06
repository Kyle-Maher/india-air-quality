# We're going to need a map of Delhi for presentation
#
##################################################
# Filter for the city Delhi
# Remove columns with limited data
# 
##################################################

# Parameters Ranked By General Harmfullness:
# CO
# PM2.5
# Benzene, Toluene, Xylene
# PM10
# NOx, NO2, NO
# Ozone


library(fpp3)
data <- readRDS("data/loaded/delhi.rds")

delhi_city <- data %>%
  filter(file_name == "DL008.csv")

observation_counts <- delhi_city %>%
  summarise(across(everything(), ~sum(!is.na(.)))) %>%
  pivot_longer(cols = everything()) %>%
  arrange(desc(value)) %>%
  mutate(percent = (value / nrow(delhi_city)) * 100)

rare_parameters <- observation_counts %>%
  filter(percent < 2) %>%
  pull(name)

df <- delhi_city %>%
  select(-all_of(rare_parameters), -file_name, -`From Date`) %>%
  rename(
    "datetime" = "To Date",
    "PM2.5" = "PM2.5 (ug/m3)",
    "PM10" = "PM10 (ug/m3)",
    "NO" = "NO (ug/m3)",
    "NO2" = "NO2 (ug/m3)",
    "NOx" = "NOx (ppb)",
    "CO" = "CO (mg/m3)",
    "Benzene" = "Benzene (ug/m3)",
    "Toluene" = "Toluene (ug/m3)",
    "Xylene" = "Xylene (ug/m3)",
    "Ozone" = "Ozone (ppb)"
  ) %>%
  as_tsibble(index = datetime) %>%
  filter(datetime >= as_datetime("2017-08-31 01:00:00"))

df %>%
  as_tibble() %>%
  summarise(across(everything(), ~sum(!is.na(.)))) %>%
  pivot_longer(cols = everything()) %>%
  arrange(desc(value)) %>%
  mutate(percent = (value / nrow(df)) * 100)

# Seasonality
df %>%
  autoplot(PM2.5)

# Seasonality
df %>%
  autoplot(PM10)

df %>%
  autoplot(NO)

df %>%
  autoplot(NO2)

df %>%
  autoplot(NOx)

df %>%
  autoplot(CO)

# Low with Spikes
df %>%
  autoplot(Benzene)

# Low with Spikes
df %>%
  autoplot(Toluene)

# Low with Spikes
df %>%
  autoplot(Xylene)

# Possibliy Seasonal
df %>%
  autoplot(Ozone)


#############################################

df %>%
  filter(year(datetime) == 2022) %>%
  autoplot(PM2.5)

df %>%
  filter(year(datetime) == 2022, month(datetime) == 7) %>%
  autoplot(PM2.5)

df %>%
  filter(date(datetime) == as.Date("2022-07-15")) %>%
  autoplot(PM2.5)



df %>%
  gg_season(PM2.5, period = "year")

df %>%
  mutate(year = year(datetime)) %>%
  gg_season(PM2.5, period = "day") +
  facet_wrap(~year) +
  theme(
    legend.position = "none"
  )

df %>%
  index_by(date = as.Date(datetime)) %>%
  summarise(PM2.5 = mean(PM2.5, na.rm = TRUE)) %>%
  autoplot(PM2.5) +
  geom_hline(yintercept = 35, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 12, color = "blue", linetype = "dashed")


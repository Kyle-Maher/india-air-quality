##################################################
# Read RDS
# Exploration Time
##################################################


df <- readRDS("data/loaded/delhi.rds")

df %>%
  filter(file_name == "DL008.csv") %>%
  filter(year(`From Date`) > 2017) %>%
  as_tsibble(index = `From Date`) %>%
  ggplot(aes(x = `From Date`)) +
  geom_point(aes(y = `PM2.5 (ug/m3)`))

df %>% head() %>% View()
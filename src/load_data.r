##################################################
# Read 40 csvs: DL001.csv through DL040.csv
# Join and store in RDS
##################################################

library(tidyverse)

files <- paste0("DL0", sprintf("%02d", 1:40), ".csv")
joined <- data.frame()

for (file in files) {
  df <- read_csv(paste0("data/raw/", file)) %>%
    mutate(file_name = file)
  joined <- bind_rows(joined, df)
}

saveRDS(as_tibble(joined), "data/loaded/delhi.rds")

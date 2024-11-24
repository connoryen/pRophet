# Preliminary ------------------------------------------------------------------

library(tidyverse)
library(arrow)

# Load in data & visualize -----------------------------------------------------

df <- read_parquet("../data/pharmaceuticals_2022and2023.parquet")

unique(df$Symbol)

df %>%
  # Remove non-pharmaceutical symbols
  filter(Symbol %in% c("NVO", "GILD")) %>%
  mutate(Log_Open = log(Open)) %>%
  ggplot(aes(x = Date, y = Log_Open, col = Symbol)) + 
  geom_line() + 
  theme_minimal()

# Test check_correlation function ----------------------------------------------

cor_results <- df %>%
  filter(Symbol %in% c("NVO", "GILD")) %>%
  analyze_ts_correlation(date_col = "Date", value_col = "Open", group_col = "Symbol")


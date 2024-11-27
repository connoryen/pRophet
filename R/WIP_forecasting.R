# Preliminary ------------------------------------------------------------------

library(tidyverse)
library(arrow)
set.seed(1212472)

df <- read_parquet("../data/pharmaceuticals_2022and2023.parquet")

# Simple Data ------------------------------------------------------------------

df_sample <- df %>% 
  filter(Symbol == "GILD") %>%
  select(Date, Open) %>%
  rename("GILD" = "Open")


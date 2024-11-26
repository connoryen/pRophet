# Preliminary ------------------------------------------------------------------

library(tidyverse)
library(arrow)
set.seed(1212472)

df <- read_parquet("../data/pharmaceuticals_2022and2023.parquet")

# Build test data --------------------------------------------------------------

RW <- function(N, x0, mu, variance) {
  z<-cumsum(rnorm(n=N, mean=0, sd=sqrt(variance)))
  t<-1:N
  x<-x0+t*mu+z
  return(x)
}

df_test <- df %>%
  group_by(Date) %>%
  summarise() %>%
  tibble::rowid_to_column("t") %>% 
  mutate(X1 = RW(n(), 20, 0.01, 0.1)) %>%
  mutate(X2 = 8 + 0.8*X1 + rnorm(n(), 0, 0.15),  # strict correlation
         # less strong correlation
         X3 = 8 + 0.8*X1 + RW(n(), 0, 0.01, 0.1) + rnorm(n(), 0, 0.05),
         # Lagging correlation (X1 is a leading indicator for X4)
         X4 = 2 + 0.9*lag(X3, n=10) + rnorm(n(), 0, 0.1)) %>%
  filter(!is.na(X4)) %>%
  pivot_longer(cols = c(X1, X2, X3, X4), names_to = "Symbol", values_to = "Open")

df_test %>%
  ggplot(aes(x = Date, y = Open, col = Symbol)) + 
  geom_line() + 
  theme_minimal()

# Test check_correlation function ----------------------------------------------

cor_results_test_X1X2 <- df_test %>%
  filter(Symbol %in% c("X1", "X2")) %>%
  analyze_ts_correlation(date_col = "Date", value_col = "Open", group_col = "Symbol")

cor_results_test_X1X3 <- df_test %>%
  filter(Symbol %in% c("X1", "X3")) %>%
  analyze_ts_correlation(date_col = "Date", value_col = "Open", group_col = "Symbol")

cor_results_test_X1X4 <- df_test %>%
  filter(Symbol %in% c("X1", "X4")) %>%
  analyze_ts_correlation(date_col = "Date", value_col = "Open", group_col = "Symbol")

cor_results_test_X1X2
cor_results_test_X1X3
cor_results_test_X1X4

cor_results_test_X1X2$plot
cor_results_test_X1X3$plot
cor_results_test_X1X4$plot

cor_results_test_X1X2$statistics
cor_results_test_X1X3$statistics
cor_results_test_X1X4$statistics


# Load in data & visualize -----------------------------------------------------

unique(df$Symbol)

# Plot real stock data
df %>%
  filter(Symbol %in% c("NVO", "GILD")) %>%
  ggplot(aes(x = Date, y = Open, col = Symbol)) + 
  geom_line() + 
  theme_minimal()







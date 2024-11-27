# Preliminary ------------------------------------------------------------------

library(tidyverse)
library(arrow)
library(astsa)
library(forecast)
set.seed(1212472)

df0 <- read_parquet("../data/pharmaceuticals_2022and2023.parquet")

# Simple Data ------------------------------------------------------------------

df <- df0 %>% 
  filter(Symbol == "GILD") %>%
  select(Date, Open) %>%
  rename("Value" = "Open") %>% 
  mutate(Group = ifelse(Date <= as.Date("2023-09-30"), "Train", "Test"))

# Plot
df %>%
  ggplot(aes(x = Date, y = Value, col = Group)) + 
  geom_line() + 
  theme_minimal()

df_train <- filter(df, Group == "Train")
df_test <- filter(df, Group == "Test")

# ARIMA(0,1,0) Model -----------------------------------------------------------

train_ts <- ts(df_train$Value, frequency=252)  # 252 trading days per year

model <- Arima(train_ts, order=c(0,1,0))
forecasts <- forecast(model, h=nrow(df_test))

results <- data.frame(
  Date = df$Date,
  Actual = df$Value,
  Predicted = c(df_train$Value, forecasts$mean),
  Type = df$Group
)

results %>%
  select(-Type) %>%
  pivot_longer(cols = c(Actual, Predicted), names_to = "Type", values_to = "Value") %>%
  ggplot(aes(x = Date, y = Value, col = Type)) + 
  geom_line() + 
  theme_minimal()

# Simple LSTM ------------------------------------------------------------------



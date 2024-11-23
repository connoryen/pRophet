library(tidyverse)
library(quantmod)
library(data.table)
library(arrow)

# Initialize data table
# ^GSPC is the S&P500
df <- getSymbols("^GSPC", from = as.Date("2022-01-01"), to = as.Date("2023-12-31"),
                 src = "yahoo", auto.assign = FALSE)
df <- data.table(df, keep.rownames = TRUE) %>%
  rename_all(~c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")) %>%
  mutate(Symbol = "GSPC")

# Other tickers to pull data for 
symbols <- c("^OEX",     # S&P100
             "^DJI",     # Dow Jones Industrial Average
             "XPH",      # SPDR S&P Pharmaceuticals ETF
             "XLK",      # Technology Select Sector SPDR Fund 
             "LLY",      # ELi Lily
             "NVO",      # Novo Nordisk
             "JNJ",      # Johnson & Johnson
             "MRK",      # Merck
             "ABBV",     # Abbvie
             "ROG.SW",   # Roche
             "AZN",      # AstraZeneca 
             "NVS",      # Novartis
             "PFE",      # Pfizer
             "AMGN",     # Amgen
             "SNY",      # Sanofi
             "BMY",      # Bristol-Myers Squibb
             "GILD"      # Gilead 
             )

for (t in symbols) {
  df_symbol <- getSymbols(t, from = as.Date("2022-01-01"), to = as.Date("2023-12-31"),
                          src = "yahoo", auto.assign = FALSE)
  df_symbol <- data.table(df_symbol, keep.rownames = TRUE) %>%
    rename_all(~c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")) %>%
    mutate(Symbol = gsub("\\^", "", t))
  
  df <- rbind(df, df_symbol)
}

df <- df %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)

# write data
write_parquet(df, "pharmaceuticals_2022and2023.parquet")

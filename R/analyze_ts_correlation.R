library(dplyr)
library(ggplot2)
library(tidyr)
library(stats)
library(forecast)
library(tseries)
library(gridExtra)
library(lmtest)  # Added for Granger test
library(zoo)     # Added for irregular time series handling

analyze_ts_correlation <- function(df, 
                                   date_col = "Date", 
                                   value_col = "Value", 
                                   group_col = "Col",
                                   max_lag = 24) {
  
  # Input validation
  required_cols <- c(date_col, value_col, group_col)
  if (!all(required_cols %in% names(df))) {
    stop("Missing required columns in dataframe")
  }
  
  # Ensure date column is in proper format
  df[[date_col]] <- as.Date(df[[date_col]])
  
  # Split data by groups
  unique_groups <- unique(df[[group_col]])
  if (length(unique_groups) != 2) {
    stop("Exactly two groups are required for correlation analysis")
  }
  
  # Create zoo objects for irregular time series
  zoo_list <- list()
  for (group in unique_groups) {
    group_data <- df[df[[group_col]] == group, ]
    group_data <- group_data[order(group_data[[date_col]]), ]
    
    # Convert to zoo object for irregular time series
    zoo_list[[group]] <- zoo(group_data[[value_col]], 
                             order.by = group_data[[date_col]])
  }
  
  # Align the time series on matching dates
  aligned_series <- merge(zoo_list[[1]], zoo_list[[2]])
  # Remove any NA values that resulted from the merge
  aligned_series <- na.omit(aligned_series)
  
  # Convert to numeric vectors for analysis
  ts1 <- as.numeric(aligned_series[,1])
  ts2 <- as.numeric(aligned_series[,2])
  
  # Calculate correlations and create visualizations
  results <- list()
  
  # 1. Time series plot with actual dates
  plot_df <- data.frame(
    Date = index(aligned_series),
    Series1 = coredata(aligned_series)[,1],
    Series2 = coredata(aligned_series)[,2]
  ) %>%
    tidyr::pivot_longer(cols = c(Series1, Series2),
                        names_to = "Series",
                        values_to = "Value")
  
  ts_plot <- ggplot(plot_df, aes(x = Date, y = Value, color = Series)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Time Series Comparison",
         x = "Date",
         y = "Value")
  
  # 2. Cross-correlation analysis
  ccf_result <- ccf(ts1, ts2, 
                    lag.max = max_lag, 
                    plot = FALSE)
  
  ccf_df <- data.frame(
    lag = ccf_result$lag,
    acf = ccf_result$acf
  )
  
  ccf_plot <- ggplot(ccf_df, aes(x = lag, y = acf)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_hline(yintercept = c(1.96/sqrt(length(ts1)), 
                              -1.96/sqrt(length(ts1))),
               linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = "Cross-Correlation Function",
         x = "Lag",
         y = "CCF")
  
  # 3. Rolling correlation analysis
  roll_cor <- rollapply(aligned_series, 
                        width = min(30, nrow(aligned_series)), 
                        FUN = function(x) cor(x[,1], x[,2]),
                        by.column = FALSE,
                        align = "right")
  
  roll_cor_df <- data.frame(
    Date = index(roll_cor),
    Correlation = coredata(roll_cor)
  )
  
  rolling_cor_plot <- ggplot(roll_cor_df, aes(x = Date, y = Correlation)) +
    geom_line(color = "steelblue") +
    theme_minimal() +
    labs(title = "Rolling Correlation (30-period window)",
         x = "Date",
         y = "Correlation")
  
  # 4. Scatter plot with regression line
  scatter_df <- data.frame(
    Series1 = ts1,
    Series2 = ts2
  )
  
  scatter_plot <- ggplot(scatter_df, aes(x = Series1, y = Series2)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "red") +
    theme_minimal() +
    labs(title = "Series Correlation Scatter Plot",
         x = unique_groups[1],
         y = unique_groups[2])
  
  # Calculate summary statistics
  contemporaneous_cor <- cor(ts1, ts2)
  max_ccf <- max(abs(ccf_result$acf))
  max_ccf_lag <- ccf_result$lag[which.max(abs(ccf_result$acf))]
  
  # Calculate returns for financial analysis
  returns1 <- diff(log(ts1))
  returns2 <- diff(log(ts2))
  returns_cor <- cor(returns1, returns2)
  
  # Combine results
  results$plots <- list(
    time_series = ts_plot,
    ccf = ccf_plot,
    rolling_correlation = rolling_cor_plot,
    scatter = scatter_plot
  )
  
  results$statistics <- list(
    contemporaneous_correlation = contemporaneous_cor,
    returns_correlation = returns_cor,
    max_cross_correlation = max_ccf,
    max_cross_correlation_lag = max_ccf_lag,
    rolling_correlation_summary = summary(roll_cor)
  )
  
  # Additional diagnostics
  results$diagnostics <- list(
    granger_test = tryCatch({
      # Create a data frame for Granger test
      granger_df <- data.frame(y = ts1, x = ts2)
      grangertest(y ~ x, order = min(5, length(ts1) - 1), data = granger_df)
    }, error = function(e) {
      message("Granger test could not be performed: ", e$message)
      return(NULL)
    }),
    stationarity_tests = list(
      series1_adf = adf.test(ts1),
      series2_adf = adf.test(ts2)
    )
  )
  
  # Add metadata about the analysis
  results$metadata <- list(
    n_observations = length(ts1),
    date_range = range(index(aligned_series)),
    missing_values = sum(is.na(df[[value_col]])),
    sampling_frequency = mean(diff(index(aligned_series)))
  )
  
  return(results)
}

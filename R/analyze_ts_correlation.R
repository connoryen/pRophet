#' Analyze Correlation Between Two Time Series
#' 
#' @description
#' Performs comprehensive correlation analysis between two time series, including
#' both time-domain and frequency-domain analyses. Particularly useful for financial
#' time series and ARIMAX modeling. Creates a combined visualization of four key plots
#' and returns various correlation statistics and diagnostic tests.
#' 
#' @param df A data frame containing the time series data
#' @param date_col Character string specifying the date column name (default: "Date")
#' @param value_col Character string specifying the value column name (default: "Value")
#' @param group_col Character string specifying the group column name (default: "Col")
#' @param max_lag Integer specifying maximum lag for cross-correlation (default: 24)
#' 
#' @return A list containing:
#' \itemize{
#'   \item plot: Combined visualization of four plots (see Details)
#'   \item statistics: List of correlation measures
#'   \item diagnostics: Statistical test results
#'   \item metadata: Analysis metadata
#' }
#' 
#' @details
#' The function creates a combined visualization with four plots:
#' 
#' A) Raw Time Series Plot
#'    - Shows both time series plotted over time
#'    - Use to visually inspect co-movement and patterns
#'    - Look for common trends, seasonality, and structural breaks
#' 
#' B) Cross-Correlation Function (CCF) Plot
#'    - Shows correlation at different lags
#'    - Red dashed lines indicate significance bounds (±1.96/√n)
#'    - Peaks outside bounds indicate significant correlation at that lag
#'    - Positive lag means first series leads second series
#' 
#' C) Rolling Correlation Plot
#'    - Shows how correlation changes over time (30-period window)
#'    - Values near 1 indicate strong positive correlation
#'    - Values near -1 indicate strong negative correlation
#'    - Useful for detecting relationship stability
#' 
#' D) Scatter Plot
#'    - Direct visualization of relationship between series
#'    - Red line shows fitted linear relationship
#'    - Tight clustering around line suggests strong linear relationship
#'    - Pattern deviations suggest non-linear relationships
#' 
#' Statistical Tests Interpretation:
#' \itemize{
#'   \item Granger Causality Test
#'     - Null hypothesis: No Granger causality
#'     - p < 0.05 suggests first series helps predict second series
#'   \item Augmented Dickey-Fuller (ADF) Test
#'     - Null hypothesis: Series has unit root (non-stationary)
#'     - p < 0.05 suggests series is stationary
#'   \item Correlation Measures
#'     - Contemporaneous: Current period correlation
#'     - Returns: Correlation of log returns
#'     - Maximum CCF: Strongest correlation at any lag
#' }
#' 
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   Date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-02", "2023-01-02")),
#'   Value = c(1.2, 1.3, 2.2, 1.8),
#'   Col = c("site_1", "site_2", "site_1", "site_2")
#' )
#' results <- analyze_ts_correlation(data)
#' results$plot  # Display combined visualization
#' }
#'
#' @importFrom dplyr %>%
#' @import ggplot2
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
    
    zoo_list[[group]] <- zoo::zoo(group_data[[value_col]], 
                                  order.by = group_data[[date_col]])
  }
  
  # Align the time series on matching dates
  aligned_series <- merge(zoo_list[[1]], zoo_list[[2]])
  aligned_series <- stats::na.omit(aligned_series)
  
  # Convert to numeric vectors for analysis
  ts1 <- as.numeric(aligned_series[,1])
  ts2 <- as.numeric(aligned_series[,2])
  
  # 1. Time series plot
  plot_df <- data.frame(
    Date = zoo::index(aligned_series),
    Series1 = zoo::coredata(aligned_series)[,1],
    Series2 = zoo::coredata(aligned_series)[,2]
  ) %>%
    tidyr::pivot_longer(cols = c(Series1, Series2),
                        names_to = "Series",
                        values_to = "Value")
  
  ts_plot <- ggplot2::ggplot(plot_df, 
                             ggplot2::aes(x = Date, y = Value, color = Series)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(subtitle = "A) Raw Time Series",
                  x = "Date",
                  y = "Value")
  
  # 2. Cross-correlation analysis
  ccf_result <- stats::ccf(ts1, ts2, 
                           lag.max = max_lag, 
                           plot = FALSE)
  
  ccf_df <- data.frame(
    lag = ccf_result$lag,
    acf = ccf_result$acf
  )
  
  ccf_plot <- ggplot2::ggplot(ccf_df, 
                              ggplot2::aes(x = lag, y = acf)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::geom_hline(yintercept = c(1.96/sqrt(length(ts1)), 
                                       -1.96/sqrt(length(ts1))),
                        linetype = "dashed", color = "red") +
    ggplot2::theme_minimal() +
    ggplot2::labs(subtitle = "B) Cross-Correlation Function",
                  x = "Lag",
                  y = "CCF")
  
  # 3. Rolling correlation
  roll_cor <- zoo::rollapply(aligned_series, 
                             width = min(30, nrow(aligned_series)), 
                             FUN = function(x) cor(x[,1], x[,2]),
                             by.column = FALSE,
                             align = "right")
  
  roll_cor_df <- data.frame(
    Date = zoo::index(roll_cor),
    Correlation = zoo::coredata(roll_cor)
  )
  
  rolling_cor_plot <- ggplot2::ggplot(roll_cor_df, 
                                      ggplot2::aes(x = Date, y = Correlation)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::theme_minimal() +
    ggplot2::labs(subtitle = "C) Rolling 30-period Correlation",
                  x = "Date",
                  y = "Correlation")
  
  # 4. Scatter plot
  scatter_df <- data.frame(
    Series1 = ts1,
    Series2 = ts2
  )
  
  scatter_plot <- ggplot2::ggplot(scatter_df, 
                                  ggplot2::aes(x = Series1, y = Series2)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_smooth(method = "lm", 
                         se = TRUE,        # Explicitly enable standard error bands
                         color = "red",
                         fill = "#FF000033", # Red with transparency
                         alpha = 0.2,      # Transparency of confidence band
                         level = 0.95) +   
    ggplot2::theme_minimal() +
    ggplot2::labs(subtitle = "D) Series Correlation Scatter Plot",
                  x = unique_groups[1],
                  y = unique_groups[2])
  
  # Combine plots
  combined_plot <- (ts_plot + ccf_plot) / (rolling_cor_plot + scatter_plot) +
    patchwork::plot_annotation(
      title = "Time Series Correlation Analysis",
      theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 16))
    )
  
  # Calculate statistics
  contemporaneous_cor <- stats::cor(ts1, ts2)
  max_ccf <- max(abs(ccf_result$acf))
  max_ccf_lag <- ccf_result$lag[which.max(abs(ccf_result$acf))]
  returns1 <- diff(log(ts1))
  returns2 <- diff(log(ts2))
  returns_cor <- stats::cor(returns1, returns2)
  
  list(
    plot = combined_plot,
    statistics = list(
      contemporaneous_correlation = contemporaneous_cor,
      returns_correlation = returns_cor,
      max_cross_correlation = max_ccf,
      max_cross_correlation_lag = max_ccf_lag,
      rolling_correlation_summary = summary(roll_cor)
    ),
    diagnostics = list(
      granger_test = tryCatch({
        granger_df <- data.frame(y = ts1, x = ts2)
        lmtest::grangertest(y ~ x, order = min(5, length(ts1) - 1), 
                            data = granger_df)
      }, error = function(e) {
        message("Granger test could not be performed: ", e$message)
        return(NULL)
      }),
      stationarity_tests = list(
        series1_adf = tseries::adf.test(ts1),
        series2_adf = tseries::adf.test(ts2)
      )
    ),
    metadata = list(
      n_observations = length(ts1),
      date_range = range(zoo::index(aligned_series)),
      missing_values = sum(is.na(df[[value_col]])),
      sampling_frequency = mean(diff(zoo::index(aligned_series)))
    )
  )
}
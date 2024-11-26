#' Analyze Correlation Between Two Time Series
#' 
#' @description
#' Performs comprehensive correlation analysis between two time series, including
#' time-domain, frequency-domain, and quantile-based analyses. Particularly useful 
#' for financial time series and ARIMAX modeling. Creates visualizations and returns
#' various correlation statistics and diagnostic tests.
#' 
#' @details 
#' The function performs several types of analyses:
#' 
#' \strong{Correlation Measures:}
#' \itemize{
#'   \item Contemporaneous Correlation: Pearson correlation between the two series
#'   \item Returns Correlation: Correlation between log returns (useful for financial data)
#'   \item Rolling Correlation: Time-varying correlation using a moving window
#'   \item Cross-Correlation: Correlation at different lags
#'   \item Quantile Correlation: Relationship strength across different market conditions
#' }
#' 
#' \strong{Statistical Tests:}
#' \itemize{
#'   \item Granger Causality Test: Tests whether one series helps predict the other
#'     \itemize{
#'       \item H0: Series 1 does not Granger-cause Series 2
#'       \item p < 0.05 suggests Series 1 helps predict Series 2
#'     }
#'   \item Augmented Dickey-Fuller (ADF) Test: Tests for stationarity
#'     \itemize{
#'       \item H0: Series has a unit root (non-stationary)
#'       \item p < 0.05 suggests series is stationary
#'     }
#' }
#' 
#' \strong{Visualizations:}
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
#'    - Shows how correlation changes over time
#'    - Values near 1 indicate strong positive correlation
#'    - Values near -1 indicate strong negative correlation
#'    - Useful for detecting relationship stability
#' 
#' D) Scatter Plot
#'    - Direct visualization of relationship between series
#'    - Red line shows fitted linear relationship
#'    - Pink band shows 95% confidence interval
#'    - Tight clustering around line suggests strong linear relationship
#' 
#' @param df A data frame containing the time series data
#' @param date_col Character string specifying the date column name (default: "Date")
#' @param value_col Character string specifying the value column name (default: "Value")
#' @param group_col Character string specifying the group column name (default: "Col")
#' @param max_lag Integer specifying maximum lag for cross-correlation (default: 24)
#' @param window_size Integer specifying the window size for rolling correlation (default: 30)
#' @param quantile_probs Numeric vector of probabilities for quantile analysis (default: c(0.1, 0.25, 0.5, 0.75, 0.9))
#' 
#' @return A list containing:
#' \itemize{
#'   \item plot: Combined visualization of four plots
#'   \item statistics: List of correlation measures including contemporaneous,
#'         returns, maximum cross-correlation, and rolling correlation summary
#'   \item diagnostics: Results of Granger causality and stationarity tests
#'   \item metadata: Analysis metadata including number of observations,
#'         date range, and sampling frequency
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
#' results  # prints summary
#' results$plot  # shows visualization
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_smooth geom_bar geom_hline theme_minimal labs theme element_text
#' @importFrom tidyr pivot_longer
#' @importFrom stats cor na.omit
#' @importFrom zoo zoo rollapply
#' @importFrom patchwork plot_annotation
#' @importFrom tseries adf.test
#' @importFrom lmtest grangertest
#' @importFrom graphics plot
#' @importFrom quantreg rq 
#'
analyze_ts_correlation <- function(df, 
                                   date_col = "Date", 
                                   value_col = "Value", 
                                   group_col = "Col",
                                   max_lag = 24,
                                   window_size = 30,
                                   quantile_probs = c(0.05, 0.5, 0.95)) {
  
  # Input validation -----------------------------------------------------------
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
  
  # Process data as time series ------------------------------------------------
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
  
  # Analysis -------------------------------------------------------------------
  
  # 1. Time series plot ........................................................
  plot_df <- data.frame(
    Date = zoo::index(aligned_series),
    Series1 = zoo::coredata(aligned_series)[,1],
    Series2 = zoo::coredata(aligned_series)[,2]
  ) %>%
    dplyr::rename_all(~c("Date", unique_groups[1], unique_groups[2])) %>%
    tidyr::pivot_longer(cols = c(unique_groups[1], unique_groups[2]),
                        names_to = "Series",
                        values_to = "Value")
  
  ts_plot <- ggplot2::ggplot(plot_df, 
                             ggplot2::aes(x = Date, y = Value, color = Series)) +
    ggplot2::geom_line() + 
    ggplot2::theme_minimal() +
    ggplot2::labs(subtitle = "A) Raw Time Series",
                  x = "Date",
                  y = "Value")
  
  # 2. Cross-correlation analysis ..............................................
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
  
  # 3. Rolling correlation .....................................................
  roll_cor <- zoo::rollapply(aligned_series, 
                             width = min(window_size, nrow(aligned_series)), 
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
    ggplot2::labs(subtitle = sprintf("C) Rolling %d-period Correlation", window_size),
                  x = "Date",
                  y = "Correlation")
  
  # 4. Quantile regression analysis ............................................

  # Quantile regression analysis function
  quantile_analysis <- function(x, y, probs) {
    # Prepare data frame for quantile regression
    qr_data <- data.frame(x = x, y = y)
    
    # Perform quantile regression for each probability
    qr_results <- lapply(probs, function(p) {
      model <- quantreg::rq(y ~ x, data = qr_data, tau = p)
      coef <- coef(model)
      list(
        quantile = p,
        intercept = coef[1],
        slope = coef[2],
        fitted = fitted(model)
      )
    })
    
    # Calculate tail dependence coefficients
    lower_tail <- mean(y <= quantile(y, 0.1) | x <= quantile(x, 0.1))
    upper_tail <- mean(y >= quantile(y, 0.9) | x >= quantile(x, 0.9))
    
    list(
      qr_results = qr_results,
      tail_dependence = list(
        lower = lower_tail,
        upper = upper_tail
      )
    )
  }
  
  # Function to assign quantile groups to points
  get_quantile_group <- function(y, probs) {
    cuts <- quantile(y, probs = probs)
    group <- cut(y, 
                 breaks = c(-Inf, cuts, Inf),
                 labels = paste0("Q", round(c(0, probs) * 100)),
                 include.lowest = TRUE)
    return(group)
  }
  
  # Update plot data preparation for 3 quantiles
  create_plot_data <- function(ts1, ts2, quantile_probs = c(0.25, 0.5, 0.75)) {
    plot_data <- data.frame(
      x = ts1,
      y = ts2,
      quantile_group = get_quantile_group(ts2, quantile_probs)
    )
    # Calculate mean quantile for color mapping
    plot_data$quantile_value <- as.numeric(sub("Q", "", plot_data$quantile_group)) / 100
    return(plot_data)
  }
  
  # Create the quantile regression plot
  create_quantile_plot <- function(plot_data, qr_lines, unique_groups) {
    # Create distinct color scales for points and lines
    point_colors <- c(
      "#FFB4B4",  # Light red
      "#FFFFB4",  # Light yellow
      "#B4FFB4"   # Light green
    )
    
    line_colors <- c(
      "#CC0000",  # Dark red
      "#CCCC00",  # Dark yellow
      "#00CC00"   # Dark green
    )
    
    ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
      # Add points with lighter colors (first layer)
      ggplot2::geom_point(
        ggplot2::aes(color = quantile_value),
        alpha = 0.85, 
        size = 1.5
      ) +
      # Custom color gradient for points
      ggplot2::scale_color_gradient2(
        name = "Quantile",
        low = point_colors[1],    
        mid = point_colors[2],    
        high = point_colors[3],   
        midpoint = 0.5,
        labels = scales::percent
      ) +
      # Add quantile regression lines on top with darker colors
      ggplot2::geom_abline(
        data = qr_lines,
        aes(
          intercept = intercept,
          slope = slope
        ),
        color = line_colors[match(qr_lines$quantile, quantile_probs)],
        linewidth = 1
      ) +
      # Add mean regression line
      ggplot2::geom_smooth(
        method = "lm", 
        se = FALSE,
        color = "grey30",
        linewidth = 0.5
      ) +
      # Theme and labels
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "right",
        legend.box = "vertical"
      ) +
      #theme(legend.position="none") + 
      ggplot2::labs(
        subtitle = "D) Series Correlation with Quantile Analysis",
        x = unique_groups[1],
        y = unique_groups[2]
      )
  }
  
  # Main quantile analysis section
  perform_quantile_analysis <- function(ts1, ts2, quantile_probs, unique_groups) {
    # Perform quantile regression analysis
    quant_results <- quantile_analysis(ts1, ts2, quantile_probs)
    
    # Extract results for plotting
    qr_lines <- do.call(rbind, lapply(quant_results$qr_results, function(res) {
      data.frame(
        quantile = res$quantile,
        intercept = res$intercept,
        slope = res$slope
      )
    }))
    
    # Create plot data
    plot_data <- create_plot_data(ts1, ts2, quantile_probs)
    
    # Create the plot
    qr_plot <- create_quantile_plot(plot_data, qr_lines, unique_groups)
    
    # Return results
    list(
      plot = qr_plot,
      qr_lines = qr_lines,
      quant_results = quant_results
    )
  }
  
  quantile_results <- perform_quantile_analysis(ts1, ts2, quantile_probs, unique_groups)
  scatter_qr_plot <- quantile_results$plot
  
  # Combine Plots ..............................................................
  
  combined_plot <- (ts_plot + ccf_plot) / (rolling_cor_plot + scatter_qr_plot) +
    patchwork::plot_annotation(
      title = paste0("Time Series Correlation Analysis: ", 
                     unique_groups[1], " & ", unique_groups[2]),
      theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 16))
    )
  
  # Statistics -----------------------------------------------------------------
  # Calculate statistics
  contemporaneous_cor <- stats::cor(ts1, ts2)
  max_ccf <- max(abs(ccf_result$acf))
  max_ccf_lag <- ccf_result$lag[which.max(abs(ccf_result$acf))]
  returns1 <- diff(log(ts1))
  returns2 <- diff(log(ts2))
  returns_cor <- stats::cor(returns1, returns2)
  
  # Return ---------------------------------------------------------------------
  results <- list(
    plot = combined_plot,
    statistics = list(
      contemporaneous_correlation = contemporaneous_cor,
      returns_correlation = returns_cor,
      max_cross_correlation = max_ccf,
      max_cross_correlation_lag = max_ccf_lag,
      rolling_correlation_summary = summary(as.numeric(roll_cor)),
      quantile_analysis = list(
        regression_results = quantile_results$quant_results$qr_results,
        tail_dependence = quantile_results$quant_results$tail_dependence
      )
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
  
  class(results) <- "ts_correlation"
  return(results)
}

#' Print method for ts_correlation objects
#' @param x Object of class ts_correlation
#' @param digits Number of digits to round to
#' @export
print.ts_correlation <- function(x, digits = 3) {
  cat("\nTime Series Correlation Analysis Summary")
  cat("\n=====================================\n")
  
  # Format date range
  date_range <- format(x$metadata$date_range, "%Y-%m-%d")
  
  # Basic information
  cat("\nData Overview:")
  cat("\n - Date Range:", date_range[1], "to", date_range[2])
  cat("\n - Number of Observations:", x$metadata$n_observations)
  cat("\n - Average Sampling Frequency:", round(x$metadata$sampling_frequency, 1), "days")
  
  # Correlation measures
  cat("\n\nCorrelation Measures:")
  cat("\n - Contemporaneous Correlation:", round(x$statistics$contemporaneous_correlation, digits))
  cat("\n - Returns Correlation:", round(x$statistics$returns_correlation, digits))
  cat("\n - Maximum Cross-Correlation:", round(x$statistics$max_cross_correlation, digits),
      "at lag", x$statistics$max_cross_correlation_lag)
  
  # Rolling correlation summary
  cat("\n\nRolling Correlation Summary:")
  roll_sum <- x$statistics$rolling_correlation_summary
  
  cat("\n - Min:", round(roll_sum[1], digits))
  cat("\n - Median:", round(roll_sum[3], digits))
  cat("\n - Mean:", round(roll_sum[4], digits))
  cat("\n - Max:", round(roll_sum[6], digits))
  
  # Statistical tests
  cat("\n\nStatistical Tests:")
  
  # Granger test
  if(!is.null(x$diagnostics$granger_test)) {
    g_test <- x$diagnostics$granger_test
    cat("\n - Granger Causality Test p-value:", 
        round(g_test$`Pr(>F)`[2], digits))
  }
  
  # ADF tests
  cat("\n - ADF Test p-values:")
  cat("\n   Series 1:", round(x$diagnostics$stationarity_tests$series1_adf$p.value, digits))
  cat("\n   Series 2:", round(x$diagnostics$stationarity_tests$series2_adf$p.value, digits))
  
  # Quantile regression results
  cat("\n\nQuantile Regression Analysis:")
  qr_results <- x$statistics$quantile_analysis$regression_results
  for(res in qr_results) {
    cat(sprintf("\n - Q%.2f: slope = %.3f, intercept = %.3f",
                res$quantile, res$slope, res$intercept))
  }
  
  # Tail dependence
  td <- x$statistics$quantile_analysis$tail_dependence
  cat("\n\nTail Dependence:")
  cat(sprintf("\n - Lower tail: %.3f", td$lower))
  cat(sprintf("\n - Upper tail: %.3f", td$upper))
  
  cat("\n\nUse $plot to view the combined visualization.\n")
  invisible(x)
}
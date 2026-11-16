# BDA400 | Assignment 2
# Technical Analysis using R
# Student: Abir Hamo

packages <- c("quantmod", "TTR", "ggplot2", "dplyr")

ensure_packages <- function(x) {
  absent <- x[!vapply(x, requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent)) install.packages(absent, repos = "https://cloud.r-project.org")
}
ensure_packages(packages)

suppressPackageStartupMessages({
  library(quantmod)
  library(TTR)
  library(ggplot2)
  library(dplyr)
})

portfolio_symbols <- function(path = "portfolio.txt") {
  if (!file.exists(path)) stop("portfolio.txt was not found in the working directory.")
  x <- unique(trimws(readLines(path, warn = FALSE)))
  x <- x[nzchar(x)]
  if (!length(x)) stop("The portfolio file is empty.")
  x
}

fetch_portfolio <- function(path = "portfolio.txt",
                            start_date = Sys.Date() - 365,
                            end_date = Sys.Date()) {
  symbols <- portfolio_symbols(path)
  result <- list()
  rejected <- character()

  for (ticker in symbols) {
    cat("Retrieving", ticker, "...\n")
    prices <- tryCatch(
      quantmod::getSymbols(ticker, src = "yahoo",
                           from = start_date, to = end_date,
                           auto.assign = FALSE, warnings = FALSE),
      error = function(e) {
        message("Download issue for ", ticker, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(prices)) {
      rejected <- c(rejected, ticker)
      next
    }

    frame <- as.data.frame(prices)
    names(frame) <- sub("^.*\\.", "", names(frame))
    frame$Date <- as.Date(rownames(frame))
    rownames(frame) <- NULL
    result[[ticker]] <- frame[, c("Date", setdiff(names(frame), "Date"))]
  }

  if (!length(result)) stop("No symbols returned data. Check connectivity and ticker names.")
  if (length(rejected)) warning("Skipped symbols: ", paste(rejected, collapse = ", "))
  result
}

stat_mode <- function(z) {
  z <- z[is.finite(z)]
  if (!length(z)) return(NA_real_)
  vals <- unique(z)
  vals[which.max(tabulate(match(z, vals)))]
}

stock_profile <- function(frame, ticker, window = 15) {
  if (!"Adjusted" %in% names(frame)) stop("Adjusted price is unavailable for ", ticker)
  close_adj <- as.numeric(frame$Adjusted)
  close_adj <- close_adj[is.finite(close_adj)]
  if (!length(close_adj)) stop("No valid adjusted prices for ", ticker)

  ma <- TTR::SMA(close_adj, n = min(window, length(close_adj)))
  valid_ma <- ma[is.finite(ma)]

  data.frame(
    Ticker = ticker,
    Number_of_Days = length(close_adj),
    Most_Recent_Adjusted_Price = tail(close_adj, 1),
    Moving_Average_15 = if (length(valid_ma)) tail(valid_ma, 1) else NA_real_,
    Average = mean(close_adj),
    Most_Common_Price = stat_mode(close_adj),
    Midpoint_Median = median(close_adj),
    Price_Std_Deviation = sd(close_adj),
    check.names = FALSE
  )
}

portfolio_summary <- function(data_list, window = 15) {
  dplyr::bind_rows(lapply(names(data_list), function(ticker) {
    stock_profile(data_list[[ticker]], ticker, window)
  }))
}

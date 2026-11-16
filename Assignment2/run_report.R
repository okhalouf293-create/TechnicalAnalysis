# Execute this file from the Assignment2 directory in RStudio.
source("abir_analysis.R")

market_data <- fetch_portfolio("portfolio.txt")
summary_table <- portfolio_summary(market_data, window = 15)

cat("\nPORTFOLIO SUMMARY\n")
print(summary_table, row.names = FALSE)

for (ticker in names(market_data)) {
  cat("\nRECENT OBSERVATIONS:", ticker, "\n")
  print(utils::tail(market_data[[ticker]], 8))
}

write.csv(summary_table, "abir_portfolio_statistics.csv", row.names = FALSE)

for (ticker in names(market_data)) {
  write.csv(market_data[[ticker]],
            paste0("prices_", ticker, ".csv"),
            row.names = FALSE)
}

chart_data <- dplyr::bind_rows(lapply(names(market_data), function(ticker) {
  d <- market_data[[ticker]]
  data.frame(Date = d$Date, Adjusted = as.numeric(d$Adjusted), Ticker = ticker)
}))

overview <- ggplot(chart_data, aes(Date, Adjusted, group = Ticker)) +
  geom_line() +
  facet_wrap(~Ticker, scales = "free_y") +
  labs(title = "Portfolio Price History",
       subtitle = "Adjusted daily closing prices",
       x = "Trading date", y = "Adjusted price (USD)") +
  theme_minimal()

print(overview)
ggsave("portfolio_price_history.png", overview, width = 11, height = 7, dpi = 300)

for (ticker in names(market_data)) {
  d <- market_data[[ticker]]
  d$Adjusted <- as.numeric(d$Adjusted)
  d$SMA_15 <- TTR::SMA(d$Adjusted, n = 15)

  p <- ggplot(d, aes(Date)) +
    geom_line(aes(y = Adjusted), linewidth = 0.55) +
    geom_line(aes(y = SMA_15), linewidth = 0.8,
              linetype = "dotted", na.rm = TRUE) +
    labs(title = paste(ticker, "— Adjusted Price and 15-Day SMA"),
         x = "Trading date", y = "Price (USD)") +
    theme_minimal()

  print(p)
  ggsave(paste0("sma_", ticker, ".png"), p,
         width = 9, height = 5.5, dpi = 300)
}

cat("\nRun completed. Review the CSV and PNG files in the project directory.\n")

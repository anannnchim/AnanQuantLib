% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_annual_performance_table.R
\name{create_annual_performance_table}
\alias{create_annual_performance_table}
\title{Create Annual Performance Table}
\usage{
create_annual_performance_table(daily_return)
}
\arguments{
\item{daily_return}{A time-series object or a numeric vector containing the daily returns of the asset.}
}
\value{
A data frame containing the annualized performance metrics such as Annualized Returns,
Average Drawdown, and Maximum Drawdown.
}
\description{
This function computes an annual performance summary for a given set of daily returns.
The function uses the PerformanceAnalytics package to calculate various performance metrics.
}
\examples{
# Example using hypothetical daily returns
daily_returns <- rnorm(252, 0, 0.02)
performance_table <- create_annual_performance_table(daily_returns)
print(performance_table)
}

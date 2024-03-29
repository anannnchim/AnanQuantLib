% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comp_ema.R
\name{comp_ema}
\alias{comp_ema}
\title{Compute Exponential Moving Average}
\usage{
comp_ema(stock_data, window)
}
\arguments{
\item{stock_data}{xts object containing stock data.}

\item{window}{Integer for the look-back window.}
}
\value{
An xts object with the computed EMA.
}
\description{
This function calculates the Exponential Moving Average (EMA) of a given stock
based on the close prices.
}
\examples{
stock_data = xts(runif(100, 100, 200), Sys.Date() + 1:100)
comp_ema(stock_data, window = 20)
}

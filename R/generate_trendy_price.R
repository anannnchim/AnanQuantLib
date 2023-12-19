# Load necessary libraries
library(xts)
library(dplyr)

#' Generate Gaussian Noise Sequence
#'
#' This function creates a sequence of Gaussian noise data points.
#' @param Nlength The length of the time series.
#' @param stdev The standard deviation of the Gaussian noise.
#' @return A vector of Gaussian noise.
generate_noise <- function(Nlength, stdev) {
  rnorm(Nlength, 0, stdev)
}

#' Create Sine-Wave Trend Data
#'
#' This function generates a sine-wave trend for a time series.
#' @param Nlength The total number of data points.
#' @param Tlength The period of one sine wave cycle.
#' @param Xamplitude The amplitude of the sine wave.
#' @return A vector containing the sine-wave trend.
generate_siney_trends <- function(Nlength, Tlength, Xamplitude) {
  halfAmplitude <- Xamplitude / 2
  cycles <- Nlength / Tlength
  cycles_as_pi <- cycles * pi
  increment <- cycles_as_pi / Nlength
  alltrends <- sin(seq(0, cycles_as_pi, by = increment)) * halfAmplitude
  alltrends[1:Nlength]
}

#' Generate Linear Trend Data
#'
#' This function generates a linear trend for a time series.
#' @param Nlength The total number of data points.
#' @param Tlength The period for one complete up and down cycle.
#' @param Xamplitude The amplitude of the linear trend.
#' @return A vector containing the linear trend.
generate_trends <- function(Nlength, Tlength, Xamplitude) {
  halfAmplitude <- Xamplitude / 2
  trend_step <- Xamplitude / Tlength
  cycles <- ceiling(Nlength / Tlength)
  trendup <- seq(-halfAmplitude, halfAmplitude, by = trend_step)
  trenddown <- seq(halfAmplitude, -halfAmplitude, by = -trend_step)
  alltrends <- rep(c(trendup, trenddown), cycles)
  alltrends[1:Nlength]
}

#' Create Synthetic Price Series
#'
#' This function generates a synthetic price series based on trends and Gaussian noise.
#' @param Nlength The total number of data points.
#' @param Tlength The period for the trend.
#' @param Xamplitude The amplitude of the trend.
#' @param Volscale The volatility scaling factor.
#' @param sines If TRUE, uses sine-wave trends. If FALSE, uses linear trends.
#' @return An xts object containing the synthetic price series.
generate_trendy_price <- function(Nlength, Tlength, Xamplitude, Volscale, sines = FALSE) {
  stdev <- Volscale * Xamplitude
  noise <- generate_noise(Nlength, stdev)

  if (sines) {
    process <- generate_siney_trends(Nlength, Tlength, Xamplitude)
  } else {
    process <- generate_trends(Nlength, Tlength, Xamplitude)
  }

  combined_price <- noise + process
  # Shift prices up
  combined_price <- combined_price + Xamplitude

  # Create xts object
  date_index <- seq.Date(from = as.Date('2020-01-01'), by = 'day', length.out = Nlength)
  data_matrix = matrix(rep(combined_price, 4), ncol = 4)
  combined_xts <- xts(data_matrix, order.by = date_index)
  colnames(combined_xts) <- c("Open", "High", "Low", "Close")

  return(combined_xts)
}

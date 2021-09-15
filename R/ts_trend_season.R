#' ts_trend_season
#' 
#' Estimate a trend and season model from a \code{ts_data} object.
#'
#' @param ts \code{ts_data} object
#' @param trend numeric or logical: if \code{trend} is TRUE then a linear trend will be estimated otherwise an exponential trend. If \code{trend}is numeric this is talen as trend values
#' @param season numeric or logical: 
#'
#' @md
#' @return returns an extended \code{ts_data} object with list elements:
#' * `t` the time points
#' * `s` the season for the time poins
#' * `xt` the time series values
#' * `trend` the fitted trend values
#' * `trend.coeff` the trend coefficients
#' * `trend.linear` the trend type, if \code{NA} then it is unknown
#' * `season` the fitted season values
#' * `season.t` the fitted season values for the time series
#' * `trend.season` the fitted values for trend and season
#' * `trend.linear` the trend type, if \code{NA} then it is unknown
#' * `var` the variance of the residuals
#' * `r.square` the \eqn{R^2} of the final model
#' @importFrom stats fitted
#' @export 
#'
#' @examples
#' ts <- ts_data(12, trend.coeff= c(sample(0:10, 1), sample(1+(1:10)/20, 1)))
#' ts_trend_season(ts)
ts_trend_season <- function(ts, trend=NULL, season=NULL) {
  stopifnot("ts_data" %in% class(ts))
  # estimate trend if necessary
  ts$trend        <- trend
  ts$trend.linear <- NA
  ts$trend.coeff  <- NULL
  if (is.logical(trend)) {
    if (trend) { # linear trend
      trend.linear <- TRUE
      lmt <- lm(ts$xt~ts$t)      
      ts$trend.coeff <- lmt$coefficients
      ts$trend <- fitted(lmt)
    } else { # exponential trend
      trend.linear <- FALSE
      lmt <- lm(log(ts$xt)~ts$t)      
      ts$trend.coeff <- exp(lmt$coefficients)
      ts$trend <- exp(fitted(lmt))
    }
  }
  # estimate season if necessary
  ts$season <- rep(0, length(season))
  ts$trend.season <- ts$trend
  if (is.logical(season)) {
    if (season) { # additive season
      ts$season <- tapply(ts$xt-ts$trend, ts$s, mean)
      ts$season.t <- rep(ts$season, length.out=length(ts$t))
      ts$trend.season <- ts$trend+ts$season.t
    } else { # multiplicative season
      ts$season   <- tapply(ts$xt/ts$trend, ts$s, mean)   
      ts$season.t <- rep(ts$season, length.out=length(ts$t))
      ts$trend.season <- ts$trend*ts$season.t
    }
  }
  ts$var       <- mean((ts$xt-ts$trend.season)^2)
  ts$r.squared <- 1-ts$var/mean((ts$xt-mean(ts$xt))^2)
  ts
}

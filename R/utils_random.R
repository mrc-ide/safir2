# utilities for drawing random variates

#' @title Make Erlang waiting time distribution
#' @description Random draws from Erlang distribution
#' scaled by time step size.
#' @param mu Mean duration
#' @param dt Size of time step
#' @param shape Shape parameter of Erlang distribution
#' @param shift number of time steps to add to sampled value
#' @importFrom stats rgamma
#' @export
make_rerlang <- function(mu, dt, shape = 2, shift = 0L) {
  stopifnot(is.finite(mu))
  stopifnot(mu > 0)
  stopifnot(is.finite(dt))
  stopifnot(dt > 0)
  stopifnot(is.finite(shape))
  stopifnot(shape > 0)
  r <- shape / mu
  function(n) {
    floor(rgamma(n = n, shape = shape, rate = r) / dt) + shift # possibility of 0 delay
  }
}


#' @title Make exponential waiting time distribution
#' @description Make geometric approximation to
#' continuous time exponential distribution.
#' @param mu Mean duration
#' @param dt Size of time step
#' @param shift number of time steps to add to sampled value
#' @importFrom stats rgeom pexp
#' @export
make_rexp <- function(mu, dt, shift = 0L) {
  stopifnot(is.finite(mu))
  stopifnot(mu > 0)
  stopifnot(is.finite(dt))
  stopifnot(dt > 0)
  r <- 1 / mu
  p <- pexp(q = r * dt)
  function(n) {
    rgeom(n = n, prob = p) + shift
  }
}

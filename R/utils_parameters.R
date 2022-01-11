# utilities for getting time varying parameters

#' @title Get contact matrix at current timestep
#' @description Return a function taking a single argument `timestep` which
#' returns a contact matrix for the current timestep
#' @param parameters a named [list]
#' @export
make_get_contact_matrix <- function(parameters) {
  stopifnot(!is.null(parameters$mix_mat_set))
  stopifnot(inherits(parameters$mix_mat_set, "array"))
  stopifnot(length(dim(parameters$mix_mat_set)) == 3L)
  stopifnot(dim(parameters$mix_mat_set)[2:3] == rep(17L, 2))

  if (dim(parameters$mix_mat_set)[1] == 1L) {
    return(
      function(timestep) {
        parameters$mix_mat_set[1, , ]
      }
    )
  } else {
    dt <- parameters$dt
    return(
      function(timestep) {
        day <- ceiling(timestep * dt)
        parameters$mix_mat_set[day, , ]
      }
    )
  }

}


#' @title Get vector or scalar parameter at current timestep
#' @description Return a function taking a single argument `timestep` which
#' returns the value of `name` at the current timestep
#' @param parameters a named [list]
#' @param name an element in the parameter list, must be numeric
#' @export
make_get_vector <- function(parameters, name) {
  stopifnot(name != "dt")
  stopifnot(is.list(parameters))
  stopifnot(!is.null(parameters[[name]]))
  stopifnot(is.numeric(parameters[[name]]))

  if (length(parameters[[name]]) == 1L) {
    val <- parameters[[name]]
    return(
      function(timestep) {
        val
      }
    )
  } else {
    dt <- parameters$dt
    val <- parameters[[name]]
    return(
      function(timestep) {
        day <- ceiling(timestep * dt)
        val[day]
      }
    )
  }
}


#' @title Get age-structured transition probabilities at current timestep
#' @description Return a function taking two arguments `timestep` and `ages` which
#' returns the probabilities stored in `name` at the current timestep and for
#' those persons who age is in `ages`.
#' @param parameters a named [list]
#' @param name an element in the parameter list, must be numeric
#' @export
make_get_age_probabilities <- function(parameters, name) {
  stopifnot(name != "dt")
  stopifnot(is.list(parameters))
  stopifnot(!is.null(parameters[[name]]))
  stopifnot(is.numeric(parameters[[name]]))

  probs <- parameters[[name]]

  if (is.matrix(probs)) {
    stopifnot(nrow(probs) == parameters$N_age)
    stopifnot(ncol(probs) == parameters$time_period)
    dt <- parameters$dt
    function(timestep, ages) {
      day <- ceiling(timestep * dt)
      probs[cbind(ages, day)]
    }
  } else {
    stopifnot(length(probs) == parameters$N_age)
    return(
      function(timestep, ages) {
        probs[ages]
      }
    )
  }
}


#' @title Produce piecewise linear Rt
#' @description Make a piecewise linear interpolating vector of Rt, for each
#' day within the simulated interval. The assumed start of simulation
#' is the first date provided in `dates`, and the end of simulation is the last
#' date of `dates` unless `max_date` is provided. Interpolation is done with
#' inclusive endpoints.
#' @param dates a vector of [Date]s
#' @param rt a vector of reproductive values at those dates
#' @param max_date the maximum date of simulation (optional)
#' @return a [list] with named elements `Rt` and `Rt_tt`, which can be provided
#' to the [safir::get_parameters] function's arguments `R0` and `tt_R0`.
#' @importFrom stats approx
#' @export
interpolate_rt <- function(dates, rt, max_date = NULL) {
  stopifnot(inherits(dates, "Date"))
  stopifnot(length(dates) > 1)
  stopifnot(all(as.integer(diff(dates)) > 0))
  stopifnot(is.finite(rt))
  stopifnot(all(rt > 0))
  stopifnot(length(dates) == length(rt))

  if (!is.null(max_date)) {
    stopifnot(inherits(max_date, "Date"))
    stopifnot(max_date > dates[length(dates)])
    dates <- c(dates, max_date)
    rt <- c(rt, rt[length(rt)])
  }

  time_interval <- vapply(X = 2:length(dates), FUN = function(d){
    as.integer(difftime(dates[d], dates[1] - 1))
  }, FUN.VALUE = integer(1))
  time_interval <- c(0, time_interval)

  total_interval <- as.integer(difftime(dates[length(dates)], dates[1] - 1))
  Rt_tt <- 1:total_interval
  Rt <- rep(NaN, total_interval)

  time_interval_list <- lapply(X = seq_len(length(time_interval) - 1), FUN = function(v){
    (time_interval[v] + 1):time_interval[v + 1]
  })

  for (i in seq_len(length(dates) - 1)) {

    if (rt[i] == rt[i + 1]) {
      Rt[time_interval_list[[i]]] <- rt[i]
    } else {
      Rt[time_interval_list[[i]]] <- approx(
        x = c(time_interval_list[[i]][1], time_interval_list[[i]][length(time_interval_list[[i]])]),
        y = c(rt[i], rt[i + 1]),
        xout = time_interval_list[[i]]
      )$y
    }

  }

  return(list(
    Rt = Rt, Rt_tt = Rt_tt
  ))
}


#' @noRd
approx_equal <- function(a, b, tol = sqrt(.Machine$double.eps)) {
  abs(a - b) < tol
}


#' @noRd
remove_non_numerics <- function(l) {
  clean <- list()
  for (key in names(l)) {
    if (storage.mode(l[[key]]) %in% c("integer", "double")) {
      clean[[key]] <- l[[key]]
    }
  }
  clean
}

#' @noRd
is_finite_numeric <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  vapply(X = x, FUN = function(xx) {
    return(is.numeric(xx) & is.finite(xx))
  }, FUN.VALUE = logical(1), USE.NAMES = FALSE)
}

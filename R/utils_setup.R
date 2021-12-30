# utilities with input parameters and setup of events


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


#' @title Add named listener to an event
#' @description Given a [individual::Event] or [individual::TargetedEvent] object,
#' add a named listener to the list of listeners stored in that event.
#' @param event a [individual::Event] or [individual::TargetedEvent] object
#' @param name a name
#' @param listener a function which takes either a single argument (`timestep`)
#' if event is a [individual::Event] or two arguments (`timestep`, `target`) if
#' the event is a [individual::TargetedEvent]
#' @importFrom stats setNames
#' @export
add_named_listener <- function(event, name, listener) {
  stopifnot(inherits(event, "Event"))
  stopifnot(inherits(name, "character"))
  stopifnot(typeof(listener) == "closure")
  if (length(event$.listeners) > 0L) {
    stopifnot(!is.null(names(event$.listeners)))
    stopifnot(!name %in% names(event$.listeners))
  }
  listener_names <- names(event$.listeners)
  listener_names <- c(listener_names, name)
  event$.listeners <- c(event$.listeners, listener)
  event$.listeners <- setNames(object = event$.listeners, nm = listener_names)
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

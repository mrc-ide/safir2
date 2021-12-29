# utilities for safir2

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


#' @title Contact matrix at current timestep
#' @description Return a function taking a single argument `timestep`.
#' @param parameters a named [list]
#' @return 2D contact matrix for the current timestep
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

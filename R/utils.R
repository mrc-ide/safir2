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

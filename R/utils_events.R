# utilities with input parameters and setup of events

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


#' @title A listener to schedule future events
#' @description a listener function to be attached to a [individual::TargetedEvent]
#' to schedule a future event when that event fires.
#' @param event the future event to be schedule
#' @param duration mean duration of waiting time to be scheduled
#' @param func either [safir2::make_rerlang] or [safir2::make_rexp]
#' @param shift add integer number of time steps to sampled value
#' @param dt size of time step
#' @export
create_event_scheduler_listener <- function(event, duration, func, shift, dt) {
  stopifnot(inherits(event, "TargetedEvent"))
  dwell <- func(mu = duration, dt = dt, shift = shift)
  function(timestep, target) {
    event$schedule(target = target, delay = dwell(n = target$size()))
  }
}


#' @title A listener to update state
#' @description a listener to be attached to a [individual::TargetedEvent]
#' to update state when that event fires.
#' @param states a [individual::CategoricalVariable] object
#' @param destination the destination state
#' @export
create_state_update_listener <- function(states, destination) {
  stopifnot(is.character(destination))
  stopifnot(inherits(states, "CategoricalVariable"))
  function(timestep, target) {
    states$queue_update(value = destination, index = target)
  }
}

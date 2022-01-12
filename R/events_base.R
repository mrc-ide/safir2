# base set of events for safir2 models

#' @title Create disease progression events
#' @description Create named list of [individual::TargetedEvent] objects which
#' simulate disease progression. The list of events is as follows:
#'
#'  * `exposure`: scheduled when an individual is infected
#'  * `mild_infection`: scheduled from `exposure`
#'  * `asymp_infection`: scheduled from `exposure`
#'  * `severe_infection`: scheduled from `exposure`
#'  * `hospitilisation`: scheduled from `severe_infection`
#'  * `ICU_get_live`: scheduled from `hospitilisation`
#'  * `ICU_get_die`: scheduled from `hospitilisation`
#'  * `hosp_get_live`: scheduled from `hospitilisation`
#'  * `hosp_get_die`: scheduled from `hospitilisation`
#'  * `ICU_not_get_live`: scheduled from `hospitilisation`
#'  * `ICU_not_get_die`: scheduled from `hospitilisation`
#'  * `hosp_not_get_live`: scheduled from `hospitilisation`
#'  * `hosp_not_get_die`: scheduled from `hospitilisation`
#'  * `stepdown`: scheduled from `ICU_get_live`
#'  * `recovery`: scheduled from `mild_infection`, `asymp_infection`, `ICU_not_get_live`, `hosp_get_live`, `hosp_not_get_live`
#'  * `immunity_loss`: scheduled from `recovery`
#'  * `death`: scheduled from `ICU_get_die`, `ICU_not_get_die`, `hosp_get_die`, `hosp_not_get_die`
#'
#' @param parameters model parameters
#' @importFrom individual TargetedEvent
#' @export
create_events <- function(parameters) {

  stopifnot(is.numeric(parameters$population))

  # pop size
  N <- sum(parameters$population)

  list(
    # Human infection events
    exposure = TargetedEvent$new(N), # S->E, scheduled by infection_process_zzz
    mild_infection = TargetedEvent$new(N), # E->IMild, scheduled by create_exposure_update_listener
    asymp_infection = TargetedEvent$new(N),
    severe_infection = TargetedEvent$new(N),
    hospitilisation = TargetedEvent$new(N),
    ICU_get_live = TargetedEvent$new(N), # need ICU, gets bed, lives
    ICU_get_die = TargetedEvent$new(N), # need ICU, gets bed, dies
    hosp_get_live = TargetedEvent$new(N), # need hosp, gets bed, lives
    hosp_get_die = TargetedEvent$new(N), # need hosp, gets bed, dies
    ICU_not_get_live = TargetedEvent$new(N), # need ICU, doesn't get bed, lives
    ICU_not_get_die = TargetedEvent$new(N), # need ICU, doesn't get bed, dies
    hosp_not_get_live = TargetedEvent$new(N), # need hosp, doesn't get bed, lives
    hosp_not_get_die = TargetedEvent$new(N), # need hosp, doesn't get bed, dies
    stepdown = TargetedEvent$new(N),
    recovery = TargetedEvent$new(N),
    immunity_loss = TargetedEvent$new(N),
    death = TargetedEvent$new(N)
  )
}



#' @title Attach listeners to events
#' @description defines processes for events that can be scheduled in the future
#'
#' @param variables list of variables in the model
#' @param events a list of events in the model
#' @param parameters the model parameters
#' @param shift schedule future events after minimum number of time step delay
#' @param shift_exposure schedule exposure event after minimum number of time step delay
#' @export
attach_event_listeners <- function(
  variables,
  events,
  parameters,
  shift = 1L,
  shift_exposure = 1L
) {

  dt <- parameters$dt

  # Exposure ----------
  add_named_listener(
    event = events$exposure,
    name = "state_update",
    listener = create_state_update_listener(
      states = variables$states,
      destination = "E"
    )
  )

  add_named_listener(
    event = events$exposure,
    name = "state_update",
    listener = create_exposure_scheduler_listener(
      events = events,
      variables = variables,
      parameters = parameters,
      dt = dt,
      shift = shift_exposure
    )
  )

  # events$exposure$add_listener(
  #   create_exposure_scheduler_listener(
  #     events,
  #     variables,
  #     parameters,
  #     dt = dt,
  #     shift = shift_exposure
  #   )
  # )

  # IMild ----------

  events$mild_infection$add_listener(
    create_state_update_listener(
      variables$states,
      "IMild"
    )
  )

  events$mild_infection$add_listener(
    create_event_scheduler_listener(
      event = events$recovery,
      duration = parameters$dur_IMild,
      func = make_rexp,
      shift = shift,
      dt = dt
    )
  )

  # IAsymp ----------

  events$asymp_infection$add_listener(
    create_state_update_listener(
      variables$states,
      "IAsymp"
    )
  )

  events$asymp_infection$add_listener(
    create_event_scheduler_listener(
      event = events$recovery,
      duration = parameters$dur_IAsymp,
      func = make_rexp,
      shift = shift,
      dt = dt
    )
  )

  # ICase ----------

  events$severe_infection$add_listener(
    create_state_update_listener(
      variables$states,
      "ICase"
    )
  )

  events$severe_infection$add_listener(
    create_event_scheduler_listener(
      event = events$hospitilisation,
      duration = parameters$dur_ICase,
      func = make_rerlang,
      shift = shift,
      dt = dt
    )
  )

  # Hospitalisation (no state update, queues other events) ----------

  events$hospitilisation$add_listener(
    create_hospital_scheduler_listener_cpp(
      parameters = parameters,
      variables = variables,
      events = events
    )
  )

  # ICU (hospitalised, mechanical ventilation) ----------

  events$ICU_get_live$add_listener(
    create_state_update_listener(
      variables$states,
      "ICUGetLive"
    )
  )

  events$ICU_get_live$add_listener(
    create_event_scheduler_listener(
      event = events$stepdown,
      duration = parameters$dur_get_mv_survive,
      func = make_rerlang,
      shift = shift,
      dt = dt
    )
  )

  events$ICU_get_die$add_listener(
    create_state_update_listener(
      variables$states,
      "ICUGetDie"
    )
  )

  events$ICU_get_die$add_listener(
    create_event_scheduler_listener(
      event = events$death,
      duration = parameters$dur_get_mv_die,
      func = make_rerlang,
      shift = shift,
      dt = dt
    )
  )

  events$ICU_not_get_live$add_listener(
    create_state_update_listener(
      variables$states,
      "ICUNotGetLive"
    )
  )

  events$ICU_not_get_live$add_listener(
    create_event_scheduler_listener(
      event = events$recovery,
      duration = parameters$dur_not_get_mv_survive,
      func = make_rerlang,
      shift = shift,
      dt = dt
    )
  )

  events$ICU_not_get_die$add_listener(
    create_state_update_listener(
      variables$states,
      "ICUNotGetDie"
    )
  )

  events$ICU_not_get_die$add_listener(
    create_event_scheduler_listener(
      event = events$death,
      duration = parameters$dur_not_get_mv_die,
      func = make_rerlang,
      shift = shift,
      dt = dt
    )
  )

  # hosp (hospitalised, oxygen) ----------

  events$hosp_get_live$add_listener(
    create_state_update_listener(
      variables$states,
      "hospGetLive"
    )
  )

  events$hosp_get_live$add_listener(
    create_event_scheduler_listener(
      event = events$recovery,
      duration = parameters$dur_get_ox_survive,
      func = make_rerlang,
      shift = shift,
      dt = dt
    )
  )

  events$hosp_get_die$add_listener(
    create_state_update_listener(
      variables$states,
      "hospGetDie"
    )
  )

  events$hosp_get_die$add_listener(
    create_event_scheduler_listener(
      event = events$death,
      duration = parameters$dur_get_ox_die,
      func = make_rerlang,
      shift = shift,
      dt = dt
    )
  )

  events$hosp_not_get_live$add_listener(
    create_state_update_listener(
      variables$states,
      "hospNotGetLive"
    )
  )

  events$hosp_not_get_live$add_listener(
    create_event_scheduler_listener(
      event = events$recovery,
      duration = parameters$dur_not_get_ox_survive,
      func = make_rerlang,
      shift = shift,
      dt = dt
    )
  )

  events$hosp_not_get_die$add_listener(
    create_state_update_listener(
      variables$states,
      "hospNotGetDie"
    )
  )

  events$hosp_not_get_die$add_listener(
    create_event_scheduler_listener(
      event = events$death,
      duration = parameters$dur_not_get_ox_die,
      func = make_rerlang,
      shift = shift,
      dt = dt
    )
  )


  # Recovery events
  events$recovery$add_listener(
    create_state_update_listener(
      variables$states,
      "R"
    )
  )

  if (is.finite(parameters$dur_R)) {
    events$recovery$add_listener(
      create_event_scheduler_listener(
        event = events$immunity_loss,
        duration = parameters$dur_R,
        func = make_rerlang,
        shift = shift,
        dt = dt
      )
    )
  }

  # Stepdown events
  events$stepdown$add_listener(
    create_state_update_listener(
      variables$states,
      "IRec"
    )
  )

  events$stepdown$add_listener(
    create_event_scheduler_listener(
      event = events$recovery,
      duration = parameters$dur_rec,
      func = make_rerlang,
      shift = shift,
      dt = dt
    )
  )

  # Death events
  events$death$add_listener(
    create_state_update_listener(
      variables$states,
      "D"
    )
  )

  # Loss of immunity
  events$immunity_loss$add_listener(
    create_state_update_listener(
      variables$states,
      "S"
    )
  )

}

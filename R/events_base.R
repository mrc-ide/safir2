# base set of events for safir2 models

#' @title Create events
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

  # pop size
  N <- sum(parameters$population)

  list(
    # Human infection events
    exposure = TargetedEvent$new(N), # S->E, scheduled by infection_process_zzz
    mild_infection = TargetedEvent$new(N), # E->IMild, scheduled by create_exposure_update_listener
    asymp_infection = TargetedEvent$new(N),
    severe_infection = TargetedEvent$new(N),
    hospitilisation = TargetedEvent$new(N),
    imv_get_live = TargetedEvent$new(N), # need ICU, gets bed, lives
    imv_get_die = TargetedEvent$new(N), # need ICU, gets bed, dies
    iox_get_live = TargetedEvent$new(N), # need hosp, gets bed, lives
    iox_get_die = TargetedEvent$new(N), # need hosp, gets bed, dies
    imv_not_get_live = TargetedEvent$new(N), # need ICU, doesn't get bed, lives
    imv_not_get_die = TargetedEvent$new(N), # need ICU, doesn't get bed, dies
    iox_not_get_live = TargetedEvent$new(N), # need hosp, doesn't get bed, lives
    iox_not_get_die = TargetedEvent$new(N), # need hosp, doesn't get bed, dies
    stepdown = TargetedEvent$new(N),
    recovery = TargetedEvent$new(N),
    immunity_loss = TargetedEvent$new(N),
    death = TargetedEvent$new(N)
  )
}

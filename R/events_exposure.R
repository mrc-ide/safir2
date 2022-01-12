



#' @title Modelling the progression to either IMild, ICase, IAsymp
#' @description Age dependent outcome of exposure
#'
#' @param events a list of events in the model
#' @param variables the available human variables
#' @param parameters model parameters
#' @param shift passed to [safir2::make_rerlang]
#' @noRd
create_exposure_scheduler_listener <- function(events, variables, parameters, shift = 0) {

  dt <- parameters$dt
  stopifnot(is_finite_numeric(dt))

  ICase_delay <- make_rerlang(mu = parameters$dur_E, dt = dt, shift = shift)
  IMild_delay <- make_rerlang(mu = parameters$dur_E, dt = dt, shift = shift)
  IAsymp_delay <- make_rerlang(mu = parameters$dur_E, dt = dt, shift = shift)

  get_prob_hosp <- make_get_age_probabilities(parameters = parameters, name = "prob_hosp")
  get_prob_asymp <- make_get_age_probabilities(parameters = parameters, name = "prob_asymp")

  get_ve_severe <- make_get_ve_severe(variables = variables, parameters = parameters)

  return(
    function(timestep, target) {

      disc_ages <- variables$discrete_age$get_values(target)
      prob_hosp <- get_prob_hosp(timestep = get_prob_hosp, ages = disc_ages)
      hosp <- target$copy()

      # calculate NAT protection against severe disease
      ve_severe <- get_ve_severe(timestep = timestep, target = hosp)

      # sample those with severe disease
      hosp$sample(prob_hosp * ve_severe)

      # those without severe disease
      not_hosp <- target$set_difference(hosp)

      if (hosp$size() > 0) {
        events$severe_infection$schedule(target = hosp, delay = ICase_delay(n = hosp$size()))
      }

      # sample asymptomatic and mild disease persons
      if (not_hosp$size() > 0) {
        disc_ages <- variables$discrete_age$get_values(not_hosp)
        prob_asymp <- get_prob_asymp(timestep = timestep, ages = disc_ages)

        to_asymp <- not_hosp$copy()
        to_asymp$sample(prob_asymp)

        not_to_asymp <- not_hosp$set_difference(to_asymp)

        if (to_asymp$size() > 0) {
          events$asymp_infection$schedule(target = to_asymp, delay = IAsymp_delay(n = to_asymp$size()))
        }

        if (not_to_asymp$size() > 0) {
          events$mild_infection$schedule(target = not_to_asymp, delay = IMild_delay(n = not_to_asymp$size()))
        }

      }

    }
  )

}

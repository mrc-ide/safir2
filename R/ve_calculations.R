#' @title Make function to calculate population NAT
#' @description This returns a function taking a single argument
#' `target` (a [individual::Bitset]) which will return the natural log scale NAT for each person in `index`.
#' @param variables a named list
#' @return a function
#' @export
make_get_NAT <- function(variables) {
  if (inherits(variables$NAT_vaccine, "DoubleVariable") & inherits(variables$NAT_infection, "DoubleVariable")) {
    NAT_vaccine <- variables$NAT_vaccine
    NAT_infection <- variables$NAT_infection
    return(
      function(target) {
        nat_vaccine <- NAT_vaccine$get_values(target)
        nat_infection <- NAT_infection$get_values(target)
        nat_overall <- exp(nat_vaccine) + exp(nat_infection)
        nat_overall <- log(nat_overall)
        return(nat_overall)
      }
    )
  } else if (inherits(variables$NAT, "DoubleVariable")) {
    NAT <- variables$NAT
    return(
      function(target) {
        NAT$get_values(target)
      }
    )
  } else {
    return(
      function(target) {
        rep(-Inf, target$size())
      }
    )
  }
}

#' @title Function for efficacy against infection
#' @description \deqn{1 - \left( \frac{1}{1 + e^{-k \cdot \log_{10}(NAT) - \log_{10}(ab_{50})}} \right) }
#' @param NAT neutralizing antibody titre on a linear scale
#' @param k half maximum parameter
#' @param ab_50 scaling parameter
ve_infection_functional_form <- function(NAT, k, ab_50) {
  ve_infection <- 1 / (1 + exp(-k * (log10(NAT) - log10(ab_50))))
  return(1 - ve_infection)
}

# make_get_ve_infection <- function(variables, parameters) {
#
# }



#' @title Make function to calculate efficacy against severe disease
#' @description Return a function taking two arguments `timestep` and `target` (a [individual::Bitset])
#' which will calculate the efficacy against severe disease from neutralizing antibody titre.
#' @param variables a named list
#' @param parameters a named list
#' @return a function
#' @export
make_get_ve_severe <- function(variables, parameters) {

  get_vfr <- make_get_vector(parameters = parameters, name = "vfr")
  get_NAT <- make_get_NAT(variables = variables)

  k <- parameters$k
  ab_50 <- parameters$ab_50
  ab_50_severe <- parameters$ab_50_severe

  # function to calculate efficacy
  get_ve <- function(timestep, target) {

    # get VFR and NAT
    VFR <- get_vfr(timestep)
    NAT <- get_NAT(target)

    # calculation efficacy
    ve_severe <- rep(1, target$size())

    if (any(is.finite(NAT))) {
      pos_NAT <- which(is.finite(NAT))
      NAT_linear <- exp(NAT[pos_NAT])
      NAT_linear <- pmax(.Machine$double.eps, NAT_linear / VFR)

      ve_infection <- ve_infection_functional_form(NAT = NAT_linear, k = k, ab_50 = ab_50)

      ve_severe_uncond <- 1 / (1 + exp(-k * (log10(NAT_linear) - log10(ab_50_severe))))
      ve_severe[pos_NAT] <-  1 - ((1 - ve_severe_uncond)/ve_infection) # 1 - (1 - ef_infection) goes from hazard reduction scale to efficacy, simplifies to ef_infection
      ve_severe[pos_NAT] <- 1 - ve_severe[pos_NAT]
      return(ve_severe)

    } else {
      return(ve_severe)
    }
  }

  return(get_ve)
}

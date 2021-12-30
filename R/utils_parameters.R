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

  if (is.matrix(parameters[[name]])) {
    dt <- parameters$dt
    function(timestep, ages) {
      day <- ceiling(timestep * dt)
      probs[cbind(ages, day)]
    }
  } else {
    return(
      function(timestep, ages) {
        probs[ages]
      }
    )
  }
}

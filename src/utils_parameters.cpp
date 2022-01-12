#include "../inst/include/utils_parameters.hpp"


/* helper function to get current day */
size_t get_day(const size_t timestep, const double dt) {
  return static_cast<size_t>(std::ceil(static_cast<double>(timestep) * dt)) - 1;
};


/*
  get function that returns the current contact matrix;
  takes args: (size_t timestep)
*/
get_contact_matrix_fn make_get_contact_matrix(const Rcpp::List& parameters) {

  SEXP mix_mat_set = parameters["mix_mat_set"];
  SEXP dims = Rf_getAttrib(mix_mat_set, R_DimSymbol);
  int d1 = INTEGER(dims)[0];
  int d2 = INTEGER(dims)[1];
  int d3 = INTEGER(dims)[2];
  double* mix_mat_ptr = REAL(mix_mat_set);

  // not time varying
  if (d1 == 1) {

    Rcpp::NumericMatrix mix_mat(d2, d3);

    for (auto j = 0u; j < d2; ++j) {
      for (auto k = 0u; k < d3; ++k) {
        mix_mat(j,k) = mix_mat_ptr[0 + (j * d1) + (k * d1 * d2)];
      }
    }

    return [mix_mat] (const size_t timestep) -> Rcpp::NumericMatrix {
      return mix_mat;
    };
    // time varying
  } else {
    double dt = Rcpp::as<double>(parameters["dt"]);
    return [mix_mat_ptr, d1, d2, d3, dt] (const size_t timestep) -> Rcpp::NumericMatrix {

      size_t day = get_day(timestep, dt);
      Rcpp::NumericMatrix mix_mat(d2, d3);

      for (auto j = 0u; j < d2; ++j) {
        for (auto k = 0u; k < d3; ++k) {
          mix_mat(j,k) = mix_mat_ptr[day + (j * d1) + (k * d1 * d2)];
        }
      }

      return mix_mat;
    };
  }

};

// R interface for testing

// [[Rcpp::export]]
Rcpp::XPtr<get_contact_matrix_fn> make_get_contact_matrix_rcpp(const Rcpp::List& parameters) {
  return Rcpp::XPtr<get_contact_matrix_fn>(
    new get_contact_matrix_fn(make_get_contact_matrix(parameters)),
    true
  );
}

// [[Rcpp::export]]
Rcpp::NumericMatrix eval_get_contact_matrix_rcpp(Rcpp::XPtr<get_contact_matrix_fn> func, const size_t timestep) {
  return (*func.get())(timestep);
};



/*
 get function that returns the current value of vector (or scalar) parameter
 takes args: (size_t timestep)
 */
get_vector_fn make_get_vector(const Rcpp::List& parameters, const std::string name) {
  if (name.compare("dt") == 0) {
    Rcpp::stop("element 'name' must not be 'dt'");
  }

  SEXP param;
  if (parameters.containsElementNamed(name.c_str())) {
    int index = parameters.findName(name);
    param = parameters[index];
  } else {
    Rcpp::stop("no element named 'name' in parameters list");
  }

  if (!Rf_isReal(param)) {
    Rcpp::stop("element 'name' must be a numeric vector");
  }

  // not time-varying
  if (Rf_length(param) == 1) {
    double val = Rf_asReal(param);
    // return lambda
    return [val] (const size_t timestep) -> double {
      return val;
    };
  } else {
    // time varying
    double dt = Rcpp::as<double>(parameters["dt"]);
    double* param_ptr = REAL(param);
    // return lambda
    return [param_ptr, dt] (const size_t timestep) -> double {
      size_t day = get_day(timestep, dt);
      return param_ptr[day];
    };
  }
};

// [[Rcpp::export]]
Rcpp::XPtr<get_vector_fn> make_get_vector_rcpp(const Rcpp::List& parameters, const std::string name) {
  return Rcpp::XPtr<get_vector_fn>(
    new get_vector_fn(make_get_vector(parameters, name)),
    true
  );
}

// [[Rcpp::export]]
double eval_get_vector_fn_rcpp(Rcpp::XPtr<get_vector_fn> func, const size_t timestep) {
  return (*func.get())(timestep);
};


/*
  age-structured (and optionally time) transition probabilities
  takes args: (const size_t timestep, const std::vector<int>& ages)
*/
get_age_probabilities_fn make_get_age_probabilities(const Rcpp::List& parameters, const std::string name) {
  if (name.compare("dt") == 0) {
    Rcpp::stop("element 'name' must not be 'dt'");
  }

  SEXP param;
  if (parameters.containsElementNamed(name.c_str())) {
    int index = parameters.findName(name);
    param = parameters[index];
  } else {
    Rcpp::stop("no element named 'name' in parameters list");
  }

  if (!Rf_isReal(param)) {
    Rcpp::stop("element 'name' must be a numeric object");
  }

  size_t N_age = Rcpp::as<size_t>(parameters["N_age"]);
  size_t time_period = Rcpp::as<size_t>(parameters["time_period"]);
  double* probs_ptr = REAL(param);

  // time varying
  if (Rf_isMatrix(param)) {
    if (Rf_ncols(param) != time_period || Rf_nrows(param) != N_age) {
      Rcpp::stop("matrix element 'name' must have 'N_age' rows and 'time_period' columns");
    }
    double dt = Rcpp::as<double>(parameters["dt"]);
    // return lambda
    return [probs_ptr, dt, N_age] (const size_t timestep, const std::vector<int>& ages) -> std::vector<double> {
      std::vector<double> probs(ages.size());
      size_t day = get_day(timestep, dt);
      for (auto i = 0u; i < ages.size(); ++i) {
        probs[i] = probs_ptr[day *  N_age + (ages[i] - 1)];
      }
      return probs;
    };
  } else {
    // constant
    if (Rf_length(param) != N_age) {
      Rcpp::stop("vector element 'name' must have 'N_age' elements");
    }
    // return lambda
    return [probs_ptr] (const size_t timestep, const std::vector<int>& ages) -> std::vector<double> {
      std::vector<double> probs(ages.size());
      for (auto i = 0u; i < ages.size(); ++i) {
        probs[i] = probs_ptr[ages[i] - 1];
      }
      return probs;
    };

  }
};

// [[Rcpp::export]]
Rcpp::XPtr<get_age_probabilities_fn> make_get_age_probabilities_rcpp(const Rcpp::List& parameters, const std::string name) {
  return Rcpp::XPtr<get_age_probabilities_fn>(
    new get_age_probabilities_fn(make_get_age_probabilities(parameters, name)),
    true
  );
}

// [[Rcpp::export]]
std::vector<double> eval_get_age_probabilities_fn_rcpp(Rcpp::XPtr<get_age_probabilities_fn> func, const size_t timestep, const std::vector<int>& ages) {
  return (*func.get())(timestep, ages);
};

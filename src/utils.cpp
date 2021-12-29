#include "../inst/include/utils.hpp"

size_t get_day(const size_t timestep, const double dt) {
  return static_cast<size_t>(std::ceil((double)timestep * dt) - 1.0);
};

get_contact_matrix_fn make_get_contact_matrix(const Rcpp::List& parameters) {

  SEXP mix_mat_set = parameters["mix_mat_set"];
  SEXP dims = Rf_getAttrib(mix_mat_set, R_DimSymbol);
  int d1 = INTEGER(dims)[0];
  int d2 = INTEGER(dims)[1];
  int d3 = INTEGER(dims)[2];
  double* mix_mat_ptr = REAL(mix_mat_set);

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

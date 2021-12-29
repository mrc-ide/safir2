#ifndef UTILS_HPP
#define UTILS_HPP

#include <functional>
#include <Rcpp.h>
#include <individual.h>

size_t get_day(const size_t timestep, const double dt);


// function to get contact matrix
using get_contact_matrix_fn = std::function<Rcpp::NumericMatrix(const size_t)>;

get_contact_matrix_fn make_get_contact_matrix(const Rcpp::List& parameters);


#endif

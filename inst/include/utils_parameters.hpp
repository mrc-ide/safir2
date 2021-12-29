#ifndef UTILS_HPP
#define UTILS_HPP

#include <functional>
#include <Rcpp.h>
#include <individual.h>

// helper to go from timestep to nearest day
size_t get_day(const size_t timestep, const double dt);

// function to get contact matrix at day
using get_contact_matrix_fn = std::function<Rcpp::NumericMatrix(const size_t)>;

// function factory to get contact matrices
get_contact_matrix_fn make_get_contact_matrix(const Rcpp::List& parameters);

// function to get vector at day
using get_vector_fn = std::function<double(const size_t)>;

get_vector_fn make_get_vector(const Rcpp::List& parameters, const std::string name);

// function to get age-(time)-probabilities
using get_age_probabilities_fn = std::function<std::vector<double>(const size_t, const std::vector<int>&)>;

get_age_probabilities_fn make_get_age_probabilities(const Rcpp::List& parameters, const std::string name);


#endif

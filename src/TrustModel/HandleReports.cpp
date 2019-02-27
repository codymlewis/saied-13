#include <algorithm>
#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;

/**
 * HandleReports.cpp - Trust Model Simulator
 * C++ code to handle the report calculations
 *
 * Author: Cody Lewis
 * Date: 2019-02-27
 */

/* Find the distance from a target context and the current context */
// [[Rcpp::export]]
int find_dist(int target, int current)
{
    return std::abs(target - current);
}

/* Find the distance from a given report to the target context */
// [[Rcpp::export]]
double report_dist(int C_j, int S_j, int N_j, int C_target, int S_target,
        int eta, int dS_max_sq, int dC_max_sq, int S_max, int C_max)
{
    double shared_part = std::sqrt(
        (dS_max_sq + dC_max_sq) * (
            (std::pow(find_dist(S_target, S_j), 2.0) / dS_max_sq) +
            (std::pow(find_dist(C_target, C_j), 2.0) / dC_max_sq)
        )
    );
    double unique_part;
    if(N_j >= 0) {
        unique_part = std::sqrt(
            (dS_max_sq + dC_max_sq) * (
                std::pow(((S_max - S_j) / (S_max - (S_target - eta))), 2.0) +
                std::pow((C_j / (C_target + eta)), 2.0)
            )
        );
    } else {
        unique_part = std::sqrt(
            (dS_max_sq + dC_max_sq) * (
                std::pow(((C_max - C_j) / (C_max - (C_target - eta))), 2.0) +
                std::pow((S_j / (S_target + eta)), 2.0)
            )
        );
    }
    return std::min(shared_part, unique_part);
}


/*** R
# report_dist(50, 50, 1, 51, 51, 1, 50, 50, 101, 101)
*/

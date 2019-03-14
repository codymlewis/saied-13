#include <cmath>
#include <cstdlib>
#include <ctime>
#include <Rcpp.h>

using namespace Rcpp;

/**
 * Attacks.cpp - Trust Model Simulator
 * C++ code to handle the various attacks
 *
 * Author: Cody Lewis
 * Date: 2019-02-27
 */

/* Make the report worse than it should be */
// [[Rcpp::export]]
int bad_mouth()
{
    return -1;
}

/* Make the report better than it should be */
// [[Rcpp::export]]
int good_mouth()
{
    return 1;
}

/* On a set out interval, change between good mouthing and bad mouthing */
// [[Rcpp::export]]
int on_off(bool is_bad_mouthing)
{
    return is_bad_mouthing ? bad_mouth() : good_mouth();
}

/* Always give a set value for the service context */
// [[Rcpp::export]]
int service_set()
{
    return 50;
}

/* Always give a set value for the capability context */
// [[Rcpp::export]]
int capability_set()
{
    return 50;
}

/* Say that a report is older than it really is */
// [[Rcpp::export]]
int time_decay(int time)
{
    return time - 5;
}

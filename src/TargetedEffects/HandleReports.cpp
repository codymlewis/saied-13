#include <algorithm>
#include <cstddef>
#include <cmath>
#include <iostream>
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

/* Calculate distances and restrict reports based on that,
 * return final distances
 */
// [[Rcpp::export]]
NumericVector restrict_reports(NumericMatrix node_reports, int C_target,
        int S_target, int C_max, int S_max, int eta, int SERVICE_INDEX,
        int CAPABILITY_INDEX, int NOTE_INDEX)
{
    // Get rid of off-by-one from passing from R
    SERVICE_INDEX--;
    CAPABILITY_INDEX--;
    NOTE_INDEX--;

    double dS_max_sq = std::pow(find_dist(S_target, S_max), 2.0);
    double dC_max_sq = std::pow(find_dist(C_target, C_max), 2.0);
    double t = std::sqrt(dS_max_sq + dC_max_sq);
    NumericVector distances(node_reports.nrow());

    for(std::size_t i = 0; i < node_reports.nrow(); ++i) {
        int c_j = node_reports(i, CAPABILITY_INDEX);
        int s_j = node_reports(i, SERVICE_INDEX);
        int n_j = node_reports(i, NOTE_INDEX);
        if(c_j < 0 || s_j < 0) {
            distances[i] = -1;
        } else {
            distances[i] = report_dist(c_j, s_j, n_j, C_target, S_target, eta,
                    dS_max_sq, dC_max_sq, S_max, C_max);
            if(distances[i] >= t) {
                distances[i] = -1;
            }
        }
    }

    return distances;
}

/* Give 1 if note is -1 else 0, emphasises a negative note */
// [[Rcpp::export]]
int find_s(int note_j) {
    return 0.5 * (std::pow(note_j, 2.0) - note_j);
}

/* Assign a weight to a report, based on its distance and time of submission */
// [[Rcpp::export]]
double weight_calc(double lambda, double theta, double dist, int note,
        int current_time, int report_time)
{
    return std::pow(lambda, dist) *
                std::pow(
                        theta,
                        (
                            (find_s(note) + 1) *
                            (current_time - report_time)
                        )
                );
}

/* Iterate through each of the reports and assign weights to each of them */
// [[Rcpp::export]]
NumericVector weigh_reports(double lambda, double theta,
        NumericMatrix node_reports, NumericVector report_distances,
        int current_time, int NOTE_INDEX, int TIME_INDEX)
{
    NOTE_INDEX--;
    TIME_INDEX--;

    NumericVector weights(node_reports.nrow());

    for(size_t i = 0; i < node_reports.nrow(); ++i) {
        if(report_distances[i] < 0) {
            weights[i] = -1;
        } else {
            weights[i] = weight_calc(lambda, theta, report_distances[i],
                    node_reports(i, NOTE_INDEX), current_time,
                    node_reports(i, TIME_INDEX));
        }
    }

    return weights;
}

/* Calculate the trust values of the nodes in the network */
// [[Rcpp::export]]
NumericVector calculate_trust(int total_nodes, NumericMatrix w,
        NumericVector QRs, NumericMatrix reported_notes)
{
    NumericVector trust_values(total_nodes);

    for(size_t i = 0; i < total_nodes; ++i) {
        double numerator = 0;
        double denominator = 0;
        for(size_t j = 0; j < total_nodes; ++j) {
            if(w(i, j) >= 0) {
                numerator += w(i, j) * QRs[j] * reported_notes(i, j);
                denominator += w(i, j);
            }
        }
        trust_values[i] = denominator == 0 ? 0 : numerator / denominator;
    }

    return trust_values;
}

/* Give a value stating the significance of older occurances */
// [[Rcpp::export]]
double find_c_i(double theta, int t_1, int t_i)
{
    return std::pow(theta, (t_1 - t_i));
}

/* Calculate the quality of reccomendation of a node */
// [[Rcpp::export]]
double calculate_QR(double theta, double QRXF, double CF, NumericVector QR,
        NumericVector time_QR)
{
    double numerator = 0;
    double denominator = 0;

    for(size_t i = 0; i < QR.size(); ++i) {
        double c_i = find_c_i(theta, time_QR[1], time_QR[i]);
        numerator += c_i * QR[i] + QRXF;
        denominator += c_i + std::abs(CF);
    }

    double new_QR = denominator == 0 ? 0 : numerator / denominator;
    if(new_QR < -1) {
        new_QR = -1;
    } else if(new_QR > 1) {
        new_QR = 1;
    }

    return new_QR;
}

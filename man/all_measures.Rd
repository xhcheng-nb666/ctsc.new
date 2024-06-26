% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{all_measures}
\alias{all_measures}
\title{all_measures}
\usage{
all_measures(
  score = data_BED_PLANNING_training$TOTAL_SCORE,
  truth = data_BED_PLANNING_training$ADMIT_FLAG == "Admitted",
  thresholds = 1:100,
  n_Boot = 1000,
  use_Boot = FALSE,
  BootID = 1:length(score),
  confidence_level = 0.95
)
}
\arguments{
\item{score}{(optional) A vector of scores or probabilities associated with each observation. Default: Random scores between 0 and 100.}

\item{truth}{(optional) A vector indicating the true class labels of the observations. Default: Random TRUE/FALSE values.}

\item{thresholds}{(optional) A numeric vector specifying the thresholds for binary classification. Default: 50.}

\item{n_Boot}{(optional) The number of bootstrap iterations for confidence interval estimation. Default: 1000.}

\item{use_Boot}{(optional) A logical indicating whether to use bootstrapping for confidence intervals. Default: FALSE.}

\item{BootID}{(optional) Indices for bootstrap sampling. Default: 1:length(score)}

\item{confidence_level}{(optional) Confidence level for constructing confidence intervals. Default: 0.95.}
}
\value{
A list containing performance measures, performance_measurements_lower_bound, performance_measurements_upper_bound, auc(area under the ROC curve), auc lower bound, auc upper bound, pr(area under the precision-recall curve, or PR AUC), pr lower bound, pr upper bound, percentage_of_truth_in_each_bin, thresholds(used for binning scores))
}
\description{
all_measures description: this function calculates various performance measures and their confidence intervals for binary classification tasks. It provides flexibility in specifying input scores, true labels, thresholds, and confidence levels, and offers the option to use bootstrap resampling for confidence interval estimation.
}
\examples{
# Example 1: Using default parameters
result_default <- all_measures()
# Example 2: Specifying custom parameters
custom_scores <- c(80, 65, 72, 90, 85)
custom_truth <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
custom_thresholds <- c(60, 70, 80)
custom_result <- all_measures(score = custom_scores, truth = custom_truth, thresholds = custom_thresholds, use_Boot = TRUE)
}

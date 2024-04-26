#' An Analysis Function to get statistics
#'
#' From a Actual vector of TRUE and FALSE, and a Prediction vector of True and FALSE, calculate statistics.
#' @param Actual A vector of True and False
#' @param Prediction A vector of True and False
#' @return True Positive(TP) is an outcome where the model correctly predicts the positive class.
#' @return True Negative(TN) is an outcome where the model correctly predicts the negative class.
#' @return False Positive(FP) is an outcome where the  model incorrectly predicts the positive class.
#' @return False Negative(FN) is an outcome where the model incorrectly predicts the negative class.
#' @return Sens: True positive rate, is the probability of a positive test result. conditioned on the individual truly being positive.
#' @return Spec: True negative rate, is the probability of a negative test result, conditioned on the individual truly being negative.
#' @return Accuracy: How close a given set of observations are to their true value
#' @return Balanced Accuracy: The arithmetic mean of sensitivity and specificity, often used to deal with imbalanced datasets.
#' @return Prevalence: The proportion of a population who have a specific characteristic in a given time period.
#' @return Positive Predictive Value: The proportion of positive results that are true positive.
#' @return Negative Predictive Value: The proportion of negative results that are true negative.
#' @return F1 score: The average of precision(i.e. ppv) and recall(i.e. Sens). It signifies that the model can effectively identify positive cases while minimizing false positives and false negatives.
#' @return Youden: A single statistic that captures the performance of a dichotomous diagnostic test.
#' @export
#' @examples
#' x = sample(c(T,F), 1000, replace = T)
#' y = sample(c(T,F), 1000, replace = T)
#' get_stat(x,y)


get_stat <- function(Actual, Prediction) {
  Actual <- factor(Actual, level = c(T, F))
  Prediction <- factor(Prediction, level = c(T, F))

  confusion_matrix = table(Prediction, Actual)
  tp = confusion_matrix[1,1]
  tn = confusion_matrix[2,2]
  fp = confusion_matrix[1,2]
  fn = confusion_matrix[2,1]

  Sens = tp/(tp+fn)
  Spec = tn/(tn+fp)

  accuracy = (tp+tn)/(tp+tn+fp+fn)
  balanced_accuracy = accuracy/2
  prevalence = sum(Actual==TRUE)/length(Actual)
  prevalence = sum(Prediction==TRUE)/length(Actual)
  positive_pred_val = tp/(tp+fp)
  negative_pred_val = tn/(fn+tn)
  f1 = (2*positive_pred_val*Sens)/(positive_pred_val+Sens)
  Youden = Sens + Spec - 1

  Prediction_Results <- c("True Positive" = tp, "True Negative" = tn, "False Positive" = fp, "False Negative" = fn, "Sensitivity" = Sens, "Specificity" = Spec, "Accuracy" = accuracy, "Balanced Accuracy" = balanced_accuracy, "Case Prevalence" = prevalence, "Positive Predictive Value" = positive_pred_val, "Negative Predictive Value" = negative_pred_val, "F1 Score" = f1, "Youden's Index" = Youden)

  return(Prediction_Results)
}




#' Hello
#'
#' Hello description
#' @param no
#' @export
#' @examples
#' hello()
hello <- function() {
  print("Hello, world!")
}



#' Get Performance Measures
#'
#' Get Performance Measures description
#' @param


get_performance_measures <- function(probability = runif(100), actual = sample(c(TRUE, FALSE), 100, replace = TRUE), thresholds = seq(0.1, 1, by = 0.1)) {
  stats <- c("True Positive", "True Negative", "False Positive", "False Negative", "Sensitivity", "Specificity", "Accuracy", "Balanced Accuracy", "Case Prevalence", "Positive Predictive Value", "Negative Predictive Value", "F1 Score", "Youden's Index")
  performance_measure_results <- matrix(0, nrow = length(thresholds), ncol = length(stats), byrow = TRUE)
  for (i in 1:length(thresholds)) {
    current_threshold = thresholds[i]
    prediction <- ifelse(probability > current_threshold, TRUE, FALSE)
    performance_measure_results[i, ] <- get_stat(actual, prediction)
  }
  rownames(performance_measure_results) <- thresholds
  colnames(performance_measure_results) <- stats
  print(performance_measure_results)
}

get_performance_measures()

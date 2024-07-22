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
#' @examples
#' x = sample(c(T,F), 1000, replace = T)
#' y = sample(c(T,F), 1000, replace = T)
#' get_stat(x,y)
#' @export
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







#' get_performance_measures
#'
#' get_performance_measures description: the function calculates various performance measures for binary classification based on provided scores and true labels. It offers flexibility in specifying thresholds for binary classification and rounding options for the output metrics.
#' @param score (optional) A vector of scores or probabilities associated with each observation. Default: Random scores between 0 and 100.
#' @param truth (optional) A vector indicating the true class labels of the observations. Default: Random TRUE/FALSE values.
#' @param thresholds (optional) A numeric vector specifying the thresholds for binary classification. Default: 50.
#' @param round (optional) A logical value indicating whether to round the output metrics. Default: TRUE.
#' @return A data table containing performance measures for each specified threshold, including sensitivity, specificity, balanced accuracy, accuracy, positive predictive value (ppv), negative predictive value (npv), F1 score, and Youden's index.
#' @examples
#' # Example 1: Using default parameters
#' library(data.table)
#' performance_metrics_default <- get_performance_measures()
#' # Example 2: Specifying custom parameters
#' library(data.table)
#' custom_scores <- c(80, 65, 72, 90, 85)
#' custom_truth <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' custom_thresholds <- c(60, 70, 80)
#' custom_metrics <- get_performance_measures(score = custom_scores, truth = custom_truth, thresholds = custom_thresholds, round = FALSE)
#' @export
get_performance_measures = function(score = data_BED_PLANNING_training$TOTAL_SCORE, truth = data_BED_PLANNING_training$ADMIT_FLAG=="Admitted", thresholds = 50, round = TRUE){
  # pred <- prediction(score, true)
  # cutoffs = pred@cutoffs[[1]]
  # sens <- performance(pred, "sens")@y.values[[1]]
  # spec <- performance(pred, "spec")@y.values[[1]]
  # balanced_accuracy = (sens + spec)/2
  # ppv <- performance(pred, "ppv")@y.values[[1]]
  # npv <- performance(pred, "npv")@y.values[[1]]
  # return(list(sens = sens, spec = spec, balanced_accuracy = balanced_accuracy, ppv = ppv, npv = npv, cutoffs = cutoffs))
  #
  # data.table::data.table(t(sapply(thresholds, function(threshold){
  #   predict <- score >= threshold
  #   TNs = sum((truth==0)&(predict==0))
  #   TPs = sum((truth==1)&(predict==1))
  #   FPs = sum((truth==0)&(predict==1))
  #   FNs = sum((truth==1)&(predict==0))
  #
  #
  #
  #   sens = TPs/(TPs + FNs)
  #   sens[is.nan(sens)] = NA
  #   spec = TNs/(TNs + FPs)
  #   spec[is.nan(spec)] = NA
  #   balanced_accuracy = (sens + spec)/2
  #   accuracy = (TNs + TPs)/length(truth)
  #
  #   prevalence = sum(truth)/length(truth)
  #   ppv = (sens * prevalence)/(sens * prevalence + (1 - spec) * (1 - prevalence))
  #   ppv[is.nan(ppv)] = NA
  #   npv = (spec * (1-prevalence))/((1 - sens) * prevalence + spec * (1 - prevalence))
  #   npv[is.nan(npv)] = NA
  #   f1_score = (ppv * npv)*2/(ppv + npv)
  #   f1_score[is.nan(f1_score)] = NA
  #
  #   youden = sens + spec - 1
  #   youden[is.nan(youden)] = NA
  #
  #
  #   if(round){
  #
  #     sens = round(sens, 3)
  #     spec = round(spec, 3)
  #     balanced_accuracy = round(balanced_accuracy, 3)
  #     accuracy = round(accuracy, 3)
  #     f1_score = round(f1_score, 3)
  #     youden = round(youden, 3)
  #
  #   }
  #
  #   return(
  #     c(sens = sens, spec = spec, balanced_accuracy = balanced_accuracy, accuracy = accuracy, ppv = ppv, npv = npv, f1_score = f1_score, youden = youden, TNs = TNs, TPs = TPs, FPs = FPs, FNs = FNs, Ps = TPs + FNs, Ns = TNs + FPs)
  #   )
  # })))

  library(data.table)

  # Assuming 'score' and 'truth' are vectors and 'thresholds' is a numeric vector

  # Convert score and truth to data.table
  dt <- data.table(score = score, truth = truth)

  # Create a data.table with all combinations of score, truth, and thresholds
  # dt_thresholds <- dt[, .(threshold = thresholds), by = .(score, truth)]
  dt_list <- lapply(thresholds, function(th) {
    dt_copy <- copy(dt)  # Copy dt to avoid modifying the original dt
    dt_copy[, threshold := th]  # Add the new column 'threshold'
    return(dt_copy)
  })

  # Combine all data.tables into one
  dt_thresholds <- rbindlist(dt_list)

  # Add a column for predictions
  dt_thresholds[, predict := score >= threshold]

  # Calculate TNs, TPs, FPs, FNs
  dt_thresholds[, TNs := sum(truth == 0 & predict == 0), by = threshold]
  dt_thresholds[, TPs := sum(truth == 1 & predict == 1), by = threshold]
  dt_thresholds[, FPs := sum(truth == 0 & predict == 1), by = threshold]
  dt_thresholds[, FNs := sum(truth == 1 & predict == 0), by = threshold]

  dt_thresholds[, Ps := TPs + FNs]
  dt_thresholds[, Ns := TNs + FPs]

  # Remove duplicates to keep one row per threshold
  dt_thresholds <- unique(dt_thresholds, by = "threshold")

  # Calculate metrics
  dt_thresholds[, sens := TPs / (TPs + FNs)]
  dt_thresholds[is.nan(sens), sens := NA]

  dt_thresholds[, spec := TNs / (TNs + FPs)]
  dt_thresholds[is.nan(spec), spec := NA]

  dt_thresholds[, balanced_accuracy := (sens + spec) / 2]

  dt_thresholds[, accuracy := (TNs + TPs) / (TNs + TPs + FNs + FPs)]

  prevalence <- sum(truth) / length(truth)

  dt_thresholds[, ppv := (sens * prevalence) / (sens * prevalence + (1 - spec) * (1 - prevalence))]
  dt_thresholds[is.nan(ppv), ppv := NA]

  dt_thresholds[, npv := (spec * (1 - prevalence)) / ((1 - sens) * prevalence + spec * (1 - prevalence))]
  dt_thresholds[is.nan(npv), npv := NA]

  dt_thresholds[, f1_score := (2 * ppv * npv) / (ppv + npv)]
  dt_thresholds[is.nan(f1_score), f1_score := NA]

  dt_thresholds[, youden := sens + spec - 1]
  dt_thresholds[is.nan(youden), youden := NA]

  # Select the required columns and print results
  results_dt <- dt_thresholds[, .(threshold, sens, spec, balanced_accuracy, accuracy, ppv, npv, f1_score, youden, TNs, TPs, FPs, FNs, Ps, Ns)]

  # Print results
  # print(results_dt)

  return(as.data.frame(results_dt))

}


#' all_measures
#'
#' all_measures description: this function calculates various performance measures and their confidence intervals for binary classification tasks. It provides flexibility in specifying input scores, true labels, thresholds, and confidence levels, and offers the option to use bootstrap resampling for confidence interval estimation.
#' @param score (optional) A vector of scores or probabilities associated with each observation. Default: Random scores between 0 and 100.
#' @param truth (optional) A vector indicating the true class labels of the observations. Default: Random TRUE/FALSE values.
#' @param thresholds (optional) A numeric vector specifying the thresholds for binary classification. Default: 50.
#' @param n_Boot (optional) The number of bootstrap iterations for confidence interval estimation. Default: 1000.
#' @param use_Boot (optional) A logical indicating whether to use bootstrapping for confidence intervals. Default: FALSE.
#' @param BootID (optional) Indices for bootstrap sampling. Default: 1:length(score)
#' @param confidence_level (optional) Confidence level for constructing confidence intervals. Default: 0.95.
#' @return A list containing performance measures, performance_measurements_lower_bound, performance_measurements_upper_bound, auc(area under the ROC curve), auc lower bound, auc upper bound, pr(area under the precision-recall curve, or PR AUC), pr lower bound, pr upper bound, percentage_of_truth_in_each_bin, thresholds(used for binning scores))
#' @examples
#' # Example 1: Using default parameters
#' result_default <- all_measures()

#' # Example 2: Specifying custom parameters
#' custom_scores <- c(80, 65, 72, 90, 85)
#' custom_truth <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' custom_thresholds <- c(60, 70, 80)
#' custom_result <- all_measures(score = custom_scores, truth = custom_truth, thresholds = custom_thresholds, use_Boot = TRUE)
#' @export
all_measures = function(score = data_BED_PLANNING_training$TOTAL_SCORE,
                        truth = data_BED_PLANNING_training$ADMIT_FLAG == 'Admitted',
                        thresholds = 1:100,
                        n_Boot = 1000,
                        use_Boot = FALSE,
                        BootID = 1:length(score),
                        confidence_level = 0.95){



  alpha = 1 - confidence_level

  pacman::p_load(ROCR, parallel, data.table)

  # score = data_BED_PLANNING_training$TOTAL_SCORE
  # truth = data_BED_PLANNING_training$ADMIT_FLAG == 'Admitted'
  # thresholds = 1:100
  # n_Boot = 1000
  # source("supporting functions.R")



  # get sens spec balanced_accuracy accuracy ppv npv f1_score youden
  performance_measures = get_performance_measures(
    score,
    truth,
    thresholds,
    round = FALSE
  )

  performance_measurements_lower_bound = performance_measurements_upper_bound = list()
  if(!use_Boot){
    # https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/PASS/Confidence_Intervals_for_One-Sample_Sensitivity.pdf
    var_sens = performance_measures$sens * (1 - performance_measures$sens) / performance_measures$Ps
    var_spec = performance_measures$spec * (1 - performance_measures$spec) / performance_measures$Ns

    z = qnorm(1-alpha/2)
    performance_measurements_lower_bound$sens = pmax(0,performance_measures$sens - z * sqrt(var_sens))
    performance_measurements_lower_bound$spec = pmax(0,performance_measures$spec - z * sqrt(var_spec))
    performance_measurements_lower_bound$ppv = pmax(0, performance_measures$ppv - z * sqrt(performance_measures$ppv * (1 - performance_measures$ppv) / (performance_measures$TPs + performance_measures$FPs)))
    performance_measurements_lower_bound$npv = pmax(0, performance_measures$npv - z * sqrt(performance_measures$npv * (1 - performance_measures$npv) / (performance_measures$TNs + performance_measures$FNs)))




    performance_measurements_upper_bound$sens = pmin(1, performance_measures$sens + z * sqrt(var_sens))
    performance_measurements_upper_bound$spec = pmin(1, performance_measures$spec + z * sqrt(var_spec))
    performance_measurements_upper_bound$ppv = pmin(1, performance_measures$ppv + z * sqrt(performance_measures$ppv * (1 - performance_measures$ppv) / (performance_measures$TPs + performance_measures$FPs)))
    performance_measurements_upper_bound$npv = pmin(1, performance_measures$npv + z * sqrt(performance_measures$npv * (1 - performance_measures$npv) / (performance_measures$TNs + performance_measures$FNs)))



    # https://stats.stackexchange.com/questions/475052/calculate-the-confidence-interval-of-a-balanced-accuracy-by-taking-the-mean-of-t
    performance_measurements_lower_bound$balanced_accuracy = pmax(0,performance_measures$balanced_accuracy - z * sqrt((var_sens + var_spec)/4))
    performance_measurements_upper_bound$balanced_accuracy = pmin(1, performance_measures$balanced_accuracy + z * sqrt((var_sens + var_spec)/4))


    performance_measurements_lower_bound$accuracy = pmax(0,performance_measures$accuracy - z * sqrt(performance_measures$accuracy * (1-performance_measures$accuracy)/(performance_measures$TNs[1] + performance_measures$TPs[1] + performance_measures$FNs[1] + performance_measures$FPs[1])))
    performance_measurements_upper_bound$accuracy = pmin(1,performance_measures$accuracy + z * sqrt(performance_measures$accuracy * (1-performance_measures$accuracy)/(performance_measures$TNs[1] + performance_measures$TPs[1] + performance_measures$FNs[1] + performance_measures$FPs[1])))





  }else{
    # sample_indexes = list()
    # unique_BootID = unique(BootID)
    # for(b in 1:n_Boot){
    #   set.seed(b)
    #   sample_indexes[[b]] = sample(unique_BootID, replace = TRUE)
    # }
    # get performance for each bootstrap sample
    # performance_measures_boot = list()
    # boot_scores = boot_truths = list()
    # for(b in 1:n_Boot){
    #   print(b/n_Boot)
    #
    #   boot_score = boot_truth = c()
    #   for(s in 1:length(sample_indexes[[b]])){
    #     boot_score = c(boot_score, score[BootID %in% sample_indexes[[b]][s]])
    #     boot_truth = c(boot_truth, truth[BootID %in% sample_indexes[[b]][s]])
    #   }
    #   boot_scores[[b]] = boot_score
    #   boot_truths[[b]] = boot_truth
    # }

    # start = Sys.time()
    # Assuming sample_indexes is already defined
    # sample_indexes <- ...

    dt <- data.table(BootID, score, truth)
    # Create a cluster
    num_cores <- detectCores() * 3/4  # Leave one core free
    cl <- makeCluster(num_cores)
    unique_BootID = unique(BootID)
    # Export necessary objects and libraries to the cluster
    clusterExport(cl, c("unique_BootID", "dt"), envir = environment())
    clusterEvalQ(cl, library(data.table))

    # Define the function to be run in parallel
    bootstrap_function <- function(b) {
      # Get the sample indexes for this bootstrap iteration
      # sample_index <- sample_indexes[[b]]
      sample_index = sample(unique_BootID, replace = TRUE)
      # Efficient subsetting using data.table
      boot_data <- dt[BootID %in% sample_index]
      # Return the results
      list(score = boot_data$score, truth = boot_data$truth)
    }

    # Run the bootstrap iterations in parallel
    results <- parLapply(cl, 1:n_Boot, bootstrap_function)

    # Extract the scores and truths from the results
    boot_scores <- lapply(results, function(res) res$score)
    boot_truths <- lapply(results, function(res) res$truth)
    stopCluster(cl)


    # if(BootParallel){
    #   print("performance_measures_boot")
    #   start = Sys.time()
    #   clusterExport(cl, c('boot_truths', 'boot_scores', 'thresholds', 'get_performance_measures', 'performance', 'prediction', 'prcurve.ap', 'aucpr.conf.int', "aucpr.conf.int.binomial"))
    #
    #   performance_measures_boot = parSapply(cl, 1:n_Boot, FUN = function(b){
    #     get_performance_measures(boot_scores[[b]], boot_truths[[b]], thresholds, round = FALSE)
    #   }, simplify = FALSE)
    #   end = Sys.time()
    #   end - start
    #
    #
    # }else{
    #   # Stop the cluster
    #   stopCluster(cl)
    # }


    start = Sys.time();
    performance_measures_boot = list()
    for(b in 1:n_Boot){
      performance_measures_boot[[b]] = get_performance_measures(score = boot_scores[[b]], truth = boot_truths[[b]], thresholds, round = FALSE);
    }
    end = Sys.time();end-start




    # calculate the 2.5% and 97.5% confidence bounds
    measurements = colnames(performance_measures)
    for(m in 1:length(measurements)){
      measure = measurements[m]
      measure_per_threshold_for_each_boot = sapply(performance_measures_boot, function(x){
        unlist(x[[measure]])
      })

      performance_measurements_lower_bound[[measure]] = apply(measure_per_threshold_for_each_boot, 1, function(x){
        quantile(x, alpha/2, na.rm = TRUE)
      })

      performance_measurements_upper_bound[[measure]] = apply(measure_per_threshold_for_each_boot, 1, function(x){
        quantile(x, 1-alpha/2, na.rm = TRUE)
      })

    }
  }



  # calculate AUC and confidence bounds
  # auc = performance(prediction(score, truth), "auc")@y.values[[1]]
  # auc = roc.curve(scores.class0 = score[[b]][truth[[b]]==0], scores.class1 = score[[b]][truth[[b]]==1])$auc
  auc = as.numeric(pROC::auc(pROC::roc(truth, score)))

  if(!use_Boot){
    ciAUC = pROC::ci(pROC::roc(truth, score), conf.level = confidence_level)
    # if(auc<0.5){ # this performance() function does not distinguish control vs case. However, pROC::ci does. By doing this, pROC::ci always returns greater than 0.5. This part is to correct this.
    #   auc_lower_bound = 1 - as.numeric(ciAUC)[3]
    #   auc_upper_bound= 1 - as.numeric(ciAUC)[1]
    # }else{
      # auc_lower_bound = as.numeric(ciAUC)[1]
      # auc_upper_bound= as.numeric(ciAUC)[3]
    # }
      auc_lower_bound = as.numeric(ciAUC)[1]
      auc_upper_bound= as.numeric(ciAUC)[3]
  }else{
    auc_boot = c()

    # if(BootParallel){
    #   print("performance prediction")
    #   auc_boot = parSapply(cl, 1:n_Boot, FUN = function(b){
    #     performance(prediction(boot_scores[[b]], boot_truths[[b]]), "auc")@y.values[[1]]
    #   }, simplify = T)
    #
    # }


    # start = Sys.time();
    auc_boot = c()
    for(b in 1:n_Boot){
      # auc_boot[b] = roc.curve(scores.class0 = boot_scores[[b]], weights.class0 = boot_truths[[b]])$auc
      auc_boot[b] = roc.curve(scores.class0 = boot_scores[[b]][boot_truths[[b]]==0], scores.class1 = boot_scores[[b]][boot_truths[[b]]==1])$auc
    }
    # end = Sys.time();end-start #Time difference of 2.891933 mins


    auc_lower_bound = quantile(auc_boot, alpha/2, na.rm = TRUE)
    auc_upper_bound = quantile(auc_boot, 1-alpha/2, na.rm = TRUE)
  }


  # PRROC::pr.curve(score[truth==1], score[truth==0])
  # calculate PR auc and confidence bounds
  pr_temp = prcurve.ap(score[truth==1], score[truth==0])
  # pr_temp =   PRROC::pr.curve(score[truth==1], score[truth==0])
  pr = pr_temp$area

  if(!use_Boot){
    ciPR = pr_temp
    pr_lower_bound = ciPR$conf.int[1]
    pr_upper_bound = ciPR$conf.int[2]
  }else{
    # pr_boot = c()
    # if(BootParallel){
    #
    #   print("prcurve.ap")
    #
    #   pr_boot = parSapply(cl, 1:n_Boot, FUN = function(b){
    #     prcurve.ap(boot_scores[[b]][boot_truths[[b]]==1], boot_scores[[b]][boot_truths[[b]]==0])$area
    #   }, simplify = T)
    #
    #   stopCluster(cl)
    #
    # }
    # start = Sys.time();
    pr_boot = c()
    for(b in 1:n_Boot){
      pr_boot[b] = ctsc.dev:::prcurve.ap(boot_scores[[b]][boot_truths[[b]]==1], boot_scores[[b]][boot_truths[[b]]==0])$area
    }
    # end = Sys.time();end-start #Time difference of 2.891933 mins

    pr_lower_bound = quantile(pr_boot, alpha/2, na.rm = TRUE)
    pr_upper_bound = quantile(pr_boot, 1-alpha/2, na.rm = TRUE)
  }


  # calibration curve
  score_bin = cut(score, breaks = thresholds)
  percentage_of_truth_in_each_bin = by(truth, score_bin, function(x){sum(x)/length(x)})



  return(list(

    performance_measures = performance_measures,
    performance_measurements_lower_bound = performance_measurements_lower_bound,
    performance_measurements_upper_bound = performance_measurements_upper_bound,
    auc = auc,
    auc_lower_bound = auc_lower_bound,
    auc_upper_bound= auc_upper_bound,
    pr = pr,
    pr_lower_bound = pr_lower_bound,
    pr_upper_bound = pr_upper_bound,
    percentage_of_truth_in_each_bin = percentage_of_truth_in_each_bin,
    thresholds = thresholds


  ))


}





#' summary_stat
#'
#' summary_stat description: this function computes summary statistics for a given numeric vector, including mean, standard deviation, minimum, lower quantiles, median, upper quantiles, maximum, and range.
#' @param x Numeric vector for which summary statistics are computed
#' @param digits (optional) Number of digits to round the summary statistics to (default is 2).
#' @param quantiles (optional) Numeric vector specifying the quantiles to compute. Defaults to c(5, 25, 75, 95).
#' @return A named numeric vector containing the computed summary statistics.
#' @examples
#' #Example 1:
#' data <- runif(n = 50, min = 1, max = 100)
#' summary_stat(data)
#' #Example 2:
#' summary_stat(data, digits = 3, quantiles = c(10, 50, 100))
#' @export
summary_stat = function(x, digits = 2, quantiles = c(5, 25, 75, 95)) {

  minimum = min(x)
  maximum = max(x)
  range = maximum - minimum
  mean = mean(x)
  median = median(x)
  standard_deviation = sd(x)

  lower_quantiles <- quantiles[quantiles < 50]
  upper_quantiles <- quantiles[quantiles > 50]

  summary_lower_quantile = unname(quantile(x, probs = lower_quantiles * 0.01))
  summary_upper_quantile = unname(quantile(x, probs = upper_quantiles * 0.01))


  result <- round(c(mean, standard_deviation, minimum, summary_lower_quantile, median, summary_upper_quantile, maximum, range), digits = digits)

  names(result) = c("Mean", "SD", "Min", paste0(lower_quantiles, "th Quantile"), "Median", paste0(upper_quantiles, "th Quantile"), "Max", "Range")
  return(result)
}



#' from_measures_to_data_frame
#'
#' from_measures_to_data_frame description: this function computes summary statistics for a given numeric vector, including mean, standard deviation, minimum, lower quantiles, median, upper quantiles, maximum, and range.
#' @param x Numeric vector for which summary statistics are computed
#' @param digits (optional) Number of digits to round the summary statistics to (default is 2).
#' @param quantiles (optional) Numeric vector specifying the quantiles to compute. Defaults to c(5, 25, 75, 95).
#' @return A named numeric vector containing the computed summary statistics.
#' @examples
#' #Example 1:
#' data <- runif(n = 50, min = 1, max = 100)
#' summary_stat(data)
#' #Example 2:
#' summary_stat(data, digits = 3, quantiles = c(10, 50, 100))
#' @export
from_measures_to_data_frame = function(measures = all_measures,
                                       include_threholds = TRUE,
                                       include_true_positive = FALSE,
                                       include_true_negative = FALSE,
                                       include_false_positive = FALSE,
                                       include_false_negative = FALSE,
                                       include_sensitivity = TRUE,
                                       include_specificity = TRUE,
                                       include_positive_predictive_value = TRUE,
                                       include_negative_predictive_value = TRUE,
                                       include_balanced_accuracy = TRUE,
                                       include_accuracy = TRUE,
                                       include_youden = FALSE){

  pacman::p_load(dplyr, kableExtra)

  thresholds = measures$thresholds

  sens = measures$performance_measures$sens
  sens_ci_low = measures$performance_measurements_lower_bound$sens
  sens_ci_up = measures$performance_measurements_upper_bound$sens

  spec = measures$performance_measures$spec
  spec_ci_low = measures$performance_measurements_lower_bound$spec
  spec_ci_up = measures$performance_measurements_upper_bound$spec

  ppv = measures$performance_measures$ppv
  ppv_ci_low = measures$performance_measurements_lower_bound$ppv
  ppv_ci_up = measures$performance_measurements_upper_bound$ppv

  npv = measures$performance_measures$npv
  npv_ci_low = measures$performance_measurements_lower_bound$npv
  npv_ci_up = measures$performance_measurements_upper_bound$npv

  if(!is.null(measures$performance_measurements_lower_bound$balanced_accuracy)){
    balanced_accuracy = measures$performance_measures$balanced_accuracy
    balanced_accuracy_ci_low = measures$performance_measurements_lower_bound$balanced_accuracy
    balanced_accuracy_ci_up = measures$performance_measurements_upper_bound$balanced_accuracy
  }else{
    balanced_accuracy = 0
    balanced_accuracy_ci_low = 0
    balanced_accuracy_ci_up = 0
  }
  if(!is.null(measures$performance_measurements_lower_bound$accuracy)){
    accuracy = measures$performance_measures$accuracy
    accuracy_ci_low = measures$performance_measurements_lower_bound$accuracy
    accuracy_ci_up = measures$performance_measurements_upper_bound$accuracy
  }else{
    accuracy = 0
    accuracy_ci_low = 0
    accuracy_ci_up = 0
  }



  TNs = measures$performance_measures$TNs
  TPs = measures$performance_measures$TPs
  FNs = measures$performance_measures$FNs
  FPs = measures$performance_measures$FPs

  # auc = measures$auc
  # auc_ci_low = measures$auc_lower_bound
  # auc_ci_up = measures$auc_upper_bound

  result = data.frame(thresholds,
                      TPs = TPs,
                      TNs = TNs,
                      FPs = FPs,
                      FNs = FNs,
                      sens = round(sens, digits = 3),
                      sens_ci = paste0("[", round(sens_ci_low, digits = 3), ", ", round(sens_ci_up, digits = 3), "]"),
                      spec = round(spec, digits = 3),
                      spec_ci = paste0("[", round(spec_ci_low, digits = 3), ", ", round(spec_ci_up, digits = 3), "]"),
                      ppv = round(ppv, digits = 3),
                      ppv_ci = paste0("[", round(ppv_ci_low, digits = 3), ", ", round(ppv_ci_up, digits = 3), "]"),
                      npv = round(npv, digits = 3),
                      npv_ci = paste0("[", round(npv_ci_low, digits = 3), ", ", round(npv_ci_up, digits = 3), "]"),
                      bal_acc = round(balanced_accuracy, digits = 3),
                      bal_acc_ci = paste0("[", round(balanced_accuracy_ci_low, digits = 3), ", ", round(balanced_accuracy_ci_up, digits = 3), "]"),
                      acc = round(accuracy, digits = 3),
                      acc_ci = paste0("[", round(accuracy_ci_low, digits = 3), ", ", round(accuracy_ci_up, digits = 3), "]"),
                      youden = round(measures$performance_measures$youden, digits = 3))

  colnames(result) = c("Thresholds",
                       "True Positive",
                       "True Negative",
                       "False Positive",
                       "False Negative",
                       "Sensitivity",
                       "Sensitivity CI",
                       "Specificity",
                       "Specificity CI",
                       "Positive Predictive Value",
                       "Positive Predictive Value CI",
                       "Negative Predictive Value",
                       "Negative Predictive Value CI",
                       "Balanced Accuracy",
                       "Balanced Accuracy CI",
                       "Accuracy",
                       "Accuracy CI",
                       "Youden's Index")


  if(!include_threholds){
    result$Thresholds = NULL
  }

  if(!include_true_positive){
    result$`True Positive` = NULL
  }

  if(!include_true_negative){
    result$`True Negative` = NULL
  }

  if(!include_false_positive){
    result$`False Positive` = NULL
  }

  if(!include_false_negative){
    result$`False Negative` = NULL
  }

  if(!include_sensitivity){
    result$Sensitivity = NULL
    result$`Sensitivity CI` = NULL
  }

  if(!include_specificity){
    result$Specificity = NULL
    result$`Specificity CI` = NULL
  }

  if(!include_positive_predictive_value){
    result$`Positive Predictive Value` = NULL
    result$`Positive Predictive Value CI` = NULL
  }

  if(!include_negative_predictive_value){
    result$`Negative Predictive Value` = NULL
    result$`Negative Predictive Value CI` = NULL
  }

  if(!include_balanced_accuracy){
    result$`Balanced Accuracy` = NULL
    result$`Balanced Accuracy CI` = NULL
  }

  if(!include_accuracy){
    result$Accuracy = NULL
    result$`Accuracy CI` = NULL
  }

  if(!include_youden){
    result[["Youden's Index"]] = NULL
  }

  # result = result %>% kable(format='html',align="ccc", caption=caption) %>% kable_classic(full_width = F)

  return(result)

}








#' from_measures_to_rmarkdown_table
#'
#' from_measures_to_rmarkdown_table description:
#' @param x Numeric vector for which summary statistics are computed
#' @return A named numeric vector containing the computed summary statistics.
#' @examples
#' #Example 1:
#' @export
from_measures_to_rmarkdown_table = function(measures = all_measures, caption = "Performance Measurements using Different Threshold",
                                            include_threholds = TRUE,
                                            include_true_positive = FALSE,
                                            include_true_negative = FALSE,
                                            include_false_positive = FALSE,
                                            include_false_negative = FALSE,
                                            include_sensitivity = TRUE,
                                            include_specificity = TRUE,
                                            include_positive_predictive_value = TRUE,
                                            include_negative_predictive_value = TRUE,
                                            include_balanced_accuracy = TRUE,
                                            include_accuracy = TRUE,
                                            include_youden = TRUE){

  result_df = from_measures_to_data_frame(measures, include_threholds, include_true_positive, include_true_negative, include_false_positive, include_false_negative, include_sensitivity, include_specificity, include_positive_predictive_value, include_negative_predictive_value, include_balanced_accuracy, include_accuracy, include_youden= include_youden)

  result = result_df %>% kable(format='html',align="ccc", caption=caption) %>% kable_classic(full_width = F)

  return(result)

}













#' from_measures_to_rmarkdown_DT
#'
#' from_measures_to_rmarkdown_DT description:
#' @param x Numeric vector for which summary statistics are computed
#' @return A named numeric vector containing the computed summary statistics.
#' @examples
#' #Example 1:
#' @export
from_measures_to_rmarkdown_DT = function(measures = all_measures, caption = "Performance Measurements using Different Threshold",
                                            include_threholds = TRUE,
                                            include_true_positive = FALSE,
                                            include_true_negative = FALSE,
                                            include_false_positive = FALSE,
                                            include_false_negative = FALSE,
                                            include_sensitivity = TRUE,
                                            include_specificity = TRUE,
                                            include_positive_predictive_value = TRUE,
                                            include_negative_predictive_value = TRUE,
                                            include_balanced_accuracy = TRUE,
                                            include_accuracy = TRUE,
                                            include_youden = TRUE,
                                         pageLength = 20){

  result_df = from_measures_to_data_frame(measures, include_threholds, include_true_positive, include_true_negative, include_false_positive, include_false_negative, include_sensitivity, include_specificity, include_positive_predictive_value, include_negative_predictive_value, include_balanced_accuracy, include_accuracy, include_youden = include_youden)

  # result = result_df %>% kable(format='html',align="ccc", caption=caption) %>% kable_classic(full_width = F)


  return(DT::datatable(result_df, rownames = FALSE, options = list(
    pageLength = pageLength
  ), caption = caption))

}









#' from_any_data_frame_to_rmarkdown_using_DT
#'
#' from_any_data_frame_to_rmarkdown_using_DT description: xxx
#' @param x xxxxxxxxxxxxxxxx
#' @examples
#' #Example 1:
#' xxxxxxxxxxxxxxx
#' @export
from_any_data_frame_to_rmarkdown_using_DT = function(df, caption = "", pageLength = 20){
  # pacman::p_load(kableExtra, dplyr)
  # as.data.frame(table) %>%
  #   kbl(caption = caption) %>%
  #   kable_paper("hover", full_width = F)

  if(!identical(sum(abs(as.numeric(df[[1]]) - 1:nrow(df))),0)){

    df = cbind(data.frame(o = 1:nrow(df)), df)

  }

  DT::datatable(df, rownames = FALSE, options = list(
    pageLength = pageLength
  ), caption = caption)
}



































from_measure_to_calibration_plot = function(measures = BED_PLANNING_training_measures, title = "Calibration Curve"){
  pacman::p_load(ggplot2)
  truth_in_each_bin = as.numeric(measures$percentage_of_truth_in_each_bin)
  truth_in_each_bin[is.na(truth_in_each_bin)] = 0
  df = data.frame(y = truth_in_each_bin,
                  x = factor(as.character(1:length(truth_in_each_bin)), levels = as.character(1:length(truth_in_each_bin))),
                  xlabels = names(measures$percentage_of_truth_in_each_bin))


  ggplot(df, aes(x=x, y=y)) +
    geom_col() +
    scale_x_discrete(labels = df$xlabels)+
    labs(y = "Percentage of prevalence", x = 'Bins', title = title) + theme_bw() + ylim(0,1)

}

from_measure_to_ROC_curve = function(measures = BED_PLANNING_training_measures, title = "ROC Curve", legend = TRUE, legend_position = 'bottom', xlab = '1 - Specificity', ylab = 'Sensitivity'){

  pacman::p_load(ggplot2)

  df = data.frame(x = 1-c(0,measures$performance_measures$spec,1),
                  y = c(1, measures$performance_measures$sens, 0,
                        1, measures$performance_measurements_lower_bound$sens, 0,
                        1, measures$performance_measurements_upper_bound$sens, 0),
                  type = rep(c('values',"lower_bound","upper_bound"), each = length(measures$performance_measures$spec)+2))
  df$type = factor(df$type, levels = c('values',"lower_bound","upper_bound"))

  df = df[nrow(df):1, ]

  ggplot(data=df, aes(x=x, y=y, group = type)) +
    geom_line(aes(linetype = type))+
    geom_point(aes(size = type))+
    scale_linetype_manual(values=c("solid","dotted","dotted"))+
    scale_size_manual(values = c(1, 0, 0))+
    theme_bw() +
    ylim(0,1)+
    xlim(0,1)+
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), size = 0.1, linetype = "dashed")+
    labs(title = title, x = xlab, y = ylab)+
    annotate(geom="text", x=0.5, y=0.1, label=paste0("AUC = ",round(measures$auc,3)," (",round(measures$auc_lower_bound,3),", ",round(measures$auc_upper_bound,3),")"),
             color="red") +
    theme(legend.position=ifelse(legend, legend_position,"none"))


}


from_measure_to_PR_curve = function(measures = BED_PLANNING_training_measures, title = "PR Curve", legend = TRUE, legend_position = 'bottom', xlab = 'Recall', ylab = 'Precision'){

  pacman::p_load(ggplot2)

  df = data.frame(x = measures$performance_measures$sens,
                  y = c(measures$performance_measures$ppv,
                        measures$performance_measurements_lower_bound$ppv,
                        measures$performance_measurements_upper_bound$ppv),
                  type = rep(c('values',"lower_bound","upper_bound"), each = length(measures$performance_measures$sens)))
  df$type = factor(df$type, levels = c('values',"lower_bound","upper_bound"))

  ggplot(data=df, aes(x=x, y=y, group = type)) +
    geom_line(aes(linetype = type))+
    geom_point(aes(size = type))+
    scale_linetype_manual(values=c("solid","dotted","dotted"))+
    scale_size_manual(values = c(1, 0, 0))+
    theme_bw() +
    ylim(0,1)+
    xlim(0,1)+
    # geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), size = 0.1, linetype = "dashed")+
    labs(title = title, x = xlab, y = ylab)+
    annotate(geom="text", x=0.5, y=0.1, label=paste0("AUC = ",round(measures$pr,3)," (",round(measures$pr_lower_bound,3),", ",round(measures$pr_upper_bound,3),")"),
             color="red")+
    theme(legend.position=ifelse(legend, legend_position,"none"))


}










table_one_way_with_percentage = function(factor = data_BED_PLANNING_training$ADMIT_YN){
  number = paste0(table(factor), " (",round((table(factor)/sum(table(factor)))*100,1),"%)")
  names(number) = names(table(factor))
  class(number) = "table"
  number
}


read_data  = function(path = "D:\\Jennly Zhang MetaboliteSD_ver03\\Raw_Phenotype_UCDavis.xlsx", sheet  = 1){
  library(data.table)


  # path = "C:\\Users\\Sili\\Documents\\Github\\Bajaj_2_5_2019\\Serum\\Bajaj complete urine and serum data transposed 12.31.18pm 90day 6month.csv"

  if(grepl("xlsx",path)){
    data = readxl::read_excel(path, col_names = FALSE, sheet  = sheet )
    data = data.table(data)
  }else{
    data = fread(path)
    data[data=='']=NA
  }



  sample_col_range = min(which(!is.na(data[1,]))):ncol(data)
  sample_row_range = 1:min(which(!is.na(data[[1]])))
  compound_col_range = 1:(min(which(!is.na(data[1,]))))
  compound_row_range = (min(which(!is.na(data[[1]])))):nrow(data)

  p = t(data[sample_row_range,sample_col_range,with=F])
  colnames(p) = p[1,]
  p = p[-1,]
  p = p[,c(ncol(p),1:(ncol(p)-1))]
  p = data.table(p)

  p = as.data.table(p)

  colnames(p) = make.unique(colnames(p), sep = "_")
  if(!"label"%in%colnames(p)){
    stop("Cannot find 'label' in your data. Please check the data format requirement.")
  }
  if(sum(is.na(p$label))>0){
    p$label[is.na(p$label)] = "na"
  }




  f = data[compound_row_range,compound_col_range,with=F]
  colnames(f) = as.character(f[1,])
  f = f[-1,]
  f = f[,c(ncol(f),1:(ncol(f)-1)),with=F]

  f = as.data.table(f)
  colnames(f) = make.unique(colnames(f), sep = "_")
  if(sum(is.na(f$label))>0){
    f$label[is.na(f$label)] = "na"
  }


  e = data[compound_row_range, sample_col_range, with = F]
  colnames(e) = as.character(e[1,])
  colnames(e)[is.na(colnames(e))] = "na"
  e = e[-1,]

  e_cat = e
  colnames(e_cat) = make.unique(colnames(e_cat), sep = "_")
  e_cat$label[is.na(e_cat$label)] = "na"
  e_cat$label = f$label
  colnames(e_cat) = c("label",p$label)

  e_cat_matrix = as.matrix(e_cat[,-1,with=F])


  e = data.table(label = e$label, sapply(e[,-1,with=F], function(x){
    as.numeric(x)
  }))

  colnames(e) = make.unique(colnames(e), sep = "_")
  e$label[is.na(e$label)] = "na"
  e$label = f$label
  colnames(e) = c("label",p$label)


  e_matrix = data.matrix(e[,-1,with=F])

  return(list(p = p, f = f, e = e, e_matrix = e_matrix,e_cat_matrix = e_cat_matrix))
}





table_two_way_with_sum = function(x,y, x_name = "", y_name = ""){
  executable_text = paste0("table(",ifelse(x_name=="", "x",paste0(x_name, " = x")),",",ifelse(y_name=="", "y)",paste0(y_name, " = y)")))
  tbl = eval(parse(text = executable_text))

  value = as.numeric(tbl)
  matrix = matrix(value, nrow = nrow(tbl))
  matrix = rbind(matrix, colSums(matrix))
  matrix = cbind(matrix, rowSums(matrix))
  data.frame = as.data.frame(matrix, check.names = F)

  rownames(data.frame) = c(paste0(ifelse(x_name=="", "", paste0(x_name, " - ")), rownames(tbl)), "Sum")
  colnames(data.frame) = c(paste0(ifelse(y_name=="", "", paste0(y_name, " - ")), colnames(tbl)), "Sum")

  return(data.frame)
}

rmarkdowncolortext <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color,x)
  } else x
}































from_any_table_to_rmarkdown = function(table, caption = ""){
  pacman::p_load(kableExtra, dplyr)
  as.data.frame(table) %>%
    kbl(caption = caption) %>%
    kable_paper("hover", full_width = F)
}


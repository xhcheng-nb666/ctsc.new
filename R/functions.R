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





















get_performance_measures = function(score = runif(100, min = 0, max = 100), truth = sample(c(TRUE, FALSE), 100, replace = TRUE), thresholds = 50, round = TRUE){
  # pred <- prediction(score, true)
  # cutoffs = pred@cutoffs[[1]]
  # sens <- performance(pred, "sens")@y.values[[1]]
  # spec <- performance(pred, "spec")@y.values[[1]]
  # balanced_accuracy = (sens + spec)/2
  # ppv <- performance(pred, "ppv")@y.values[[1]]
  # npv <- performance(pred, "npv")@y.values[[1]]
  # return(list(sens = sens, spec = spec, balanced_accuracy = balanced_accuracy, ppv = ppv, npv = npv, cutoffs = cutoffs))

  data.table::data.table(t(sapply(thresholds, function(threshold){
    predict <- score >= threshold
    TNs = sum((truth==0)&(predict==0))
    TPs = sum((truth==1)&(predict==1))
    FPs = sum((truth==0)&(predict==1))
    FNs = sum((truth==1)&(predict==0))



    sens = TPs/(TPs + FNs)
    sens[is.nan(sens)] = NA
    spec = TNs/(TNs + FPs)
    spec[is.nan(spec)] = NA
    balanced_accuracy = (sens + spec)/2
    accuracy = (TNs + TPs)/length(truth)

    prevalence = sum(truth)/length(truth)
    ppv = (sens * prevalence)/(sens * prevalence + (1 - spec) * (1 - prevalence))
    ppv[is.nan(ppv)] = NA
    npv = (spec * (1-prevalence))/((1 - sens) * prevalence + spec * (1 - prevalence))
    npv[is.nan(npv)] = NA
    f1_score = (ppv * npv)*2/(ppv + npv)
    f1_score[is.nan(f1_score)] = NA

    youden = sens + spec - 1
    youden[is.nan(youden)] = NA


    if(round){

      sens = round(sens, 3)
      spec = round(spec, 3)
      balanced_accuracy = round(balanced_accuracy, 3)
      accuracy = round(accuracy, 3)
      f1_score = round(f1_score, 3)
      youden = round(youden, 3)

    }

    return(
      c(sens = sens, spec = spec, balanced_accuracy = balanced_accuracy, accuracy = accuracy, ppv = ppv, npv = npv, f1_score = f1_score, youden = youden, TNs = TNs, TPs = TPs, FPs = FPs, FNs = FNs, Ps = TPs + FNs, Ns = TNs + FPs)
    )
  })))







}





all_measures = function(score = runif(100, min = 0, max = 100), truth = sample(c(TRUE, FALSE), 100, replace = TRUE),
                        thresholds = 1:100,
                        n_Boot = 1000,
                        use_Boot = FALSE,
                        BootID = 1:length(score),
                        confidence_level = 0.95){

  alpha = 1 - confidence_level

  pacman::p_load(ROCR)

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
    sample_indexes = list()
    for(b in 1:n_Boot){
      set.seed(b)

      sample_indexes[[b]] = sample(unique(BootID), replace = TRUE)

      # sample_indexes[[b]] = sample(1:length(score), replace = T)
    }
    # get performance for each bootstrap sample
    performance_measures_boot = list()
    for(b in 1:n_Boot){
      # print(b/n_Boot)
      performance_measures_boot[[b]] =
        get_performance_measures(score[BootID %in% sample_indexes[[b]]], truth[BootID %in% sample_indexes[[b]]], thresholds, round = FALSE)
    }
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
  auc = performance(prediction(score, truth), "auc")@y.values[[1]]

  if(!use_Boot){
    ciAUC = pROC::ci(pROC::roc(truth, score), conf.level = confidence_level)
    if(auc<0.5){ # this performance() function does not distinguish control vs case. However, pROC::ci does. By doing this, pROC::ci always returns greater than 0.5. This part is to correct this.
      auc_lower_bound = 1 - as.numeric(ciAUC)[3]
      auc_upper_bound= 1 - as.numeric(ciAUC)[1]
    }else{
      auc_lower_bound = as.numeric(ciAUC)[1]
      auc_upper_bound= as.numeric(ciAUC)[3]
    }

  }else{
    auc_boot = c()
    for(b in 1:n_Boot){
      auc_boot[b] = performance(prediction(score[BootID %in% sample_indexes[[b]]], truth[BootID %in% sample_indexes[[b]]]), "auc")@y.values[[1]]
    }
    auc_lower_bound = quantile(auc_boot, alpha/2)
    auc_upper_bound = quantile(auc_boot, 1-alpha/2)
  }



  # calculate PR auc and confidence bounds
  pr_temp = prcurve.ap(score[truth==1], score[truth==0])
  pr = pr_temp$area
  if(!use_Boot){
    ciPR = pr_temp
    pr_lower_bound = ciPR$conf.int[1]
    pr_upper_bound = ciPR$conf.int[2]
  }else{
    pr_boot = c()
    for(b in 1:n_Boot){
      pr_boot[b] = prcurve.ap(score[BootID %in% sample_indexes[[b]]][truth[BootID %in% sample_indexes[[b]]]==1], score[BootID %in% sample_indexes[[b]]][truth[BootID %in% sample_indexes[[b]]]==0])$area
    }
    pr_lower_bound = quantile(pr_boot, alpha/2)
    pr_upper_bound = quantile(pr_boot, 1-alpha/2)
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

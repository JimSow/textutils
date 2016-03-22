
#' Generic method to quickly plot ROC curve and print the AUC value
#'
#' @param predictedValues: These is the vector of predicted value
#' @param trueValues: These are true outcomes against the predicted value
#' @return None
#' @export
getAUCandPlotROC <- function(predictedValues,trueValues){
  pred <- prediction(predictedValues, trueValues);
  RP.perf <- performance(pred, "prec", "rec");
  plot (RP.perf);
  ROC.perf <- performance(pred, "tpr", "fpr");
  plot (ROC.perf);

  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  auc
}

#'show the model performance statistics like AUC, ROC, confusion matrix and more
#'
#' @param model : the model to be evaluated
#' @param trainDataSet : the dataset with features used for training the model
#' @param testDataSet: the dataset with features used for testing the model
#' @export
getModelPerformance <- function(model,trainDataSet,testDataSet){
  print(summary(model))
  out <- predict(model,testDataSet,type='response')
  print(paste( "AUC = ",getAUCandPlotROC (out,testDataSet$outcome)))

  fitted.results <- ifelse(out > 0.50,1,0)
  misClassifierError <- mean(fitted.results != testDataSet$outcome)
  print(paste('Accuracy',1-misClassifierError))

  confusionMatrix <- table(pred = fitted.results, true = testDataSet$outcome)
  print(confusionMatrix(data=fitted.results,reference=testDataSet$outcome , positive = "1"))
}

#' get the features above a threshold value
#'
#' @param model : model
#' @param threshold:
#' @export
getFeaturesAboveThreshold <- function(model,threshold){
  coeff <- summary(model)$coeff[-c(1),]
  rownames(coeff[coeff[,4] < threshold,])
}

#' Given a model, will detect the significant features and rerun the show the model performance statistics
#' like AUC, ROC, confusion matrix and more
#'
#' @param model : the model to be evaluated
#' @param trainDataSet : the dataset with features used for training the model
#' @param modelfun : the function to apply to create model
#' @param testDataSet: the dataset with features used for testing the model
#' @export
recreateAndRunModelWithSelectedFeatures <- function(model,modelfun,trainDataSet,testDataSet){
  # run it with new features
  selectedFeatures <- getFeaturesAboveThreshold(model,0.1)
  trainDataSetNew <- trainDataSet[,c(selectedFeatures,"outcome")]
  testDataSetNew <- testDataSet[,c(selectedFeatures,"outcome")]
  model = modelfun(outcome ~ ., data = trainDataSetNew, family=binomial)
  getModelPerformance(model,trainDataSet,testDataSet)
}

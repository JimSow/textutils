
#' Run LDA based on gibbs method
#'
#' @param dtm : Document term matrix
#' @param k : the number of topics that the algorithm should use to classify documents
#' @return LDA object
#'
runLDAGibbs <- function(dtm,k = 5){
  # model tuning parameters
  # burn-in period
  burnin <- 4000
  #  Following the burn-in period, perform 2000 iterations, taking every 500th  iteration for further use
  iter <- 2000
  thin <- 500
  seed <- list(826,96,353,19894328,882125)
  #five independent runs
  nstart <- 5
  # return results of the run with the highest posterior probability.
  best <- TRUE
  ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
}

#' Get top terms from lda object
#'
#' @param ldaObj : LDA object returned after LDA run
#' @param count : The count of top terms to be returned
#' @return vector of top terms
getLDATopTopicTerms <- function(ldaObj,count){
  ldaOut.topics <- as.matrix(topics(ldaObj))
  rows <- ldaOut.topics[,1]
  sapply(unique(rows),function(x) print(paste(names(rows[rows == x]))))
}

#' Get LDA top Topic terms for docs
#' @param ldaObj : LDA object returned after LDA run
getLDATopicForDocs <- function(ldaObj){
  as.matrix(topics(ldaOut))
}

#' Get the topic probability distribution for the docs
#'
#' @param k : the number of topics that the algorithm should use to classify documents
#' @return a dataframe
getDocProbabilityDistribution <- function(ldaObj,k){
  gammaDF <- as.data.frame(ldaObj@gamma)
  names(gammaDF) <- c(1:k)
  gammaDF
}

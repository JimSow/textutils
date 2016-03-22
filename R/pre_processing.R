#' Get the stopwords to be used for feature building
#'
#' @param nltk_stopwords_path : Path to the nltk english stopwords file
#' @return stopwords combining the nltk and tm package stopwords
#' @export
getStopWords <- function(nltk_stopwords_path){
  stpwords <- addNLTKStopwords(nltk_stopwords_path,stopwords("english"))
  stpwords
}

#' Get test and train dataset
#'
#' @description There are two dataset concept
#' Generic dataset : This is a generic marked dataset with outcome column marked 0 and 1
#' This is generic so has very few of the terms marked with 1. Around 25% of terms
#' The negative values in this dataset is of interest since they fit into none of the domain dataset
#' and can be used the negative case for training
#' Domain Dataset : This is the domain datset where all the rows are marked 1
#' In this function we create a new dataset which is combination of the domain dataset and the
#' negative values from the generic dataset
#' Then we return the test and train value
#' @param path_generic : Path to a generic dataframe with columns title, outcome (0 and 1) and id fields
#' @param path_generic : Path to a domain dataframe with columns title, outcome (0 and 1) and id fields
#' @return test and train dataframe
#' @export
getTestTrainDomainDataset <- function(path_generic, path_domain){

    # get the generic dataset
    df_list <- getTestTrainGenericDataset(path_generic);
    df_negative <- df_list[["negative"]];

    # get the specific dataset
      df.bd <- read.csv(path_domain,header=TRUE,stringsAsFactors = FALSE)
      df.bd$outcome = rep(1,nrow(df.bd))
      # all the elements of this domain dataset are considered to be positive
      df.bd_positive <- df.bd
      # create a new domain datset combining the negative terms from the generic dataset
      df.bd <- rbind(df.bd, data.frame("title" = df_negative$title, "outcome" = df_negative$outcome ))
      df.bd$id <- c(1: nrow(df.bd))

      # split dataset into test and training set
      repeatedSplits.bd <- createDataPartition(df.bd$id, p = .80, times = 1)
      df.bd_train <- df.bd[repeatedSplits.bd$Resample1,]
      df.bd_test <- df.bd[-repeatedSplits.bd$Resample1,]
      list("train" = df.bd_train, "test" = df.bd_test,
             "all" = df.bd, "positive" = df.bd_positive)
}

#' get the test train generic dataset
#'
#' @param path_generic : Path to the generic dataset
#' @return a list of values containing the various datasets
#' @export
getTestTrainGenericDataset <- function(path_generic){

  df <- read.csv(path_generic,header=TRUE,stringsAsFactors = FALSE)
  df$outcome <- unname(sapply(df$outcome,function(x) ifelse(x == "y",1, 0)))
  df$id <- c(1: nrow(df))
  # There are three corpus
  # all docs, positive docs and negative docs
  df_all <- df
  df_positive <- df[df$outcome == 1,]
  df_negative <- subset(df, df$outcome != 1)

  # split dataset into test and training set
  repeatedSplits <- createDataPartition(df_all$id, p = .80, times = 1)
  df_train <- df_all[repeatedSplits$Resample1,]
  df_test <- df_all[-repeatedSplits$Resample1,]

  list("positive" = df_positive , "negative" = df_negative,
       "all" = df_all, "train" = df_train , "test" = df_test)
  }

#' center and scale a numeric dataset
#'
#' @param df_numeric : a dataframe with only numeric columns
#' @return scaled dataframe
#' @export
centerScaleData <- function(df_numeric){
  # obj <- preProcess(df_numeric, method=c("center","scale"))
  # predict.preProcess(obj,df_numeric)
  df_numeric
}

#' remove non numeric features from dataframe
#'
#' @param dataset
#' @export
removeNonNumericFeatures <- function(dataset){
  dataset$title <- NULL
  dataset$id <- NULL
  dataset$outcome <- NULL
  dataset
}

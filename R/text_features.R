#' Apply a function to create feature vector from dataset
#'
#' @param df : dataframe with a title column
#' @param fun : function to apply to create the features from text extracted from dataframe
#' @return a vector of word features extracted by the fun
#' @export
getWordFeatureVector <- function(df, fun,arg1 = NULL){
  if(is.null(arg1)){
    unname(sapply(createTextVectorFromDataset(df), fun))
  }else{
    unname(sapply(createTextVectorFromDataset(df), function(x) fun(x,arg1)))
  }

}

#' Apply a function to create feature vector from dataset with database handle
#'
#' @param df : dataframe with a title column
#' @param fun : function to apply to create the features from text extracted from dataframe
#' @param  dbh: database handle
#' @return a vector of word features extracted by the fun
#
getWordFeatureVectorWithdbh <- function(df, fun,dbh){
  unname(sapply(createTextVectorFromDataset(df,TRUE), function(x) fun(dbh,x)))
}

#' get the feature vector form term document matrix (tdm)
#'
#' @param tdm: the term document matrix
#' @return a vector of words
#' @export
getWordFeaturesFromTdm <- function(tdm){
  tdm_non_sparse <- removeSparseTerms(tdm,0.98)
  tdm_mtx <- as.matrix(tdm_non_sparse)
  word_freq <- colSums(tdm_mtx)
  filtered_words <- word_freq[word_freq > 1 & word_freq < as.integer(0.05 *       length(word_freq))]
  filtered_words
}

#' get freq words from tdm
#'
#' @param tdm: term document matrix
#' @return  vector of words
#' @import tm
#' @export
getFreqWordFeaturesFromTdm <- function(tdm, low_freq_thresh,high_freq_thresh){
  findFreqTerms(tdm,
                lowfreq=  low_freq_thresh*100 ,
                highfreq= round(high_freq_thresh * tdm$nrow) )
}

#' set bag of words in a data frame
#'
#' @param row : the data frame row
#' @param wordFeatures : the word features
#' @return  the updated row
#' @export
setBOWFeatureInDataFrame <- function(row,wordFeatures){
  string <- row$title
  words <- getWords(tolower(string))
  matches <- words[!is.na(match(words,wordFeatures))]

  if(length(matches)){
    row[,unique(matches)] <- 1
  }
  row
}

#' set all the words in a dataframe
#'
#' @param df: the data frame
#' @param wordFeatures:  the word features to set in the dataframe column
#' @return  updated dataframe
#' @import tm
#' @export
setAllBOWFeaturesInDataFrame <- function(df,wordFeatures){
  list_of_dfrows <- lapply(df$id, FUN = function(id){
    setBOWFeatureInDataFrame(df[df$id == id,],wordFeatures)
  })
  new_df <- do.call(rbind.data.frame,list_of_dfrows)
  new_df
}

#' get BOW features binary - y/n
#'
#' @param dataset : input dataframe
#' @return  dataframe
#' @export
getBOWfeatures_Binary <- function(dataset,low_freq_thresh,high_freq_thresh){

  corpus <-  getCorpusFromTextVector ( createTextVectorFromDataset(dataset) )
  tdm <- DocumentTermMatrix(corpus)
  keywords <- getFreqWordFeaturesFromTdm (tdm,low_freq_thresh,high_freq_thresh)

  # At start you know the number of docs. So we can initialize the df with all the rows and columns
  df_positive_withfeatures <- cbind(dataset,getDataframeFromWordVector(keywords, nrow(dataset)))

  df_positive_withfeatures <- setAllBOWFeaturesInDataFrame(df_positive_withfeatures,keywords)
  df_positive_withfeatures
}

#' get bag of words test dataset
#'
#' @param dataset: dataframe
#' @param keywords: the keywords
#' @return  dataframe
#' @export
getBOWfeaturesTestDataset <- function(dataset,keywords){

  # At start you know the number of docs. So we can initialize the df with all the rows and columns
  df_withfeatures <- cbind(dataset,getDataframeFromWordVector(keywords, nrow(dataset)))

  df_withfeatures_set <- setAllBOWFeaturesInDataFrame(df_withfeatures,keywords)
  df_withfeatures_set
}

#' get bag of words with tfidf
#'
#' @param dataset : dataframe
#' @return  dataframe
#' @export
getBOWfeatures_Tfidf <- function(dataset){
  corpus <-  getCorpusFromTextVector ( createTextVectorFromDataset(dataset) )

  tdm <- TermDocumentMatrix(corpus,control = list(weighting = function(x)
    weightTfIdf(x, normalize = FALSE),stopwords = TRUE))

  top_terms <- sort(sapply(rownames(tdm), function(x) sum(tdm[c(x), ])), decreasing = TRUE)
  top_terms_log <- log(top_terms)
  top_terms_log_filtered <- top_terms_log [top_terms_log > median(top_terms_log)]

  df_features_filtered <- data.frame(t(inspect(tdm[c(names(top_terms_log_filtered)),])))
  #   df_features_100 <- as.data.frame(t(inspect(tdm[c(names(top_terms[1:100])),])))
  df_positive_withfeatures <- cbind(dataset,df_features_filtered)

}
#' get bag of words features with frew
#'
#' @param dataset : dataframe
#' @return  dataframe .
#' @export
getBOWfeatures_freq <- function(dataset,test_dataset){
  corpus <-  getCorpusFromTextVector ( createTextVectorFromDataset(dataset,TRUE) )
  tdm <- TermDocumentMatrix(corpus)

  top_terms <- sort(sapply(rownames(tdm), function(x) sum(tdm[c(x), ]) ) , decreasing = TRUE)

  top_terms_log <- log(top_terms)
  top_terms_log_filtered <- top_terms_log [top_terms_log > median(top_terms_log)]

  df_features_filtered <- data.frame(t(inspect(tdm[c(names(top_terms_log_filtered)),])))

  df_positive_withfeatures <- cbind(dataset,df_features_filtered)

  # for test dataset
  keywords <- colnames(df_features_filtered)
  df_withfeatures_test <- getBOWfeaturesTestDataset(test_dataset,keywords)

  list(df_positive_withfeatures, df_withfeatures_test)
}

#' get bagofwords keywords from a dataset
#'
#' @param dataset: dataframe with title and id column
#' @return keywords
#' @export
getBOWKeywords <- function(dataset){
  corpus <-  getCorpusFromTextVector ( createTextVectorFromDataset(dataset,TRUE) )
  tdm <- TermDocumentMatrix(corpus)

  top_terms <- sort(sapply(rownames(tdm), function(x) sum(tdm[c(x), ]) ) , decreasing = TRUE)

  top_terms_log <- log(top_terms)
  top_terms_log_filtered <- top_terms_log [top_terms_log > median(top_terms_log)]

  df_features_filtered <- data.frame(t(inspect(tdm[c(names(top_terms_log_filtered)),])))

  # for test dataset
  keywords <- colnames(df_features_filtered)
}

#' return the dataset with bag of words keywords set
#'
#' @param dataset : dataframe
#' @param keywords : keyword
#' @return dataframe
#' @export
setBOWkeywords <- function(dataset,keywords){
  getBOWfeaturesTestDataset(dataset,keywords)
}

#' get data set with features
#'
#' @param dataset : dataframe which would be the train dataset
#' @param funToGetBOWFeatures : function to get bag of words features from dataframe
#' @param stopwords : Stop word
#' @param test_dataset : dataframe which would be the test dataset
#' @return a list of dataframes with various bag of words features. First one is the test and second is train dataframe
#' @export
getDataSetWithFeatures <- function(dataset,funToGetBOWFeatures,test_dataset,
                                   funargs = NULL, otherargs = NULL ){

  # df_withfeatures <- funToGetBOWFeatures(dataset,test_dataset)
  #
  # df_positive_withfeatures <- data.frame(df_withfeatures[1])
  # df_positive_withfeatures_test <- data.frame(df_withfeatures[2])
  keywords <- otherargs["keywords"][[1]]
  stpwords <- otherargs["stopwords"][[1]]
  dbh_hn <- otherargs["dbh"][[1]]

  df_positive_withfeatures <- getFeaturesForDataset(dataset,keywords,stpwords,dbh_hn)
  df_positive_withfeatures_test <- getFeaturesForDataset(test_dataset,keywords,stpwords,dbh_hn)

  list(df_positive_withfeatures,df_positive_withfeatures_test)
}

#' Given a dataset, it returns the dataset updated with features
#'
#' @description The dataset should have a column title. The features are decided based on that
#' @param dataset : Input dataset
#' @param keywords : These are the domain keywords unigrams
#' @param stpwords : This is the stopwords
#' @param dbh : The database handle where tables for feature extraction exists
#' @export
getFeaturesForDataset <- function(dataset,keywords,stpwords,dbh){

  df_withfeatures <- setBOWkeywords(dataset,keywords)
  getMiscFeatures <- function(dataset_org){
    feature_capletters <- getWordFeatureVector(dataset_org,getCapLettersToCharactersRatio)
    feature_acryonym <- getWordFeatureVector(dataset_org,getAcronymRatio)
    feature_symbols <- getWordFeatureVector(dataset_org,getSymbolToWordsRatio)
    feature_symbol_freq <- getWordFeatureVector(dataset_org,getSymbolCount)
    feature_ratio <- getWordFeatureVector(dataset_org,getStopWordsRatio,stpwords)
    feature_stopwords_freq <- getWordFeatureVector(dataset_org,getStopWordsCount,stpwords)
    feature_digit_freq <- getWordFeatureVector(dataset_org,getDigitCount)
    feature_word_weight <- getWordFeatureVectorWithdbh(dataset_org,getKeywordWeights,dbh)

    dset_features <- cbind(
      "capletters" = feature_capletters,
      "stpratio" = feature_ratio,
      "weights" = feature_word_weight
      # rest are found to be not informative in the model
      #"acronym" = feature_acryonym,
      #"symbols" = feature_symbols,
      #"stpfreq" = feature_stopwords_freq,
      #"digits" = feature_digit_freq,
    )
    dset_features

  }
  df_withfeatures <- cbind(getMiscFeatures(dataset),df_withfeatures)

  df_withfeatures
}

#' get predicted results for the model built for text only dataset
#'
#' @param dataset : the dataframe with a title field
#' @param model : the model used for making predictions
#' @param keywords : the domain keywords used for building features
#' @param stpwords : the stopwords list used for building features
#' @param dbh : the database handle
#' @return the dataset with prediction as 0 or 1. The values are stored in a pred column
#' @export
getPredictionsForModel <- function(dataset,model,keywords,stpwords,dbh){
  # testing prediction of bigdata model on generic dataset
  df_all_features <- getFeaturesForDataset(dataset,keywords,stpwords,dbh)
  df.bd_features_num <- removeNonNumericFeatures(df_all_features)
  df.bd_features_all <- centerScaleData(df.bd_features_num)
  out <- predict(model,df.bd_features_all,type='response')
  fitted.results <- ifelse(out > 0.50,1,0)
  dataset$pred <- fitted.results
  dataset
}


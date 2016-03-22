#' Get stop Words Count in string
#' @param string : input string
#' @return : count of number of stopwords
#' @description : None
#' @import tm
#' @export
getStopWordsCount <- function(string,stpwords){
  words = getWords(string);
  length(words[words %in% stpwords])
}
#' Get the ratio of stop words
#'
#' @param  string : input string
#' @param  output_path : path where the  output csv file is to be written
#' @return integer value
#' @import tm
#' @export
getStopWordsRatio <- function(string,stpwords){
  words <- getWords(tolower(string))
  matches <- match(words,stpwords)
  ifelse(length(words),
         length( matches[ !is.na(matches)])/length(words),
         0)
}

#' Apply a regex pattern on each word of a string and find the ratio of matches to word length
#'
#' @param string : the input string
#' @param pattern : the regex patter to apply on each word of the string
#' @param splitby : instead of splitting the string into words, it can be split using this regex pattern
#' @return a decimal value
#' @export
getRatio <- function(string,pattern,splitby = "\\s"){

  words <-getWords(string,splitby)
  matches <- grep( pattern, words, perl = TRUE)
  ifelse(length(words),length(matches)/length(words) ,0 )
}

#' get count by pattern
#'
#' @param string: input string
#' @param pattern: the regex pattern to apply on each word of the string
#' @param splitby: instead of splitting the string into words, it can be split using this regex pattern
#' @return an integer value
#' @export
getCountByPattern <- function(string,pattern,splitby = "\\s"){

  words <-getWords(string,splitby)
  matches <- grep( pattern, words, perl = TRUE)
  ifelse(length(words),length(matches) ,0 )
}

#' get capital letters to character ratio
#'
#' @param string : input string
#' @return decimal value
#' @export
getCapLettersToCharactersRatio <- function(string){
  getRatio(string,"[A-Z]","")
}

#' get Acronym Ratio
#'
#' @param string : Get input string
#' @return decimal value
#' @export
getAcronymRatio <- function(string){
  getRatio(string,"^[A-Z][A-Z]+")
}

#' get Symbol to words ratio
#'
#' @param string : input string
#' @return decimal value
#' @export
getSymbolToWordsRatio <- function(string){
  getRatio(string,"[^A-Za-z .0-9]")
}

#' Get symbol count
#'
#' @param  string : input string
#' @return integer value
#' @export
getSymbolCount <- function(string){
  getCountByPattern(string,"[^A-Za-z 0-9]","")
}

#' Get digit count
#'
#' @param  string : input string
#' @return integer value
#' @export
getDigitCount <- function(string){
  getCountByPattern(string,"[0-9]","")
}

#' Get freq of keywords in dataset
#'
#' @param  dataset : a dataframe with a "title" column
#' @return integer value
#' @export
getFreqOfKeywordInDataset <- function(dataset){
  table(unlist(unname(
    sapply(dataset[,"title"],function(x) getCamelCaseKeywords(x)))))
}

#' Get freq of keywords in dataset
#'
#' @param  input_path : path to csv file
#' @param  output_path : path where the  output csv file is to be written
#' @return integer value
#' @export
createFreqOfKeywordsTerms <- function(input_path,output_path){
  df <- read.csv(input_path,header=TRUE,stringsAsFactors = FALSE)
  terms <- getFreqOfKeywordInDataset(df)

  #remove the stopwords
  kw <- rownames(terms)
  toremove <- which(tolower(kw) %in% stopwords)
  terms <- terms[-c(toremove)]

  terms_sorted <- sort(terms)
  terms_sorted <-  terms_sorted[terms_sorted > 1]
  write.csv(terms_sorted, output_path)
}


#' Get the suma of weights of all keywords in string
#'
#' @param  dbh : datbase handle to keywords database
#' @param  string : input string
#' @return decimal value
#' @export
getKeywordWeights <- function(dbh,string){
  words <- getWords(tolower(string))
  makeQuery <- function(x) paste("select * from good_word_filtered where goodword = '",x,"' limit 1",sep = "")
  weight <- 0
  weights <- sapply(words,FUN = function(x) {
    row <- dbGetQuery(dbh,makeQuery(x))
    ifelse(nrow(row), row$weight,0 )})

  tryCatch({ sum(unlist(weights))}, error = function(e){
    print("Got error")
    0
    })

  #sum(unlist(weights))
}

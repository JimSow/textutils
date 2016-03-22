#' Get Character Vector
#'
#' @param string : this is the text which needs to be transformed to character vector
#' @return character vector
#' @description Get words from string
#' @export
getCharacterVector <- function(string){
  trim <- function (x) gsub("\\s", "", x)
  unique(unlist(strsplit(trim(string),"")))
}

#' Get a clean string which is based on hueristic for document titles
#'
#' @param string : title string
#' @return string
#' @export
cleanTitleString <- function(string){

  # remove DOS'd to DOS
  string <- gsub("(\\'[A-Za-z]+)"," ",string)
  # replace comma in 100,000 to 100000
  string <- gsub("\\d(,)\\d","",string)
  # replace comman in sdf,sdf,sdf with spaces
  string <- gsub(","," ",string)

  # new: should be made as new dummycolon
  string <- gsub('([A-Za-z])(:)\\s',"\\1 dummycolon ",string)
  # specific to hn where show hn has significance
  string <- gsub('\\bshow hn\\b',"show-hn",string,ignore.case = TRUE)
  string <- gsub('\\bask hn\\b',"ask-hn",string,ignore.case = TRUE)

  #remove all punctuations
  string <- gsub('[^A-Za-z0-9 -]|(\\s-\\s)',"",string)

  string <- gsub('\\s+'," ",string)
  string <- gsub('^\\s+|\\s+$',"",string)
  string
}

#' Get Words
#'
#' @param string : this is the text which needs to be transformed
#' @param splitby : the regex pattern on which the string needs to be split.
#' The default is split by space like:  "\\s"
#' @param min : the min word length
#' @return vector of words
#' @description Nothing special
#' @export
getWords <- function(string,splitby = "\\s", min = 0){
    words <- unlist(strsplit(string,splitby))
  if(min == 0){
    words
  }else{
    words[sapply(words, function(x) length(unlist(strsplit(x,"")))) >= min]
  }
}

#' Get clean text from dataframe
#'
#' @param df: A dataframe with column "title" and "text"
#' @return vector of clean text for each row in dataframe
#' @description Get a clean text vector by merging the columns "title" and "text" of a dataframe
#' @export
getTextVectorAll <- function(df) {
  createTextVectorFromDataset(df,removePunctuation = FALSE,withTextColumn = TRUE)
}

#' createTextVectorFromDataset from title
#'
#' @param df: A dataframe with column "title" or optional "text" column
#' @param removePunctuation : The default is FALSE. If true will remove all the punctuations. All entities other than space, numbers and characters
#' @param withTextColumn : Whether the input dataframe has text column. The default is FALSE
#' @return vector of clean text for each row in dataframe
#' @description Get a clean text vector from the columns "title" without punctuations
#' @export
createTextVectorFromDataset <- function(df, removePunctuation = FALSE, withTextColumn = FALSE) {
  if(is.null(df$title) || ( withTextColumn && is.null(df$text) )){
    stop("not a valid dataframe")
  }

  trim <- function (x, removePunctuation = FALSE) {
    if(removePunctuation){
      cleanTitleString(x)
    }else{
      gsub("^\\s+|\\s+$", "", x)
    }
  }

  if(is.null(df$text)){
        mapply( df$title, FUN = function(x) trim(paste(trim(x,removePunctuation))))
  }else{
        mapply(df$text, df$title, FUN = function(x,y) trim(paste(trim(x)," ",trim(y))))
  }
}

#' Get camel case keywords from string
#'
#' @param string: input string
#' @return a vector of camel case string words
#' @export
getCamelCaseKeywords <- function(string){
  string <- gsub("[^A-Za-z0-9 ]","",string)
  words <- getWords( gsub("[^A-Za-z0-9 ]","",string) )
  words[grep("(^[A-Z][a-z]+)|([A-Z][0-9]+)",words)]
}

#' add NLTK stopwords to the current list of stopwords
#'
#' @param nltk_path : path to nltk stopwords file
#' @param stopwords : stopwords list
#' @return a vector of stopwords
#' @import tm
#' @export
addNLTKStopwords <- function(nltk_path,stop.words) {
  nltk <- read.csv(nltk_path,stringsAsFactors = FALSE)
  new_words <- nltk[which(!sapply(nltk,function(x) x %in% stop.words )),]
  stop.words <- c(new_words,stop.words )
  stop.words
}

#' Creates a dataframe whose columns are words in wordvector where the rows are all set to 0.
#'
#' @param wordvec : a word vector
#' @param nrows : number of rows. By default is 1
#' @return a datframe
#' @import tm
#' @export
getDataframeFromWordVector <- function(wordVec,nrows = 1){
  dim <- length(wordVec )
  data <- rep(0,nrows * dim)
  mtx <- matrix(data, nrow = nrows, ncol = dim)
  dfx <- data.frame(mtx)
  colnames(dfx) <- wordVec
  dfx
}

#' get corpus from text vector
#'
#' @param textVector: a vector of text documents from which to create the corpus
#' @return a corpus entity of tm package
#' @import tm
#' @export
getCorpusFromTextVector <- function(textVector) {
  corpus <- Corpus( VectorSource(textVector))
  ms <- tm_map(corpus,stripWhitespace)
  ms <- tm_map(ms,content_transformer(tolower))
  ms <- tm_map(ms,removeNumbers)
  #ms <- tm_map(ms,removePunctuation,preserve_intra_word_dashes = TRUE)
  ms <- tm_map(ms,removeWords, stopwords("english"))
  ms <- tm_map(ms, PlainTextDocument)
  ms
}



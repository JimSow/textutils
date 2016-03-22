
#' Add keywords to an sqlite database
#' 
#' @param dbh: sqlite dbh handle. Should have table named words_english
#' @param words_df: dataframe to insert  
#' @return  OK or failed message
#' @export
addWordsToDB <- function(dbh,words_df){
  colnames(words_df) <- "word"
  dbGetQuery(dbh, "delete from words_english")
  dbWriteTable(dbh,"words_english",words_df,overwrite = TRUE)
  num <- dbGetQuery(dbh, "select count(*) from words_english")
  ifelse(nrow(words_df) == num , "OK",paste("Failed ! ",
                                            "words to insert = ", nrow(words_df),
                                            "words inserted = ",num) )
}

#' Checks if the word is a dict word
#' 
#' @param dbh: sqlite dbh handle. Should have table named words_english
#' @param word: the word to check
#' @return  TRUE or FALSE
#' @export

# checks whether the word exists in english dictionary
isDictWord <- function(dbh,word){
  query <- paste("select exists(select * from words_english where word = \"", tolower(word),"\")",sep ="" )
  as.vector(ifelse( dbGetQuery(dbh, query) == 1, TRUE, FALSE))
}

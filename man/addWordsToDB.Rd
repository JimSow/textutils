% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corpus_utils.R
\name{addWordsToDB}
\alias{addWordsToDB}
\title{Add keywords to an sqlite database}
\usage{
addWordsToDB(dbh, words_df)
}
\arguments{
\item{dbh:}{sqlite dbh handle. Should have table named words_english}

\item{words_df:}{dataframe to insert}
}
\value{
OK or failed message
}
\description{
Add keywords to an sqlite database
}


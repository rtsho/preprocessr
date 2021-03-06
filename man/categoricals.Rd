% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Preprocessor.R
\name{categoricals}
\alias{categoricals}
\title{Helper function to select character or factor variables.}
\usage{
categoricals(data)
}
\description{
Helper function to select character or factor variables.
}
\examples{
df <- data.frame(a=c(1,2,NA,3), b=c(10,20,30,NA), c=c("c1", NA, "c3", "c3"))
prep <- impute(categoricals, most_frequent)
prep$fit(df)
prep$transform(df)
}

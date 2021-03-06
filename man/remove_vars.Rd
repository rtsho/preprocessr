% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Preprocessor.R
\name{remove_vars}
\alias{remove_vars}
\title{Remove variables.}
\usage{
remove_vars(vars)
}
\arguments{
\item{vars}{Function or formula that returns selected columns to exclude from a data.frame. Alternatively, character vector of column names.}
}
\description{
Remove variables.
}
\examples{
df <- data.frame(ID=seq(1,4), B=c("b1", "b2", "b3", "b4"), C=c("c","c","c","c"), X=c(1,1,1,1))
prep <- remove_vars("ID")
prep$fit(df)
prep$transform(df)
# or more succintly
prep$fit_transform(df)
}

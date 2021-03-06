% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Preprocessor.R
\name{normalize}
\alias{normalize}
\title{Normalize columns (i.e. center around the mean and divide by standard deviation).}
\usage{
normalize(vars = numerics)
}
\arguments{
\item{vars}{Function or formula that returns selected columns from a data.frame. Alternatively, character vector of column names.}
}
\description{
Normalize columns (i.e. center around the mean and divide by standard deviation).
}
\examples{
df <- data.frame(A=c("a1","a1","a2","a3"), B=c("b1", "b2", "b3", "b4"), X=c(1,2,3,4))
prep <- normalize()
prep$fit(df)
prep$transform(df)
# or more succintly
prep$fit_transform(df)
}

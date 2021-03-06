% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Preprocessor.R
\name{encode_one_hot}
\alias{encode_one_hot}
\title{Create one hot encoder, that turns factor variables into dummy variables.}
\usage{
encode_one_hot(vars = categoricals, sparse = FALSE)
}
\arguments{
\item{vars}{Function or formula that returns selected columns from a data.frame. Alternatively, character vector of column names.}

\item{sparse}{If true, return value will be a sparse Matrix of class dcgMatrix. Otherwise, regular data frame.}
}
\description{
Create one hot encoder, that turns factor variables into dummy variables.
}
\details{
If sparse is TRUE, this should be the last step of your preprocessing pipeline.
}
\examples{
df <- data.frame(A=c("a1","a1","a2","a3"), B=c("b1", "b2", "b3", "b4"), X=c(1,2,3,4))
prep <- encode_one_hot()
prep$fit(df)
prep$transform(df)
# or more succintly
prep$fit_transform(df)
}

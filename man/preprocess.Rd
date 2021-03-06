% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Preprocessor.R
\name{preprocess}
\alias{preprocess}
\title{Create a preprocessing pipeline: a list of Preprocessor objects that will be executed in sequence.}
\usage{
preprocess(...)
}
\arguments{
\item{...}{The preprocessor objects.}
}
\description{
Typical usage will be:
\code{train <- pipe$fit_transform(train)}
\code{test <- pipe$transform(test)}
}
\examples{
df <- data.frame(a=c(1,2,NA,3), b=c(10,20,30,NA), c=c("c1", "c2", "c3", "c4"), D=rep("d", 4), X=c(1,2,3,NA))
pipe <- preprocess(remove_constants(), impute(numerics, mean, na.rm=TRUE), impute(categoricals, most_frequent))
pipe$fit(df)
pipe$transform(df)
# or more succintly
pipe$fit_transform(df)
}

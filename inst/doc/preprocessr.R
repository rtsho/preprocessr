## ----data----------------------------------------------------------------
set.seed(123)

data <- 
  dplyr::data_frame(
    ID = 1:100,
    A = c(rep("a1", 50), rep("a2", 50)),
    B = sample(c("b1", "b2", NA), 100, replace=TRUE),
    C = rnorm(100),
    D = 1,
    E = sample(c(10,20,30,40,NA), 100, replace=TRUE)
  )

data

## ----preprocess, message=FALSE, warning=FALSE----------------------------
library(dplyr)
library(preprocessr)

prep <- 
  preprocess(
    remove_vars("ID"),
    remove_constants(),
    impute(numerics, median, na.rm=TRUE),
    impute(nonnumerics, most_frequent),
    bin("C", 10),
    encode_one_hot("A"),
    encode_numeric("B")
  )

prep$fit_transform(data) %>% as_data_frame

## ----test----------------------------------------------------------------
test <- 
  dplyr::data_frame(
    ID = 1:100,
    A = c(rep("a1", 50), rep("a2", 50)),
    B = sample(c("b1", "b2", "b3", NA), 100, replace=TRUE),
    C = rnorm(100),
    D = 1,
    E = sample(c(10,20,30,40,NA), 100, replace=TRUE)
  )

prep$transform(test) %>% as_data_frame

## ----preprocess2, message=FALSE, warning=FALSE---------------------------
prep <- 
  preprocess(
    remove_vars("ID"),
    remove_constants(),
    impute(numerics, median, na.rm=TRUE),
    impute(nonnumerics, most_frequent),
    bin("C", 10),
    factorize(),
    encode_one_hot("A"),
    encode_numeric("B")
  )

prep$fit(data)

prep$transform(test) %>% as_data_frame


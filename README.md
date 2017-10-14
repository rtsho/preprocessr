# preprocessr
Collection of preprocessing functions following the fit/transform pattern of scikit-learn.

A typical usage looks like this:

```{r}
prep <- 
  preprocess(
    remove_vars("ID"),
    remove_constants(),
    impute(numerics, median, na.rm=TRUE),
    impute(nonnumerics, most_frequent),
    bin("C", breaks=10),
    encode_one_hot("A"),
    encode_numeric("B")
  )

prep$fit_transform(train)
prep$transform(test)
```

For more information, see the [vignette](vignettes/preprocessr.Rmd).

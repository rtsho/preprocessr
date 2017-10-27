
### Preprocessor: base class for preprocessing objects
Preprocessor <-
  R6::R6Class(
    "Preprocessor",

    public = list(

      vars = NULL,
      varnames = NULL,

      initialize = function(vars){
        if(inherits(vars, 'formula')){
          self$vars <- rlang::as_function(vars)
        }
        else if(is.function(vars)){
          self$vars <- vars
        }
        else{
          self$vars <- function(data) dplyr::select_at(data, vars)
        }
      },

      fit = function(data){
        self$varnames <- data %>% (self$vars) %>% colnames
      },

      transform = function(data){

      },

      fit_transform = function(data){
        self$fit(data)
        self$transform(data)
      },

      toJSON = function(){}
    )
  )


### Pipe: this is simply a list of Preprocessor objects that will be executed in sequence.
Pipe <-
  R6::R6Class(
    "Pipeline",

    inherit = Preprocessor,

    public = list(

      preps = NULL,

      initialize = function(...){
        self$preps <- list(...)
      },

      fit = function(data){
        for(prep in self$preps){
          prep$fit(data)
          data <- prep$transform(data)
        }
      },

      transform = function(data){
        for(prep in self$preps){
          data <- prep$transform(data)
        }
        data
      },

      toJSON = function(){
        paste0(
          "[",
          paste(purrr::map_chr(self$preps, ~ .$toJSON()), collapse=","),
          "]"
        )
      }
    )
  )


#' Create a preprocessing pipeline: a list of Preprocessor objects that will be executed in sequence.
#'
#' Typical usage will be:
#' \code{train <- pipe$fit_transform(train)}
#' \code{test <- pipe$transform(test)}
#' @param ... The preprocessor objects.
#' @examples
#' df <- data.frame(a=c(1,2,NA,3), b=c(10,20,30,NA), c=c("c1", "c2", "c3", "c4"), D=rep("d", 4), X=c(1,2,3,NA))
#' pipe <- preprocess(remove_constants(), impute(numerics, mean, na.rm=TRUE), impute(categoricals, most_frequent))
#' pipe$fit(df)
#' pipe$transform(df)
#' # or more succintly
#' pipe$fit_transform(df)
#' @export
preprocess <- function(...){
  Pipe$new(...)
}



#### Helper functions

#' Helper function to select numeric variables.
#' @examples
#' df <- data.frame(a=c(1,2,NA,3), b=c(10,20,30,NA), c=c("c1", "c2", "c3", "c4"))
#' prep <- impute(numerics, mean, na.rm = TRUE)
#' prep$fit(df)
#' prep$transform(df)
#' @export
numerics <- function(data) dplyr::select_if(data, is.numeric)


#' Helper function to select factor variables.
#' @examples
#' df <- data.frame(a=c(1,2,NA,3), b=c(10,20,30,NA), c=c("c1", NA, "c3", "c3"))
#' prep <- impute(factors, most_frequent)
#' prep$fit(df)
#' prep$transform(df)
#' @export
factors <- function(data) dplyr::select_if(data, is.factor)


#' Helper function to select character variables.
#' @examples
#' df <- data.frame(a=c(1,2,NA,3), b=c(10,20,30,NA), c=c("c1", NA, "c3", "c3"))
#' prep <- impute(characters, most_frequent)
#' prep$fit(df)
#' prep$transform(df)
#' @export
characters <- function(data) dplyr::select_if(data, is.character)


#' Helper function to select character or factor variables.
#' @examples
#' df <- data.frame(a=c(1,2,NA,3), b=c(10,20,30,NA), c=c("c1", NA, "c3", "c3"))
#' prep <- impute(categoricals, most_frequent)
#' prep$fit(df)
#' prep$transform(df)
#' @export
categoricals <- function(data) dplyr::select_if(data, ~ is.character(.) | is.factor(.))


#' Helper function to select all variables.
#' @export
allvars <- function(data) dplyr::select_all(data)


#### Mapper

Mapper <-
  R6::R6Class(
    "Mapper",

    inherit = Preprocessor,

    public = list(

      mapping = NULL,
      rename = NULL,

      initialize = function(vars, mapping, rename){
        super$initialize(vars)
        self$mapping <- mapping
        self$rename <- rename

        if(!is.vector(mapping) && is.null(names(mapping))) stop("vars must be a named vector.")
        if(sum(names(mapping) == "") > 1) stop("There should be only 1 unnamed element in mapping.")
      },

      fit = function(data){
        super$fit(data)
      },

      transform = function(data){

        transformed_data <- data

        for(v in self$varnames){

          original_class <- class(transformed_data[[v]])
          original_mode <- mode(transformed_data[[v]])

          transformed_data[[v]] <- as.factor(transformed_data[[v]])
          original_levels <- levels(transformed_data[[v]])

          mapping_non_other_to <- self$mapping[names(self$mapping) != ""]
          mapping_non_other_from <- names(self$mapping)[names(self$mapping) != ""]
          mapping_non_other_idx <- match(mapping_non_other_from, original_levels)

          levels(transformed_data[[v]])[mapping_non_other_idx] <- mapping_non_other_to
          levels(transformed_data[[v]])[-mapping_non_other_idx] <- self$mapping[names(self$mapping) == ""]

          # if(!identical(original_class, "factor")){
          #   transformed_data[[v]] <- as.vector(transformed_data[[v]], original_mode)
          # }
        }

        transformed_data %>% dplyr::rename_at(self$varnames, self$rename)
      },

      toJSON = function(){
        mapping2 <- self$mapping
        names(mapping2)[nchar(names(mapping2))==0] <- 'Remainder'
        output_names <- purrr::map_chr(self$varnames, self$rename)

        paste0(
          GetoptLong::qq("
                         {
                         'Encoder':'Mapping',
                         'InputName':'@{self$varnames},'
                         'Maps':{"),
          GetoptLong::qq("
                         '@{names(mapping2)}':'@{mapping2}'
                         "),
          "  },
          'OutputName': '@{output_names}'
                         }")
      }

          )
  )

#' Map values of variables.
#' @param func Function to do the imputation (e.g. mean, median).
#' Can be a user defined function or a formula, where a dot represents the column.
#' @param vars Function or formula that returns selected columns from a data.frame. Alternatively, character vector of column names.
#' @param mapping The mapping to apply. This should be a named vector, where the names are the old values, and the elements are the new values.
#' There should be at most one unnamed element, representing `all other` elements.
#' @details Currently, this function always turns the mapped variables into factors.
#' @importFrom maggritr "%>%"
#' @examples
#' df <- data.frame(a=c(1,2,NA,3), b=c(10,20,30,NA), c=c("c1", "c2", "c3", "c4"))
#' prep <- map_values("c", c(c1="C1", c4="C4", "other"))
#' prep$fit(df)
#' prep$transform(df)
#' # or more succintly
#' prep$fit_transform(df)
#' @export
map_values <- function(vars, mapping, rename){
  Mapper$new(vars, mapping, rename)
}



#### ValueReplacer
ValueReplacer <-
  R6::R6Class(
    "NAReplacer",

    inherit = Preprocessor,

    public = list(

      values = NULL,
      func = NULL,
      other_args = NULL,
      fitted = NULL,
      rename = NULL,

      initialize = function(vars, values, func, ..., rename){
        super$initialize(vars)

        if(is.atomic(func)){
          if(is.vector(func)) stop("func cannot be a vector")
          else func <- function() func
        }

        self$values <- values

        self$func <- func

        other_args <- list(...)

        self$rename <- rlang::as_function(rename)

        if(length(other_args) > 0){
          self$func <- purrr::partial(func, ...)
        }
      },

      fit = function(data){
        super$fit(data)

        self$fitted <-
          data %>%
          (self$vars) %>%
          purrr::map(self$func)
      },

      transform = function(data){
        transformed_data <-
          purrr::map2_df(
            data %>% dplyr::select(dplyr::one_of(self$varnames)),
            (self$fitted),
            function(x, y){
              ifelse(x %in% self$values, y, x)
            }
          )

        dplyr::bind_cols(
          data %>% (dplyr::select)(-(dplyr::one_of)(colnames(transformed_data))),
          transformed_data %>% dplyr::rename_all(self$rename)
        )
      },

      toJSON = function(){
        replace_values <- paste(self$values, collapse=",")
        output_names <- purrr::map_chr(self$varnames, self$rename)
        GetoptLong::qq("
                       {
                       'Encoder':'ImputationSpecificValue',
                       'InputName':'@{self$varnames}',
                       'Impute': @{unlist(self$fitted)},
                       'ReplaceValues':[@{replace_values}],
                       'OutputName': '@{output_names}'
                       }")
      }
        )
        )

#' Replace specific values in a column (usually NAs).
#' @param func Function to do the replacement (e.g. mean, median).
#' Can be a constant, a user defined function or a formula, where a dot represents the column.
#' @param vars Function or formula that returns selected columns from a data.frame. Alternatively, character vector of column names.
#' @param values Vector of values to replace. Default is NA.
#' @param ... Other arguments passed to \code{func}.
#' @param rename Function to rename the output variables (input: the original column name, output: the new column name).
#' @importFrom maggritr "%>%"
#' @examples
#' df <- data.frame(a=c(1,2,NA,4), b=c(10,20,40,NA), c=c("c1", "c2", "c3", "c4"))
#' prep <- replace_values(numerics, c(NA, 0), mean, na.rm = TRUE, rename = ~ paste(., "interim", sep="_"))
#' prep$fit(df)
#' prep$transform(df)
#' # or more succintly
#' prep$fit_transform(df)
#' @export
replace_values <- function(vars = numerics, values = NA, func, ..., rename = function(x) x){
  ValueReplacer$new(vars, values, func, ..., rename = rename)
}

#' Helper function to find the most frequent values in a variable.
#' @param v Variable for which you want the top-n most frequent values.
#' @param n How many values to return.
most_frequent <- function(v, n = 1){
  freqs <- table(v)
  names(sort(freqs, decreasing=TRUE))[n]
}


#### OneHotEncoder

OneHotEncoder <-
  R6::R6Class(
    "OneHotEncoder",

    inherit = Preprocessor,

    public = list(

      model = NULL,
      contrasts = NULL,
      sparse = FALSE,

      initialize = function(vars, sparse = FALSE){
        super$initialize(vars)
        self$sparse <- sparse
      },

      fit = function(data){
        super$fit(data)
        sub_data <- self$vars(data)
        self$contrasts <- purrr::map(sub_data, ~ contrasts(as.factor(.), contrasts=FALSE))
      },

      transform = function(data){
        sub_data <-
          data %>%
          dplyr::select(dplyr::one_of(self$varnames)) %>%
          mutate_all(as.factor)

        if(self$sparse){
          transformed_data <- Matrix::sparse.model.matrix(~ 0 + ., data=sub_data, contrasts.arg = self$contrasts)
          colnames(transformed_data) <- make.names(colnames(transformed_data))

          cbind(
            transformed_data,
            Matrix::Matrix(as.matrix(data %>% dplyr::select(-dplyr::one_of(self$varnames))), sparse = TRUE)
          )
        }else{
          transformed_data <- as.data.frame(model.matrix(~ 0 + ., data=sub_data, contrasts.arg = self$contrasts))
          colnames(transformed_data) <- make.names(colnames(transformed_data))

          (dplyr::bind_cols)(
            transformed_data,
            data %>% dplyr::select(-dplyr::one_of(self$varnames))
          )
        }
      }
    )
  )

#' Create one hot encoder, that turns factor variables into dummy variables.
#' @param vars Function or formula that returns selected columns from a data.frame. Alternatively, character vector of column names.
#' @param sparse If true, return value will be a sparse Matrix of class dcgMatrix. Otherwise, regular data frame.
#' @details If sparse is TRUE, this should be the last step of your preprocessing pipeline.
#' @importFrom maggritr "%>%"
#' @examples
#' df <- data.frame(A=c("a1","a1","a2","a3"), B=c("b1", "b2", "b3", "b4"), X=c(1,2,3,4))
#' prep <- encode_one_hot()
#' prep$fit(df)
#' prep$transform(df)
#' # or more succintly
#' prep$fit_transform(df)
#' @export
encode_one_hot <- function(vars = categoricals, sparse = FALSE){
  OneHotEncoder$new(vars)
}



#### OneHotFuser

OneHotFuser <-
  R6::R6Class(
    "OneHotFuser",

    inherit = Preprocessor,

    public = list(

      out = NULL,
      missing = NULL,
      keep = NULL,

      initialize = function(vars, out, missing, keep){
        super$initialize(vars)
        self$out <- out
        self$missing <- missing
        self$keep <- keep
      },

      fit = function(data){
        super$fit(data)
      },

      transform = function(data){
        sub_data <- data %>% dplyr::select(dplyr::one_of(self$varnames)) %>% as.matrix

        values <- apply(sub_data, 1, function(v) if(all(v == 0)) self$missing else colnames(sub_data)[which(v == 1)])

        if(!self$keep) data <- data %>% dplyr::select(-dplyr::one_of(self$varnames))

        data[[self$out]] <- values

        data
      }
    )
  )

#' Take a set of one hot encoded columns (values: 0 or 1) and transform them back into a single factor column.
#' @param vars Function or formula that returns selected columns from a data.frame. Alternatively, character vector of column names.
#' @param out Name of the new column
#' @param keep Should the original one hot encoded column be kept
#' @param missing Value to use for the rows where all the one hot columns are zero.
#' @importFrom maggritr "%>%"
#' @examples
#' df <- data.frame(A=c("a1","a1","a2","a3"), B=c(1, 0, 0, 1), C=c(0, 1, 0, 0))
#' prep <- fuse_one_hot(vars = c('B', 'C'))
#' prep$fit(df)
#' prep$transform(df)
#' # or more succintly
#' prep$fit_transform(df)
#' @export
fuse_one_hot <- function(vars, out = '.out', missing = 'missing', keep = FALSE){
  OneHotFuser$new(vars, out, missing, keep)
}


#### NumericEncoder

NumericEncoder <-
  R6::R6Class(
    "NumericEncoder",

    inherit = Preprocessor,

    public = list(

      levels = NULL,

      initialize = function(vars){
        super$initialize(vars)
      },

      fit = function(data){
        super$fit(data)
        self$levels <- purrr::map(self$vars(data), ~ levels(as.factor(.)))
      },

      transform = function(data){
        transformed_data <- data

        for(v in intersect(names(self$levels), self$varnames)){
          transformed_data[[v]] <- factor(transformed_data[[v]], levels = self$levels[[v]]) %>% as.integer
        }

        transformed_data
      }
    )
  )

#' Create numeric encoder, that turns factor variables into integers.
#' @param vars Function or formula that returns selected columns from a data.frame. Alternatively, character vector of column names.
#' @importFrom maggritr "%>%"
#' @examples
#' df <- data.frame(A=c("a1","a1","a2","a3"), B=c("b1", "b2", "b3", "b4"), X=c(1,2,3,4))
#' prep <- encode_numeric(~select(., A))
#' prep$fit(df)
#' prep$transform(df)
#' # or more succintly
#' prep$fit_transform(df)
#' @export
encode_numeric <- function(vars = factors){
  NumericEncoder$new(vars)
}


#### ImpactEncoder

ImpactEncoder <-
  R6::R6Class(
    "ImpactEncoder",

    inherit = Preprocessor,

    public = list(

      levels = NULL,
      target = NULL,

      initialize = function(vars, target){
        super$initialize(vars)
        self$target <- target
      },

      fit = function(data){
        super$fit(data)

        subdata <- self$vars(data) %>% dplyr::mutate_all(as.factor)
        vars <- colnames(subdata)
        subdata <- cbind(subdata, data[[self$target]])

        formula_vars <- purrr::rep_along(vars, "(1")
        formula_vars <- paste(formula_vars, paste0(vars, ")"), sep="|")
        formula_vars <- paste(formula_vars, collapse=" + ")

        formula <- as.formula(paste0(self$target, " ~ ", formula_vars))

        sub_model <- lme4::lmer(formula, data=data)

        sub_model_coefs <- coef(sub_model)

        self$levels <- purrr::map(sub_model_coefs, ~ .[[1]])
      },

      transform = function(data){
        transformed_data <- data

        for(v in self$varnames){
          transformed_data[[v]] <- factor(transformed_data[[v]])
          levels(transformed_data[[v]]) <- self$levels[[v]]
          transformed_data[[v]] <- as.numeric(transformed_data[[v]])
        }

        transformed_data
      }
    )
  )

#' Create impact encoder, that turns factor variables into integers.
#' Where as the numeric encoder attributes arbitrary integers to the factor levels,
#' the impact encoder produces ordered levels (with respect to the target).
#' @param vars Function or formula that returns selected columns from a data.frame. Alternatively, character vector of column names.
#' @param target Name of the target variable.
#' @details An approximate description of the algorithm is that the levels of the factors are ordered according to the mean value of the target for each level,
#' but credibility is also taken into account. Under the hood, a model with random effects is used.
#' @importFrom maggritr "%>%"
#' @examples
#' df <- data.frame(A=sample(c('a1', 'a2'), 100, replace=TRUE), y = runif(100))
#' prep <- encode_impact(vars = 'A', target = 'y')
#' prep$fit(df)
#' prep$transform(df)
#' # or more succintly
#' prep$fit_transform(df)
#' @export
encode_impact <- function(vars = categoricals, target){
  ImpactEncoder$new(vars, target)
}



#### Factorizer

Factorizer <-
  R6::R6Class(
    "Factorizer",

    inherit = Preprocessor,

    public = list(

      levels = NULL,
      most_frequent_levels = NULL,

      initialize = function(vars){
        super$initialize(vars)
      },

      fit = function(data){
        super$fit(data)

        transformed_data <-
          data %>%
          (self$vars)

        self$levels <- purrr::map(self$vars(transformed_data), ~ levels(as.factor(.)))

        fitted_freqs <- purrr::map(transformed_data, ~ table(.))
        self$most_frequent_levels <- purrr::map(fitted_freqs, ~ names(sort(., decreasing=TRUE))[1])
      },

      transform = function(data){

        transformed_data <- data

        for(v in intersect(names(self$levels), self$varnames)){

          new_var <- as.factor(transformed_data[[v]])
          new_levels <- levels(new_var)
          unseen_levels <- new_levels[!(new_levels %in% self$levels[[v]])]

          if(length(unseen_levels) > 0){
            # get most frequent fitted level for that factor
            levels(new_var) <- unique(c(levels(new_var), self$most_frequent_levels[[v]]))
            new_var[new_var %in% unseen_levels] <- self$most_frequent_levels[[v]]
          }

          transformed_data[[v]] <- factor(new_var, levels = self$levels[[v]])
        }

        transformed_data
      }
    )
  )

#' Make harmonized factor variables.
#'
#' This function makes selected variables as factors and also harmonizes the levels of a new dataset to match those of the fitted dataset.
#' This is important when a factor variable in a test set contains levels that were not present for that variable during training.
#' In that case, the new levels are replaced by the most frequent value found during training.
#' @param vars Function or formula that returns selected columns from a data.frame. Alternatively, character vector of column names.
#' @importFrom maggritr "%>%"
#' @examples
#' df <- data.frame(A=c("a1","a1","a1","a2"), stringsAsFactors = FALSE)
#' df2 <- data.frame(A=c("a1","a1","a4","a5"), stringsAsFactors = FALSE)
#' prep <- factorize()
#' prep$fit(df)
#' prep$transform(df2)
#' @export
factorize <- function(vars = categoricals){
  Factorizer$new(vars)
}


#### Normalizer

Normalizer <-
  R6::R6Class(
    "Normalizer",

    inherit = Preprocessor,

    public = list(

      means = NULL,
      sds = NULL,

      initialize = function(vars){
        super$initialize(vars)
      },

      fit = function(data){
        super$fit(data)

        transformed_data <-
          data %>%
          (self$vars)

        self$means <- purrr::map(transformed_data, mean, na.rm = TRUE)
        self$sds <- purrr::map(transformed_data, sd, na.rm = TRUE)
      },

      transform = function(data){

        transformed_data <- data

        for(v in intersect(names(self$means), self$varnames)){
          transformed_data[[v]] <- (transformed_data[[v]] - self$means[[v]])/self$sds[[v]]
        }

        transformed_data
      }
    )
  )

#' Normalize columns (i.e. center around the mean and divide by standard deviation).
#' @param vars Function or formula that returns selected columns from a data.frame. Alternatively, character vector of column names.
#' @importFrom maggritr "%>%"
#' @examples
#' df <- data.frame(A=c("a1","a1","a2","a3"), B=c("b1", "b2", "b3", "b4"), X=c(1,2,3,4))
#' prep <- normalize()
#' prep$fit(df)
#' prep$transform(df)
#' # or more succintly
#' prep$fit_transform(df)
#' @export
normalize <- function(vars = numerics){
  Normalizer$new(vars)
}



#### Scaler

Scaler <-
  R6::R6Class(
    "Scaler",

    inherit = Preprocessor,

    public = list(

      mins = NULL,
      maxs = NULL,

      initialize = function(vars){
        super$initialize(vars)
      },

      fit = function(data){
        super$fit(data)

        transformed_data <-
          data %>%
          (self$vars)

        self$mins <- purrr::map(transformed_data, min, na.rm = TRUE)
        self$maxs <- purrr::map(transformed_data, max, na.rm = TRUE)
      },

      transform = function(data){

        transformed_data <- data

        for(v in intersect(names(self$mins), self$varnames)){
          transformed_data[[v]] <- (transformed_data[[v]] - self$mins[[v]])/(self$maxs[[v]] - self$mins[[v]])
        }

        transformed_data
      }
    )
  )

#' Scale columns so that values lie between min and max value.
#' @param vars Function or formula that returns selected columns from a data.frame. Alternatively, character vector of column names.
#' @importFrom maggritr "%>%"
#' @examples
#' df <- data.frame(A=c("a1","a1","a2","a3"), B=c("b1", "b2", "b3", "b4"), X=c(1,2,3,4))
#' prep <- scale()
#' prep$fit(df)
#' prep$transform(df)
#' # or more succintly
#' prep$fit_transform(df)
#' @export
scale_minmax <- function(vars = numerics){
  Scaler$new(vars)
}


#### VarRemover

VarRemover <-
  R6::R6Class(
    "VarExcluder",

    inherit = Preprocessor,

    public = list(

      vars_to_remove = NULL,

      initialize = function(vars){
        super$initialize(vars)
      },

      fit = function(data){
        super$fit(data)
      },

      transform = function(data){
        data %>% dplyr::select(-dplyr::one_of(self$varnames))
      }
    )
  )

#' Remove variables.
#' @param vars Function or formula that returns selected columns to exclude from a data.frame. Alternatively, character vector of column names.
#' @importFrom maggritr "%>%"
#' @examples
#' df <- data.frame(ID=seq(1,4), B=c("b1", "b2", "b3", "b4"), C=c("c","c","c","c"), X=c(1,1,1,1))
#' prep <- remove_vars("ID")
#' prep$fit(df)
#' prep$transform(df)
#' # or more succintly
#' prep$fit_transform(df)
#' @export
remove_vars <- function(vars){
  VarRemover$new(vars)
}


#### ConstantRemover

ConstantRemover <-
  R6::R6Class(
    "ConstantRemover",

    inherit = Preprocessor,

    public = list(

      vars_to_remove = NULL,

      initialize = function(vars){
        super$initialize(vars)
      },

      fit = function(data){
        super$fit(data)

        transformed_data <-
          data %>%
          (self$vars)

        self$vars_to_remove <- names(transformed_data)[purrr::map_lgl(transformed_data, ~ dplyr::n_distinct(.) <= 1)]
      },

      transform = function(data){
        data %>% dplyr::select(-dplyr::one_of(self$vars_to_remove))
      }
    )
  )

#' Remove constant columns.
#' @param vars Function or formula that returns selected columns from a data.frame. Alternatively, character vector of column names.
#' @importFrom maggritr "%>%"
#' @examples
#' df <- data.frame(A=c("a1","a1","a2","a3"), B=c("b1", "b2", "b3", "b4"), C=c("c","c","c","c"), X=c(1,1,1,1))
#' prep <- remove_constants()
#' prep$fit(df)
#' prep$transform(df)
#' # or more succintly
#' prep$fit_transform(df)
#' @export
remove_constants <- function(vars = allvars){
  ConstantRemover$new(vars)
}


#### Binner

Binner <-
  R6::R6Class(
    "Binner",

    inherit = Preprocessor,

    public = list(

      cuts = NULL,
      breaks = NULL,
      labels = NULL,

      initialize = function(vars, breaks, labels=FALSE){
        super$initialize(vars)
        self$breaks <- breaks
        self$labels <- labels
      },

      fit = function(data){
        super$fit(data)

        transformed_data <-
          data %>%
          (self$vars)

        if(length(self$breaks)==1){

          self$cuts <- purrr::map(transformed_data, ~ quantile(., breaks=seq(0, 1, by=1/self$breaks), na.rm=TRUE))

          for(i in 1:length(self$cuts)){
            self$cuts[[i]][1] <- -Inf
            self$cuts[[i]][length(self$cuts[[i]])] <- Inf
          }
        }else{
          self$breaks <- c(-Inf, self$breaks, Inf)
          self$cuts <- purrr::rerun(ncol(transformed_data), self$breaks) %>% setNames(colnames(transformed_data))
        }
      },

      transform = function(data){

        transformed_data <- data

        for(v in intersect(names(self$cuts), self$varnames)){

          transformed_data[[v]] <- cut(transformed_data[[v]],
                                       breaks=self$cuts[[v]],
                                       include.lowest=TRUE,
                                       labels=self$labels)
        }

        transformed_data
      }
    )
  )

#' Bin numeric columns.
#' @param vars Function or formula that returns selected columns from a data.frame. Alternatively, character vector of column names.
#' @param breaks Either an integer representing the number of bins or a vector of break points.
#' @importFrom maggritr "%>%"
#' @examples
#' df <- data.frame(A=rep("a", 100), B=rnorm(100))
#' prep <- bin()
#' prep$fit(df)
#' prep$transform(df)
#' # or more succintly
#' prep$fit_transform(df)
#' @export
bin <- function(vars = numerics, breaks=10){
  Binner$new(vars, breaks=breaks)
}




#' @param x argument to determine properties
#' @param lower lower value `x` can have (scalar)
#' @param upper upper value `x` can have (scalar)
#' @param finite flag indicating if `x` must have only
#' finite values (`TRUE`) or not (flag)
#' @param any.missing flag indicating if missing values are
#' allowed in `x` (flag)
#' @param all.missing flag indicating if the special case where
#' `x` is only composed of missing values is allowed (flag)
#' @param null.ok flag indicating if `x` can be NULL. If TRUE and if
#' `x` is NULL then other parameters are ignored.
#' @name checkmate_usual_params
NULL


#' @importFrom magrittr %>%

# Check on different types of data ----------------------------------------

#' Check the presence of names
#' @inheritParams checkmate_usual_params
#' @name check_names
#' @keywords internal
NULL

#' @describeIn check_names Check if `x` has row names
#' @inheritParams checkmate_usual_params
#' @keywords internal
checkRownamed <- function(x)
{
  if (is.null(rownames(x)))
    return("argument doesn't have row names")

  TRUE
}

#' @importFrom checkmate makeAssertionFunction
#' @name check_names
#' @keywords internal
assertRownamed <- makeAssertionFunction(checkRownamed)

#' @importFrom checkmate makeTestFunction
#' @name check_names
#' @keywords internal
testRownamed <- makeTestFunction(checkRownamed)

#' @describeIn check_names Check if `x` has column names
#' @keywords internal
checkColnamed <- function(x)
{
  if (is.null(colnames(x)))
    return("argument doesn't have column names")

  TRUE
}

#' @importFrom checkmate makeAssertionFunction
#' @name check_names
#' @keywords internal
assertColnamed <- makeAssertionFunction(checkColnamed)

#' @importFrom checkmate makeTestFunction
#' @name check_names
#' @keywords internal
testColnamed <- makeTestFunction(checkColnamed)


# Check on numeric data ------------------------------------------------


#' Check if a value is a vector made of numeric values
#' @inheritParams checkmate_usual_params
#' @name check_numeric_vecto
NULL

#' @importFrom checkmate assertFlag checkVector checkNumeric
#' @keywords internal
checkNumericVector <- function(x, lower = -Inf, upper = Inf, finite = FALSE,
                               any.missing = FALSE, all.missing = FALSE,
                               min.len = 1L, len = NULL, max.len = NULL,
                               unique = FALSE, sorted = FALSE,
                               names = NULL, typed.missing = FALSE,
                               null.ok = FALSE)
{
  assertFlag(null.ok)
  if (null.ok && is.null(x))
    return(TRUE)

  vectorCheck <- checkVector(x,
                             len = len,
                             min.len = min.len,
                             max.len = max.len,
                             null.ok = FALSE)

  if (!isTRUE(vectorCheck))
    return(vectorCheck)

  checkNumeric(x,
               any.missing = any.missing, all.missing = all.missing,
               lower = lower, upper = upper, finite = finite,
               unique = unique, sorted = sorted,
               names = names, typed.missing = typed.missing,
               null.ok = FALSE)

}

#' @importFrom checkmate makeAssertionFunction
#' @keywords internal
assertNumericVector <- makeAssertionFunction(checkNumericVector)

#' @importFrom checkmate makeExpectationFunction
#' @keywords internal
expect_NumericVector <- makeExpectationFunction(checkNumericVector)

#' @importFrom purrr partial
#' @importFrom checkmate makeExpectationFunction
#' @keywords internal
checkPositiveVector <- partial(checkNumericVector, lower = 0.0)
expect_PositiveVector <- makeExpectationFunction(checkPositiveVector)

#' @importFrom purrr partial
#' @importFrom checkmate makeExpectationFunction
#' @keywords internal
checkNegativeVector <- partial(checkNumericVector, upper = 0.0)
expect_NegativeVector <- makeExpectationFunction(checkNegativeVector)



#' @importFrom purrr partial
#' @keywords internal
checkDoubleScalar <- partial(checkNumericVector,
                             min.len = 1L, len = 1L, max.len = 1L,
                             any.missing = FALSE, all.missing = FALSE,
                             null.ok = FALSE, unique = FALSE, sorted = FALSE)

#' @importFrom purrr partial
#' @keywords internal
assertDoubleScalar <- partial(assertNumericVector,
                              min.len = 1L, len = 1L, max.len = 1L,
                              any.missing = FALSE, all.missing = FALSE,
                              null.ok = FALSE, unique = FALSE, sorted = FALSE)


#' @importFrom checkmate checkMatrix checkDataFrame checkTibble
#' @keywords internal
checkTable <- function(table,
                       any.missing = FALSE, all.missing = FALSE,
                       min.rows = 1L, nrows = NULL,
                       min.cols = 1L, ncols = NULL,
                       null.ok = FALSE, mode = NULL, col.names = NULL)
{
  if (requireNamespace("tibble", quietly = TRUE) && tibble::is_tibble(table))
    funCheck <- checkTibble

  else if (is.data.frame(table))
    funCheck <- checkDataFrame

  else if (is.matrix(table))
    funCheck <- checkMatrix

  else
    return("argument is supposed to be a tibble, a data.frame or a matrix")

  if (is.null(mode))
    types <- character(0L) # nolint: object_usage_linter
  else
    types <- mode # nolint: object_usage_linter

  dataEnv <- as.list(environment())

  dataEnv <- dataEnv[intersect(names(dataEnv), names(formals(funCheck)))]

  formals(funCheck)[names(dataEnv)] <- dataEnv

  funCheck(table)
}


# Check on statistics data ------------------------------------------------

#' Checks on statistics data
#' @name check_stats_data
#' @keywords internal
NULL


#' @importFrom checkmate assertFlag
#' @keywords internal
checkProbabilityVec <- function(x,
                                len = NULL, min.len = 1L, max.len = NULL,
                                unique = FALSE,
                                striclyPos = FALSE, striclyUnsure = FALSE)
{
  assertFlag(striclyPos)
  assertFlag(striclyUnsure)

  checkProb <- checkNumericVector(x,
                                  min.len = min.len, len = len,
                                  max.len = max.len,
                                  lower = 0.0, upper = 1.0,
                                  finite = TRUE,
                                  unique = unique)

  if (!isTRUE(checkProb))
    return(checkProb)

  if (striclyPos && any(x == 0.0))
    return("probabilities are supposed to be stricly positive")

  if (striclyUnsure && any(x == 1.0))
    return("probabilities are supposed to be inferior to one")

  TRUE
}

#' @importFrom checkmate makeAssertionFunction
#' @keywords internal
assertProbabilityVec <- makeAssertionFunction(checkProbabilityVec)

#' @keywords internal
checkProbabilityScalar <- partial(checkProbabilityVec,
                                  min.len = 1L,len = 1L, max.len = 1L,
                                  unique = FALSE)

#' @importFrom purrr partial
#' @keywords internal
assertProbabilityScalar <- partial(assertProbabilityVec,
                                   min.len = 1L,len = 1L, max.len = 1L,
                                   unique = FALSE)


#' @describeIn check_stats_data Check if a vector is a double vector
#' made of finite values
#' @importFrom purrr partial
#' @keywords internal
checkMeans <- partial(checkNumericVector, finite = TRUE,
                      any.missing = FALSE, all.missing = FALSE,
                      null.ok = FALSE)

#' @name check_stats_data
#' @importFrom checkmate makeAssertionFunction
#' @keywords internal
assertMeans <- makeAssertionFunction(checkMeans)

#' @name check_stats_data
#' @importFrom purrr partial
#' @keywords internal
checkMean <- partial(checkDoubleScalar, finite = TRUE)


#' @name check_stats_data
#' @importFrom checkmate makeAssertionFunction
#' @keywords internal
assertMean <- makeAssertionFunction(checkMean)

#' Check if a vector if a vector of variances / standar deviations
#' @importFrom purrr partial
#' @keywords internal
checkVariances <- checkSDs <-
  partial(checkNumericVector, lower = 0.0, finite = TRUE,
          any.missing = FALSE, all.missing = FALSE,
          null.ok = FALSE)

#' @keywords internal
checkSDs <- checkVariances

#' @importFrom purrr partial
#' @keywords internal
assertVariances <- assertSDs <-
  partial(assertNumericVector,
          lower = 0.0, finite = TRUE,
          any.missing = FALSE, all.missing = FALSE,
          null.ok = FALSE)


#' @importFrom purrr partial
#' @keywords internal
checkVariance <- checkSD <-
  partial(checkDoubleScalar, lower = 0.0, finite = TRUE)


#' @importFrom checkmate makeAssertionFunction
#' @keywords internal
assertVariance <- assertSD <- makeAssertionFunction(checkVariance)

#' @importFrom checkmate checkMatrix
#' @keywords internal
checkCovarMat <- function(mat, p = nrow(mat))
{
  check <- checkMatrix(mat, mode = "numeric",
                       any.missing = FALSE, all.missing = FALSE,
                       min.rows = 1L, nrows = p, ncols = p,
                       null.ok = FALSE)

  if (!isTRUE(check))
    return(check)


  if (any(diag(mat) < 0.0) || any(is.infinite(mat)))
    return("covariance matrix is supposed to have only finite positive values")

  if (!isSymmetric(mat))
    return("covariance matrix is supposed to be symmetric")

  weigths <- (1.0 / diag(mat)) %>% sqrt()
  covarStand <- weigths %*% mat %*% weigths

  if (any(abs(covarStand) > 1.0))
    return("Some covariances are higher than the variances")

  TRUE
}

#' @importFrom checkmate makeAssertionFunction
#' @keywords internal
assertCovarMat <- makeAssertionFunction(checkCovarMat)

checkCorMat <- function(mat, p = nrow(mat))
{
  checkCovar <- checkCovarMat(mat, p)

  if (!isTRUE(checkCovar))
    return(checkCovar)

  if (!all(diag(mat) == 1.0))
    return("Some elements on the diagonal are not equal to 0")

  TRUE
}

#' @importFrom checkmate makeAssertionFunction
#' @keywords internal
assertCorMat <- makeAssertionFunction(checkCorMat)

#' @importFrom checkmate assertFlag assertCount
#' @keywords internal
checkProbTable <- function(probsTable, N = nrow(probsTable), probVec = TRUE)
{
  assertCount(N, positive = TRUE)
  assertFlag(probVec)
  checkType <- checkTable(probsTable, mode = "numeric", nrows = N)

  if (!isTRUE(checkType))
    return(checkType)

  if (!testColnamed(probsTable))
    return("columns must be named")

  if (probVec)
  {
    probsTable <- add_nr_prob(probsTable, "nC")

    if (any(rowSums(probsTable) != 1.0))
      return("Each row must be a probability")
  }


  if (!all(probsTable >= 0.0) || !all(probsTable <= 1.0))
    return("all values must be probabilities")

  TRUE

}

#' @importFrom checkmate makeAssertionFunction
#' @keywords internal
assertProbTable <- makeAssertionFunction(checkProbTable)

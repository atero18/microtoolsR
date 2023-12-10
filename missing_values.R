#' Somme tools about missing values
#' @param x some object
#' @param notNan flag indicating if NaN values should be considered
#' (`FALSE`) or not (`TRUE`) as missing values (`NA`).
#' @param arr.ind see [which()] for documentation about this parameter.
#' @param useNames see [which()] for documentation about this parameter.
#' @name missing_values_tools
NULL

#' @describeIn missing_values_tools Find if there is at least one NaN in `x`
#' @keywords internal
anyNaN <- function(x) any(is.nan(x))

#' @describeIn missing_values_tools Find if all elements of `x` are missing
#' values.
#' @importFrom checkmate assertFlag
#' @keywords internal
allNA <- function(x, notNan = FALSE)
{
  assertFlag(notNan)
  anyNA(x) && all(is.na(x)) && ifelse(notNan, !anyNaN(x), TRUE)
}


#' @describeIn missing_values_tools Find what element of `x` are true missing
#' values, i.e. `NA` but not `NaN`.
#' @keywords internal
is.na.not.nan <- function(x) is.na(x) & !is.nan(x)

#' @describeIn missing_values_tools Give the position of the missing values
#' in `x`.
#' @importFrom checkmate assertFlag
#' @keywords internal
which.na <- function(x, notNan = FALSE, arr.ind = FALSE, useNames = TRUE)
{
  assertFlag(notNan)

  if (notNan)
    mask <- is.na.not.nan(x)
  else
    mask <- is.na(x)

  which(mask, arr.ind = arr.ind, useNames = useNames)
}

#' @describeIn missing_values_tools Give the position of the NaN values
#' in `x`.
#' @keywords internal
which.nan <- function(x, arr.ind = FALSE, useNames = TRUE)
{
  which(is.nan(x), arr.ind = arr.ind, useNames = useNames)
}


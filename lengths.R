#' Somme tools about length / size
#' @param x some object
#' @name length_tools
NULL

#' @describeIn length_tools Does `x` contains no element?
#' @keywords internal
is_empty <- function(x) length(x) == 0L

#' @describeIn length_tools Does `x` contains no element but is not equal
#' to `NULL` ?
#' @keywords internal
is_empty_but_not_null <- function(x) length(x) == 0L && !is.null(x)

#' @describeIn length_tools Does `x` contains at least one element?
#' @importFrom purrr negagte
#' @keywords internal
is_not_empty <- negate(is_empty)


#' @describeIn length_tools Does `x` has a length in some range `[[m, M]]`?
#' @param m minimum length allowed (positive integer)
#' @param M maximum length allowed (positive integer)
#' @importFrom checkmate assertCount assertInt assertScalar
#' @keywords internal
has_length_between <- function(x, m = 0L, M = Inf)
{
  assertCount(m)
  assertScalar(M)
  
  if (is.finite(M))
  {
    assertInt(M, lower = m)
    m <= length(x) && length(x) <= M
  }
  else
    m <= length(x)
  
}

#' @describeIn length_tools Does `x` has more than one element (at least 2)?
#' @importFrom purrr partuak
#' @keywords internal
has_more_than_one_element <- partial(has_length_between, m = 2L, M = Inf)

#' @describeIn length_tools Does `x` as some specific length?
#' @param len value to compare with actuel length of `x`
#' @importFrom checkmate assertCount assertInt
#' @keywords internal
has_length <- function(x, len)
{
  assertCount(len)
  length(x) == len
}

#' @importFrom purrr negate

#' @describeIn length_tools Does `x` have a length different of `l`?
#' @param l length to compare with the length of `x`.
#' @importFrom checkmate assertCount
#' @keywords internal
length_differs <- negate(has_length)

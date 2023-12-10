none <- function(x, na.rm = FALSE) !any(x, na.rm = na.rm)

some <- function(x, notAll = TRUE)
{
  any(x, na.rm = TRUE) && (!notAll || anyNA(x) || !all(x))
}

not_all <- function(x, na.rm = FALSE) !all(x, na.rm = na.rm)

TRUElogical <- function(n) rep(TRUE, n)

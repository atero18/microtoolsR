#' @importFrom rstudioapi getSourceEditorContext
#' @importFrom stringr str_match
find_needed_libraries <-
  function(file = getSourceEditorContext()$path)
  {
    lines <- readLines(file)

    if (length(lines) == 0L)
      return(character(0L))

    regexs <- c(#"library\\(([\\w]+),?.*\\)",
                #"require\\(([\\w]+),?.*\\)",
                "#' *@importFrom ([\\w]+) \\w",
                "#' *@import (\\w+)\\W?")

    match <- rep(NA_character_, length(lines))
    neededLibraries <- NULL
    for (regex in regexs)
    {
      match <- stringr::str_match(lines, pattern = regex)[, 2L]
      neededLibraries <- c(neededLibraries, match[!is.na(match)])
      match <- match[is.na(match)]
      if (length(match) == 0L)
        break
    }

    unique(neededLibraries)
  }

#' @importFrom checkmate assertFlag
#' @importFrom rstudioapi getSourceEditorContext
#' @export
load_needed_libraries <-
  function(file = rstudioapi::getSourceEditorContext()$path, quietly = FALSE)
  {
    checkmate::assertFlag(quietly)
    for (lib in find_needed_libraries(file))
      library(lib, character.only = TRUE, quietly = quietly)
  }

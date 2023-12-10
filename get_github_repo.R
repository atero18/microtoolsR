require(checkmate)
require(readxl)
require(stringr)

#' @importFrom checkmate assertCharacter
get_github_repo <- function(packages, excel = "./liste_packages_R.xlsx")
{
  if (length(packages) == 0L)
    return(character(0L))

  assertCharacter(packages, min.chars = 2L,
                  any.missing = FALSE, all.missing = FALSE)



  dataPackages <- bind_sheets(excel)[, c("Package", "Github")]
  pos <- match(tolower(packages), tolower(dataPackages$Package))
  as.character(dataPackages$Github[pos])
}

#' @importFrom readxl excel_sheets read_excel
#' @importFrom checkmate assertFileExists
bind_sheets <- function(file, ...)
{
  assertFileExists(file, access = "r")
  sheetsNames <- excel_sheets(file)
  data <- read_excel(path = file, sheet = sheetsNames[1L], ...)
  data$sheet <- sheetsNames[1L]
  for (sheet in sheetsNames[-1L])
  {
    newData <- read_excel(path = file, sheet = sheet, ...)
    newData$sheet <- sheet
    if (nrow(newData) == 0L)
      next

    missingActual <- setdiff(colnames(newData), colnames(data))
    for (var in missingActual)
      data[, var] <- NA

    missingNew <- setdiff(colnames(data), colnames(newData))
    for (var in missingNew)
      newData[, var] <- NA

    data <- rbind(data, newData)

  }
  data
}

#' @importFrom checkmate assertString
#' @importFrom stringr str_match
find_github_package <- function(package)
{
  assertString(package, min.chars = 2L)
  numPath <- 1L
  librariesDirs <- .libPaths()
  while (numPath <= length(librariesDirs))
  {
    descriptionPath <- file.path(librariesDirs[numPath],
                                 package, "DESCRIPTION")
    if (file.exists(descriptionPath))
      break

    numPath <- numPath + 1L
  }

  if (numPath > length(librariesDirs))
  {
    URLs <- CRANPACKAGES$URL[CRANPACKAGES$Package == package]
  }
  else
  {
    URLs <- read.dcf(descriptionPath, "URL", all = FALSE)[, 1L]

  }

  REGEX <- "github.com/([\\w-\\.]+/[\\w-\\.]+)/* *$" #"github.com/(\\w+/\\w+)/* *$"
  URLs <- unlist(strsplit(URLs, ", *"))
  matches <- str_match(URLs, REGEX)[, 2L]

  if (!all(is.na(matches)))
    return(matches[!is.na(matches)][1L])

  NA_character_
}

assign("CRANPACKAGES", tools::CRAN_package_db()[, c("Package", "URL")],
       envir = environment(find_github_package))

find_all_github <- function(excel = "./liste_packages_R.xlsx")
{
  data <- bind_sheets(excel)[, c("sheet", "Package", "Github")]

  missingGithub <- which(is.na(data$Github))
  CRANPACKAGES <- NULL
  for (i in missingGithub)
  {
    package <- data$Package[i]
    cat(package)
  }

  data

}

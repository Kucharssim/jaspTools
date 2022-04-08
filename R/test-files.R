#' @title Create or edit test files
#'
#' @description Both functions mimic [usethis::use_test()].
#' [useTest()] creates a "consistency" test file,
#' [useTestVignette()] creates a "verified" test file,
#' used as a vignette and a verified unit test.
#'
#' @param analysis String name of the analysis to create a test file for (case sensitive).
#' @param name String name of the test (optional).
#' @param open Do you want to open the file automatically in RStudio?
#'
#' @name useTest
NULL

#' @rdname useTest
#' @export
useTest <- function(analysis, name=NULL, open = rlang::is_interactive()) {
  usethis::use_directory(file.path("tests", "testthat"))

  if(is.null(name)) {
    file <- sprintf("test-%1$s.R",      analysis)
  } else {
    file <- sprintf("test-%1$s-%2$s.R", analysis, name)
  }

  path <- file.path("tests", "testthat", file)

  if(!file.exists(path)) {
    template <- "consistency-test.R"
    usethis::use_template(
      template = template, save_as = path, open = FALSE, package = "jaspTools",
      data = list(analysis = analysis, name = name)
      )
  }
  usethis::edit_file(usethis::proj_path(path), open = open)
}

#' @title Create or edit JASP vignettes
#'
#' @description This function mimics [usethis::use_article()],
#' but uses a template for the vignette that is tailored for JASP-styled vignettes.
#' This vignette then can be run as a verified unit test using [testVignette()] and [testVignettes()].
#'
#' @details Running this function configures the module to have test vignettes.
#' If necessary, it creates the \code{vignettes/tests} folder, adds \code{knitr} to the \code{Suggests}
#' field and configures \code{VignetteBuilder} in the \code{DESCRIPTION} file,
#' adds \code{inst/doc} folder in \code{.gitignore}, and adds \code{vignettes/tests} into \code{.Rbuildignore}.
#'
#' Then, it creates a file based on the analysis name and name of the test file.
#'
#' [testVignette()] and [testVignettes()] can be used for testing the vignette(s).
#'
#' @param analysis String name of the analysis that is shown in the vignette (case sensitive).
#' @param title String title of the vignette (optional).
#' @param name String name of the test file.
#' @param open Do you want to open the file automatically in RStudio?
#'
#' @export
useTestVignette <- function(analysis, title, name = title, open = rlang::is_interactive()) {
  setupTestVignettes()

  name <- name |>
    strsplit("[^a-zA-Z0-9_-]+") |>
    unlist() |>
    paste(collapse = "-") |>
    tolower()
  file <- paste0(tolower(analysis), "-", name, ".Rmd")
  path <- file.path("vignettes", "tests", file)
  if(!file.exists(path)) {
    usethis::use_template(template = "vignette.Rmd", save_as = path, open = FALSE, package = "jaspTools",
                          data = list(module   = usethis:::project_name(),
                                      analysis = analysis,
                                      name     = name,
                                      title    = title)
                          )
  }
  usethis::edit_file(usethis::proj_path(path), open = open)
}

setupTestVignettes <- function() {
  setKnitrAsVignetteBuilder()
  usethis::use_directory(file.path("vignettes", "tests"))
  usethis::use_build_ignore("vignettes/tests")
  usethis::use_git_ignore("inst/doc")
}

setKnitrAsVignetteBuilder <- function () {
  usethis::use_package(package = "knitr", type = "Suggests")

  name  <- "VignetteBuilder"
  value <- "knitr"
  curr  <- desc::desc_get(name, file = usethis::proj_get())[[1]]
  curr  <- gsub("^\\s*|\\s*$", "", curr)
  if (identical(curr, value)) {
    return(invisible())
  }
  usethis::ui_done("Setting {usethis::ui_field(name)} field in DESCRIPTION to {usethis::ui_value(value)}")
  desc::desc_set(name, value, file = usethis::proj_get())
  invisible()
}

#' @title Test JASP Vignettes
#'
#' @description These functions allow you to test vignettes in the \code{vignettes/tests} folder.
#' \code{testVignette()} tests one specific file, \code{testVignettes()} can test all files in the folder,
#' or files associated with a specific analysis.
#'
#' @details See \link{useTestVignette()} for help on how to create a JASP test vignette.
#'
#' @param name Name of the file inside of the \code{vignettes/tests} folder
#' @param analysis String name of the analysis to be tested.
#'
#' @name testVignette
NULL

#' @rdname testVignette
#' @export
testVignette <- function(name) {
  testFile <- .makeTestVignetteFile(name)
  testthat::test_file(testFile)
  file.remove(testFile)
  invisible()
}

#' @rdname testVignette
#' @export
testVignettes <- function(analysis) {
  testFiles <- .makeTestVignetteFiles(analysis)
  results <- testthat::test_local(filter = "vignette-")

  for(file in testFiles) {
    file.remove(file)
  }
  invisible(results)
}

.makeTestVignetteFile <- function(name) {
  input <- usethis::proj_path("vignettes", "tests", name, ext = "Rmd")
  if(!file.exists(input)) stop("Vignette does not exist!")

  usethis::use_directory(file.path("tests", "testthat"))
  file   <- paste0("test-vignette-", name, ".R")
  output <- usethis::proj_path("tests", "testthat", file)

  purl <- knitr::purl(input = input, output = output, documentation = 1, quiet = TRUE)
  return(purl)
}

.makeTestVignetteFiles <- function(analysis) {
  if(missing(analysis)) {
    pattern <- NULL
  } else {
    pattern <- paste0(tolower(analysis), "-")
  }

  path <- usethis::proj_path("vignettes", "tests")
  files <- list.files(path, recursive = FALSE, pattern = pattern, full.names = FALSE, ignore.case = TRUE)
  names <- gsub(".Rmd$", "", files)

  testFiles <- vapply(names, .makeTestVignetteFile, character(1))

  return(testFiles)
}

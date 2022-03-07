#' @title Create or edit test files
#'
#' @description Both functions mimic \link[usethis]{usethis::use_test()}.
#' \code{useTest()} creates a "consistency" test file,
#' \code{useVerifiedTest()} creates a "verified" test file,
#' meant to be converted into a vignette and a chapter in the jasp-verification-project.
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
  .useTest(analysis, name, "consistency", open)
}

#' @rdname useTest
#' @export
useVerifiedTest <- function(analysis, name=NULL, open = rlang::is_interactive()) {
  .useTest(analysis, name, "verified", open)
}

.useTest <- function(analysis, name=NULL, type = c("consistency", "verified"), open = rlang::is_interactive()) {
  type <- match.arg(type)

  if(is.null(name)) {
    file <- sprintf("test-%1$s-%2$s.R",      type, analysis      )
  } else {
    file <- sprintf("test-%1$s-%2$s-%3$s.R", type, analysis, name)
  }

  path <- file.path("tests", "testthat", file)

  if(!file.exists(path)) {
    template <- switch(type,
                       "consistency" = "consistency-test.R",
                       "verified"    = "verified-test.R"
                       )
    usethis::use_template(template, save_as = path, open = FALSE, package = "jaspTools", data = list(analysis = analysis, name = name))
  }
  usethis::edit_file(usethis::proj_path(path), open = open)
}

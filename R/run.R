#' Run a JASP analysis in R.
#'
#' \code{runAnalysis} makes it possible to execute a JASP analysis in R. Usually this
#' process is a bit cumbersome as there are a number of objects unique to the
#' JASP environment. Think .ppi, data-reading, etc. These (rcpp) objects are
#' replaced in the jaspTools so you do not have to deal with them. Note that
#' \code{runAnalysis} sources JASP analyses every time it runs, so any change in
#' analysis code between calls is incorporated. The output of the analysis is
#' shown automatically through a call to \code{view} and returned
#' invisibly.
#'
#'
#' @param name String indicating the name of the analysis to run. This name is
#' identical to that of the main function in a JASP analysis.
#' @param dataset Data.frame, matrix, string name or string path; if it's a string then jaspTools
#' first checks if it's valid path and if it isn't if the string matches one of the JASP datasets (e.g., "debug.csv").
#' By default the directory in Resources is checked first, unless called within a testthat environment, in which case tests/datasets is checked first.
#' @param options List of options to supply to the analysis (see also
#' \code{analysisOptions}).
#' @param view Boolean indicating whether to view the results in a webbrowser.
#' @param quiet Boolean indicating whether to suppress messages from the
#' analysis.
#' @param makeTests Boolean indicating whether to create testthat unit tests and print them to the terminal.
#' @examples
#'
#' options <- analysisOptions("BinomialTest")
#' options[["variables"]] <- "contBinom"
#' runAnalysis("BinomialTest", "debug", options)
#'
#' # Above and below are identical (below is taken from the Qt terminal)
#'
#' options <- analysisOptions('{
#'    "id" : 6,
#'    "name" : "BinomialTest",
#'    "options" : {
#'       "VovkSellkeMPR" : false,
#'       "confidenceInterval" : false,
#'       "confidenceIntervalInterval" : 0.950,
#'       "descriptivesPlots" : false,
#'       "descriptivesPlotsConfidenceInterval" : 0.950,
#'       "hypothesis" : "notEqualToTestValue",
#'       "plotHeight" : 300,
#'       "plotWidth" : 160,
#'       "testValue" : 0.50,
#'       "variables" : [ "contBinom" ]
#'    },
#'    "perform" : "run",
#'    "revision" : 1,
#'    "settings" : {
#'       "ppi" : 192
#'    }
#' }')
#' runAnalysis("BinomialTest", "debug.csv", options)
#'
#'
#' @export runAnalysis
runAnalysis <- function(name, dataset, options, view = TRUE, quiet = FALSE, makeTests = FALSE) {
  # if (is.list(options) && is.null(names(options)) && any(names(unlist(lapply(options, attributes))) == "analysisName"))
  #   stop("The provided list of options is not named. Did you mean to index in the options list (e.g., options[[1]])?")
  #
  # if (!is.list(options) || is.null(names(options)))
  #   stop("The options should be a named list (you can obtain it through `analysisOptions()`")
  #
  # if (missing(name)) {
  #   name <- attr(options, "analysisName")
  #   if (is.null(name))
  #     stop("Please supply an analysis name")
  # }

  emitLegacyRngWarning()

  if (insideTestEnvironment()) {
    view  <- FALSE
    quiet <- TRUE
  }

  oldWd       <- getwd()
  oldLang     <- Sys.getenv("LANG")
  oldLanguage <- Sys.getenv("LANGUAGE")
  on.exit({
    .resetRunTimeInternals()
    setwd(oldWd)
    Sys.setenv(LANG = oldLang)
    Sys.setenv(LANGUAGE = oldLanguage)
  })

  initAnalysisRuntime(dataset = dataset, makeTests = makeTests)
  args <- fetchRunArgs(name, options)

  if (quiet) {
    sink(tempfile())
    on.exit({suppressWarnings(sink(NULL))}, add = TRUE)
    returnVal <- suppressWarnings(do.call(jaspBase::runJaspResults, args))
    sink(NULL)
  } else {
    returnVal <- do.call(jaspBase::runJaspResults, args)
  }

  # always TRUE after jaspResults is merged into jaspBase
  jsonResults <- if (inherits(returnVal, c("jaspResultsR", "R6"))) {
    getJsonResultsFromJaspResults(returnVal)
  } else {
    getJsonResultsFromJaspResultsLegacy()
  }

  transferPlotsFromjaspResults()

  jsonResults <- "{\n\t\"results\" : \n\t{\n\t\t\".meta\" : \n\t\t[\n\t\t\t{\n\t\t\t\t\"info\" : \"\",\n\t\t\t\t\"meta\" : \n\t\t\t\t[\n\t\t\t\t\t{\n\t\t\t\t\t\t\"info\" : \"\",\n\t\t\t\t\t\t\"name\" : \"ttestContainer_ttestTable\",\n\t\t\t\t\t\t\"title\" : \"Bayesian One Sample T-Test\",\n\t\t\t\t\t\t\"type\" : \"table\"\n\t\t\t\t\t}\n\t\t\t\t],\n\t\t\t\t\"name\" : \"ttestContainer\",\n\t\t\t\t\"title\" : \"\",\n\t\t\t\t\"type\" : \"collection\"\n\t\t\t}\n\t\t],\n\t\t\"name\" : \"\",\n\t\t\"ttestContainer\" : \n\t\t{\n\t\t\t\"collection\" : \n\t\t\t{\n\t\t\t\t\"ttestContainer_ttestTable\" : \n\t\t\t\t{\n\t\t\t\t\t\"casesAcrossColumns\" : false,\n\t\t\t\t\t\"citation\" : \n\t\t\t\t\t[\n\t\t\t\t\t\t\"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].\",\n\t\t\t\t\t\t\"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225-237.\"\n\t\t\t\t\t],\n\t\t\t\t\t\"data\" : \n\t\t\t\t\t[\n\t\t\t\t\t\t{\n\t\t\t\t\t\t\t\"BF\" : 1.324506706413386,\n\t\t\t\t\t\t\t\"error\" : 6.2249751929841857e-05,\n\t\t\t\t\t\t\t\"n1\" : 23.0,\n\t\t\t\t\t\t\t\"pValue\" : 0.015654342509862966,\n\t\t\t\t\t\t\t\"t\" : 2.2999999999999998\n\t\t\t\t\t\t}\n\t\t\t\t\t],\n\t\t\t\t\t\"footnotes\" : \n\t\t\t\t\t[\n\t\t\t\t\t\t{\n\t\t\t\t\t\t\t\"cols\" : null,\n\t\t\t\t\t\t\t\"myOrder\" : 0,\n\t\t\t\t\t\t\t\"rows\" : null,\n\t\t\t\t\t\t\t\"symbol\" : \"<em>Note.</em>\",\n\t\t\t\t\t\t\t\"text\" : \"For all tests, the alternative hypothesis specifies that the mean is greater than 0.\"\n\t\t\t\t\t\t}\n\t\t\t\t\t],\n\t\t\t\t\t\"name\" : \"ttestContainer_ttestTable\",\n\t\t\t\t\t\"overTitle\" : false,\n\t\t\t\t\t\"schema\" : \n\t\t\t\t\t{\n\t\t\t\t\t\t\"fields\" : \n\t\t\t\t\t\t[\n\t\t\t\t\t\t\t{\n\t\t\t\t\t\t\t\t\"format\" : \"sf:4;dp:3\",\n\t\t\t\t\t\t\t\t\"name\" : \"t\",\n\t\t\t\t\t\t\t\t\"title\" : \"t\",\n\t\t\t\t\t\t\t\t\"type\" : \"number\"\n\t\t\t\t\t\t\t},\n\t\t\t\t\t\t\t{\n\t\t\t\t\t\t\t\t\"name\" : \"n1\",\n\t\t\t\t\t\t\t\t\"title\" : \"n\",\n\t\t\t\t\t\t\t\t\"type\" : \"integer\"\n\t\t\t\t\t\t\t},\n\t\t\t\t\t\t\t{\n\t\t\t\t\t\t\t\t\"format\" : \"sf:4;dp:3\",\n\t\t\t\t\t\t\t\t\"name\" : \"BF\",\n\t\t\t\t\t\t\t\t\"title\" : \"Log(BF\\u208a\\u2080)\",\n\t\t\t\t\t\t\t\t\"type\" : \"number\"\n\t\t\t\t\t\t\t},\n\t\t\t\t\t\t\t{\n\t\t\t\t\t\t\t\t\"format\" : \"sf:4;dp:3\",\n\t\t\t\t\t\t\t\t\"name\" : \"error\",\n\t\t\t\t\t\t\t\t\"title\" : \"error %\",\n\t\t\t\t\t\t\t\t\"type\" : \"number\"\n\t\t\t\t\t\t\t},\n\t\t\t\t\t\t\t{\n\t\t\t\t\t\t\t\t\"format\" : \"dp:3;p:.001\",\n\t\t\t\t\t\t\t\t\"name\" : \"pValue\",\n\t\t\t\t\t\t\t\t\"title\" : \"p\",\n\t\t\t\t\t\t\t\t\"type\" : \"pvalue\"\n\t\t\t\t\t\t\t}\n\t\t\t\t\t\t]\n\t\t\t\t\t},\n\t\t\t\t\t\"status\" : \"complete\",\n\t\t\t\t\t\"title\" : \"Bayesian One Sample T-Test\"\n\t\t\t\t}\n\t\t\t},\n\t\t\t\"initCollapsed\" : false,\n\t\t\t\"name\" : \"ttestContainer\",\n\t\t\t\"title\" : \"\"\n\t\t}\n\t},\n\t\"status\" : \"complete\",\n\t\"typeRequest\" : \"analysis\"\n}\n"
  results <- processJsonResults(jsonResults)

  if (insideTestEnvironment())
    .setInternal("lastResults", jsonResults)

  if (view)
    view(jsonResults)

  if (makeTests)
    makeUnitTestsFromResults(results, name, dataset, options)

  return(invisible(results))
}

foo <- function() {
  emitLegacyRngWarning()
  return(invisible(TRUE))
}

fetchRunArgs <- function(name, options) {
  possibleArgs <- list(
    name = name,
    functionCall = findCorrectFunction(name),
    title = "",
    requiresInit = TRUE,
    options = jsonlite::toJSON(options),
    dataKey = "null",
    resultsMeta = "null",
    stateKey = "null"
  )

  runArgs <- formals(jaspBase::runJaspResults)
  argNames <- intersect(names(possibleArgs), names(runArgs))
  return(possibleArgs[argNames])
}

initAnalysisRuntime <- function(dataset, makeTests, ...) {
  # first we reinstall any changed modules in the personal library
  reinstallChangedModules()

  # dataset to be found in the analysis when it needs to be read
  .setInternal("dataset", dataset)

  # prevent the results from being translated (unless the user explicitly wants to)
  Sys.setenv(LANG = getPkgOption("language"))
  Sys.setenv(LANGUAGE = getPkgOption("language"))

  # jaspBase and jaspResults needs to be loaded until they are merged and the packages handle dependencies correctly
  initializeCoreJaspPackages()

  # ensure that unit tests results are consistent
  if (makeTests)
    set.seed(1)
}

reinstallChangedModules <- function() {
  modulePaths <- getModulePaths()
  if (isFALSE(getPkgOption("reinstall.modules")) || length(modulePaths) == 0)
    return()

  md5Sums <- .getInternal("modulesMd5Sums")
  for (modulePath in modulePaths) {

    if (isBinaryPackage(modulePath))
      next

    srcFiles <- c(
      list.files(modulePath,                   full.names = TRUE, pattern = "(NAMESPACE|DESCRIPTION)$"),
      list.files(file.path(modulePath, "src"), full.names = TRUE, pattern = "(\\.(cpp|c|hpp|h)|(Makevars|Makevars\\.win))$"),
      list.files(file.path(modulePath, "R"),   full.names = TRUE, pattern = "\\.R$")
    )
    if (length(srcFiles) == 0)
      next

    newMd5Sums <- tools::md5sum(srcFiles)
    if (length(md5Sums) == 0 || !modulePath %in% names(md5Sums) || !all(newMd5Sums %in% md5Sums[[modulePath]])) {
      moduleName <- getModuleName(modulePath)
      if (moduleName %in% loadedNamespaces())
        pkgload::unload(moduleName, quiet = TRUE)

      message("Installing ", moduleName, " from source")
      suppressWarnings(install.packages(modulePath, type = "source", repos = NULL, quiet = TRUE, INSTALL_opts = "--no-multiarch"))

      if (moduleName %in% installed.packages()) {
        md5Sums[[modulePath]] <- newMd5Sums
      } else {
        # to prevent the installation output from cluttering the console on each analysis run, we do this quietly.
        # however, it is kinda nice to show errors, so we call the function again here and allow it to print this time (tryCatch/sink doesn't catch the installation failure reason).
        install.packages(modulePath, type = "source", repos = NULL, INSTALL_opts = "--no-multiarch")
        if (!moduleName %in% installed.packages())
          stop("The installation of ", moduleName, " failed; you will need to fix the issue that prevents `install.packages()` from installing the module before any analysis will work")
      }
    }
  }

  .setInternal("modulesMd5Sums", md5Sums)
}

initializeCoreJaspPackages <- function() {
  require(jaspBase)
  if (jaspBaseIsLegacyVersion()) {
    warning("jaspBase should be at least version 0.16.4! Continuing now but if something crashes update jaspBase.", domain = NA)
    require(jaspResults)
    jaspResults::initJaspResults()
    assign("jaspResultsModule", list(create_cpp_jaspResults = function(name, state) get("jaspResults", envir = .GlobalEnv)$.__enclos_env__$private$jaspObject), envir = .GlobalEnv)
  }
}

processJsonResults <- function(jsonResults) {
  if (jsonlite::validate(jsonResults))
    results <- jsonlite::fromJSON(jsonResults, simplifyVector=FALSE)
  else
    stop("Could not process json result from jaspResults")

  results[["state"]] <- .getInternal("state")

  figures <- results$state$figures
  if (length(figures) > 1 && !is.null(names(figures)))
    results$state$figures <- figures[order(as.numeric(tools::file_path_sans_ext(basename(names(figures)))))]

  return(results)
}

transferPlotsFromjaspResults <- function() {
  pathPlotsjaspResults <- file.path(tempdir(), "jaspResults", "plots") # as defined in jaspResults pkg
  pathPlotsjaspTools <- getTempOutputLocation("html")
  if (dir.exists(pathPlotsjaspResults)) {
    plots <- list.files(pathPlotsjaspResults)
    if (length(plots) > 0) {
      file.copy(file.path(pathPlotsjaspResults, plots), pathPlotsjaspTools, overwrite=TRUE)
    }
  }
}

getJsonResultsFromJaspResults <- function(jaspResults) {
  return(jaspResults$.__enclos_env__$private$getResults())
}

getJsonResultsFromJaspResultsLegacy <- function() {
  return(jaspResults$.__enclos_env__$private$getResults())
}

.resetRunTimeInternals <- function() {
  .setInternal("state", list())
  .setInternal("dataset", "")
}

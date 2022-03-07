options <- jaspTools::analysisOptions("{{{ analysis }}}")

set.seed(1)
results <- jaspTools::runAnalysis(
  name    = "{{{ analysis }}}",
  dataset = "test.csv",
  options = options
  )

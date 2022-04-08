#' JASP Snapshot Testing
#'
#' @description Wrapper around [testthat::expect_snapshot()] taking care of testing tables and plots produced by JASP.
#'
#' @param x Code to evaluate.
#' @param digits Numerical tolerance; any differences (in the sense of [base::all.equal()]) smaller than this value will be ignored.
#' @param name The name of the reference plot (a .svg stored in \code{/tests/testthat/_snaps}).
#' @name expect_jasp_output
NULL

#' @rdname expect_jasp_output
#' @export
expect_jasp_table <- function(x, digits = 10) {
  testthat::expect_snapshot_output(x = print(x, digits = digits))
}

#' @rdname expect_jasp_output
#' @export
expect_jasp_plot <- function(x, name) {
  expect_equal_plots(test = x, name = name)
}

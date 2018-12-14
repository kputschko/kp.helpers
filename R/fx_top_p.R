#' Title
#'
#' @param data A data.frame
#' @param p The proportion to sample
#' @param wt The unquoted column to use for ranking
#'
#' @return A data.frame
#' @export
#'
#' @examples fx_top_p(data = mtcars, p = 0.25, wt = "mpg")

fx_top_p <- function(data, p, wt) {

  if (!p <= 1 && p >= 0) rlang::abort("p must be between 0 and 1")
  if (!is.character(wt)) rlang::abort("wt must be a quoted column from the data.frame")
  if (!wt %in% colnames(data)) rlang::abort("wt must be a quoted column from the data.frame")

  n <- nrow(data)
  p <- ceiling(n * p)
  wt <- rlang::sym(wt)

  dplyr::top_n(x = data, n = p, wt = !!wt)

}

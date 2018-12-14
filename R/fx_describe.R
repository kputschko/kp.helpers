#' Title
#'
#' @param data A data.frame
#' @param output_format Type of values the output table should have.  Default is "character" for proper display. Use "numeric" for unrounded values.
#'
#' @return character vector of names
#'
#' @importFrom foreach %do%
#'
#' @export
#' @examples fx_describe(mtcars)

fx_describe <- function(data, output_format = "character") {

  if (!is.data.frame(data)) {
    rlang::abort(
      stringr::str_glue("Function requires a data.frame.  You have given an object of class {class(data)[[1]]}")
    )}

  if (!requireNamespace("foreach", quietly = TRUE)) {
    rlang::abort("Package foreach is required for this function.")}

  if (!output_format %in% c("numeric", "character")) {
    rlang::abort(
      stringr::str_glue("You specified {output_format}.  Please use either 'numeric' or 'character'")
    )}

  foreach::foreach(i = seq_along(data), .combine = "rbind") %do% {
  # foreach(i = 1:ncol(data), .combine = "rbind") %do% {

    index <- i
    column_name  <- data %>% colnames() %>% purrr::pluck(index)
    column_type  <- data[[column_name]] %>% class()
    column_rlang <- rlang::sym(column_name)
    filter_rlang <- rlang::parse_quo(x = stringr::str_glue("is.na({column_name})"), env = rlang::caller_env())

    # n_missing <- data %>% filter(!!! filter_rlang) %>% nrow()

    if (column_type %in% c("factor", "character")) {

      data %>%
        dplyr::summarise("n" = dplyr::n(),
                         "n_distinct" = dplyr::n_distinct(!! column_rlang, na.rm = TRUE),
                         "n_missing"  = sum(is.na(!! column_rlang))) %>%
        dplyr::mutate("column_name" = !! column_name,
                      "column_type" = !! column_type,
                      "min" = NA,
                      "med" = NA,
                      "max" = NA,
                      "mean" = NA,
                      "sd" = NA
        ) %>%
        dplyr::select("column_name",
                      "column_type",
                      "n",
                      "n_missing",
                      "n_distinct",
                      "min",
                      "max",
                      "mean",
                      "sd")

    } else {

      calculations <-
        data %>%
        dplyr::summarise("n" = dplyr::n(),
                         "n_distinct" = dplyr::n_distinct(!! column_rlang, na.rm = TRUE),
                         "n_missing"  = sum(is.na(!! column_rlang)),
                         "mean" = mean(!! column_rlang, na.rm = TRUE),
                         "sd" = stats::sd(!! column_rlang, na.rm = TRUE),
                         "min" = min(!! column_rlang, na.rm = TRUE),
                         "med" = stats::median(!! column_rlang, na.rm = TRUE),
                         "max" = max(!! column_rlang, na.rm = TRUE)) %>%
        dplyr::mutate("column_name" = !! column_name,
                      "column_type" = !! column_type) %>%
        dplyr::select("column_name",
                      "column_type",
                      "n",
                      "n_missing",
                      "n_distinct",
                      "min",
                      "max",
                      "mean",
                      "sd")

      if (output_format == "character") {
        calculations %>% dplyr::mutate_if(is.numeric, scales::number_format(accuracy = 0.0001))
      } else {
        calculations
      }
    }
  }
}

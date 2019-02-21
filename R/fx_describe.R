#' Title
#'
#' @param data A data.frame
#' @param output_format Type of values the output table should have.  Default is "character" for proper display. Use "numeric" for unrounded values.
#' @param percentile_include Should a listing of percentiles be included in the output summary?
#' @param percentile_probs The way in which the percentiles
#'
#' @return character vector of names
#'
#' @importFrom foreach %do%
#'
#' @export
#' @examples fx_describe(mtcars)

fx_describe <- function(data,
                        output_format = "character",
                        percentile_include = FALSE,
                        percentile_probs = seq(0, 1, by = 0.10)) {

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

  if (!is.logical(percentile_include)) {
    rlang::abort("percentile_include value must be either TRUE or FALSE")
  }

  if (any(percentile_probs > 1, percentile_probs < 0, !is.numeric(percentile_probs))) {
    rlang::abort("percentile_probs must be a vector of numbers between 0 and 1")
  }


  default_summary <-
    foreach::foreach(i = seq_along(data), .combine = "rbind") %do% {
      # foreach(i = 1:ncol(data), .combine = "rbind") %do% {

      index <- i
      column_name  <- data %>% colnames() %>% purrr::pluck(index)
      column_type  <- data[[column_name]] %>% class()
      column_rlang <- rlang::sym(column_name)

      # filter_rlang <- rlang::parse_quo(x = stringr::str_glue("is.na({column_name})"), env = rlang::caller_env())
      # n_missing <- data %>% filter(!!! filter_rlang) %>% nrow()

      if (column_type %>% stringr::str_detect(c("factor", "character")) %>% any()) {

        data %>%
          dplyr::summarise("n" = dplyr::n(),
                           "n_distinct" = dplyr::n_distinct(!! column_rlang, na.rm = TRUE),
                           "n_missing"  = sum(is.na(!! column_rlang))) %>%
          dplyr::mutate("column_name" = !! column_name,
                        "column_type" = stringr::str_c(!! column_type, collapse = ", "),
                        "pct_missing" = n_missing / n,
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
                        "pct_missing",
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
                        "column_type" = stringr::str_c(!! column_type, collapse = ", "),
                        "pct_missing" = n_missing / n) %>%
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
          calculations %>% dplyr::mutate_if(is.numeric, scales::number_format(accuracy = 0.0001, big.mark = ",")) %>% tibble::as_tibble()
        } else {
          calculations %>% tibble::as_tibble()
        }
      }
    } # close foreach

  if (percentile_include) {

    quantiles <-
      mtcars %>%
      purrr::map_if(is.numeric, quantile, probs = percentile_probs) %>%
      tibble::enframe() %>%
      tidyr::unnest() %>%
      dplyr::group_by(name) %>%
      dplyr::mutate("p" = sequence(n()),
             "p2" = stringr::str_c(p*10, "%")) %>%
      dplyr::select(-p) %>%
      tidyr::spread(p2, value)


    default_summary %>% dplyr::left_join(quantiles)

  } else {
    default_summary
  }

} # close function

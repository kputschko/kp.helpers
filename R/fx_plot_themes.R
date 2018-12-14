
#' Title KP's ggplot themes
#'
#' @return ggplot theme for light and dark plots
#' @export
#'
#' @examples
#' kp_themes <- fx_plot_themes()
#' kp_themes$light_theme
#' kp_themes$dark_theme

fx_plot_themes <- function() {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required to use this function.")
  }

  list(

    light_theme =
      ggplot2::theme_minimal() +
      ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, face = "bold"),
                     strip.text.x = ggplot2::element_text(face = "bold"),
                     panel.background = ggplot2::element_rect(color = "gray"),
                     panel.grid.major.x = ggplot2::element_line(color = "gray", linetype = 3),
                     panel.grid.minor.x = ggplot2::element_line(color = "gray", linetype = 3),
                     legend.title.align = 0.5),

    dark_theme =
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank(),
                     strip.text.y = ggplot2::element_text(angle = 0, face = "bold"),
                     strip.text.x = ggplot2::element_text(face = "bold"),
                     panel.grid.major.y = ggplot2::element_line(linetype = "solid", color = "#707073"),
                     panel.background = ggplot2::element_rect(color = "gray"),
                     rect = ggplot2::element_rect(fill = "#2a2a2b"),
                     legend.title.align = 0.5)
  )

}

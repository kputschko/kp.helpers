
#' Title KP's ggplot themes
#'
#' @param style Chosen style for ggplot output. Valid options are 'light' or 'dark'
#'
#' @return ggplot theme for light and dark plots
#' @export
#'
#' @examples
#' theme_kp("light")
#' theme_kp("dark")

theme_kp <- function(style = "light") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("call library(ggplot2) to use this function")
  }

  if (!style %in% c("light", "dark")) {
    stop("only 'light' and 'dark' themes are available")
  }

  switch(style,

         light =
           ggplot2::theme_minimal() +
           ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, face = "bold"),
                          strip.text.x = ggplot2::element_text(face = "bold"),
                          panel.background = ggplot2::element_rect(color = "gray"),
                          panel.grid.major.x = ggplot2::element_line(color = "gray", linetype = 3),
                          panel.grid.minor.x = ggplot2::element_line(color = "gray", linetype = 3),
                          legend.title.align = 0.5),

         dark =
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

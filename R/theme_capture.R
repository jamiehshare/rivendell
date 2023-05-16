#' theme_capture
#'
#' A ggplot2 based theme for Capture graphics
#'
#' @param base_size base font size, given in pts
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#' @param ...
#'
#' @export
#'
#' @examples
#' p1 <- ggplot(mtcars2) +
#' geom_point(aes(x = wt, y = mpg, colour = gear)) +
#' + theme_capture()
#'
theme_capture <- function(base_size = 11, base_family = "GT Walsheim Pro",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22, ...) {

  half_line <- base_size / 2

  ggplot2::theme_grey(base_size = base_size,
                      base_family = base_family,
                      base_line_size = base_line_size,
                      base_rect_size = base_rect_size) %+replace%
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 15,
                                         hjust = 0.5,
                                         vjust = 1,
                                         margin = margin(b = half_line)),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white",
                                               colour = NA),
      axis.line = ggplot2::element_line(colour = "grey20"),
      axis.ticks = ggplot2::element_line(colour = "grey20"),
      axis.text = ggplot2::element_text(colour = "grey30",
                                        size = rel(0.8)),
      axis.title = ggplot2::element_text(colour = "grey30"),
      panel.grid = ggplot2::element_line(colour = "grey92"),
      panel.grid.minor = ggplot2::element_line(linewidth = rel(0.5)),
      strip.background = ggplot2::element_rect(fill = "grey85",
                                               colour = "grey20"),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      complete = TRUE)

}

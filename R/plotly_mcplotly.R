#' Create a Plotly scatter plot with flexible aesthetics
#'
#' A wrapper around \code{plotly::plot_ly()} for quickly creating scatter plots
#' with optional colour and shape mappings. Particularly useful for visualising
#' dimensionality reduction outputs (e.g., UMAP, t-SNE) with hover text.
#'
#' @param df Data frame containing the data to plot.
#' @param x_col Character. Name of column to use for x-axis. Default is "V1".
#' @param y_col Character. Name of column to use for y-axis. Default is "V2".
#' @param text_col Character. Name of column to use for hover text. Default is "umap_text".
#' @param colour_col Character. Optional name of column to colour points by. Can be
#'   categorical (will be converted to factor) or continuous (numeric). If NULL,
#'   uses \code{single_colour} instead.
#' @param single_colour Character. Colour to use when \code{colour_col} is NULL.
#'   Default is "purple".
#' @param shape_col Character. Optional name of column to use for point shapes.
#'   Will be converted to factor. If NULL, all points are circles.
#' @param shape_col Character. Optional name of column to use for point sizes.
#' @param opacity Numeric. Point opacity between 0 and 1. Default is 1.
#' @param size_range Numeric. Vector to represent the max and min scaling for size when mapped to column.
#' @param size Numeric. Point size. Default is 1.
#'
#' @return A plotly object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple plot with single colour
#' plotly_mcplotly(umap_data, x_col = "umap_1", y_col = "umap_2")
#'
#' # Colour by categorical variable
#' plotly_mcplotly(umap_data, colour_col = "cluster")
#'
#' # Colour by continuous variable
#' plotly_mcplotly(umap_data, colour_col = "sentiment_score")
#'
#' # Colour and shape by different variables
#' plotly_mcplotly(umap_data, colour_col = "topic", shape_col = "source")
#' }
#'
plotly_mcplotly <- function(df, x_col = "V1", y_col = "V2", text_col = "umap_text",
                            colour_col = NULL, single_colour = "purple",
                            shape_col = NULL,
                            size_col = NULL,
                            opacity = 1,
                            size_range = c(5, 30),
                            size = 1) {

  # Check if colour column is continuous or categorical
  is_continuous <- !is.null(colour_col) && is.numeric(df[[colour_col]])

  # Determine colour mapping
  colour_mapping <- if (!is.null(colour_col)) {
    if (is_continuous) {
      ~.data[[colour_col]]
    } else {
      ~factor(.data[[colour_col]])
    }
  } else {
    I(single_colour)
  }

  # Determine size mapping
  if (!is.null(size_col)) {
    # Scale the size variable to the specified range
    size_values <- df[[size_col]]
    size_scaled <- scales::rescale(size_values, to = size_range)
    marker_size <- size_scaled
  } else {
    marker_size <- size  # Use fixed size if no size_col specified
  }

  # Create the base plot
  p <- df %>%
    plotly::plot_ly(
      x = ~.data[[x_col]],
      y = ~.data[[y_col]],
      type = 'scatter',
      mode = 'markers',
      text = ~.data[[text_col]],
      hoverinfo = 'text',
      color = colour_mapping,
      colors = if (is_continuous) "viridis" else NULL,
      symbol = if (!is.null(shape_col)) ~factor(.data[[shape_col]]) else I("circle"),
      marker = list(
        size = marker_size,
        opacity = opacity,
        line = list(width = 2)
      )
    ) %>%
    plotly::layout(
      plot_bgcolor = "white",
      xaxis = list(title = x_col),
      yaxis = list(title = y_col)
    )

  # Add colorbar title for continuous variables
  if (is_continuous) {
    p <- p %>% plotly::colorbar(title = colour_col)
  }

  # Add legend title for categorical variables
  if (!is.null(colour_col) && !is_continuous) {
    p <- p %>% plotly::layout(
      legend = list(title = list(text = colour_col))
    )
  }

  return(p)
}

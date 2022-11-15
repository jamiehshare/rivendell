#' jh_viz_ngram
#'
#' @param tbl_graph tbl_graph
#' @param emphasis emphasis
#'
#' @return ngram plot
#' @export

jh_viz_ngram <-function(tbl_graph, emphasis = TRUE) {

  {if (emphasis == TRUE) {

    # Get scale breaks
    node_freq <- tbl_graph %>%
      tidygraph::activate(nodes) %>%
      tidygraph::as_tibble() %>%
      dplyr::pull(word_freq)

    node_breaks <- (c(max(node_freq), min(node_freq))) %>%
      stats::quantile(probs = c(seq(0, 1, 0.25))) %>%
      round()
    names(node_breaks) <- NULL

    edge_freq <- tbl_graph %>%
      tidygraph::activate(edges) %>%
      tidygraph::as_tibble() %>%
      dplyr::pull(ngram_freq)

    edge_breaks <- (c(max(edge_freq), min(edge_freq))) %>%
      stats::quantile(probs = c(seq(0, 1, 0.25))) %>%
      round()
    names(edge_breaks) <- NULL

    # Make network
    ngram_network <- ggraph::ggraph(tbl_graph, layout = "nicely") +
      ggraph::geom_node_point(ggplot2::aes(size = word_freq,
                                           alpha = word_freq,
                                           color = word_freq),
                              stroke = 0) +
      ggraph::geom_edge_fan(ggplot2::aes(edge_alpha = ngram_freq,
                                         edge_colour = ngram_freq),
                            arrow = grid::arrow(type = "closed",
                                                length = grid::unit(.1, "inches")),
                            end_cap = ggraph::circle(.05, "inches")) +
      ggraph::geom_node_text(ggplot2::aes(label = word),
                             size = 4,
                             vjust = 0.5,
                             hjust = 1,
                             colour = "black",
                             bg.colour = "white",
                             repel = TRUE) +
      ggplot2::scale_color_viridis_c("Term\nFrequency",
                                     breaks = node_breaks,
                                     labels = scales::comma,
                                     guide = "legend") +
      ggplot2::scale_size_continuous("Term\nFrequency",
                                     breaks = node_breaks,
                                     labels = scales::comma) +
      ggplot2::scale_alpha_continuous("Term\nFrequency",
                                      breaks = node_breaks,
                                      range = c(0.5, 1),
                                      labels = scales::comma) +
      ggraph::scale_edge_color_viridis("N-gram\nFrequency",
                                       breaks = edge_breaks,
                                       labels = scales::comma,
                                       guide = "legend") +
      ggraph::scale_edge_alpha_continuous("N-gram\nFrequency",
                                          breaks = edge_breaks,
                                          range = c(0.5, 1),
                                          labels = scales::comma) +
      ggplot2::guides(color = guide_legend(order = 0),
                      fill  = guide_legend(order = 1)) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "right",
                     title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12))
  } else{
    ngram_network <- ggraph::ggraph(tbl_graph, layout = "nicely") +
      ggraph::geom_node_point() +
      ggraph::geom_node_text(ggplot2::aes(label = word),
                             size = 3.5,
                             vjust = 1.1,
                             hjust = 1) +
      ggraph::geom_edge_fan(ggplot2::aes(edge_alpha = ngram_freq),
                            arrow = grid::arrow(type = "closed",
                                                length = grid::unit(.1, "inches")),
                            end_cap = ggraph::circle(.05, "inches"),
                            show.legend = FALSE) +
      ggplot2::theme_void()
  }}

  # Output

  ngram_network

}

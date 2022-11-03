#' Plot mean and median coherence scores for different values of K topics
#'
#' @param explore_tbl_df Explore tbl_df from explore_ldas
#' @param freq_cutoff Frequency cutoff value
#'
#' @return ggplot line plot
#' @export

plot_coherence <- function(explore_tbl_df, freq_cutoff) {

coh <- explore_tbl_df %>%
    dplyr::filter(freq_cutoff == freq_cutoff) %>%
    dplyr::pull(coherence) %>%
    data.table::rbindlist(idcol = T) %>%
    dplyr::group_by(.id) %>%
    dplyr::mutate(k = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(k) %>%
    dplyr::summarise(mean = mean(coherence),
              sd = stats::sd(coherence),
              median = stats::median(coherence))

coh %>%
    ggplot2::ggplot(ggplot2::aes(x = k)) +
    ggplot2::geom_point(ggplot2::aes(y = mean, colour = "mean"),
               size = 2) +
    ggplot2::geom_point(ggplot2::aes(y = median, colour = "median"),
               size = 2) +
    ggplot2::geom_line(ggplot2::aes(y = mean, colour = "mean")) +
    ggplot2::geom_line(ggplot2::aes(y = median, colour
                  = "median")) +
    ggplot2::scale_x_continuous(breaks = seq(min(coh$k), max(coh$k), 1)) +
    ggplot2::scale_colour_viridis_d(breaks = c("mean", "median")) +
    ggplot2::labs(x = "K",
         y = "Mean value of Coherence",
         title = "Coherence across K's",
         colour = NULL) +
    ggplot2::theme_bw()

}

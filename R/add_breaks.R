#' Add HTML line breaks to text at regular intervals
#'
#' Inserts \code{<br>} tags after every nth word in a text string. Particularly useful
#' for wrapping long text labels in Plotly hover text to prevent them running off
#' the page. Also works for ggplot2 labels or any HTML output. Vectorised over
#' the text input.
#'
#' @param text Character vector. The text string(s) to process.
#' @param n Integer. Insert a line break after every n words.
#'
#' @return Character vector of the same length as `text` with \code{<br>} tags inserted.
#' @export
#'
#' @examples
#' # Single string
#' add_br("The quick brown fox jumps over the lazy dog", n = 3)
#'
#' # Multiple strings - typical use case for plotly hover text
#' hover_text <- c("Short label",
#'                 "This is a much longer label that would run off the plotly hover box")
#' add_br(hover_text, n = 5)
#'
add_br <- function(text, n) {
  purrr::map_chr(text, ~ {
    words <- stringr::str_split(.x, " ", simplify = TRUE) %>% as.character()
    if (length(words) == 0) return(.x)

    words_with_br <- purrr::imap_chr(words, ~ dplyr::if_else((.y %% n) == 0,
                                                             stringr::str_c(.x, "<br>"),
                                                             .x))

    stringr::str_c(words_with_br, collapse = " ")
  })
}


#' Add HTML line breaks to text at regular intervals (parallel version)
#'
#' Parallelised version of \code{\link{add_br}} using the \code{furrr} package.
#' Useful for processing large vectors of text strings (e.g., thousands of hover
#' labels for large plotly visualisations).
#'
#' \strong{Important:} You must set up a parallel backend with \code{future::plan()}
#' before calling this function. If no parallel backend is configured, it will
#' run sequentially.
#'
#' @param text Character vector. The text string(s) to process.
#' @param n Integer. Insert a line break after every n words.
#'
#' @return Character vector of the same length as `text` with \code{<br>} tags inserted.
#' @export
#'
#' @examples
#' \dontrun{
#' # Set up parallel processing first (adjust workers to your CPU cores)
#' future::plan(future::multisession, workers = 4)
#'
#' # Process large vector of plotly hover text
#' long_hover_text <- rep("Your very long hover text here with many words", 10000)
#' result <- add_br_parallel(long_hover_text, n = 5)
#'
#' # Always reset to sequential processing when done
#' future::plan(future::sequential)
#' }
#'
add_br_parallel <- function(text, n) {
  furrr::future_map_chr(text, ~ {
    words <- stringr::str_split(.x, " ", simplify = TRUE) %>% as.character()
    if (length(words) == 0) return(.x)

    words_with_br <- purrr::imap_chr(words, ~ dplyr::if_else((.y %% n) == 0,
                                                             stringr::str_c(.x, "<br>"),
                                                             .x))

    stringr::str_c(words_with_br, collapse = " ")
  })
}

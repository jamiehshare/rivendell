#' Get first and last day of week from a date column
#'
#' Extracts and prints the day of the week for the earliest and latest dates
#' in a dataset. Useful for quickly checking the temporal span of your data.
#'
#' @param data Data frame containing the date column.
#' @param date_col Unquoted name of the date column to analyse. Should be of
#'   class Date or POSIXct.
#'
#' @return Invisibly returns NULL. The function is called for its side effect
#'   of printing the first and last days of the week.
#' @export
#'
#' @examples
#' \dontrun{
#' # Check temporal span of a dataset
#' day_of_week(my_data, created_date)
#'
#' # Works with Date or POSIXct columns
#' day_of_week(tweets_df, tweet_timestamp)
#' }
#'
day_of_week <- function(data, date_col) {

  date_col <- rlang::ensym(date_col)

  first_day <- data %>%
    dplyr::arrange(dplyr::desc({{ date_col }})) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::mutate(day = lubridate::wday({{ date_col }}, label = TRUE)) %>%
    dplyr::pull(day)

  last_day <- data %>%
    dplyr::arrange(dplyr::desc({{ date_col }})) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::mutate(day = lubridate::wday({{ date_col }}, label = TRUE)) %>%
    dplyr::pull(day)

  print(paste0("First day = ", first_day, "; ",
               "Last day = ", last_day))

  invisible(NULL)
}

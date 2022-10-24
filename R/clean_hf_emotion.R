#' Wrangle distilbert output into a nice format
#'
#' @param df The dataframe output from the python emotion script
#'
#' @return A dataframe
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#'

extract_emotion_distilbert <- function(df) {

  df %>%
    janitor::clean_names() %>%
    dplyr::mutate(document = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = -.data$document) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    tidyr::separate(.data$value, into = c("entity_type","entity_score"), sep = ",")%>%
    dplyr::mutate(entity_type = stringr::str_remove(.data$entity_type, "\\{'label': '"),
                  entity_type = stringr::str_remove(.data$entity_type, "'"),
                  entity_score = readr::parse_number(.data$entity_score)) %>%
    dplyr::select(-.data$name) %>%
    dplyr::group_by(.data$document) %>%
    dplyr::arrange(.data$entity_type, .by_group = TRUE) %>%
    tidyr::pivot_wider(names_from = .data$entity_type, values_from = .data$entity_score)

}

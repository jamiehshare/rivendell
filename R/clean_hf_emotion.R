#' Wrangle distilbert output into a nice format
#'
#' @param df The dataframe output from the python emotion script
#'
#' @return A dataframe
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @export
#'

extract_emotion_distilbert <- function(df) {

  df %>%
    janitor::clean_names() %>%
    dplyr::mutate(document = row_number()) %>%
    tidyr::pivot_longer(cols = -document) %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::separate(value, into = c("entity_type","entity_score"), sep = ",")%>%
    dplyr::mutate(entity_type = stringr::str_remove(entity_type, "\\{'label': '"),
                  entity_type = stringr::str_remove(entity_type, "'"),
                  entity_score = readr::parse_number(entity_score)) %>%
    dplyr::select(-name) %>%
    dplyr::group_by(document) %>%
    dplyr::arrange(entity_type, .by_group = TRUE) %>%
    dplyr::pivot_wider(names_from = entity_type, values_from = entity_score)

}

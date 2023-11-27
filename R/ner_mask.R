#' Mask named entities (brands or products) with a single term
#'
#' @param ner_df The dataframe that has been run through NER in huggingface
#' @param input_df The dataframe that contains the social post with a document id number
#' @param text_var The column containing the text variable
#'
#' @return A dataframe
#' @export
#'
ner_mask <- function(ner_df, input_df, text_var, mask_term = "mask") {

  pattern <- "'entity_group':\\s+'(\\w+)',\\s+'score':\\s+([\\d.]+),\\s+'word':\\s+'([^']+)',\\s+'start':\\s+(\\d+),\\s+'end':\\s+(\\d+)"

  ner_clean <- ner_df %>%
    janitor::clean_names() %>%
    dplyr::rename(document = x1) %>%
    tidyr::pivot_longer(dplyr::contains("x")) %>%
    dplyr::filter(!is.na(value),
                  !str_detect(value, "'word': ','")) %>%
    tidyr::extract(value, pattern, into = c("entity_type", "entity_score", "entity", "start_index", "end_index")) %>%
    dplyr::mutate(entity_type = stringr::str_remove(entity_type,
                                                    "\\{'entity_group': '"),
                  entity_type = stringr::str_remove(entity_type, "'"),
                  entity_score = stringr::str_remove(entity_score, " 'score': "),
                  entity_score = stringr::str_remove(entity_score, " '"),
                  entity = stringr::str_remove(entity, " 'word': '"),
                  entity = stringr::str_remove(entity, "'"),
                  start_index = parse_number(start_index),
                  end_index = parse_number(end_index)) %>%
    dplyr::mutate(entity_score = round(as.numeric(entity_score), 3),
                  start_index = start_index + 1,
                  end_index = end_index,
                  document = document + 1) %>%
    dplyr::select(-name) %>%
    dplyr::filter(entity_type %in% c("MISC", "ORG", NA),
                  entity_score > 0.8)

  # Function to replace words based on start and end indices using stri_sub_replace_all
  replace_words <- function(text, start_idx, end_idx, replacement) {
    replacements <- stringi::stri_sub_replace_all(text, start = start_idx, end = end_idx, replacement = replacement)
    return(replacements)
  }

  result_df <- input_df %>%
    select({{text_var}}) %>%
    mutate(document = row_number())
  # Apply the function to replace words in the 'text' column
  result_df <- input_df %>%
    select(message_ner) %>%
    mutate(document = row_number()) %>%
    dplyr::left_join(ner_clean, by = "document") %>%
    dplyr::group_by(document) %>%
    dplyr::mutate(replacement_text = dplyr::case_when(!is.na(entity_type) ~
                                                     stringi::stri_sub_replace_all({{text_var}}, start_index, end_index, replacement = mask_term),
                                     TRUE ~ {{text_var}}
    )) %>%
    dplyr::distinct(document, .keep_all = T) %>%
    ungroup()

  # Display the result
  return(result_df[, c("document", "replacement_text")])
}


#' Mask named entities (brands or products) with a seperate terms for ORG or MISC
#'
#' @param ner_df The dataframe that has been run through NER in huggingface
#' @param input_df The dataframe that contains the social post with a document id number
#' @param text_var The column containing the text variable
#'
#' @return A dataframe
#' @export
#'
ner_brand_product <- function(ner_df, input_df, text_var) {

  pattern <- "'entity_group':\\s+'(\\w+)',\\s+'score':\\s+([\\d.]+),\\s+'word':\\s+'([^']+)',\\s+'start':\\s+(\\d+),\\s+'end':\\s+(\\d+)"

  ner_clean <- ner_df %>%
    janitor::clean_names() %>%
    dplyr::rename(document = x1) %>%
    tidyr::pivot_longer(dplyr::contains("x")) %>%
    dplyr::filter(!is.na(value),
                  !str_detect(value, "'word': ','")) %>%
    tidyr::extract(value, pattern, into = c("entity_type", "entity_score", "entity", "start_index", "end_index")) %>%
    dplyr::mutate(entity_type = stringr::str_remove(entity_type,
                                                    "\\{'entity_group': '"),
                  entity_type = stringr::str_remove(entity_type, "'"),
                  entity_score = stringr::str_remove(entity_score, " 'score': "),
                  entity_score = stringr::str_remove(entity_score, " '"),
                  entity = stringr::str_remove(entity, " 'word': '"),
                  entity = stringr::str_remove(entity, "'"),
                  start_index = parse_number(start_index),
                  end_index = parse_number(end_index)) %>%
    dplyr::mutate(entity_score = round(as.numeric(entity_score), 3),
                  start_index = start_index + 1,
                  end_index = end_index,
                  document = document + 1) %>%
    dplyr::select(-name) %>%
    dplyr::filter(entity_type %in% c("MISC", "ORG", NA),
                  entity_score > 0.8)

  result <- input_df %>%
    select({{text_var}}) %>%
    mutate(document = row_number()) %>%
    rename(text = {{text_var}}) %>%
    dplyr::left_join(ner_clean, by = "document") %>%
    dplyr::group_by(document) %>%
    dplyr::mutate(replacement = dplyr::case_when(stringr::str_detect(entity_type, "ORG") ~ "brand",
                                                 T ~ "product")) %>%
    dplyr::summarise(
      text = first(text),
      entity_type = list(entity_type),
      entity_score = list(entity_score),
      entity = list(entity),
      start_index = list(start_index),
      end_index = list(end_index),
      replacement = list(replacement)
    )

  result_df <- result %>%
    dplyr::full_join(input_df, by = c("document")) %>%
    dplyr::arrange(document) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(replacement_text = dplyr::case_when(!is.null(entity_type) ~ stringi::stri_sub_replace_all(text, from = unlist(start_index), to = unlist(end_index), replacement = replacement),
                                                      TRUE ~ text)) %>%
    tidyr::unnest(cols = c(entity_type, entity_score, entity, start_index, end_index),
                  keep_empty = T) %>%
    dplyr::distinct(document, .keep_all = T)

  return(result_df[, c("document", "replacement_text")])

}

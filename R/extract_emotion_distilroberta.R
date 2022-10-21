#' Wrangle distilroberta output into a nice format
#'
#' @param df The dataframe output from the python emotion script
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' extract_emotion_distilroberta(twitter_sent_2)
extract_emotion_distilroberta <- function(df) {

  extract_emotion <- function(df, emotion, column_a, column_b, column_c, column_d, column_e, column_f, column_g) {

    df %>%
      dplyr::mutate(!!quo_name(emotion) := dplyr::case_when(str_detect({{ column_a }}, emotion) ~ readr::parse_number({{ column_a }}),
                                                     str_detect({{ column_b }}, emotion) ~ readr::parse_number({{ column_b }}),
                                                     str_detect({{ column_c }}, emotion) ~ readr::parse_number({{ column_c }}),
                                                     str_detect({{ column_d }}, emotion) ~ readr::parse_number({{ column_d }}),
                                                     str_detect({{ column_e }}, emotion) ~ readr::parse_number({{ column_e }}),
                                                     str_detect({{ column_f }}, emotion) ~ readr::parse_number({{ column_f }}),
                                                     str_detect({{ column_g }}, emotion) ~ readr::parse_number({{ column_g }}),
                                                     T ~ NA_real_))
  }

  df %>%
    extract_emotion(emotion = "anger", column_a = a, column_b = b, column_c = c, column_d = d,
                    column_e = e, column_f = f, column_g = g) %>%
    extract_emotion(emotion = "disgust", column_a = a, column_b = b, column_c = c, column_d = d,
                    column_e = e, column_f = f, column_g = g) %>%
    extract_emotion(emotion = "fear", column_a = a, column_b = b, column_c = c, column_d = d,
                    column_e = e, column_f = f, column_g = g) %>%
    extract_emotion(emotion = "joy", column_a = a, column_b = b, column_c = c, column_d = d,
                    column_e = e, column_f = f, column_g = g) %>%
    extract_emotion(emotion = "neutral", column_a = a, column_b = b, column_c = c, column_d = d,
                    column_e = e, column_f = f, column_g = g) %>%
    extract_emotion(emotion = "sadness", column_a = a, column_b = b, column_c = c, column_d = d,
                    column_e = e, column_f = f, column_g = g) %>%
    extract_emotion(emotion = "surprise", column_a = a, column_b = b, column_c = c, column_d = d,
                    column_e = e, column_f = f, column_g = g) %>%
    dplyr::select(anger, disgust, fear, joy, neutral, sadness, surprise)

}

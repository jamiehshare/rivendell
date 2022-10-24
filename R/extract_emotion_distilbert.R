extract_emotion_distilbert <- function(df) {

  extract_emotion <- function(df, emotion, column_a, column_b, column_c, column_d, column_e, column_f) {

    df %>%
      dplyr::mutate(!!quo_name(emotion) := case_when(str_detect({{ column_a }}, emotion) ~ readr::parse_number({{ column_a }}),
                                                     str_detect({{ column_b }}, emotion) ~ readr::parse_number({{ column_b }}),
                                                     str_detect({{ column_c }}, emotion) ~ readr::parse_number({{ column_c }}),
                                                     str_detect({{ column_d }}, emotion) ~ readr::parse_number({{ column_d }}),
                                                     str_detect({{ column_e }}, emotion) ~ readr::parse_number({{ column_e }}),
                                                     str_detect({{ column_f }}, emotion) ~ readr::parse_number({{ column_f }}),
                                                     T ~ NA_real_))
  }

  df %>%
    extract_emotion(emotion = "joy", column_a = a, column_b = b, column_c = c, column_d = d,
                    column_e = e, column_f = f) %>%
    extract_emotion(emotion = "sadness", column_a = a, column_b = b, column_c = c, column_d = d,
                    column_e = e, column_f = f) %>%
    extract_emotion(emotion = "fear", column_a = a, column_b = b, column_c = c, column_d = d,
                    column_e = e, column_f = f) %>%
    extract_emotion(emotion = "anger", column_a = a, column_b = b, column_c = c, column_d = d,
                    column_e = e, column_f = f) %>%
    extract_emotion(emotion = "love", column_a = a, column_b = b, column_c = c, column_d = d,
                    column_e = e, column_f = f) %>%
    extract_emotion(emotion = "surprise", column_a = a, column_b = b, column_c = c, column_d = d,
                    column_e = e, column_f = f) %>%
    select(anger, fear, joy, love, sadness, surprise)

}

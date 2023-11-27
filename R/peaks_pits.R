#' pp_products
#'
#' @param df input dataframe
#' @param text_var column with posts
#'
#' @return Data Frame or Tibble object with text variable edited inline
#' @export
#'
pp_products <- function(df, text_var){

  entities <- LimpiaR::entities

  products <- rivendell::product_entity

  strings <- entities$token
  strings_2 <- products$pattern
  strings <- c(strings, strings_2)
  strings <- paste0(strings, collapse = "|")
  replacement <- "product"

  df %>%
    dplyr::mutate({{text_var}} := stringr::str_replace_all(tolower( {{text_var}} ), strings, replacement))
}


#' pp_brands
#'
#' @param df Input dataframe
#' @param text_var Column with posts
#'
#' @return Data Frame or Tibble object with text variable edited inline
#' @export
#'
pp_brands <- function(df, text_var){

  brands <- rivendell::brand_entity
  strings <- brands$pattern
  strings <- paste0(strings, collapse = "|")
  replacement <- "brand"

  df %>%
    dplyr::mutate({{text_var}} := stringr::str_replace_all(tolower( {{text_var}} ), strings, replacement))
}

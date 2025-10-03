#' Add token count column to a dataframe
#'
#' Calculates the number of tokens in a text column using a specified tokenizer
#' and adds the result as a new column. Useful for understanding text length in
#' tokens rather than characters, particularly important for transformer models
#' with token limits.
#'
#' @param data Data frame containing the text to tokenize.
#' @param text_col Unquoted name of the column containing text to tokenize.
#' @param model_name Character string. Name of the pretrained tokenizer model to
#'   load (e.g., "BAAI/bge-large-en-v1.5"). Only used if \code{tokenizer} is NULL.
#'   Default is "BAAI/bge-large-en-v1.5".
#' @param tokenizer Optional. A pre-loaded tokenizer object from the \code{tok}
#'   package. If provided, this takes precedence over \code{model_name}. Use this
#'   for better performance when calling the function multiple times.
#' @param output_col Character string. Name for the new column containing token
#'   counts. Default is "token_count".
#'
#' @return The input data frame with an additional column containing token counts.
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple usage - loads tokenizer automatically
#' df_with_tokens <- add_token_count(df, clean_message)
#'
#' # Use a different model
#' df_with_tokens <- add_token_count(df, text,
#'                                    model_name = "bert-base-uncased")
#'
#' # For better performance with multiple calls: load tokenizer once
#' tokenizer_bge <- tok::tokenizer$from_pretrained("BAAI/bge-large-en-v1.5")
#' df1 <- add_token_count(df1, text, tokenizer = tokenizer_bge)
#' df2 <- add_token_count(df2, text, tokenizer = tokenizer_bge)
#'
#' # Custom output column name
#' df_with_tokens <- add_token_count(df, text, output_col = "bge_tokens")
#' }
#'
add_token_count <- function(data, text_col,
                            model_name = "BAAI/bge-large-en-v1.5",
                            tokenizer = NULL,
                            output_col = "token_count") {

  # Capture the text column name
  text_col <- rlang::ensym(text_col)

  # Load tokenizer if not provided
  if (is.null(tokenizer)) {
    tokenizer <- tok::tokenizer$from_pretrained(model_name)
  }

  # Helper function to get token length
  get_token_length <- function(text_var, tokenizer) {
    length(tokenizer$encode(text_var)$ids)
  }

  # Add token count column
  data %>%
    dplyr::mutate(!!output_col := purrr::map_int({{ text_col }},
                                                 ~get_token_length(.x, tokenizer)))
}

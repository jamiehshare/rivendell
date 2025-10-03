#' Count tokens in a text string
#'
#' Calculates the number of tokens in a text string using a specified tokenizer.
#' Useful for understanding text length in tokens rather than characters,
#' particularly important for transformer models with token limits.
#'
#' @param text Character string. The text to tokenize.
#' @param model_name Character string. Name of the pretrained tokenizer model to
#'   load (e.g., "BAAI/bge-large-en-v1.5"). Only used if \code{tokenizer} is NULL.
#'   Default is "BAAI/bge-large-en-v1.5".
#' @param tokenizer Optional. A pre-loaded tokenizer object from the \code{tok}
#'   package. If provided, this takes precedence over \code{model_name}. Use this
#'   for better performance when calling the function multiple times.
#'
#' @return Integer. The number of tokens in the text.
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple usage - single text string
#' get_token_count("Hello, how are you today?")
#'
#' # Use a different model
#' get_token_count("Some text", model_name = "bert-base-uncased")
#'
#' # For better performance with multiple calls: load tokenizer once
#' tokenizer_bge <- tok::tokenizer$from_pretrained("BAAI/bge-large-en-v1.5")
#' get_token_count("First text", tokenizer = tokenizer_bge)
#' get_token_count("Second text", tokenizer = tokenizer_bge)
#'
#' # Use with dataframes
#' library(dplyr)
#' df <- df %>%
#'   mutate(token_count = purrr::map_int(text_column, get_token_count))
#'
#' # Or with a pre-loaded tokenizer for efficiency
#' tokenizer_bge <- tok::tokenizer$from_pretrained("BAAI/bge-large-en-v1.5")
#' df <- df %>%
#'   mutate(bge_tokens = purrr::map_int(text_column,
#'                                      ~get_token_count(.x, tokenizer = tokenizer_bge)))
#' }
#'
get_token_count <- function(text,
                            model_name = "BAAI/bge-large-en-v1.5",
                            tokenizer = NULL) {

  # Load tokenizer if not provided
  if (is.null(tokenizer)) {
    tokenizer <- tok::tokenizer$from_pretrained(model_name)
  }

  # Return token count
  length(tokenizer$encode(text)$ids)
}

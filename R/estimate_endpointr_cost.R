#' Estimate API cost before sending
#'
#' Estimates the cost of classifying a data frame of texts through Anthropic's
#' Messages or Batches API, calculated locally before any request is sent.
#' Produces two independent estimates, one from character counts (~4 chars per
#' token) and one from word counts (~0.75 words per token), so the spread
#' between them acts as an uncertainty band. No prompt caching is assumed: a
#' typical 1-2k token prompt plus schema falls below the size that benefits
#' from caching in practice, so both are billed as input on every row. The
#' \code{schema} is counted from its serialised JSON (as sent via
#' \code{output_config}), not as prompt text, so it should not also be pasted
#' into \code{system_prompt} or it will be counted twice. Output tokens cannot
#' be known before sending, so \code{expected_output_tokens} is applied per row
#' as an assumption (this is expected billed output, not \code{max_tokens}).
#' Batch pricing is assumed to be 50\% of standard. Prices default to Claude
#' Haiku 4.5 rates and should be verified before use.
#'
#' @param df Data frame containing the texts to be classified.
#' @param message_col Character string. Name of the column in \code{df} holding
#'   the text (raw or cleaned).
#' @param system_prompt Character string. The system prompt, billed in full on
#'   every row.
#' @param schema An EndpointR \code{json_schema} object (from
#'   \code{create_json_schema()}). Its inner schema definition is counted as
#'   input once per row. If NULL, no schema is included. Default is NULL.
#' @param expected_output_tokens Integer. Assumed billed output tokens per call,
#'   not \code{max_tokens}. Default is 100.
#' @param input_price_per_1m Numeric. Cost in USD per 1 million input tokens
#'   for the standard tier. Batch is assumed to be 50\% of this. Default is
#'   1.00 (Claude Haiku 4.5).
#' @param output_price_per_1m Numeric. Cost in USD per 1 million output tokens
#'   for the standard tier. Batch is assumed to be 50\% of this. Default is
#'   5.00 (Claude Haiku 4.5).
#' @param chars_per_token Numeric. Characters-per-token ratio for the
#'   character-based estimate. Default is 4.
#' @param words_per_token Numeric. Words-per-token ratio for the word-based
#'   estimate. Default is 0.75.
#'
#' @return A tibble with one row per pricing tier (standard, batch) and columns
#'   \code{tier}, \code{prompt_tok_each}, \code{schema_tok_each},
#'   \code{char_estimate_usd}, and \code{word_estimate_usd}.
#' @export
#'
#' @examples
#' \dontrun{
#' # schema built with EndpointR (loaded in real use)
#' classification_schema <- create_json_schema(
#'   name = "sentiment",
#'   schema = schema_object(
#'     sentiment = schema_enum(c("positive", "negative", "neutral")),
#'     confidence = schema_number(minimum = 0, maximum = 1),
#'     required = c("sentiment", "confidence")
#'   )
#' )
#'
#' estimate_endpointr_cost(
#'   df = posts_df,
#'   message_col = "clean_message",
#'   system_prompt = prompt,
#'   schema = classification_schema,
#'   expected_output_tokens = 100
#' )
#' }
#'
estimate_endpointr_cost <- function(df,
                                message_col,
                                system_prompt,
                                schema = NULL,
                                expected_output_tokens = 100,
                                input_price_per_1m = 1.00,
                                output_price_per_1m = 5.00,
                                chars_per_token = 4,
                                words_per_token = 0.75) {

  msg <- df[[message_col]]
  n   <- nrow(df)

  # drop NA rows so they don't poison the token sums
  if (anyNA(msg)) {
    cli::cli_warn("`{message_col}` contains NA rows; dropping them from the estimate.")
    msg <- msg[!is.na(msg)]
    n   <- length(msg)
  }

  # schema is always an EndpointR json_schema object in practice; extract its
  # inner definition without taking a hard dependency on EndpointR
  schema_list <- if (!is.null(schema)) schema@schema else NULL
  schema_json <- if (!is.null(schema_list)) {
    jsonlite::toJSON(schema_list, auto_unbox = TRUE)
  } else NULL

  # fixed input (prompt + schema), re-sent in full on every request
  prompt_chars <- nchar(system_prompt)
  prompt_words <- stringr::str_count(system_prompt, "\\w+")
  schema_chars <- if (!is.null(schema_json)) nchar(schema_json) else 0
  schema_words <- if (!is.null(schema_json)) stringr::str_count(schema_json, "\\w+") else 0

  fixed_chars <- (prompt_chars + schema_chars) * n
  fixed_words <- (prompt_words + schema_words) * n

  # variable input (the posts themselves)
  post_chars <- sum(nchar(msg))
  post_words <- sum(stringr::str_count(msg, "\\w+"))

  # token estimates: characters vs words
  input_tok_char <- (fixed_chars + post_chars) / chars_per_token
  input_tok_word <- (fixed_words + post_words) / words_per_token
  output_tok     <- expected_output_tokens * n

  price_it <- function(in_price, out_price, tier) {
    tibble::tibble(
      tier              = tier,
      prompt_tok_each   = round(prompt_chars / chars_per_token),
      schema_tok_each   = round(schema_chars / chars_per_token),
      char_estimate_usd = (input_tok_char * in_price + output_tok * out_price) / 1e6,
      word_estimate_usd = (input_tok_word * in_price + output_tok * out_price) / 1e6
    )
  }

  dplyr::bind_rows(
    price_it(input_price_per_1m,         output_price_per_1m,         "standard"),
    price_it(input_price_per_1m * 0.5,   output_price_per_1m * 0.5,   "batch (50% off)")
  )
}
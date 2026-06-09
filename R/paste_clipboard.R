#' paste_clipboard
#'
#' @param header Logical. Whether the first row should be treated as column headers. Default is FALSE.
#'
#' @return The data from the clipboard get's saved in the R
#' @export
#'
#' @examples
#' \dontrun{
#' df <- paste_clipboard(header = FALSE)
#' }
#'
paste_clipboard <- function(header = FALSE) {
  # Function code here
  utils::read.table(pipe("pbpaste"), header = header, comment.char = "")

}

#' paste_clipboard
#'
#' @param header
#'
#' @return The data from the clipboard get's saved in the R
#' @export
#'
#' @examples
#' df <- paste_clipboard(header = F)
#'
paste_clipboard <- function(header = F) {
  # Function code here
  utils::read.table(pipe("pbpaste"), header = header, comment.char = "")

}

.old <- c(LETTERS, letters, 0:9)
.new <- sample(.old)

.old_string <- paste(.old, collapse = "")
.new_string <- paste(.new, collapse = "")

encrypt <- function(...) {
  message <- paste(..., sep = "")
    chartr(.old_string, .new_string, message)
}

#' Use decrypt to reveal the position of the real data.
#'
#' The real data position is encrypted by the lineup function, and
#' writes this out as a text string. Decrypt, decrypts this text
#' string to reveal which where the real data is.
#'
#' @param ... character vector to decrypt
#' @export
#' @examples
#' decrypt('0uXR2p rut L2O2')
decrypt <- function(...) {
    message <- paste(..., sep = "")
    m <- chartr(.new_string, .old_string, message)
    m_num <- as.numeric(substr(m, 23, 24)) - 10
    paste(substr(m, 1, 22), m_num)
}

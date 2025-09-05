#' Make a safe slug for URIs
#'
#' @param x Character vector.
#'
#' @return Character vector with safe identifiers.
#' @keywords internal
slugify_id <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9_-]", "-", x)
  gsub("-+", "-", x)
}



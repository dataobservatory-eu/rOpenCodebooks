#' Validate a codebook dataset
#'
#' Checks uniqueness of concept IDs and safety of URI fragments.
#'
#' @param cb A `codebook_df`.
#'
#' @return TRUE if valid, otherwise throws an error.
#' @export
validate_codebook_df <- function(cb) {
  stopifnot(inherits(cb, "codebook_df"))
  ids <- cb$concept_id
  if (anyDuplicated(ids)) {
    stop("Duplicate concept_id values found: ", paste(unique(ids[duplicated(ids)]), collapse = ", "))
  }
  if (!all(grepl("^[A-Za-z0-9_-]+$", ids))) {
    stop("Invalid concept_id(s): ", paste(unique(ids[!grepl("^[A-Za-z0-9_-]+$", ids)]), collapse = ", "))
  }
  TRUE
}

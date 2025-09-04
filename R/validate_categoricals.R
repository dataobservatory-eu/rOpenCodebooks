#' Validate categorical values in a response dataset
#'
#' @param responses A dataset_df with responses.
#' @param codebooks A named list of codebook_df objects, keyed by scheme_uri.
#'
#' @return TRUE if valid, otherwise a list of problems.
#' @export
validate_categoricals <- function(responses, codebooks) {
  problems <- list()
  for (v in names(responses)) {
    cl <- attr(responses[[v]], "code_list_uri")
    if (is.null(cl)) next
    if (!cl %in% names(codebooks)) {
      problems[[v]] <- paste("No codebook registered for scheme:", cl)
      next
    }
    allowed <- codebooks[[cl]]$notation
    observed <- unique(responses[[v]])
    missing <- setdiff(observed, allowed)
    if (length(missing)) problems[[v]] <- paste("Unmapped codes:", paste(missing, collapse = ", "))
  }
  if (length(problems)) return(problems)
  TRUE
}

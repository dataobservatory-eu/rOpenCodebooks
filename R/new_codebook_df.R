#' Create a new codebook dataset
#'
#' Define a codebook as a dataset_df object with metadata and concepts.
#'
#' @param concepts A data.frame with columns: concept_id, pref_label, notation,
#'   language. Optional: position, is_missing, exact_match, definition.
#' @param scheme_meta A named list with metadata: doc_uri, scheme_id,
#'   scheme_fragment (optional), version, label.
#'
#' @return An object of class `codebook_df`, extending `dataset_df`.
#' @export
new_codebook_df <- function(concepts, scheme_meta) {
  stopifnot(is.data.frame(concepts))
  # TODO: enforce required cols
  df <- dataset::dataset_df(concepts)
  class(df) <- c("codebook_df", class(df))
  attr(df, "scheme_meta") <- scheme_meta
  df
}

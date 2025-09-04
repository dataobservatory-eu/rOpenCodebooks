#' Read a codebook from CSV
#'
#' @param path Path to a CSV file with codebook fields.
#'
#' @return A `codebook_df`.
#' @importFrom utils read.csv
#' @export
read_codebook_csv <- function(path) {
  df <- utils::read.csv(path, na.strings = c("", "NA"), check.names = FALSE)
  new_codebook_df(df, scheme_meta = list(
    doc_uri = unique(df$doc_uri)[1],
    scheme_id = unique(df$scheme_id)[1],
    scheme_fragment = unique(df$scheme_fragment)[1],
    version = unique(df$scheme_version)[1],
    label = unique(df$scheme_label)[1]
  ))
}

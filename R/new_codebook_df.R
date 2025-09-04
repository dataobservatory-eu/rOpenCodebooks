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
new_codebook_df <- function(concepts,
                            scheme_meta,
                            bib = "datacite",
                            bib_args = list()) {
  stopifnot(is.data.frame(concepts))
  # TODO: enforce required cols

  if ( bib == "datacite") {
    default_bibentry  <- datacite_default(scheme_meta, bib_args)
  }

  if ( bib == "dublincore") {
    default_bibentry  <- dublincore_default(scheme_meta, bib_args)
  }

  df <- dataset::dataset_df(concepts, dataset_bibentry = default_bibentry)
  class(df) <- c("codebook_df", class(df))
  attr(df, "scheme_meta") <- scheme_meta
  df
}

#' @importFrom dataset datacite
#' @keywords internal
datacite_default <- function(scheme_meta, bib_args = list()) {
  dataset::datacite(
    Title              = scheme_meta$label,
    Creator            = bib_args$creator %||% person("Unknown"),
    Publisher          = bib_args$publisher %||% getOption("opencodebooks.publisher","Self-published"),
    PublicationYear    = bib_args$publication_year %||% as.integer(format(Sys.Date(), "%Y")),
    Type               = bib_args$resource_type %||% "Codebook",
    Version            = bib_args$version %||% scheme_meta$version,
    Rights             = bib_args$rights %||% getOption("opencodebooks.license","MIT"),
    Identifier         = bib_args$identifier  # optional DOI/URI if available
  )
}

#' @importFrom dataset dublincore
#' @keywords internal
dublincore_default <- function(scheme_meta, bib_args = list()) {
  dataset::dublincore(
    title        = scheme_meta$label,
    creator      = bib_args$creator %||% person("Unknown"),
    publisher    = bib_args$publisher %||% getOption("opencodebooks.publisher","Self-published"),
    dataset_date = bib_args$date %||% as.integer(format(Sys.Date(), "%Y")),
    description  = bib_args$description,
    identifier   = bib_args$identifier,
    rights       = bib_args$rights %||% getOption("opencodebooks.license","MIT"))
}

#' @rdname codebook_df
print_codebook <- function(cb) {
  meta <- attr(cb, "dataset_bibentry")
  title <- if (!is.null(meta$title)) {
    meta$title } else {
      attr(cb, "scheme_meta")$label }

  cat(sprintf("%s [%s]\n", title, "codebook"))

  out <- cb[, c("concept_uri","notation","pref_label",
                "position","language","is_missing")]
  rownames(out) <- NULL
  print(out, row.names = FALSE)
  invisible(cb)
}

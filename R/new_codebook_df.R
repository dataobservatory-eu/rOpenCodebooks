#' codebook_df: a lightweight subclass of dataset_df for codelists
#'
#' @description
#' `codebook_df()` objects are plain tabular data (one row per concept)
#' with **codebook-specific metadata**:
#' - `scheme_meta` (document URI, scheme identifier, version, label, optional fragment)
#' - `dataset_bibentry` (a bibliographic record; **DataCite** by default)
#'
#' They print with concept URIs and key fields (notation, label, language, order),
#' and can be rendered to HTML/Turtle via templating.
#'
#' @section Required columns in `concepts`:
#' - `concept_id` (character): URI-fragment-safe id, e.g. `"scale-4-agree"`.
#' - `pref_label` (character): human-readable label.
#' - `notation` (character): short code used in data (e.g. `"AG"`).
#' - `language` (character): ISO 639-1 (e.g. `"en"`).
#'
#' @section Optional columns:
#' - `position` (integer): order for ordinal scales.
#' - `is_missing` (logical): mark DK/Refused/NA categories.
#' - `definition` (character): short definition or usage note.
#' - `exact_match` (character): external URI(s) for mappings.
#'
#' @param concepts A `data.frame` with at least the required columns above.
#' @param scheme_meta A named list with metadata:
#'   - `doc_uri` (character, required): base document IRI used for hash URIs.
#'   - `scheme_id` (character, required): short identifier of the scheme family.
#'   - `scheme_fragment` (character, optional): fragment id of this ConceptScheme
#'      when multiple schemes share one document (e.g., `"scale-3"`, `"scale-4"`).
#'   - `version` (character, required): human version label (e.g., `"v1"`).
#'   - `label` (character, required): human-readable scheme title.
#' @param bib Either `"datacite"` (default) or `"dublincore"`. Controls the
#'   default bibliographic record attached as `dataset_bibentry`.
#' @param bib_args A named list of fields to override in the default
#'   bibliographic record (e.g., `creator`, `publisher`, `publication_year`,
#'   `rights`, `identifier`, etc.).
#'
#' @return An object of class `codebook_df`, extending `dataset_df`.
#'
#' @examples
#' # Minimal two-concept example (EN only)
#' rows <- data.frame(
#'   concept_id = c("yes", "no"),
#'   pref_label = c("Yes", "No"),
#'   notation   = c("Y", "N"),
#'   language   = "en",
#'   position   = c(2L, 1L),
#'   is_missing = FALSE,
#'   stringsAsFactors = FALSE
#' )
#'
#' meta <- list(
#'   doc_uri         = "https://example.org/codelists/yesno/v1",
#'   scheme_id       = "yesno",
#'   scheme_fragment = "binary",
#'   version         = "v1",
#'   label           = "Yes/No (binary)"
#' )
#'
#' cb <- new_codebook_df(rows, meta,
#'                       bib = "datacite",
#'                       bib_args = list(
#'                         creator = "Doe, Jane",
#'                         publisher = "Data Observatory",
#'                         rights = "MIT"
#'                       ))
#'
#' cb                 # prints with concept URIs and key fields
#'
#' # Multilingual label: bind another language row for the same concept_id
#' rows_hu <- transform(rows, language = "hu",
#'                      pref_label = c("Igen", "Nem"))
#' cb_hu <- new_codebook_df(rbind(rows, rows_hu), meta)
#' cb_hu
#'
#' @seealso [dataset::datacite()], [dataset::dublincore()]
#' @export
new_codebook_df <- function(concepts,
                            scheme_meta,
                            bib = "datacite",
                            bib_args = list()) {
  stopifnot(is.data.frame(concepts))
  .require_cols(concepts, c("concept_id", "pref_label", "notation", "language"))
  .check_ids(concepts$concept_id)

  # Build default bibliographic record
  bib <- match.arg(bib, c("datacite", "dublincore"))
  default_bibentry <- switch(
    bib,
    datacite   = datacite_default(scheme_meta, bib_args),
    dublincore = dublincore_default(scheme_meta, bib_args)
  )

  # Construct the dataset_df, attach bibentry immediately
  df <- dataset::dataset_df(concepts, dataset_bibentry = default_bibentry)

  # Class + attributes specific to codebooks
  class(df) <- c("codebook_df", class(df))
  attr(df, "scheme_meta") <- scheme_meta

  df
}

#' Default DataCite record for a codebook
#'
#' Creates a `dataset_bibentry` using DataCite fields with sensible defaults.
#' Override with `bib_args` (e.g., `creator`, `publisher`, `publication_year`,
#' `rights`, `identifier`, `version`, `resource_type`, `resource_type_general`).
#'
#' @param scheme_meta Named list; see [new_codebook_df()].
#' @param bib_args Named list of overrides.
#'
#' @return A DataCite `dataset_bibentry`.
#' @keywords internal
#' @importFrom dataset datacite
datacite_default <- function(scheme_meta, bib_args = list()) {
  dataset::datacite(
    Title                 = scheme_meta$label,
    Creator               = bib_args$creator %||% "Unknown",
    Publisher             = bib_args$publisher %||%
      getOption("opencodebooks.publisher", "Self-published"),
    PublicationYear       = bib_args$publication_year %||%
      as.integer(format(Sys.Date(), "%Y")),
    Type                  = bib_args$resource_type %||% "Codebook (codelist)",
    ResourceTypeGeneral   = bib_args$resource_type_general %||% "Dataset",
    Version               = bib_args$version %||% scheme_meta$version,
    Rights                = bib_args$rights %||%
      getOption("opencodebooks.license", "MIT"),
    Identifier            = bib_args$identifier # optional DOI/URI
  )
}

#' Default Dublin Core record for a codebook
#'
#' Creates a minimal Dublin Core `dataset_bibentry`. Prefer `datacite_default()`
#' for richer repository/catalogue support.
#'
#' @param scheme_meta Named list; see [new_codebook_df()].
#' @param bib_args Named list of overrides.
#'
#' @return A Dublin Core `dataset_bibentry`.
#' @keywords internal
#' @importFrom dataset dublincore
dublincore_default <- function(scheme_meta, bib_args = list()) {
  dataset::dublincore(
    title        = scheme_meta$label,
    creator      = bib_args$creator %||% "Unknown",
    publisher    = bib_args$publisher %||%
      getOption("opencodebooks.publisher", "Self-published"),
    date         = bib_args$date %||% as.integer(format(Sys.Date(), "%Y")),
    description  = bib_args$description,
    identifier   = bib_args$identifier,
    rights       = bib_args$rights %||%
      getOption("opencodebooks.license", "MIT")
  )
}

#' Print method for codebook_df
#'
#' Displays a compact, URI-forward view of a `codebook_df`, including
#' concept URIs (minted as `doc_uri#concept_id`), notation, label, language,
#' position, and missing flag.
#'
#' @param x A `codebook_df`.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
#' @method print codebook_df
print.codebook_df <- function(x, ...) {
  bib <- attr(x, "dataset_bibentry")
  meta <- attr(x, "scheme_meta")

  title <- {
    # dataset::datacite stores Title; dataset::dublincore stores title
    t <- bib$Title %||% bib$title %||% meta$label
    if (is.null(t)) meta$label else t
  }
  creators <- bib$Creator %||% bib$creator
  version  <- meta$version %||% bib$Version %||% bib$version
  doc_uri  <- meta$doc_uri %||% ""

  cat(paste0(title, " [codebook]\n"))
  if (!is.null(creators)) {
    if (is.character(creators)) {
      cat("Creators: ", paste(creators, collapse = "; "), "\n", sep = "")
    } else {
      cat("Creators: ", paste0(utils::capture.output(print(creators)), collapse = " "), "\n", sep = "")
    }
  }
  if (!is.null(version)) cat("Version: ", version, "\n", sep = "")
  if (!identical(doc_uri, "")) cat("Doc URI: ", doc_uri, "\n", sep = "")
  cat("\n", sep = "")

  concept_uri <- if (!identical(doc_uri, "")) paste0(doc_uri, "#", x$concept_id) else x$concept_id

  cols <- data.frame(
    concept_uri = concept_uri,
    notation    = x$notation,
    label       = x$pref_label,
    lang        = x$language,
    position    = x$position %||% NA,
    missing     = x$is_missing %||% FALSE,
    stringsAsFactors = FALSE
  )

  rownames(cols) <- NULL
  print(cols, row.names = FALSE)
  invisible(x)
}

# --- small internal helpers ----------------------------------------------------

#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' @keywords internal
.require_cols <- function(df, needed) {
  miss <- setdiff(needed, names(df))
  if (length(miss)) {
    stop("Missing required columns in `concepts`: ", paste(miss, collapse = ", "), call. = FALSE)
  }
}

#' @keywords internal
.check_ids <- function(ids) {
  bad <- !grepl("^[A-Za-z0-9_-]+$", ids %||% "")
  if (any(bad)) {
    stop("Invalid `concept_id` values (only A–Z, a–z, 0–9, `_`, `-` allowed): ",
         paste(unique(ids[bad]), collapse = ", "), call. = FALSE)
  }
}


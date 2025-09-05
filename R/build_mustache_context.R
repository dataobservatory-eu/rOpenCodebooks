#' Convert a codebook_df into a Mustache context
#'
#' @param cb A codebook_df.
#'
#' @return A list suitable for whisker::whisker.render().
#' @keywords internal
build_mustache_context <- function(cb) {
  meta <- attr(cb, "scheme_meta")

  concepts <- lapply(seq_len(nrow(cb)), function(i) {
    list(
      id = cb$concept_id[i],
      label = cb$pref_label[i],
      notation = cb$notation[i],
      position = if (!is.null(cb$position[i])) cb$position[i] else NULL
    )
  })

  list(
    lang = cb$language[1],
    doc = list(
      uri = meta$doc_uri,
      title = meta$label,
      version = meta$version,
      issued = format(Sys.Date())
    ),
    schemes = list(list(
      id = meta$scheme_fragment %||% "doc",
      label = meta$label,
      concepts = concepts
    ))
  )
}

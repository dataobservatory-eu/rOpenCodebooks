#' Publish a codebook as HTML and Turtle
#'
#' Uses Mustache templates to render human and machine-readable codebooks.
#'
#' @param cb A codebook_df.
#' @param out_dir Output directory (created if missing).
#' @param templates_dir Path to Mustache templates.
#' @param html Logical, render HTML index page.
#' @param ttl Logical, render Turtle file.
#'
#' @return Path to output directory.
#' @export
publish_codebook_hash <- function(cb, out_dir, templates_dir, html = TRUE, ttl = TRUE) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ctx <- build_ctx(cb)
  tpl <- function(name) paste(readLines(file.path(templates_dir, name), warn = FALSE), collapse = "\n")
  if (ttl) {
    ttl_txt <- whisker::whisker.render(tpl("scheme.ttl.mustache"), ctx)
    writeLines(ttl_txt, file.path(out_dir, "scheme.ttl"))
  }
  if (html) {
    html_txt <- whisker::whisker.render(tpl("index.html.mustache"), ctx)
    writeLines(html_txt, file.path(out_dir, "index.html"))
  }
  invisible(normalizePath(out_dir))
}

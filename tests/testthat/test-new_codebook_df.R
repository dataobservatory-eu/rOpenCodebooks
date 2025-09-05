# Helper: minimal rows and meta used across tests
min_rows <- data.frame(
  concept_id = c("agree", "disagree"),
  pref_label = c("Agree", "Disagree"),
  notation   = c("AG", "DI"),
  language   = "en",
  stringsAsFactors = FALSE
)

min_meta <- list(
  doc_uri         = "https://example.org/codelists/demo/v1",
  scheme_id       = "demo_scale",
  scheme_fragment = "scale-2",
  version         = "v1",
  label           = "Agreement (2-point)"
)

test_that("constructor creates a codebook_df with expected attributes", {
  cb <- new_codebook_df(min_rows, min_meta, "datacite", bib_args = list())

  expect_s3_class(cb, "codebook_df")
  expect_s3_class(cb, "dataset_df")  # inherits
  expect_true(!is.null(attr(cb, "scheme_meta")))
  expect_type(attr(cb, "scheme_meta"), "list")
  expect_true(!is.null(attr(cb, "dataset_bibentry")))
  expect_equal(nrow(cb), nrow(min_rows))
})

test_that("constructor requires the minimal columns", {
  bad <- min_rows[, c("concept_id", "pref_label", "language")]
  expect_error(
    new_codebook_df(bad, min_meta),
    "Missing required column"
  )
})

test_that("constructor validates concept_id as URI-fragment-safe", {
  bad <- min_rows
  bad$concept_id[1] <- "bad id!"    # space + exclamation
  expect_error(
    new_codebook_df(bad, min_meta),
    "Invalid concept_id"
  )
})

test_that("DataCite is the default bib; Dublin Core optional", {
  cb_dc <- new_codebook_df(min_rows, min_meta)  # default datacite
  bib_dc <- attr(cb_dc, "dataset_bibentry")
  # title may be stored as 'title' by our helper
  expect_true(!is.null(bib_dc$title) || !is.null(bib_dc$Title))

  cb_dc2 <- new_codebook_df(min_rows, min_meta, bib = "dublincore")
  bib_dub <- attr(cb_dc2, "dataset_bibentry")
  expect_true(!is.null(bib_dub$title))
  # date often present in dublincore defaults
  expect_true(!is.null(bib_dub$date) || !is.null(bib_dub$Date))
})

test_that("bib_args overrides default bibliographic fields", {
  cb <- new_codebook_df(
    min_rows, min_meta,
    bib = "datacite",
    bib_args = list(
      title = "Custom title",
      creator = c("Doe, Jane", "Smith, Ádám"),
      publisher = "Data Observatory",
      publication_year = 2030,
      rights = "MIT"
    )
  )
  bib <- attr(cb, "dataset_bibentry")
  # Depending on dataset::datacite structure, title may be 'title'
  expect_equal(bib$title %||% bib$Title, "Custom title")
  expect_true(any(grepl("Doe, Jane", paste(bib$creator, collapse = " "))))
  expect_true(any(grepl("Data Observatory", as.character(bib$publisher))))
  expect_true(any(as.integer(bib$publication_year %||% NA_integer_) == 2030))
})

test_that("print shows header, concept URIs, and key fields", {
  cb <- new_codebook_df(min_rows, min_meta)
  out <- capture.output(print(cb))

  # Header contains label and codebook tag
  expect_true(any(grepl("Agreement \\(2-point\\).*\\[codebook\\]", out)))

  # Concept URIs appear (doc_uri#concept_id)
  expect_true(any(grepl("https://example.org/codelists/demo/v1#agree", out)))
  expect_true(any(grepl("https://example.org/codelists/demo/v1#disagree", out)))

  # Notations and labels appear
  expect_true(any(grepl("\\bAG\\b", out)))
  expect_true(any(grepl("\\bDisagree\\b", out)))

  # print returns invisibly
  expect_invisible(print(cb))
})

test_that("multilingual rows are accepted and printed", {
  hu <- transform(min_rows, language = "hu",
                  pref_label = c("Egyetértek", "Nem értek egyet"))
  cb <- new_codebook_df(rbind(min_rows, hu), min_meta)

  out <- capture.output(print(cb))
  expect_true(any(grepl("Egyetértek", out)))
  expect_true(any(grepl("Nem értek egyet", out)))
})

test_that("optional columns (position, is_missing) print when present", {
  rows <- min_rows
  rows$position <- c(2L, 1L)
  rows$is_missing <- c(FALSE, FALSE)
  cb <- new_codebook_df(rows, min_meta)

  out <- capture.output(print(cb))
  expect_true(any(grepl("\\bposition\\b", tolower(out))))  # column header line
  expect_true(any(grepl("\\b2\\b", out)))
})


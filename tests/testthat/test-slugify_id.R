test_that("slugify_id()", {
  expect_equal(slugify_id("Baltazar+"), "baltazar-")
})

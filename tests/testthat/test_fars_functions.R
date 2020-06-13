## Test package functionality


test_that("FARS data read successful", {
  expect_error(fars_read(file.path(normalizePath('../../'), make_filename(2013))), NA)
})

test_that("Read years successful", {
  expect_warning(fars_read_years(list(2013)), NA)
  expect_equal(nrow(fars_read_years(list(2013))[[1]]), 30202)
})

test_that("Summary successful", {
  expect_equal(nrow(fars_summarize_years(list(2013, 2014, 2015))), 12)
})

test_that("Map generation successful", {
  expect_error(fars_map_state(1, 2014), NA)
})

test_that("Error raised for invalid year", {
  expect_error(fars_map_state(1, 999))
})

test_that("Error raised for invalid state", {
  expect_error(fars_map_state(60, 2013))
})

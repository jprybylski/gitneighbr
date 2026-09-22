test_that("gitneighbr_addin() calls open_repo() with its defaults and returns its session invisibly", {
  called_with <- NULL
  fake_session <- structure(list(), class = "gitneighbr_session")
  testthat::local_mocked_bindings(
    open_repo = function(...) {
      called_with <<- list(...)
      fake_session
    }
  )
  ret <- withVisible(gitneighbr_addin())
  expect_false(ret$visible)
  expect_identical(ret$value, fake_session)
  expect_equal(length(called_with), 0L) # equivalent to open_repo() with no args
})

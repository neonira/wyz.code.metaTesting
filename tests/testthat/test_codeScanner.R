context("codeScanner")

test_that("codeScanner", {
  expect_error(codeScanner('\u03DF\u03D9\u03E1 x y z', 'x', 'x_i_3m'))
  expect_equal(codeScanner('x y z', 'alpha', 'al_i_3m'), 'x y z')

  expect_length(codeScanner('x <- y + 1', 'y', 'y_i_3', TRUE), 3)

  expect_equal(codeScanner('x <- paste("y", y) # y patch', 'y', 'y_i_3'),
               'x <- paste("y", y_i_3) # y patch')
})

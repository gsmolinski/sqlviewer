test_that("resolve_dot_dot_dot correctly captures and prepares dot-dot-dot args", {
  test_fun <- function(...) {
    list(...) # this is the same what we do in `open` fun before passing this to `resolve_dot_dot_dot` fun
  }
  dot_content <- test_fun(123, char = "nothing", TRUE, double = 1.23, nchar = nchar("test"))
  obj <- vapply(seq_len(length(dot_content)), resolve_dot_dot_dot, character(1), dot_content)
  expect_length(obj, 5)
  expect_type(obj, "character")
  expect_equal(obj, c("123", "char = \"nothing\"", "TRUE", "double = 1.23", "nchar = 4L"
  ))
})

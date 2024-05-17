test_that("prepare_content_to_evaluate recognizes correctly label in query", {
  expect_true(prepare_content_to_evaluate(c("", "--# fgh")))
  expect_true(prepare_content_to_evaluate(c("", "--# 123][x./fgh")))
  expect_false(prepare_content_to_evaluate(c("fgh", "--# 123][x./fgh")))
  expect_false(prepare_content_to_evaluate(c("      sfd", "--# 123][x./fgh")))
  expect_true(prepare_content_to_evaluate(c("      ", "--# 123][x./fgh")))
})

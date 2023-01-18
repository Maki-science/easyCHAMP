test_that("default processing works", {
  expect_equal(evalPurency(path="//HERE/COMES/YOUR/PATH/", dataReturn = TRUE, test = TRUE), 
               testdata.default)
})


# usethis::use_data(presets, testdata.default, overwrite = TRUE, internal = TRUE)


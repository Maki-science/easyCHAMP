test_that("Default processing works", {
  expect_equal(evalPurency(path="//HERE/COMES/YOUR/PATH/", 
                           dataReturn = TRUE, test = TRUE), 
               testdata.default)
})


test_that("Changing size classes works", {
  expect_equal(evalPurency(path="//HERE/COMES/YOUR/PATH/", 
                           sizeclasses = c(10,100,500), 
                           dataReturn = TRUE, test = TRUE), 
               testdata.size)
})


test_that("Skipping eocsum works", {
  expect_equal(evalPurency(path="//HERE/COMES/YOUR/PATH/", 
                           eocsum = FALSE, 
                           dataReturn = TRUE, test = TRUE), 
               testdata.noeocsum)
})


usethis::use_data(presets, testdata.default, testdata.size, testdata.noeocsum,
                  overwrite = TRUE, internal = TRUE)


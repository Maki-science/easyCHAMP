test_that("Default processing works", {
  expect_equal(easyCHAMP(path="//HERE/COMES/YOUR/PATH/", 
                           dataReturn = TRUE, test = TRUE), 
               testdata.default)
})

test_that("Labpresets works", {
  expect_equal(easyCHAMP(path="//HERE/COMES/YOUR/PATH/",
                           labpreset = "Laforsch",
                           dataReturn = TRUE, test = TRUE), 
               testdata.default)
})


test_that("Changing size classes works", {
  expect_equal(easyCHAMP(path="//HERE/COMES/YOUR/PATH/", 
                           sizeclasses = c(10,100,500), 
                           dataReturn = TRUE, test = TRUE), 
               testdata.size)
})


test_that("Skipping eocsum works", {
  expect_equal(easyCHAMP(path="//HERE/COMES/YOUR/PATH/", 
                           eocsum = FALSE, 
                           dataReturn = TRUE, test = TRUE), 
               testdata.noeocsum)
})

# check whether this is similar with easyCHAMP-config-helper.R
# usethis::use_data(easyCHAMPPresets, testdata.default, testdata.size, testdata.noeocsum, testdata.load, test.config,
#                   testdata.particles.default, testdata.particles.colourSep, testdata.particles.noBlank,
#                   overwrite = TRUE, internal = TRUE)


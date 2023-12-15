test_that("Default processing works", {
  expect_equal(evalPurency.particles(path="//HERE/COMES/YOUR/PATH/", 
                                      dataReturn = TRUE, 
                                     test = TRUE), 
               testdata.particles.default)
})

test_that("Processing with colourSep works", {
  expect_equal(evalPurency.particles(path="//HERE/COMES/YOUR/PATH/", 
                                     colourSep = "transparent",
                                     dataReturn = TRUE, 
                                     test = TRUE), 
               testdata.particles.colourSep
  )
})

test_that("Processing without blank works", {
  expect_equal(evalPurency.particles(path="//HERE/COMES/YOUR/PATH/", 
                                     dataReturn = TRUE, 
                                     noBlank = TRUE,
                                     test = TRUE), 
               testdata.particles.noBlank)
})

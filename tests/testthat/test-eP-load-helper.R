test_that("Test data loading and pre-processing works", {
  expect_equal(ep.load.helper(path = "//HERE/COMES/YOUR/PATH/",
                              particleNumbers = FALSE,
                              sep = test.config$sep, 
                              dec = test.config$dec, 
                              colL = test.config$colL,
                              colPol = test.config$colPol,
                              startrow = test.config$startrow, 
                              colReqPol = test.config$colReqPol, 
                              ReqPolKey = test.config$ReqPolKey, 
                              colShape = test.config$colShape, 
                              colCol = test.config$colCol, 
                              colLFib = test.config$colLFib,
                              colArea = test.config$colArea,
                              colWidth = test.config$colWidth,
                              test = TRUE,
                              fibre = test.config$fibre,
                              sphere = test.config$sphere,
                              fragment = test.config$fragment,
                              pixel = test.config$pixel), 
               testdata.load)
})
 
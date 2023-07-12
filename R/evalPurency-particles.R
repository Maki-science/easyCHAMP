######## evalPurency.particles() ###########
#' Automated evaluation of Purency data.
#' @description
#' Instead of a summarized version of *.csv files from Purency like from evalPurency(), this function
#' provides a list of all particles. Blank correction is done with a most parsimony particle comparison.
#' 
#' @param path The path where the files can be found. All csv files in this folder will be evaluated. Also saves the
#' resulting files in this directory.
#' @param polymers A vector containing the abbreviations of polymers to be considered. Default vector contains 22 
#' polymers.
#' @param divFactor Set a division factor, that is used to divide the results by this factor. All samples/filters
#' and blanks must have the same factor (otherwise apply manually after running the function). Defaults to 1.
#' @param colourSep Whether a special color of particles should be treated separately from other colours. Colours are
#' usually difficult to distinguish in the FTIR pictures. But e.g., black and non-blacks are distinguishable.
#' In some cases it is of interest to separately handle black particles (e.g., black foliage used in experiment). 
#' How the colour is named in Purency can be set here (e.g., "black" or "schwarz"). Defaults to FALSE, i.e., no separation.
#' @param dataReturn If set TRUE, a data frame will be returned containing the data of all measurement with 
#' the necessary information.
#' @param labpreset A preset for most of the parameters (except: path, polymers, dataReturn, eocsum). 
#' Can be requested by other labs, to be implemented, that they don't have to be set manually all the time. 
#' @param blankKey The key word to distinguish blanks from other measurements/samples. It is case sensitive to prevent
#' accidental matching. Defaults to "Blank".
#' @param noBlank Can be set TRUE if you don't have a blank at all and just want to summarize your data (defaults to FALSE).
#' @param sep Symbol in your *.csv files indicating new a column. Defaults to ';'.
#' @param dec Symbol in your *.csv files indicating decimal sign. Defaults to ','.
#' @param colPol Column number where the polymer type is stated. In the TOEKI lab this is column 6 (Class Name). 
#' The polymer names (or abbreviations) can be set manually in 'polymers' in case the default does not apply.
#' Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
#' @param colL Column number for the particle length. In the TOEKI lab this is column 17 (Length 5µ). 
#' Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
#' @param colReqPol Column number for the particle check, whether the particle is a polymer or not. 
#' In the TOEKI lab this is column 24 (Plastik? or Plastik ja/nein). Could also be provided as column name, but only 
#' in ASCII encoding (e.g., special character as . and ä = d).
#' @param ReqPolKey key word or sentence of 'colReqPol' that indicates that it is a plastic particle. Default is 'ja'.
#' @param colShape Column number for the particle shape. In the TOEKI lab this is column 25 (Form).
#' Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
#' @param colCol Column number for the particle color In the TOEKI lab this is column 26 (Farbe).
#' Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
#' @param colLFib Column number for the particle length in case of a fibre with corrected length (because of curvy shape)
#' In the TOEKI lab this is column 27 (Länge). Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
#' @param colArea Column number for the particle length in case of a fibre with corrected length (because of curvy shape)
#' In the TOEKI lab this is column 4 (Area µm²). Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
#' @param colWidth Column number for the particle length in case of a fibre with corrected length (because of curvy shape)
#' In the TOEKI lab this is column 18 (Width µm). Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
#' @param fibre How fibres are called in colShape (Form). In the TOEKI lab it is 'Faser'.
#' @param sphere How spheres are called in colShape (Form). In the TOEKI lab it is 'Kugel'.
#' @param fragment How fragments are called in colShape (Form). In the TOEKI lab it is 'Fragment'.
#' @param pixel How pixels are called in colShape (Form). In the TOEKI lab it is 'Pixel'.
#' @param test Can be set TRUE when the function should be run in testing mode.
#' @param startrow Only required rarely. If you use a Purency version that saves the csv files slightly
#' differently, you might check at which line the data starts (including header). This number of line should
#' be set here (default 40).
#' 
#' @return If dataReturn = TRUE, the function returns a list object including all 
#'  processed data of each processing step and the summary values.
#' 
#' @examples 
#' # For this example the path doesn't matter. 
#' # If you want to analyse your own data, set test = FALSE (or simply delete this parameter).
#' mydata <- evalPurency.particles(path="//HERE/COMES/YOUR/PATH/", dataReturn = TRUE, test = TRUE)
#' 
#'
#' @references https://www.purency.ai/microplastics-finder
#'
#' @export
#' @import writexl
#' @importFrom utils read.csv
#' @importFrom stats aggregate


evalPurency.particles <- function(path, 
                                  polymers = c("PU", "EVAc", "PA", "PAN", "PBT", "PET", "PE", "PMMA", "PP", 
                                               "POM", "PS", "PVC", "PC", "ABS", "PPSU", "CA", "PEEK", "EVOH", 
                                               "PSU", "SILICONE", "PLA", "PLAPBAT"),
                                  divFactor = 1,
                                  colourSep = FALSE,
                                  dataReturn = FALSE,
                                  labpreset = FALSE,
                                  blankKey = "Blank",
                                  noBlank = FALSE,
                                  sep = ";",
                                  dec = ",",
                                  colPol = 6, 
                                  colL = 17,
                                  colReqPol = 24, 
                                  ReqPolKey = "ja",
                                  colShape = 25, 
                                  colCol = 26,
                                  colLFib = 27,
                                  colArea = 4,
                                  colWidth = 18,
                                  fibre = "Faser",
                                  sphere = "Kugel",
                                  fragment = "Fragment",
                                  pixel = "Pixel",
                                  test = FALSE,
                                  startrow = 40
                                  ){
  
  # set/correct the configuration (if labpreset was chosen)
  config <- eP.config.helper(labpreset = labpreset,
                             blankKey = blankKey,
                             sep = sep,
                             dec = dec,
                             colPol = colPol, 
                             colL = colL,
                             colReqPol = colReqPol, 
                             ReqPolKey = ReqPolKey,
                             colShape = colShape, 
                             colCol = colCol,
                             colLFib = colLFib,
                             colArea = colArea,
                             colWidth = colWidth,
                             fibre = fibre,
                             sphere = sphere,
                             fragment = fragment,
                             pixel = pixel,
                             startrow = startrow)
  
  #### load data #####
  temp <- ep.load.helper(path = path, 
                         sep = config$sep, 
                         dec = config$dec, 
                         colL = config$colL,
                         colPol = config$colPol,
                         startrow = config$startrow, 
                         colReqPol = config$colReqPol, 
                         ReqPolKey = config$ReqPolKey, 
                         colShape = config$colShape, 
                         colCol = config$colCol, 
                         colLFib = config$colLFib,
                         colArea = config$colArea,
                         colWidth = config$colWidth,
                         test = test,
                         fibre = config$fibre,
                         sphere = config$sphere,
                         fragment = config$fragment,
                         pixel = config$pixel
  )
  
  #### start processing ####
  obj <- list() # object to be returned holding the rawdata, as well as the corrected and summarized data
  
  obj$rawData <- temp
  
  if(noBlank == FALSE){
    
    # split blanks and samples
    dataBlanks <- temp[grepl(config$blankKey, temp$sample, fixed = TRUE) == TRUE,]
    dataMeasurements <- temp[grepl(config$blankKey, temp$sample, fixed = TRUE) != TRUE,]

    # add blank and sample list to obj
    obj$blanks <- dataBlanks
    obj$samples <- dataMeasurements
    
    #### TODO: colourSep = "schwarz"
    
    #### Q: How to treat several blanks for one sample ("average" or sum?)
    #### TODO: compare blanks and calculate a mean of very similar particles - delete the original
    
    # iterate over blank samples
    for(i in 1:length(levels(factor(dataBlanks$sample)))){
      # check if more than one replicate for one blank is available (replicates)
      if(length(levels(factor(dataBlanks$measurement[which(dataBlanks$sample == levels(factor(dataBlanks$sample))[i])]))) > 1){
        # how many blank replicates are there?
        n_lev <- length(levels(factor(dataBlanks$measurement[which(dataBlanks$sample == levels(factor(dataBlanks$sample))[i])])))
        
        # make subset with only the replicates of one blank. 
        # take one as main frame
        # compare with others the particles. if similar particles can be found:
        # take average of all (what if its more than the number of replicates? - is it then 2 exact same
        # average particles?).
        # OR 
        # pick randomly one or more particles from the similar ones? Should the other be deleted, or
        # remain for further comparisons (-> some particles might be counted several times -> number of
        # blank particles inceases)?
        
      }
    }
    
    
    #### TODO: add processed blanks as list to obj
    
    #### TODO: delete particles from samples that are very similar to blank particles. 
    # In case of more than one fit, delete the closest.
    
    #### TODO: add corrected particle list to obj
    
    #### TODO: apply division factor
    #### Q: how do that? Maybe use random numbers according to mean and variance? Bootstrap? combine both?
    
  }
  else{ # noBlank == TRUE
    # without blanks, no processing must take place. Thus, just the raw data table is provided
  }
  
  
  if(dataReturn == TRUE){
    return(obj)
  }

}
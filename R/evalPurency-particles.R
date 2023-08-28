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
#' @param sizeclasses A vector containing desired sizeclasses to be evaluated. Default is c(10, 20, 50, 100, 150, 
#' 200, 250, 300, 350, 400, 450, 500) The function starts at 0 and then uses the set steps. 
#' It always uses values up to the provided higher number but excluding the former number (e.g., for
#' the default values, the function uses 0 to <= 10, >10 to <= 20, >20 to <= 50, ..., all >500).
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
                                  sizeclasses = c(10, 20, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
                                  divFactor = 1,
                                  colourSep = FALSE,
                                  dataReturn = FALSE,
                                  labpreset = FALSE,
                                  noBlank = FALSE,
                                  test = FALSE,
                                  blankKey = "Blank",
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
  
  #### keep data vertically ####
  # This way a shapewise size classification can be realized, further allowing a blank correction processing
  
  vdata <- temp
  vdata$sizeClass <- NA
  
  for(i in 1:nrow(vdata)){
    for(j in 1:length(sizeclasses)){
      if(vdata$actualLength[i] <= sizeclasses[1]){
        vdata$sizeClass[i] <- paste("from", "0", "to", sizeclasses[1], sep="")
      }
      else if(vdata$actualLength[i] > sizeclasses[length(sizeclasses)]){
        vdata$sizeClass[i] <- paste("above", sizeclasses[length(sizeclasses)], sep="")
      }
      else if(j > 1 && vdata$actualLength[i] > sizeclasses[j-1] && vdata$actualLength[i] <= sizeclasses[j]){
        vdata$sizeClass[i] <- paste("from", sizeclasses[j-1], "to", sizeclasses[j], sep="")
      }
      else{ # otherwise do nothing and go on to the next
      }
    } # end for j
  } # end for i
  
  
  if(noBlank == FALSE){
    
    # split blanks and samples
    dataBlanks <- vdata[grepl(config$blankKey, vdata$sample, fixed = TRUE) == TRUE,]
    dataMeasurements <- vdata[grepl(config$blankKey, vdata$sample, fixed = TRUE) != TRUE,]

    # add blank and sample list to obj
    obj$blanks <- dataBlanks
    obj$samples <- dataMeasurements
    
    
    #### TODO: colourSep = "schwarz"
    #### For this, in every step I have to check, whether colourSep != FALSE. 
    #### In case, an additionall loop has to be included to separate the given colour from all others.
    
    
    
    #### TODO: compare blanks and calculate a mean of very similar particles - delete the original
    #### take blanks of the same sample, and check whether there are 
    #### similar particles (polymer, shape, color, sizeclass).
    #### For each of x similar particles, take the median particle into a new correction list 
    #### (x = amount of blanks for this sample), and delete the one from the blanks (or a temp).
    #### If there are more than x similar particles, take another median from the rest particles, etc..
    #### This will result in a particles correction list, used for the blank correction in the next step
    
    # create a correctionList, containing the particles that should be substracted from sample
    correctionList <- data.frame()
    
    # iterate over blank samples
    for(i in 1:length(levels(factor(dataBlanks$sample)))){
      # check if more than one replicate for one blank is available (replicates)
      if(length(levels(factor(dataBlanks$measurement[which(dataBlanks$sample == levels(factor(dataBlanks$sample))[i])]))) > 1){
        # how many blank replicates are there?
        n_lev <- length(levels(factor(dataBlanks$measurement[which(dataBlanks$sample == levels(factor(dataBlanks$sample))[i])])))
        
        # iterate over polymers
        for(j in 1:length(polymers)){
          
        }
        
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
    obj$samples <- vdata
  }
  
  
  if(dataReturn == TRUE){
    return(obj)
  }

}
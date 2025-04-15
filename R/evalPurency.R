######## evalPurency() ###########
#' Automated evaluation of Purency data. 
#' @description
#' Evaluate *.csv files, produced by Purency. It will count occurences of of fibres, fragments, spheres and pixels,
#' as well as size fractions (customisable) for each polymer. Each file (i.e., each measurement)
#' is evaluated separately, as well as summarized for all files (i.e., one sample). 
#' 
#' @param path The path where the files can be found. All csv files in this folder will be evaluated. Also saves the
#' resulting files in this directory.
#' @param polymers A vector containing the abbreviations of polymers to be considered. Default vector contains 22 
#' polymers.
#' @param sizeclasses A vector containing desired sizeclasses to be evaluated. Default is c(10, 20, 50, 100, 150, 
#' 200, 250, 300, 350, 400, 450, 500) The function starts at 0 and then uses the set steps. 
#' It always uses values up to the provided higher number but excluding the former number (e.g., for
#' the default values, the function uses 0 to <= 10, >10 to <= 20, >20 to <= 50, ..., all >500).
#' @param setDivFactor If set to 'filterwise', the function will request division factors for each filter during the processing.
#' If set to 'samplewise', the function will request division factors for each sample (all filters of one sample).
#' Sometimes the filters have to be divided because there are too many particles for one measurement. Then 
#' only a part of the original sample is analysed, which has to be corrected during the blank correction.
#' It further might be, that the blank has a different division factor than the sample, since it might be good
#' to use the full blank particle numbers and/or if a filter breaks appart, the factor might change for a single
#' sample. Therefore each sample (and blank) need its own factor. Defaults to FALSE.
#' @param dataReturn If set TRUE, a data frame will be returned containing the data of all measurement with 
#' the necessary information.
#' @param eocsum If TRUE (default) it adds a column sum at the end of each column of the summary panel.
#' @param formFillDefault If desired you can provide a default form that will be filled in, if no form is provided (NA). Should be
#' one of the values of the parameters 'fibre', 'sphere', 'fragment' or 'pixel' (see below).
#' @param colourFillDefault If desired you can provide a default colour that will be filled in, if no colour is provided in the data (NA).
#' However, in this function colour has no further meansing.
#' @param colourSep whether to separate colours in the analysis or not. Only usable in evalPurency.particles()!
#' @param labpreset A preset for most of the parameters (except: path, polymers, dataReturn, eocsum). 
#' Can be requested by other labs, to be implemented, that they don't have to be set manually all the time. 
#' @param blankKey The key word to distinguish blanks from other measurements/samples. It is case sensitive to prevent
#' accidental matching. Defaults to "Blank".
#' @param noBlank Can be set TRUE if you don't have a blank at all and just want to summarize your data (defaults to FALSE).
#' @param sep Symbol in your csv files indicating new a column. Defaults to ';'.
#' @param dec Symbol in your csv files indicating decimal sign. Defaults to ','.
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
#' @param startrow Number of rows that can be omitted from the *.csv files. Usually preprocessing software 
#' provides a bunch of meta data, that are not of interest here. Usually 40 rows can be skipped (more or less - 0 in case of siMPle).
#' This is automated now. However, automation can be disabled if a value is defined (e.g., for troubleshooting).
#' @param particleNumbers set TRUE if you would like to get an extra file with just plastic and non-plastic 
#' particle numbers for each file loaded.
#' 
#' @return If dataReturn = TRUE, the function returns a list object including all 
#'  processed data of each processing step and the summary values.
#' 
#' @examples 
#' # For this example the path doesn't matter. 
#' # If you want to analyse your own data, set test = FALSE (or simply delete this parameter).
#' mydata <- evalPurency(path="//HERE/COMES/YOUR/PATH/", dataReturn = TRUE, test = TRUE)
#' 
#' # Change evaluated size classes (evaluate <10, 10-100, 100-500 and >500 µm).
#' mydata <- evalPurency(path="//HERE/COMES/YOUR/PATH/", 
#'                       sizeclasses = c(10,100,500), dataReturn = TRUE, test = TRUE)
#'
#' # Include a division factor for the samples and blanks (in case filters have been divided).
#' # Only works in interactive Session.
#' mydata <- evalPurency(path="//HERE/COMES/YOUR/PATH/", 
#'                      setDivFactor = "samplewise", dataReturn = TRUE, test = TRUE)
#'
#' # Skip the summary row at bottom of each column in the sample summary.
#' mydata <- evalPurency(path="//HERE/COMES/YOUR/PATH/", 
#'                       eocsum = FALSE, dataReturn = TRUE, test = TRUE)
#'
#' @references https://www.purency.ai/microplastics-finder, https://maki-science.github.io/evalPurency/index.html
#'
#' @export
#' @import writexl
#' @importFrom utils read.csv
#' @importFrom stats aggregate
#'
evalPurency <- function(path, 
                        polymers = c("PU", "EVAc", "PA", "PAN", "PBT", "PET", "PE", "PMMA", "PP", 
                                     "POM", "PS", "PVC", "PC", "ABS", "PPSU", "CA", "PEEK", "EVOH", 
                                     "PSU", "SILICONE", "PLA", "PLAPBAT"),
                        sizeclasses = c(10, 20, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
                        setDivFactor = FALSE,
                        dataReturn = FALSE,
                        eocsum = TRUE,
                        particleNumbers = FALSE,
                        formFillDefault = FALSE,
                        colourFillDefault = FALSE,
                        colourSep = FALSE,
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
                        startrow = "auto"
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
                         particleNumbers = particleNumbers,
                         sep = config$sep, 
                         dec = config$dec, 
                         formFillDefault = formFillDefault,
                         colourFillDefault = colourFillDefault,
                         colourSep = colourSep,
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
  
  #### keep data vertically ####
  # This way a shapewise size classification can be realized, further allowing a blank correction processing
  
  vdata <- temp
  vdata$sizeClass <- NA
  
  for(i in 1:nrow(vdata)){
    if(vdata$className[i] == "none"){} # if no particle was found in one sample, no sizeclass can be assigned
    else{
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
    } # end else
  } # end for i
  
  # add raw data to return object
  obj$rawData <- vdata
  
  #cat("Data loaded \n")
  
  #### sort data into size categories ####
  # create a data frame with just the columns of the size classes. Thereby, we don't need to repeat that
  # several times.
  # add columns for sizeclasses
  sizecolumns <- data.frame(x = 0)
  for(i in 1:(length(sizeclasses)+1)){
    if(i == 1){ # for the first number it's 0 to <=x
      sizecolumns <- cbind(sizecolumns, data.frame(x = NA))
      colnames(sizecolumns) <- c(colnames(sizecolumns)[-length(colnames(sizecolumns))], paste("from", "0", "to", sizeclasses[i], sep=""))
    }
    else if(i == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
      sizecolumns <- cbind(sizecolumns, data.frame(x = NA))
      colnames(sizecolumns) <- c(colnames(sizecolumns)[-length(colnames(sizecolumns))], paste("above", sizeclasses[i-1], sep=""))
    }
    else{
      sizecolumns <- cbind(sizecolumns, data.frame(x = NA))
      colnames(sizecolumns) <- c(colnames(sizecolumns)[-length(colnames(sizecolumns))], paste("from", sizeclasses[i-1], "to", sizeclasses[i], sep=""))
    }
  }
  sizecolumns <- sizecolumns[, -1]
  
  # create a data frame to hold all data, to return it if dataReturn set TRUE
  dataBlankCorr <- data.frame(
    sample = NA,
    measurement = NA,
    polymer = NA,
    form = NA
  )
  # add columns for sizeclasses
  dataBlankCorr <- cbind(dataBlankCorr, sizecolumns)
  # delete NA line
  dataBlankCorr <- dataBlankCorr[-1,]
  
  
  # the data will be subsetted, to only use the subsetted data in lower level loops. Otherwise it will create a lot
  # of 0 lines for each sample-measurement-polymer-form combination. This takes much longer processing times.
  # By subsetting, the processing time is way shorter (seconds compared to minutes!)
  for(i in 1:length(levels(factor(vdata$sample)))){
    vdatai <- droplevels(subset(vdata, vdata$sample == levels(factor(vdata$sample))[i]))
    
    for(m in 1:length(levels(factor(vdatai$measurement)))){
      vdatam <- droplevels(subset(vdatai, vdatai$measurement == levels(factor(vdatai$measurement))[m]))
      
      for(j in 1:length(levels(factor(vdatam$className)))){
        vdataj <- droplevels(subset(vdatam, vdatam$className == levels(factor(vdatam$className))[j]))
        
        if(levels(factor(vdatam$className))[j] == "none"){# no particle measured in this sample
          
          dataBlankCorrTemprow <- data.frame(
            sample = levels(factor(vdata$sample))[i],
            measurement = levels(factor(vdatai$measurement))[m],
            polymer = levels(factor(vdatam$className))[j],
            form = "none"
          )
          
          # create a new column for each size class and set NA
          for(l in 1:(length(sizeclasses)+1)){
            if(l == 1){ # for the first number it's 0 to <=x
              
              dataBlankCorrTemprow <- cbind(dataBlankCorrTemprow, data.frame(x = NA))
              colnames(dataBlankCorrTemprow) <- c(colnames(dataBlankCorrTemprow)[-length(colnames(dataBlankCorrTemprow))], paste("from", "0", "to", sizeclasses[l], sep=""))
              
              dataBlankCorrTemprow[,paste("from", "0", "to", sizeclasses[l], sep="")] <- 0
            }
            else if(l == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
              dataBlankCorrTemprow <- cbind(dataBlankCorrTemprow, data.frame(x = NA))
              colnames(dataBlankCorrTemprow) <- c(colnames(dataBlankCorrTemprow)[-length(colnames(dataBlankCorrTemprow))], paste("above", sizeclasses[l-1], sep=""))
              
              dataBlankCorrTemprow[,paste("above", sizeclasses[l-1], sep="")] <- 0
              
            }
            else{ # all other size classes
              dataBlankCorrTemprow <- cbind(dataBlankCorrTemprow, data.frame(x = NA))
              colnames(dataBlankCorrTemprow) <- c(colnames(dataBlankCorrTemprow)[-length(colnames(dataBlankCorrTemprow))], paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep=""))
              
              dataBlankCorrTemprow[,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")] <- 0
            }
          }
          
          dataBlankCorr <- rbind(dataBlankCorr, dataBlankCorrTemprow)

        
        } # end if no particle measured in this sample
        else{ 
          for(k in 1:length(levels(factor(vdataj$form)))){
            vdatak <- droplevels(subset(vdataj, vdataj$form == levels(factor(vdataj$form))[k]))
            
            dataBlankCorrTemprow <- data.frame(
              sample = levels(factor(vdata$sample))[i],
              measurement = levels(factor(vdatai$measurement))[m],
              polymer = levels(factor(vdatam$className))[j],
              form = levels(factor(vdataj$form))[k]
            )
            # create a new column for each size class and count the number of observations
            for(l in 1:(length(sizeclasses)+1)){
              if(l == 1){ # for the first number it's 0 to <=x
    
                dataBlankCorrTemprow <- cbind(dataBlankCorrTemprow, data.frame(x = NA))
                colnames(dataBlankCorrTemprow) <- c(colnames(dataBlankCorrTemprow)[-length(colnames(dataBlankCorrTemprow))], paste("from", "0", "to", sizeclasses[l], sep=""))
                
                dataBlankCorrTemprow[,paste("from", "0", "to", sizeclasses[l], sep="")] <- nrow(
                  vdatak[
                    which(vdatak$sizeClass == paste("from", "0", "to", sizeclasses[l], sep="")
                          ),
                    ]
                  )
              }
              else if(l == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
                dataBlankCorrTemprow <- cbind(dataBlankCorrTemprow, data.frame(x = NA))
                colnames(dataBlankCorrTemprow) <- c(colnames(dataBlankCorrTemprow)[-length(colnames(dataBlankCorrTemprow))], paste("above", sizeclasses[l-1], sep=""))
              
                dataBlankCorrTemprow[,paste("above", sizeclasses[l-1], sep="")] <- nrow(
                  vdatak[
                    which(
                          vdatak$sizeClass == paste("above", sizeclasses[l-1], sep="")
                    ),
                  ]
                )
                
              }
              else{ # all other size classes
                dataBlankCorrTemprow <- cbind(dataBlankCorrTemprow, data.frame(x = NA))
                colnames(dataBlankCorrTemprow) <- c(colnames(dataBlankCorrTemprow)[-length(colnames(dataBlankCorrTemprow))], paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep=""))
              
                dataBlankCorrTemprow[,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")] <- nrow(
                  vdatak[
                    which(
                          vdatak$sizeClass == paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")
                    ),
                  ]
                )
                
                }
            }
            
            dataBlankCorr <- rbind(dataBlankCorr, dataBlankCorrTemprow)
          } # end for k
        } # end else (no particle measured)
      } # end for j
    } # end for m
  } # end for i
  
  #cat("size categorized \n")
  
  #### divide data into blanks and samples ####

  # now we need the blanks, calculate a mean over all blanks for each sample and 
  # substract this mean number from each sample after adding up all measurements of each sample
  dataBlanks <- dataBlankCorr[grepl(config$blankKey, dataBlankCorr$sample, fixed = TRUE) == TRUE,]
  if(nrow(dataBlanks) == 0 && noBlank == FALSE){ # it may happen that there are no particles in the blank. Then also no lines are available for later processing, triggering an error
    cat("Warning: There are no plastic particles detected in all your blanks. Consider to use 'noBlank = TRUE' and remove the respective files from your folder, in case further errors occur.")
    # add an empty line to prevent further errors (theoretically)
    dataBlanks <- rbind(dataBlanks, c("noBlankparticle", "noBlankparticle", "none", "none", rep(NA,length(sizeclasses)+1)))
    colnames(dataBlanks) <- colnames(dataBlankCorr)
    noBlank <- TRUE # set TRUE. So in the next steps blanks will be ignored, and no corrections (must) take place
  }
  dataMeasurements <- dataBlankCorr[grepl(config$blankKey, dataBlankCorr$sample, fixed = TRUE) != TRUE,]
  
  # the summing of the sample values should be done before the blank correction
  # thus, we will sum up all values of the samples form-size wise perform the correction and then summarize the sample without the form-size wise columns
  
  #### sort data form-size wise ####
  # aggregate the data set to get the sums of particle numbers for each size class (form-polymerwise)
  # samples
  data.agg.formwise <- aggregate(dataMeasurements[5:(length(sizeclasses)+5)], by=list(factor(dataMeasurements$sample), factor(dataMeasurements$measurement), factor(dataMeasurements$polymer), factor(dataMeasurements$form)), sum, na.rm = TRUE)
  colnames(data.agg.formwise) <- c("sample", "measurement", "polymer", "form", colnames(data.agg.formwise)[5:length(colnames(data.agg.formwise))])
  
  if(noBlank == FALSE){
    # blanks
    dataBlanks <- aggregate(dataBlanks[5:(length(sizeclasses)+5)], by=list(factor(dataBlanks$sample), factor(dataBlanks$measurement), factor(dataBlanks$polymer), factor(dataBlanks$form)), sum, na.rm = TRUE)
    colnames(dataBlanks) <- c("sample", "measurement", "polymer", "form", colnames(dataBlanks)[5:length(colnames(dataBlanks))])
  }
  # else{
  #   # samples
  #   data.agg.formwise <- aggregate(dataMeasurements[5:(length(sizeclasses)+5)], by=list(factor(dataMeasurements$sample), factor(dataMeasurements$polymer), factor(dataMeasurements$form)), sum, na.rm = TRUE)
  #   colnames(data.agg.formwise) <- c("sample", "polymer", "form", colnames(data.agg.formwise)[4:length(colnames(data.agg.formwise))])
  #   
  #   if(noBlank == FALSE){
  #     # blanks
  #     dataBlanks <- aggregate(dataBlanks[5:(length(sizeclasses)+5)], by=list(factor(dataBlanks$sample), factor(dataBlanks$polymer), factor(dataBlanks$form)), sum, na.rm = TRUE)
  #     colnames(dataBlanks) <- c("sample", "polymer", "form", colnames(dataBlanks)[4:length(colnames(dataBlanks))])
  #   }
  # }
  
  # add uncorrected data and blanks to obj
  obj$sampleDataUncorrected <- data.agg.formwise
  obj$blanks <- dataBlanks

  
  if(noBlank == FALSE){
    #### check for blank existence ####
    # check whether each sample has a blank to use for correction
    # otherwise throw a warning message
    sampleBlankChecklist <- c(rep(FALSE, length(levels(factor(data.agg.formwise$sample)))))
    for(i in 1:length(levels(factor(unlist(strsplit(as.character(dataBlanks$sample), config$blankKey)))))){
      for(j in 1:length(levels(factor(data.agg.formwise$sample)))){
        if(grepl(levels(factor(unlist(strsplit(as.character(dataBlanks$sample), config$blankKey))))[i], levels(factor(data.agg.formwise$sample))[j], fixed = TRUE) == TRUE){
          sampleBlankChecklist[j] <- TRUE
        }
      }
    }
  
    for(i in 1:length(levels(factor(data.agg.formwise$sample)))){
      if(sampleBlankChecklist[i] == FALSE){
        cat(paste("Warning: There is no blank for ", levels(factor(data.agg.formwise$sample))[i], "\n", sep = ""))
      }
    }
  } # end if noBlank == FALSE
  
  #### apply division factors ####
  # sometimes the filters have to be divided because there are too many particles for one measurement. Then 
  # only a part of the original sample is analysed, which has to be corrected during the blank correction
  # It further might be, that the blank has a different division factor than the sample, since it might be good
  # to use the full blank particle numbers and/or if a filter breaks appart, the factor might change for a single
  # sample Therefore each sample (and blank) need its own factor.
  
  # Martin want to keep the division factors trackable. Therefore, I will leave this column in the data.
  # Here I set the default. If no division factors are set
  obj$divisionFactors <- data.frame(divFactor = "No division factors have been chosen. Therefore, the factor defaults to 1 (no division).")
  
  
  if(interactive() && setDivFactor != FALSE){
    # add a new column with the division factor (default = 1, meaning no division)
    data.agg.formwise$divFactor <- 1
    if(noBlank == FALSE){
      dataBlanks$divFactor <- 1
    }
    
    # if setDivFactor is not set correctly
    if((setDivFactor == "samplewise" || setDivFactor == "filterwise" || setDivFactor == TRUE) != TRUE){
      stop(paste("You defined setDivFactor with unknown value: ", setDivFactor, sep=""))
    }
    else{
      
      # get the sample/blank names if setDivFactor == "samplewise" or TRUE or the filter names if setDivFactor == "filterwise"
      if(setDivFactor == "samplewise" || setDivFactor == TRUE){
        sampleFilters <- levels(factor(data.agg.formwise$sample))
        if(noBlank == FALSE){
          blankFilters <- levels(factor(dataBlanks$sample))
        }
      }
      if(setDivFactor == "filterwise"){
        sampleFilters <- levels(factor(data.agg.formwise$measurement))
        if(noBlank == FALSE){
          blankFilters <- levels(factor(dataBlanks$measurement))
        }
      }
      
      cat(
          "\nYou chose to set division factors for your samples/filters. \nPlease insert the factors in the same order like the samples are requested below with a single number between 0 and 1 and press 'enter' (e.g., 1 or 0.25, etc.):")
      #cat(sampleFilters)
      
      sampleDivFactors <- c()
      
      i <- 0
      suppressWarnings(
      while(length(sampleFilters) != length(sampleDivFactors)){
        i <- i+1
        newDivFactor <- as.numeric(readline(paste("Set the factors for the sample '", sampleFilters[i], "': ", sep = "")))
        while(newDivFactor <= 0 || newDivFactor > 1 || is.na(newDivFactor) == TRUE){
          reDivFactor <- readline(paste("Entry is invalid for the sample '", sampleFilters[i], "'! Please try again: ", sep = ""))
          if(reDivFactor == "stop" || reDivFactor == "stop()"){
            stop("You manually stopped the program.")
          }
          newDivFactor <- as.numeric(reDivFactor)
        }
        sampleDivFactors <- c(sampleDivFactors, newDivFactor)
      }
      )
      
      if(noBlank == FALSE){
        cat("And now do the same for the blanks (if similar to the samples, just insert the same numbers):\n")
        
        blankDivFactors <- c()
        
        i <- 0
        suppressWarnings(
        while(length(blankFilters) != length(blankDivFactors)){
          i <- i+1
          newDivFactor <- as.numeric(readline(paste("Set the factors for the blank '", blankFilters[i], "': ", sep = "")))
          while(newDivFactor <= 0 || newDivFactor > 1 || is.na(newDivFactor) == TRUE){
            reDivFactor <- readline(paste("Entry is invalid for the blank '", blankFilters[i], "'! Please try again: ", sep = ""))
            if(reDivFactor == "stop" || reDivFactor == "stop()"){
              stop("You manually stopped the program.")
            }
            newDivFactor <- as.numeric(reDivFactor)
          }
          blankDivFactors <- c(blankDivFactors, newDivFactor)
        }
        )
      } # end noBlank == FALSE
      
      # set the division factor for each line in the data set
      if(setDivFactor == "samplewise" || setDivFactor == TRUE){
        # for samples
        for(i in 1:length(sampleDivFactors)){
          for(j in 1:nrow(data.agg.formwise)){
            if(data.agg.formwise$sample[j] == sampleFilters[i]){
              data.agg.formwise$divFactor[j] <- sampleDivFactors[i]
            }
          }
        }
        if(noBlank == FALSE){
          # for blanks
          for(i in 1:length(blankDivFactors)){
            for(j in 1:nrow(dataBlanks)){
              if(dataBlanks$sample[j] == blankFilters[i]){
                dataBlanks$divFactor[j] <- blankDivFactors[i]
              }
            }
          }
        }
      } # end if setDivFactor == "samplewise"
      if(setDivFactor == "filterwise"){
        # for filters
        for(i in 1:length(sampleDivFactors)){
          for(j in 1:nrow(data.agg.formwise)){
            if(data.agg.formwise$measurement[j] == sampleFilters[i]){
              data.agg.formwise$divFactor[j] <- sampleDivFactors[i]
            }
          }
        }
        if(noBlank == FALSE){
          # for blanks
          for(i in 1:length(blankDivFactors)){
            for(j in 1:nrow(dataBlanks)){
              if(dataBlanks$measurement[j] == blankFilters[i]){
                dataBlanks$divFactor[j] <- blankDivFactors[i]
              }
            }
          }
        }
      } # end setDivFactor == "filterwise"
      
      # Martin want to keep the division factors trackable. Therefore, I will leave this column in the data.
      if(noBlank == FALSE){
        obj$divisionFactors <- data.frame(sampleOrfilter = c(blankFilters, sampleFilters),
                                          divFactor = c(blankDivFactors, sampleDivFactors))
      }
      else{
        obj$divisionFactors <- data.frame(sampleOrfilter = c(sampleFilters),
                                          divFactor = c(sampleDivFactors))
      }
      
      # divide all numbers by the division factor
      if(noBlank == FALSE){
        for(j in 1:nrow(dataBlanks)){
          # iterate over the size classes to perform the division factor processing for each size class
          for(k in 1:(length(sizeclasses)+1)){
            if(k == 1){ # for the first number it's 0 to <=x
              dataBlanks[,paste("from", "0", "to", sizeclasses[k], sep="")][j] <- ceiling(dataBlanks[,paste("from", "0", "to", sizeclasses[k], sep="")][j] / dataBlanks$divFactor[j])
            }
            else if(k == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
              dataBlanks[,paste("above", sizeclasses[k-1], sep="")][j] <- ceiling(dataBlanks[,paste("above", sizeclasses[k-1], sep="")][j] / dataBlanks$divFactor[j])
            }
            else{
              dataBlanks[,paste("from", sizeclasses[k-1], "to", sizeclasses[k], sep="")][j] <- ceiling(dataBlanks[,paste("from", sizeclasses[k-1], "to", sizeclasses[k], sep="")][j] / dataBlanks$divFactor[j])
            }
          } # end for k
        } # end for j
      } # end if noBlank == FALSE
        
      for(j in 1:nrow(data.agg.formwise)){
        # iterate over the size classes to perform the division factor processing for each size class
        for(k in 1:(length(sizeclasses)+1)){
          if(k == 1){ # for the first number it's 0 to <=x
            data.agg.formwise[,paste("from", "0", "to", sizeclasses[k], sep="")][j] <- ceiling(data.agg.formwise[,paste("from", "0", "to", sizeclasses[k], sep="")][j] / data.agg.formwise$divFactor[j])
          }
          else if(k == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
            data.agg.formwise[,paste("above", sizeclasses[k-1], sep="")][j] <- ceiling(data.agg.formwise[,paste("above", sizeclasses[k-1], sep="")][j] / data.agg.formwise$divFactor[j])
          }
          else{
            data.agg.formwise[,paste("from", sizeclasses[k-1], "to", sizeclasses[k], sep="")][j] <- ceiling(data.agg.formwise[,paste("from", sizeclasses[k-1], "to", sizeclasses[k], sep="")][j] / data.agg.formwise$divFactor[j])
          }
        } # end for k
      } # end for j
      
      # delete the division factor column because they are not needed any more
      data.agg.formwise$divFactor <- NULL
      if(noBlank == FALSE){
        dataBlanks$divFactor <- NULL
      }
    }# end else if setDivFactor = samplewise or filterwise
    
    #cat("Division correction done.\n")
  } # end if setdivisionfactor = TRUE
  #else{}

  # create sample summary if the div factor was set filterwise (sum again on sample level, not measurement level)
  # if(setDivFactor == "filterwise"){ # is now commented out, since I should always use the filterwise approach (see above, when splitting the data)
  # samples
  data.agg.formwise <- aggregate(data.agg.formwise[c(5:length(colnames(data.agg.formwise)))], by=list(data.agg.formwise$sample, data.agg.formwise$polymer, data.agg.formwise$form), sum, na.rm = TRUE)
  colnames(data.agg.formwise) <- c("sample", "polymer", "form", colnames(data.agg.formwise)[4:length(colnames(data.agg.formwise))])
  
  # blanks
  if(noBlank == FALSE){
    dataBlanks <- aggregate(dataBlanks[c(5:length(colnames(dataBlanks)))], by=list(dataBlanks$sample, dataBlanks$polymer, dataBlanks$form), sum, na.rm = TRUE)
    colnames(dataBlanks) <- c("sample", "polymer", "form", colnames(dataBlanks)[4:length(colnames(dataBlanks))])
  }
  
  
  #cat("Data sorted \n")
  
  
  if(noBlank == FALSE){
  #### check whether several blanks should be averaged (as indicated by ...Blank1_, ...Blank2_, etc.) ####  
  for(i in 1:length(levels(factor(unlist(strsplit(as.character(dataBlanks$sample),"_")))))){
    blanklevels <- levels(factor(unlist(strsplit(as.character(dataBlanks$sample),"_"))))
    if(!is.na(strsplit(blanklevels[i], config$blankKey)[[1]][2]) == TRUE){ 
      # when there is a number behind Blank, take the average of all with a similar level
      dataBlanks <- aggregate(dataBlanks[c(4:length(colnames(dataBlanks)))], by=list(unlist(sapply(strsplit(as.character(dataBlanks$sample), config$blankKey, fixed = TRUE), getElement, 1)), dataBlanks$polymer, dataBlanks$form), FUN = sum, na.rm = TRUE)
      # iterate over the blanks and divide the numbers of each lines by the number of blanks for this respective blank group
      for(j in 1:length(unique(unlist(sapply(strsplit(blanklevels, config$blankKey, fixed = TRUE), getElement, 1))))){
        # get the count of those blank files
        blanklevelsnumber <- unlist(sapply(strsplit(blanklevels, config$blankKey, fixed = TRUE), getElement, 1))[unlist(sapply(strsplit(blanklevels, config$blankKey, fixed = TRUE), getElement, 1)) == unique(unlist(sapply(strsplit(blanklevels, config$blankKey, fixed = TRUE), getElement, 1)))[j]]
        dataBlanks[, 4:length(colnames(dataBlanks))] <- ceiling(dataBlanks[, 4:length(colnames(dataBlanks))] / length(blanklevelsnumber))
      }
      colnames(dataBlanks) <- c("sample", "polymer", "form", colnames(dataBlanks)[4:length(colnames(dataBlanks))])
      # reset the sample column (that the word "Blank" is included again)
      dataBlanks$sample <- paste(unlist(strsplit(as.character(dataBlanks$sample), config$blankKey)), config$blankKey, sep = "")
    }
  }  
    
  
  #### perform blank correction #### 
    # iterate over the samples (with existing Blanks - should be all, though)
    for(i in 1:length(levels(factor(unlist(strsplit(as.character(dataBlanks$sample), config$blankKey)))))){
      # iterate over all rows of the data to be corrected
      for(j in 1:nrow(data.agg.formwise)){
        # only perform a correction when the sample is the same
        if(grepl(levels(factor(unlist(strsplit(as.character(dataBlanks$sample), config$blankKey))))[i], data.agg.formwise$sample[j], fixed = TRUE) == TRUE){
          
          # calculate a correction factor. It is the mean of all Blanks for this sample of the respective polymer and form rounded to the next higher integer (to be conservative)
          # to calculate the mean we take the sum and divide manually by the number of blanks, since there are NAs (0 particles) in many cases and mean() would not count them in for the division
          
          # iterate over the size classes to perform the blank processing for each size class
          for(k in 1:(length(sizeclasses)+1)){
            if(k == 1){ # for the first number it's 0 to <=x
              # calculate a correction factor. It is the mean of all Blanks for this sample of the respective polymer, form and size class, rounded to the next higher integer (to be conservative)
              # added a 0 to the vector for mean calculation, to have a 0 if NA (no data in Blanks available for this polymer)
              corrFactor <- ceiling(sum(dataBlanks[,paste("from", "0", "to", sizeclasses[k], sep="")][grepl(levels(factor(unlist(strsplit(as.character(dataBlanks$sample), config$blankKey))))[i], dataBlanks$sample, fixed = TRUE) & dataBlanks$polymer == as.character(data.agg.formwise$polymer[j]) & as.character(dataBlanks$form) == data.agg.formwise$form[j]], na.rm = TRUE)/length(levels(factor(dataBlanks$sample[grepl(levels(factor(unlist(strsplit(as.character(dataBlanks$sample), config$blankKey))))[i], dataBlanks$sample, fixed = TRUE)]))))
              # if no data is available, set 0 (in this case no observations have been done)
              if(is.na(corrFactor)){
                corrFactor <- 0
              }
              # correct the number of particles by substracting the mean of the blanks
              data.agg.formwise[,paste("from", "0", "to", sizeclasses[k], sep="")][j] <- data.agg.formwise[,paste("from", "0", "to", sizeclasses[k], sep="")][j] - corrFactor
              
              if(data.agg.formwise[,paste("from", "0", "to", sizeclasses[k], sep="")][j] < 0){
                data.agg.formwise[,paste("from", "0", "to", sizeclasses[k], sep="")][j] <- 0
              }
            }
            else if(k == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
              # calculate a correction factor. It is the mean of all Blanks for this sample of the respective polymer, form and size class, rounded to the next higher integer (to be conservative)
              # added a 0 to the vector for mean calculation, to have a 0 if NA (no data in Blanks available for this polymer)
              corrFactor <- ceiling(sum(dataBlanks[,paste("above", sizeclasses[k-1], sep="")][grepl(levels(factor(unlist(strsplit(as.character(dataBlanks$sample), config$blankKey))))[i], dataBlanks$sample, fixed = TRUE) & dataBlanks$polymer == as.character(data.agg.formwise$polymer[j]) & as.character(dataBlanks$form) == data.agg.formwise$form[j]], na.rm = TRUE)/length(levels(factor(dataBlanks$sample[grepl(levels(factor(unlist(strsplit(as.character(dataBlanks$sample), config$blankKey))))[i], dataBlanks$sample, fixed = TRUE)]))))
              # if no data is available, set 0 (in this case no observations have been done)
              if(is.na(corrFactor)){
                corrFactor <- 0
              }
              # correct the number of particles by substracting the mean of the blanks
              data.agg.formwise[,paste("above", sizeclasses[k-1], sep="")][j] <- data.agg.formwise[,paste("above", sizeclasses[k-1], sep="")][j] - corrFactor
              
              if(data.agg.formwise[,paste("above", sizeclasses[k-1], sep="")][j] < 0){
                data.agg.formwise[,paste("above", sizeclasses[k-1], sep="")][j] <- 0
              }
            }
            else{
              # calculate a correction factor. It is the mean of all Blanks for this sample of the respective polymer, form and size class, rounded to the next higher integer (to be conservative)
              # added a 0 to the vector for mean calculation, to have a 0 if NA (no data in Blanks available for this polymer)
              corrFactor <- ceiling(sum(dataBlanks[,paste("from", sizeclasses[k-1], "to", sizeclasses[k], sep="")][grepl(levels(factor(unlist(strsplit(as.character(dataBlanks$sample), config$blankKey))))[i], dataBlanks$sample, fixed = TRUE) & dataBlanks$polymer == as.character(data.agg.formwise$polymer[j]) & as.character(dataBlanks$form) == data.agg.formwise$form[j]], na.rm = TRUE)/length(levels(factor(dataBlanks$sample[grepl(levels(factor(unlist(strsplit(as.character(dataBlanks$sample), config$blankKey))))[i], dataBlanks$sample, fixed = TRUE)]))))
              # if no data is available, set 0 (in this case no observations have been done)
              if(is.na(corrFactor)){
                corrFactor <- 0
              }
              # correct the number of particles by substracting the mean of the blanks
              data.agg.formwise[,paste("from", sizeclasses[k-1], "to", sizeclasses[k], sep="")][j] <- data.agg.formwise[,paste("from", sizeclasses[k-1], "to", sizeclasses[k], sep="")][j] - corrFactor
              
              if(data.agg.formwise[,paste("from", sizeclasses[k-1], "to", sizeclasses[k], sep="")][j] < 0){
                data.agg.formwise[,paste("from", sizeclasses[k-1], "to", sizeclasses[k], sep="")][j] <- 0
              }
            }
          }
          
        }
      } # end for j
    } # end for i
  } # end if noBlank == FALSE
  
  dataMeasurementsCorrected <- data.agg.formwise
  # add the corrected data (sample-measurement-polymer-form-wise) to to obj
  obj$correctedData <- dataMeasurementsCorrected
  
  #cat("Blank correction processed. \n")

  #### create summary data ####
  # create a summary for each sample with the (polymer-wise) numbers of each form, and size classes
  dataSampleSummary <- data.frame(
    sample = NA,
    measurement = NA,
    polymer = NA,
    fibres = NA,
    fragments = NA,
    spheres = NA,
    pixels = NA,
    particlesTotal = NA
  )
  # add columns for sizeclasses
  dataSampleSummary <- cbind(dataSampleSummary, sizecolumns)
  # delete NA line
  dataSampleSummary <- dataSampleSummary[-1,]

  # aggregate the data set to get the sums of particle numbers for each size class
  data.agg.sizewise <- aggregate(dataMeasurementsCorrected[4:(length(sizeclasses)+4)], by=list(factor(dataMeasurementsCorrected$sample), factor(dataMeasurementsCorrected$polymer)), sum, na.rm = TRUE)
  data.agg.formwise <- aggregate(dataMeasurementsCorrected[4:(length(sizeclasses)+4)], by=list(factor(dataMeasurementsCorrected$sample), factor(dataMeasurementsCorrected$polymer), factor(dataMeasurementsCorrected$form)), sum, na.rm = TRUE)


  # iterate over the samples to sum the numbers of all measurements polymer-form wise and polymer-size wise
  for(i in 1:length(levels(factor(dataMeasurementsCorrected$sample)))){
    currentSample <- levels(factor(dataMeasurementsCorrected$sample))[i]
    # create a row that can be added to the summary data set
    rowX <- data.frame(
      sample = currentSample,
      measurement = "sample summary",
      polymer = NA,
      fibres = NA,
      fragments = NA,
      spheres = NA,
      pixels = NA,
      particlesTotal = NA
    )
    # add columns for sizeclasses
    rowX <- cbind(rowX, sizecolumns)

    # now iterate over the polymers and sum the respective numbers (for each form and size class)
    for(j in 1:length(levels(factor(polymers)))){
      currentPolymer <- levels(factor(polymers))[j]

      rowX$polymer <- currentPolymer
      # since at some systems (Paulines) an error occurs if there is no available row for a case, I adpot this code.
      # it is not very elegant way, but I could not figure out the reason thus far...
      if(nrow(data.agg.formwise[which(data.agg.formwise$Group.1 == currentSample & data.agg.formwise$Group.2 == currentPolymer & data.agg.formwise$Group.3 == config$fibre), 4:length(colnames(data.agg.formwise))]) == 0){
        rowX$fibres <- 0
      }
      else{
        rowX$fibres <- sum(c(0, sum(data.agg.formwise[which(data.agg.formwise$Group.1 == currentSample & data.agg.formwise$Group.2 == currentPolymer & data.agg.formwise$Group.3 == config$fibre), 4:length(colnames(data.agg.formwise))], na.rm = TRUE)))
      }
      if(nrow(data.agg.formwise[which(data.agg.formwise$Group.1 == currentSample & data.agg.formwise$Group.2 == currentPolymer & data.agg.formwise$Group.3 == config$fragment), 4:length(colnames(data.agg.formwise))]) == 0){
        rowX$fragments <- 0
      }
      else{
        rowX$fragments <- sum(c(0, sum(data.agg.formwise[which(data.agg.formwise$Group.1 == currentSample & data.agg.formwise$Group.2 == currentPolymer & data.agg.formwise$Group.3 == config$fragment), 4:length(colnames(data.agg.formwise))], na.rm = TRUE)))
      }
      if(nrow(data.agg.formwise[which(data.agg.formwise$Group.1 == currentSample & data.agg.formwise$Group.2 == currentPolymer & data.agg.formwise$Group.3 == config$sphere), 4:length(colnames(data.agg.formwise))]) == 0){
        rowX$spheres <- 0
      }
      else{
        rowX$spheres <- sum(c(0, sum(data.agg.formwise[which(data.agg.formwise$Group.1 == currentSample & data.agg.formwise$Group.2 == currentPolymer & data.agg.formwise$Group.3 == config$sphere), 4:length(colnames(data.agg.formwise))], na.rm = TRUE)))
      }
      if(nrow(data.agg.formwise[which(data.agg.formwise$Group.1 == currentSample & data.agg.formwise$Group.2 == currentPolymer & data.agg.formwise$Group.3 == config$pixel), 4:length(colnames(data.agg.formwise))]) == 0){
        rowX$pixels <- 0
      }
      else{
        rowX$pixels <- sum(c(0, sum(data.agg.formwise[which(data.agg.formwise$Group.1 == currentSample & data.agg.formwise$Group.2 == currentPolymer & data.agg.formwise$Group.3 == config$pixel), 4:length(colnames(data.agg.formwise))], na.rm = TRUE)))
      }
      rowX$particlesTotal <- sum(c(0, rowX$fibres, rowX$fragments, rowX$spheres, rowX$pixels), na.rm = TRUE)

      # now sum for the sizeclasses
      for(k in 1:(length(sizeclasses)+1)){
        if(k == 1){ # for the first number it's 0 to <=x
          rowX[, paste("from", "0", "to", sizeclasses[k], sep="")] <- sum(c(0,data.agg.sizewise[, paste("from", "0", "to", sizeclasses[k], sep="")][which(data.agg.sizewise$Group.1 == currentSample & data.agg.sizewise$Group.2 == currentPolymer)]), na.rm = TRUE)
        }
        else if(k == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
          rowX[, paste("above", sizeclasses[k-1], sep="")] <- sum(c(0,data.agg.sizewise[, paste("above", sizeclasses[k-1], sep="")][which(data.agg.sizewise$Group.1 == currentSample & data.agg.sizewise$Group.2 == currentPolymer)]), na.rm = TRUE)
        }
        else{
          rowX[, paste("from", sizeclasses[k-1], "to", sizeclasses[k], sep="")] <- sum(c(0,data.agg.sizewise[, paste("from", sizeclasses[k-1], "to", sizeclasses[k], sep="")][which(data.agg.sizewise$Group.1 == currentSample & data.agg.sizewise$Group.2 == currentPolymer)]), na.rm = TRUE)
        }
      }

      # add the current row to the summary data set
      dataSampleSummary <- rbind(dataSampleSummary, rowX)
    } # end for j
  } # end for i

  obj$sampleSummary <- dataSampleSummary
  
  #cat("data summarized \n")
  
  
  #### write excel files ####
  # Now we create one excel file for the three data sets in obj (to trace back the process, if desired) and one excel file for each sample
  if(test == FALSE){
    writexl::write_xlsx(list(samples_summary = obj$sampleSummary, 
                             single_measurements_corrected = obj$correctedData,
                             unprocessed_blanks = obj$blanks,
                             unprocessed_data = obj$sampleDataUncorrected,
                             raw_data = obj$rawData,
                             division_factors = obj$divisionFactors),
                        paste(path, "processing data.xls", sep=""))
  }
  
  # for the single samples, there is the option to add a bottom line with the sum of each column (requested feature)
  for(i in 1:length(levels(factor(dataSampleSummary$sample)))){
    
    dataSubsample <- droplevels(subset(dataSampleSummary, dataSampleSummary$sample == levels(factor(dataSampleSummary$sample))[i]))
    dataSubX <- dataSubsample
    
    if(eocsum == TRUE){
      
      tempsumrow <- data.frame(
        sample = "column sum",
        measurement = "column sum",
        polymer = "column sum",
        fibres = sum(dataSubX$fibres),
        fragments = sum(dataSubX$fragments),
        spheres = sum(dataSubX$spheres),
        pixels = sum(dataSubX$pixels),
        particlesTotal = sum(dataSubX$particlesTotal)
      )
      # add columns for sizeclasses
      for(l in 1:(length(sizeclasses)+1)){
        if(l == 1){ # for the first number it's 0 to <=x
          tempsumrow <- cbind(tempsumrow, data.frame(x = sum(dataSubX[,paste("from", "0", "to", sizeclasses[l], sep="")])))
          colnames(tempsumrow) <- c(colnames(tempsumrow)[-length(colnames(tempsumrow))], paste("from", "0", "to", sizeclasses[l], sep=""))
        }
        else if(l == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
          tempsumrow <- cbind(tempsumrow, data.frame(x = sum(dataSubX[,paste("above", sizeclasses[l-1], sep="")])))
          colnames(tempsumrow) <- c(colnames(tempsumrow)[-length(colnames(tempsumrow))], paste("above", sizeclasses[l-1], sep=""))
        }
        else{
          tempsumrow <- cbind(tempsumrow, data.frame(x = sum(dataSubX[,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")])))
          colnames(tempsumrow) <- c(colnames(tempsumrow)[-length(colnames(tempsumrow))], paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep=""))
        }
      }
      
      dataSubX <- rbind(dataSubX, tempsumrow)
    } # end if eocsum = TRUE
    
    # additionally to the total summary, a form-wise summary was requested (Martin):
    colnames(data.agg.formwise) <- c("sample", "polymer", "form", colnames(data.agg.formwise)[4:length(colnames(data.agg.formwise))])
    
    forms <- c(config$fibre, config$fragment, config$sphere, config$pixel)
    formwiseSums <- list()
    
    for(j in 1:length(forms)){
      formSum <- data.frame()
      
      for(k in 1:length(levels(factor(polymers)))){
        currentPolymer <- levels(factor(polymers))[k]
        
        tempsumrow <- data.frame(
          sample = levels(factor(dataSampleSummary$sample))[i],
          measurement = "sample summary",
          polymer = currentPolymer,
          form = forms[j]
        )
        # add columns for sizeclasses
        for(l in 1:(length(sizeclasses)+1)){
          if(l == 1){ # for the first number it's 0 to <=x
            tempsumrow <- cbind(tempsumrow, data.frame(x = sum(data.agg.formwise[data.agg.formwise$polymer == currentPolymer & data.agg.formwise$sample == levels(factor(dataSampleSummary$sample))[i] & data.agg.formwise$form == forms[j], paste("from", "0", "to", sizeclasses[l], sep="")])))
            colnames(tempsumrow) <- c(colnames(tempsumrow)[-length(colnames(tempsumrow))], paste("from", "0", "to", sizeclasses[l], sep=""))
          }
          else if(l == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
            tempsumrow <- cbind(tempsumrow, data.frame(x = sum(data.agg.formwise[data.agg.formwise$polymer == currentPolymer & data.agg.formwise$sample == levels(factor(dataSampleSummary$sample))[i] & data.agg.formwise$form == forms[j] ,paste("above", sizeclasses[l-1], sep="")])))
            colnames(tempsumrow) <- c(colnames(tempsumrow)[-length(colnames(tempsumrow))], paste("above", sizeclasses[l-1], sep=""))
          }
          else{
            tempsumrow <- cbind(tempsumrow, data.frame(x = sum(data.agg.formwise[data.agg.formwise$polymer == currentPolymer & data.agg.formwise$sample == levels(factor(dataSampleSummary$sample))[i] & data.agg.formwise$form == forms[j] ,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")])))
            colnames(tempsumrow) <- c(colnames(tempsumrow)[-length(colnames(tempsumrow))], paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep=""))
          }
        } # end for l
        
        # calculate total numbers per line
        tempsumrow$particlesTotal <- rowSums(tempsumrow[5:length(colnames(tempsumrow))])
        
        formSum <- rbind(formSum, tempsumrow)
        
      } # end for k
      
      formwiseSums[[j]] <- formSum
    } # end for j
    
    if(test == FALSE){
      writexl::write_xlsx(list(sample_summary = dataSubX,
                               sample_fibres = formwiseSums[[1]],
                               sample_fragments = formwiseSums[[2]],
                               sample_spheres = formwiseSums[[3]],
                               sample_pixels = formwiseSums[[4]]
                               ),
                          paste(path, levels(factor(dataSampleSummary$sample))[i], "_evaluated.xls", sep=""))
    }
  } # end for i  
    
  
  #### finish ####
  cat("Done.")
  
  # return the whole data set as data frame
  if(dataReturn == TRUE){
    return(obj)
  }
  
}# end function

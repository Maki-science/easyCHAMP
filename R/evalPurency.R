######## evalPurency() ###########
#' Automated evaluation of Purency data
#' @description
#' Evaluate csv files, produced by Purency. It will count occurences of of fibres, fragments, spheres and pixels,
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
#' @param fibre How fibres are called in colShape (Form). In the TOEKI lab it is 'Faser'.
#' @param sphere How spheres are called in colShape (Form). In the TOEKI lab it is 'Kugel'.
#' @param fragment How fragments are called in colShape (Form). In the TOEKI lab it is 'Fragment'.
#' @param pixel How pixels are called in colShape (Form). In the TOEKI lab it is 'Pixel'.
#' @param test Can be set TRUE when the function should be run in testing mode.
#' @param startrow Only required rarely. If you use a Purency version that saves the csv files slightly
#' differently, you might check at which line the data starts (including header). This number of line should
#' be set here (default 41).
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
#' @references https://www.purency.ai/microplastics-finder
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
                        fibre = "Faser",
                        sphere = "Kugel",
                        fragment = "Fragment",
                        pixel = "Pixel",
                        test = FALSE,
                        startrow = 40
){
  
  # set/correct the configuration (if labpreset was chosen)
  config <- eP.config.helper(labpreset,
                             blankKey,
                             sep,
                             dec,
                             colPol, 
                             colL,
                             colReqPol, 
                             ReqPolKey,
                             colShape, 
                             colCol,
                             colLFib,
                             fibre,
                             sphere,
                             fragment,
                             pixel)
  
  if(test == FALSE){
    #### load files ####
    # get all files in the set folder
    Dateien <- list.files(path=path,pattern=".csv") # search for files with .csv ending
    # get the number of files
    name_measurement <- c(1:length(Dateien))
    
    # prepare files names to be used in read.csv()
    # and get the sample names (all before the fist _)
    for (i in 1:length(Dateien)){  
      names(Dateien)[i] <-Dateien[i] # generate names for values
      names(Dateien)[i] <-strsplit(names(Dateien)[i],".csv")[[1]][1]  # cut of '.txt' of the name
      name_measurement[i] <- strsplit(names(Dateien)[i],"_")[[1]][1]  # cut of '.txt' of the name
    }
    
    # temp data frame to store content of all files
    temp <- data.frame()
    
    # read all files and gather their data in temp
    # each particle will be stored in one line with the measurement number and sample name (unique combination).
    suppressWarnings(
      for (i in 1:length(Dateien)){
        # need to read data as ASCII. Otherwise it sometimes makes problems with the column names with special character
        assign("Hilfsobjekt",read.csv(paste0(path,Dateien[i]),sep=config$sep, dec=config$dec, skip=startrow, fileEncoding = "ASCII")) # read data and skip the first 40 lines
        
        # now with column numbers in the default form, but can also be provided als column name
        Hilfsobjekt <- droplevels(subset(Hilfsobjekt, Hilfsobjekt[config$colReqPol] == config$ReqPolKey))
        
        
        
        
        temp2 <- data.frame(sample = name_measurement[i],
                            measurement = Dateien[i],
                            className = Hilfsobjekt[, config$colPol], # polymer type
                            length = Hilfsobjekt[, config$colL], # length of the particle
                            form = Hilfsobjekt[, config$colShape], # fragment, pixel, fibre, sphere
                            color = Hilfsobjekt[, config$colCol],
                            lengthFibre = Hilfsobjekt[, config$colLFib]) # in case the fibre is curved, the length can be found here
        temp <- rbind(temp, temp2)
      }
    ) # end supressWarnings
    # until here the sample data are produced. This is what Purency provides
  } # end if test == FALSE
  else{ # if test == TRUE
    # load data from package sample data
    temp <- evalPurency::purencySampleData
  }  
  
  # quality control
  rowstodelete <- c()
  for(i in 1:nrow(temp)){
    check <- FALSE
    suppressWarnings( # I supress the warnings to have a simpler message that can be interpreted fast.
      # To add a quality control, I check whether the field of form or length is NA. If yes, a warning will be
      # thrown, including the sample and measurement.
      if(is.na(temp$form[i]) == TRUE || (temp$form[i] == fibre || temp$form[i] == fragment || temp$form[i] == pixel || temp$form[i] == sphere) == FALSE){
        check <- TRUE
        if(is.na(temp$form[i]) == TRUE){
          cat(warning(paste("Warning: There is a value missing in column ", colnames(Hilfsobjekt[config$colShape]), " in ", temp$measurement[i], "\n")))
        }
        else{
          cat(warning(paste("Warning: There is an unknown value in column ", colnames(Hilfsobjekt[config$colShape]), " in ", temp$measurement[i], "\n")))
        }
      }
    ) # end supressWarnings
    suppressWarnings(
      if(is.na(as.numeric(temp$length[i])) == TRUE){
        check <- TRUE
        cat(warning(paste("Warning: There is a value missing in column ", colnames(Hilfsobjekt[config$colL]), " in ", temp$measurement[i], "\n")))
      }
    ) # end supressWarnings
    if(check == TRUE){
      rowstodelete <- c(rowstodelete, i)
    }
  } # end for(i)
  if(length(rowstodelete) > 0){
    temp <- temp[-c(rowstodelete),]
  }
    
  # The length is not always correct for fibres.
  # Thus create a new column with the correct length for all particles (that I don't need to select the correct column further below)
  temp$actualLength <- NA
  for(i in 1:nrow(temp)){
    # If lengthFibre is NA, but form is still Faser, the length is the value to be taken.
    if(temp$form[i] == config$fibre && !is.na(temp$lengthFibre[i]) == TRUE){
      temp$actualLength[i] <- temp$lengthFibre[i]
    }
    else{
      temp$actualLength[i] <- temp$length[i]
    }
  } # end for i

  
  #### start processing ####
  obj <- list() # object to be returned holding the rawdata, as well as the corrected and summarized data
  
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
      } # end for j
    } # end for m
  } # end for i
  
  #cat("size categorized \n")
  
  #### divide data into blanks and samples ####

  # now we need the blanks, calculate a mean over all blanks for each sample and 
  # substract this mean number from each sample after adding up all measurements of each sample
  dataBlanks <- dataBlankCorr[grepl(config$blankKey, dataBlankCorr$sample, fixed = TRUE) == TRUE,]
  dataMeasurements <- dataBlankCorr[grepl(config$blankKey, dataBlankCorr$sample, fixed = TRUE) != TRUE,]
  
  # the summing of the sample values should be done before the blank correction
  # thus, we will sum up all values of the samples form-size wise perform the correction and then summarize the sample without the form-size wise columns
  
  #### sort data form-size wise ####
  # aggregate the data set to get the sums of particle numbers for each size class (form-polymerwise)
  if(setDivFactor == "filterwise"){
    data.agg.formwise <- aggregate(dataMeasurements[5:(length(sizeclasses)+5)], by=list(factor(dataMeasurements$sample), factor(dataMeasurements$measurement), factor(dataMeasurements$polymer), factor(dataMeasurements$form)), sum, na.rm = TRUE)
    colnames(data.agg.formwise) <- c("sample", "measurement", "polymer", "form", colnames(data.agg.formwise)[5:length(colnames(data.agg.formwise))])
  }
  else{
    data.agg.formwise <- aggregate(dataMeasurements[5:(length(sizeclasses)+5)], by=list(factor(dataMeasurements$sample), factor(dataMeasurements$polymer), factor(dataMeasurements$form)), sum, na.rm = TRUE)
    colnames(data.agg.formwise) <- c("sample", "polymer", "form", colnames(data.agg.formwise)[4:length(colnames(data.agg.formwise))])
  }
  
  # add uncorrected data and blanks to obj
  obj$sampleDataUncorrected <- data.agg.formwise
  obj$blanks <- dataBlanks

  
  if(noBlank == FALSE){
    #### check for blank existence ####
    # check whether each sample has a blank to use for correction
    # otherwise throw a warning message
    sampleBlankChecklist <- c(rep(FALSE, length(levels(factor(data.agg.formwise$sample)))))
    for(i in 1:length(levels(factor(unlist(strsplit(dataBlanks$sample, config$blankKey)))))){
      for(j in 1:length(levels(factor(data.agg.formwise$sample)))){
        if(grepl(levels(factor(unlist(strsplit(dataBlanks$sample, config$blankKey))))[i], levels(factor(data.agg.formwise$sample))[j], fixed = TRUE) == TRUE){
          sampleBlankChecklist[j] <- TRUE
        }
      }
    }
  
    for(i in 1:length(levels(factor(data.agg.formwise$sample)))){
      if(sampleBlankChecklist[i] == FALSE){
        cat(paste("Warning: There is no blank for ", levels(factor(data.agg.formwise$sample))[i], sep = ""))
      }
    }
  } # end if noBlank == FALSE
  
  #### apply division factors ####
  # sometimes the filters have to be divided because there are too many particles for one measurement. Then 
  # only a part of the original sample is analysed, which has to be corrected during the blank correction
  # It further might be, that the blank has a different division factor than the sample, since it might be good
  # to use the full blank particle numbers and/or if a filter breaks appart, the factor might change for a single
  # sample Therefore each sample (and blank) need its own factor.
  
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
    
    # create sample summary if the div factor was set filterwise (sum again on sample level, not measurement level)
    if(setDivFactor == "filterwise"){
      data.agg.formwise <- aggregate(data.agg.formwise[c(5:length(colnames(data.agg.formwise)))], by=list(data.agg.formwise$sample, data.agg.formwise$polymer, data.agg.formwise$form), sum, na.rm = TRUE)
      colnames(data.agg.formwise) <- c("sample", "polymer", "form", colnames(data.agg.formwise)[4:length(colnames(data.agg.formwise))])
    }
    
    #cat("Division correction done.\n")
  } # end if setdivisionfactor = TRUE
  #else{}

  
  #cat("Data sorted \n")
  
  if(noBlank == FALSE){
  #### perform blank correction #### 
    # iterate over the samples (with existing Blanks - should be all, though)
    for(i in 1:length(levels(factor(unlist(strsplit(dataBlanks$sample, config$blankKey)))))){
      # iterate over all rows of the data to be corrected
      for(j in 1:nrow(data.agg.formwise)){
        # only perform a correction when the sample is the same
        if(grepl(levels(factor(unlist(strsplit(dataBlanks$sample, config$blankKey))))[i], data.agg.formwise$sample[j], fixed = TRUE) == TRUE){
          
          # calculate a correction factor. It is the mean of all Blanks for this sample of the respective polymer and form rounded to the next higher integer (to be conservative)
          # to calculate the mean we take the sum and divide manually by the number of blanks, since there are NAs (0 particles) in many cases and mean() would not count them in for the division
          
          # iterate over the size classes to perform the blank processing for each size class
          for(k in 1:(length(sizeclasses)+1)){
            if(k == 1){ # for the first number it's 0 to <=x
              # calculate a correction factor. It is the mean of all Blanks for this sample of the respective polymer, form and size class, rounded to the next higher integer (to be conservative)
              # added a 0 to the vector for mean calculation, to have a 0 if NA (no data in Blanks available for this polymer)
              corrFactor <- ceiling(sum(dataBlanks[,paste("from", "0", "to", sizeclasses[k], sep="")][grepl(levels(factor(unlist(strsplit(dataBlanks$sample, config$blankKey))))[i], dataBlanks$sample, fixed = TRUE) & dataBlanks$polymer == data.agg.formwise$polymer[j] & dataBlanks$form == data.agg.formwise$form[j]], na.rm = TRUE)/length(levels(factor(dataBlanks$sample[grepl(levels(factor(unlist(strsplit(dataBlanks$sample, config$blankKey))))[i], dataBlanks$sample, fixed = TRUE)]))))
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
              corrFactor <- ceiling(sum(dataBlanks[,paste("above", sizeclasses[k-1], sep="")][grepl(levels(factor(unlist(strsplit(dataBlanks$sample, config$blankKey))))[i], dataBlanks$sample, fixed = TRUE) & dataBlanks$polymer == data.agg.formwise$polymer[j] & dataBlanks$form == data.agg.formwise$form[j]], na.rm = TRUE)/length(levels(factor(dataBlanks$sample[grepl(levels(factor(unlist(strsplit(dataBlanks$sample, config$blankKey))))[i], dataBlanks$sample, fixed = TRUE)]))))
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
              corrFactor <- ceiling(sum(dataBlanks[,paste("from", sizeclasses[k-1], "to", sizeclasses[k], sep="")][grepl(levels(factor(unlist(strsplit(dataBlanks$sample, config$blankKey))))[i], dataBlanks$sample, fixed = TRUE) & dataBlanks$polymer == data.agg.formwise$polymer[j] & dataBlanks$form == data.agg.formwise$form[j]], na.rm = TRUE)/length(levels(factor(dataBlanks$sample[grepl(levels(factor(unlist(strsplit(dataBlanks$sample, config$blankKey))))[i], dataBlanks$sample, fixed = TRUE)]))))
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
                             raw_data = obj$rawData),
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

######## evalPurency() ###########
#' created by Marvin Kiene, use this function at own risk!
#' @description
#' Evaluate csv files, produced by Purency. It will count occurences of of fibres, fragments, spheres and pixels,
#' as well as size fractions (<10, 10-20, 20-50, 50-100, 100-150, 150-200,..., >500) for each polymer. Each file (i.e., each measurement)
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
#' @param setDivFactor If set TRUE, the function will request division factors for each filter during the processing.
#' sometimes the filters have to be divided because there are too many particles for one measurement. Then 
#' only a part of the original sample is analysed, which has to be corrected during the blank correction.
#' It further might be, that the blank has a different division factor than the sample, since it might be good
#' to use the full blank particle numbers and/or if a filter breaks appart, the factor might change for a single
#' sample Therefore each sample (and blank) need its own factor. Defaults to FALSE.
#' @param dataReturn If set TRUE, a data frame will be returned containing the data of all measurement with 
#' the necessary information.
#' @param eocsum If TRUE (default) it adds a column sum at the end of each column of the summary panel.
#' @param labpreset A preset for most of the parameters (except: path, polymers, dataReturn, eocsum). 
#' Can be requested by other labs, to be implemented, that they don't have to be set manually all the time. 
#' @param blankKey The key word to distinguish blanks from other measurements/samples. It is case sensitive to prevent
#' accidental matching. Defaults to "Blank".
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
                        blankKey = "Blank",
                        labpreset = FALSE,
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
                        pixel = "Pixel"
){
  # set the path, where the files are stored, and where the result should be saved
  PATH <- path 
  
  #### lab presets ####
  # if a preset is set, load the respective preset (if available)
  # this is how a preset is created:
  # presets <- data.frame(
  #   labname = "Laforsch",
  #   blankKey = "blank",
  #   colPol = 6,
  #   colL = 17,
  #   colReqPol = 24,
  #   ReqPolKey = "ja",
  #   colShape = 25,
  #   colCol = 26,
  #   colLFib = 27,
  #   fibre = "Faser",
  #   sphere = "Kugel",
  #   fragment = "Fragment",
  #   pixel = "Pixel"
  # )
  # usethis::use_data(presets, internal = TRUE, overwrite = TRUE)
  if(labpreset != FALSE){
    if( length(presets$labname[which(presets$labname == labpreset)]) == 0){
      stop("You selected a labpreset, that does not exist. Please check this parameter for typo or request a new labpreset for your lab. \n")
    }
    
    colPol <- presets$colPol[which(presets$labname == labpreset)]
    colL <- presets$colL[which(presets$labname == labpreset)]
    colReqPol <- presets$colReqPol[which(presets$labname == labpreset)]
    colShape <- presets$colShape[which(presets$labname == labpreset)]
    colCol <- presets$colCol[which(presets$labname == labpreset)]
    colLFib <- presets$colLFib[which(presets$labname == labpreset)]
    fibre <- presets$fibre[which(presets$labname == labpreset)]
    sphere <- presets$sphere[which(presets$labname == labpreset)]
    fragment <- presets$fragment[which(presets$labname == labpreset)]
    pixel <- presets$pixel[which(presets$labname == labpreset)]
  }
  
  
  #### load files ####
  # get all files in the set folder
  Dateien <- list.files(path=PATH,pattern=".csv") # search for files with .csv ending
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
      assign("Hilfsobjekt",read.csv(paste0(PATH,Dateien[i]),sep=";", dec=",", skip=40, fileEncoding = "ASCII")) # read data and skip the first 40 lines
      
      # I need to check whether the column was called Plastik? (Eva) or Plastik ja/nein (Martin)
      # otherwise it does not work properly in all cases
      # if(length(Hilfsobjekt$Plastik.ja.nein) > 0){
      #   Hilfsobjekt <- droplevels(subset(Hilfsobjekt, Hilfsobjekt$Plastik.ja.nein == ReqPolKey))
      #   
      # }
      # else{
      #   Hilfsobjekt <- droplevels(subset(Hilfsobjekt, Hilfsobjekt$Plastik. == ReqPolKey))
      # }
      # now with column numbers in the default form, but can also be provided als column name
      Hilfsobjekt <- droplevels(subset(Hilfsobjekt, Hilfsobjekt[colReqPol] == ReqPolKey))
      
      temp2 <- data.frame(sample = name_measurement[i],
                          measurement = Dateien[i],
                          className = Hilfsobjekt[, colPol], # polymer type
                          length = Hilfsobjekt[, colL], # length of the particle
                          form = Hilfsobjekt[, colShape], # fragment, pixel, fibre, sphere
                          color = Hilfsobjekt[, colCol],
                          lengthFibre = Hilfsobjekt[, colLFib]) # in case the fibre is curved, the length can be found here
      temp <- rbind(temp, temp2)
    }
  ) # end supressWarnings
  
  # The length is not always correct for fibres.
  # Thus create a new column with the correct length for all particles (that I don't need to select the correct column further below)
  temp$actualLength <- NA
  for(i in 1:nrow(temp)){
    # If lengthFibre is NA, but form is still Faser, the length is the value to be taken.
    if(temp$form[i] == fibre && !is.na(temp$lengthFibre[i]) == TRUE){
      temp$actualLength[i] <- temp$lengthFibre[i]
    }
    else{
      temp$actualLength[i] <- temp$length[i]
    }
    suppressWarnings( # I supress the warnings to have a simpler message that can be interpreted fast.
      # To add a quality control, I check whether the field of form or length is na. If yes, a warning will be
      # thrown, including the sample and measurement.
      if(is.na(temp$form[i]) == TRUE || temp$form[i] == ""){
        cat(warning(paste("Warning: There is a value missing in column ", colnames(Hilfsobjekt[colShape]), " in ", temp$measurement[i], "\n")))
      }
    ) # end supressWarnings
    suppressWarnings(
      if(is.na(temp$length[i]) == TRUE || temp$length[i] == ""){
        cat(warning(paste("Warning: There is a value missing in column ", colnames(Hilfsobjekt[colL]), " in ", temp$measurement[i], "\n")))
      }
    ) # end supressWarnings
  } # end for(i)
  
  #### start processing ####
  obj <- list() # object to be returned holding the rawdata, as well as the corrected and summarized data
  
  #if(testveval == TRUE){ # testing another way to evaluate everything. 
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
    # create a data frame to hold all data, to return it if dataReturn set TRUE
    dataBlankCorr <- data.frame(
      sample = NA,
      measurement = NA,
      polymer = NA,
      form = NA
    )
    # add columns for sizeclasses
    for(i in 1:(length(sizeclasses)+1)){
      if(i == 1){ # for the first number it's 0 to <=x
        dataBlankCorr <- cbind(dataBlankCorr, data.frame(x = NA))
        colnames(dataBlankCorr) <- c(colnames(dataBlankCorr)[-length(colnames(dataBlankCorr))], paste("from", "0", "to", sizeclasses[i], sep=""))
      }
      else if(i == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
        dataBlankCorr <- cbind(dataBlankCorr, data.frame(x = NA))
        colnames(dataBlankCorr) <- c(colnames(dataBlankCorr)[-length(colnames(dataBlankCorr))], paste("above", sizeclasses[i-1], sep=""))
      }
      else{
        dataBlankCorr <- cbind(dataBlankCorr, data.frame(x = NA))
        colnames(dataBlankCorr) <- c(colnames(dataBlankCorr)[-length(colnames(dataBlankCorr))], paste("from", sizeclasses[i-1], "to", sizeclasses[i], sep=""))
      }
    }
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
                    which(#vdatak$sample == levels(factor(vdatak$sample))[i] & 
                            #vdatak$measurement == levels(factor(vdatak$measurement))[m] &
                            #vdatak$className == levels(factor(vdatak$className))[j] &
                            #vdatak$form == levels(factor(vdatak$form))[k] &
                            vdatak$sizeClass == paste("from", "0", "to", sizeclasses[l], sep="")
                          ),
                    ]
                  )
              }
              else if(l == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
                dataBlankCorrTemprow <- cbind(dataBlankCorrTemprow, data.frame(x = NA))
                colnames(dataBlankCorrTemprow) <- c(colnames(dataBlankCorrTemprow)[-length(colnames(dataBlankCorrTemprow))], paste("above", sizeclasses[l-1], sep=""))
              
                dataBlankCorrTemprow[,paste("above", sizeclasses[l-1], sep="")] <- nrow(
                  vdatak[
                    which(#vdatak$sample == levels(factor(vdatak$sample))[i] & 
                            #vdatak$measurement == levels(factor(vdatak$measurement))[m] &
                            #vdatak$className == levels(factor(vdatak$className))[j] &
                            #vdatak$form == levels(factor(vdatak$form))[k] &
                            vdatak$sizeClass == paste("above", sizeclasses[l-1], sep="")
                    ),
                  ]
                )
                
              }
              else{
                dataBlankCorrTemprow <- cbind(dataBlankCorrTemprow, data.frame(x = NA))
                colnames(dataBlankCorrTemprow) <- c(colnames(dataBlankCorrTemprow)[-length(colnames(dataBlankCorrTemprow))], paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep=""))
              
                dataBlankCorrTemprow[,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")] <- nrow(
                  vdatak[
                    which(#vdatak$sample == levels(factor(vdatak$sample))[i] & 
                            #vdatak$measurement == levels(factor(vdatak$measurement))[m] &
                            #vdatak$className == levels(factor(vdatak$className))[j] &
                            #vdatak$form == levels(factor(vdatak$form))[k] &
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
    # correct the number of particles sample-measurement-polymer-form-wise, regardless of the size class
    # this was desired to be done separately, even if there might be discrepancies between the numbers of size classes and forms
    # Therefore we calculate the sum of each row before the correction happens
    dataBlankCorr$particleSum <- NA
    for(i in 1:nrow(dataBlankCorr)){
      dataBlankCorr$particleSum[i] <- rowSums(dataBlankCorr[i, 5:(length(sizeclasses)+5)], na.rm = TRUE)
    }
    # this row can now be corrected together with the next step
    
    # now we need the blanks, calculate a mean over all blanks for each sample and 
    # substract this mean number from each sample after adding up all measurements of each sample
    dataBlanks <- dataBlankCorr[grepl(blankKey, dataBlankCorr$sample, fixed = TRUE) == TRUE,]
    dataMeasurements <- dataBlankCorr[grepl(blankKey, dataBlankCorr$sample, fixed = TRUE) != TRUE,]
    
    # the summing of the sample values should be done before the blank correction
    # thus, we will sum up all values of the samples form-size wise perform the correction and then summarize the sample without the form-size wise columns
    
    #### sort data form-size wise ####
    # aggregate the data set to get the sums of particle numbers for each size class (form-polymerwise)
    data.agg.formwise <- aggregate(dataMeasurements[5:(length(sizeclasses)+6)], by=list(factor(dataMeasurements$sample), factor(dataMeasurements$polymer), factor(dataMeasurements$form)), sum, na.rm = TRUE)
    colnames(data.agg.formwise) <- c("sample", "polymer", "form", colnames(data.agg.formwise)[4:length(colnames(data.agg.formwise))])
    
    # add uncorrected data and blanks to obj
    obj$sampleDataUncorrected <- data.agg.formwise
    obj$blanks <- dataBlanks
    
    #### check for blank existence ####
    # check whether each sample has a blank to use for correction
    # otherwise throw a warning message
    sampleBlankChecklist <- c(rep(FALSE, length(levels(factor(data.agg.formwise$sample)))))
    for(i in 1:length(levels(factor(unlist(strsplit(dataBlanks$sample, blankKey)))))){
      for(j in 1:length(levels(factor(data.agg.formwise$sample)))){
        if(grepl(levels(factor(unlist(strsplit(dataBlanks$sample, blankKey))))[i], levels(factor(data.agg.formwise$sample))[j], fixed = TRUE) == TRUE){
          sampleBlankChecklist[j] <- TRUE
        }
      }
    }
    
    for(i in 1:length(levels(factor(data.agg.formwise$sample)))){
      if(sampleBlankChecklist[i] == FALSE){
        cat(paste("Warning: There is no blank for ", levels(factor(data.agg.formwise$sample))[i], sep = ""))
      }
    }
    
    #### apply division factors ####
    # sometimes the filters have to be divided because there are too many particles for one measurement. Then 
    # only a part of the original sample is analysed, which has to be corrected during the blank correction
    # It further might be, that the blank has a different division factor than the sample, since it might be good
    # to use the full blank particle numbers and/or if a filter breaks appart, the factor might change for a single
    # sample Therefore each sample (and blank) need its own factor.
    
    if(interactive() && setDivFactor == TRUE){
      # add a new column with the division factor (default = 1, meaning no division)
      data.agg.formwise$divFactor <- 1
      dataBlanks$divFactor <- 1
      
      # get the sample/blank names
      sampleFilters <- levels(factor(data.agg.formwise$sample))
      blankFilters <- levels(factor(dataBlanks$sample))
      
      cat(
          "\nYou chose to set division factors for your samples. \nPlease insert the factors in the same order like the samples are stated below, separated by a , [comma] (e.g., 0.25,0.5,1):\n")
      cat(sampleFilters)
      sampleDivFactors <- readline("Set the factors for the samples (e.g., 0.25,0.5,1,1,...): ") 
      sampleDivFactors <- as.numeric(unlist(strsplit(sampleDivFactors, ",")))
      while(length(sampleFilters) != length(sampleDivFactors)){
        sampleDivFactors <- readline("The number of factors is incorrect. Please try again (e.g., 0.25,0.5,1,1): ")
        sampleDivFactors <- as.numeric(unlist(strsplit(sampleDivFactors, ",")))
      }
      
      cat("And now do the same for the blanks (if similar to the samples, just insert the same numbers):\n")
      cat(blankFilters)
      blankDivFactors <- readline("Set the factors for the blanks (e.g., 0.25,0.5,1,1,...): ")
      blankDivFactors <- as.numeric(unlist(strsplit(blankDivFactors, ",")))
      while(length(blankFilters) != length(blankDivFactors)){
        blankDivFactors <- readline("The number of factors is incorrect. Please try again (e.g., 0.25,0.5,1,1): ")
        blankDivFactors <- as.numeric(unlist(strsplit(blankDivFactors, ",")))
      }
      
      # set the division factor for each line in the data set
      # for samples
      for(i in 1:length(sampleDivFactors)){
        for(j in 1:nrow(data.agg.formwise)){
          if(data.agg.formwise$sample[j] == sampleFilters[i]){
            data.agg.formwise$divFactor[j] <- sampleDivFactors[i]
          }
        }
      }
      # for blanks
      for(i in 1:length(blankDivFactors)){
        for(j in 1:nrow(dataBlanks)){
          if(dataBlanks$sample[j] == blankFilters[i]){
            dataBlanks$divFactor[j] <- blankDivFactors[i]
          }
        }
      }
      
      # divide all numbers by the division factor
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
      
      # recalculate the row sum of particles
      for(i in 1:nrow(data.agg.formwise)){
        data.agg.formwise$particleSum[i] <- rowSums(data.agg.formwise[i, 4:(length(sizeclasses)+4)], na.rm = TRUE)
      }
      for(i in 1:nrow(dataBlanks)){
        dataBlanks$particleSum[i] <- rowSums(dataBlanks[i, 5:(length(sizeclasses)+5)], na.rm = TRUE)
      }
      
      # delete the division factor column because they are not needed any more
      data.agg.formwise$divFactor <- NULL
      dataBlanks$divFactor <- NULL
      
      #cat("Division correction done.\n")
    }
    else{}
  
    
    #cat("Data sorted \n")
    
    #### perform blank correction #### 
    # iterate over the samples (with existing Blanks - should be all, though)
    for(i in 1:length(levels(factor(unlist(strsplit(dataBlanks$sample, blankKey)))))){
      # iterate over all rows of the data to be corrected
      for(j in 1:nrow(data.agg.formwise)){
        # only perform a correction when the sample is the same
        if(grepl(levels(factor(unlist(strsplit(dataBlanks$sample, blankKey))))[i], data.agg.formwise$sample[j], fixed = TRUE) == TRUE){
          
          # calculate a correction factor. It is the mean of all Blanks for this sample of the respective polymer and form rounded to the next higher integer (to be conservative)
          # to calculate the mean we take the sum and divide manually by the number of blanks, since there are NAs (0 particles) in many cases and mean() would not count them in for the division
          corrFactorParticleSum <- ceiling(sum(dataBlanks[,"particleSum"][grepl(levels(factor(unlist(strsplit(dataBlanks$sample, blankKey))))[i], dataBlanks$sample, fixed = TRUE) & dataBlanks$polymer == data.agg.formwise$polymer[j] & dataBlanks$form == data.agg.formwise$form[j]], na.rm = TRUE)/length(levels(factor(dataBlanks$measurement[grepl(levels(factor(unlist(strsplit(dataBlanks$sample, blankKey))))[i], dataBlanks$sample, fixed = TRUE)]))))
          # if there is no value for this polymer in the blanks the value is NA (which means it has to be set to 0)
          if(is.na(corrFactorParticleSum)){
            corrFactorParticleSum <- 0
          }
          
          data.agg.formwise$particleSum[j] <- data.agg.formwise$particleSum[j] - corrFactorParticleSum
          # if a value falls below 0, set to 0
          if(data.agg.formwise$particleSum[j] < 0){
            data.agg.formwise$particleSum[j] <- 0
          }
          
          # iterate over the size classes to perform the blank processing for each size class
          for(k in 1:(length(sizeclasses)+1)){
            if(k == 1){ # for the first number it's 0 to <=x
              # calculate a correction factor. It is the mean of all Blanks for this sample of the respective polymer, form and size class, rounded to the next higher integer (to be conservative)
              # added a 0 to the vector for mean calculation, to have a 0 if NA (no data in Blanks available for this polymer)
              corrFactor <- ceiling(sum(dataBlanks[,paste("from", "0", "to", sizeclasses[k], sep="")][grepl(levels(factor(unlist(strsplit(dataBlanks$sample, blankKey))))[i], dataBlanks$sample, fixed = TRUE) & dataBlanks$polymer == data.agg.formwise$polymer[j] & dataBlanks$form == data.agg.formwise$form[j]], na.rm = TRUE)/length(levels(factor(dataBlanks$measurement[grepl(levels(factor(unlist(strsplit(dataBlanks$sample, blankKey))))[i], dataBlanks$sample, fixed = TRUE)]))))
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
              corrFactor <- ceiling(sum(dataBlanks[,paste("above", sizeclasses[k-1], sep="")][grepl(levels(factor(unlist(strsplit(dataBlanks$sample, blankKey))))[i], dataBlanks$sample, fixed = TRUE) & dataBlanks$polymer == data.agg.formwise$polymer[j] & dataBlanks$form == data.agg.formwise$form[j]], na.rm = TRUE)/length(levels(factor(dataBlanks$measurement[grepl(levels(factor(unlist(strsplit(dataBlanks$sample, blankKey))))[i], dataBlanks$sample, fixed = TRUE)]))))
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
              corrFactor <- ceiling(sum(dataBlanks[,paste("from", sizeclasses[k-1], "to", sizeclasses[k], sep="")][grepl(levels(factor(unlist(strsplit(dataBlanks$sample, blankKey))))[i], dataBlanks$sample, fixed = TRUE) & dataBlanks$polymer == data.agg.formwise$polymer[j] & dataBlanks$form == data.agg.formwise$form[j]], na.rm = TRUE)/length(levels(factor(dataBlanks$measurement[grepl(levels(factor(unlist(strsplit(dataBlanks$sample, blankKey))))[i], dataBlanks$sample, fixed = TRUE)]))))
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
    for(i in 1:(length(sizeclasses)+1)){
      if(i == 1){ # for the first number it's 0 to <=x
        dataSampleSummary <- cbind(dataSampleSummary, data.frame(x = NA))
        colnames(dataSampleSummary) <- c(colnames(dataSampleSummary)[-length(colnames(dataSampleSummary))], paste("from", "0", "to", sizeclasses[i], sep=""))
      }
      else if(i == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
        dataSampleSummary <- cbind(dataSampleSummary, data.frame(x = NA))
        colnames(dataSampleSummary) <- c(colnames(dataSampleSummary)[-length(colnames(dataSampleSummary))], paste("above", sizeclasses[i-1], sep=""))
      }
      else{
        dataSampleSummary <- cbind(dataSampleSummary, data.frame(x = NA))
        colnames(dataSampleSummary) <- c(colnames(dataSampleSummary)[-length(colnames(dataSampleSummary))], paste("from", sizeclasses[i-1], "to", sizeclasses[i], sep=""))
      }
    }
    # delete NA line
    dataSampleSummary <- dataSampleSummary[-1,]

    # aggregate the data set to get the sums of particle numbers for each size class
    data.agg.sizewise <- aggregate(dataMeasurementsCorrected[4:(length(sizeclasses)+5)], by=list(factor(dataMeasurementsCorrected$sample), factor(dataMeasurementsCorrected$polymer)), sum, na.rm = TRUE)
    data.agg.formwise <- aggregate(dataMeasurementsCorrected[4:(length(sizeclasses)+5)], by=list(factor(dataMeasurementsCorrected$sample), factor(dataMeasurementsCorrected$polymer), factor(dataMeasurementsCorrected$form)), sum, na.rm = TRUE)


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
      for(k in 1:(length(sizeclasses)+1)){
        if(k == 1){ # for the first number it's 0 to <=x
          rowX <- cbind(rowX, data.frame(x = NA))
          colnames(rowX) <- c(colnames(rowX)[-length(colnames(rowX))], paste("from", "0", "to", sizeclasses[k], sep=""))
        }
        else if(k == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
          rowX <- cbind(rowX, data.frame(x = NA))
          colnames(rowX) <- c(colnames(rowX)[-length(colnames(rowX))], paste("above", sizeclasses[k-1], sep=""))
        }
        else{
          rowX <- cbind(rowX, data.frame(x = NA))
          colnames(rowX) <- c(colnames(rowX)[-length(colnames(rowX))], paste("from", sizeclasses[k-1], "to", sizeclasses[k], sep=""))
        }
      }

      # now iterate over the polymers and sum the respective numbers (for each form and size class)
      for(j in 1:length(levels(factor(polymers)))){
        currentPolymer <- levels(factor(polymers))[j]

        rowX$polymer <- currentPolymer
        rowX$fibres <- sum(c(0, data.agg.formwise$particleSum[which(data.agg.formwise$Group.1 == currentSample & data.agg.formwise$Group.2 == currentPolymer & data.agg.formwise$Group.3 == fibre)]), na.rm = TRUE)
        rowX$fragments <- sum(c(0, data.agg.formwise$particleSum[which(data.agg.formwise$Group.1 == currentSample & data.agg.formwise$Group.2 == currentPolymer & data.agg.formwise$Group.3 == fragment)]), na.rm = TRUE)
        rowX$spheres <- sum(c(0, data.agg.formwise$particleSum[which(data.agg.formwise$Group.1 == currentSample & data.agg.formwise$Group.2 == currentPolymer & data.agg.formwise$Group.3 == sphere)]), na.rm = TRUE)
        rowX$pixels <- sum(c(0, data.agg.formwise$particleSum[which(data.agg.formwise$Group.1 == currentSample & data.agg.formwise$Group.2 == currentPolymer & data.agg.formwise$Group.3 == pixel)]), na.rm = TRUE)
        rowX$particlesTotal <- sum(c(0, rowX$fibres, rowX$fragments, rowX$spheres, rowX$pixels), na.rm = TRUE)

        # add columns for sizeclasses
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
    writexl::write_xlsx(list(samples_summary = obj$sampleSummary, 
                             single_measurements_corrected = obj$correctedData,
                             blanks = obj$blanks,
                             uncorected_data = obj$sampleDataUncorrected,
                             raw_data = obj$rawData),
                        paste(PATH, "processing data.xls", sep=""))
    
    # for the single samples, there is the option to add a bottom line with the sum of each column (requested feature)
    for(i in 1:length(levels(factor(dataSampleSummary$sample)))){
      
      dataSubX <- droplevels(subset(dataSampleSummary, dataSampleSummary$sample == levels(factor(dataSampleSummary$sample))[i]))
      
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
      
      writexl::write_xlsx(list(sample_summary = dataSubX),
                          paste(PATH, levels(factor(dataSampleSummary$sample))[i], "_evaluated.xls", sep=""))
      
    } # end for i  
      
    
    #### finish ####
    cat("Done.")
    
    # return the whole data set as data frame
    if(dataReturn == TRUE){
      return(obj)
    }
  
    #stop("Testveval finished.")
    
  #} # end if testveval
  
  # #### to be deleted soon ####
  # # create a data frame to hold all data, to return it if dataReturn set TRUE
  # returnObj <- data.frame(
  #   sample = NA,
  #   measurement = NA,
  #   polymer = NA,
  #   fibres = NA,
  #   fragments = NA,
  #   spheres = NA,
  #   pixels = NA,
  #   particlesTotal = NA
  # )
  # # add columns for sizeclasses
  # for(i in 1:(length(sizeclasses)+1)){
  #   if(i == 1){ # for the first number it's 0 to <=x
  #     returnObj <- cbind(returnObj, data.frame(x = NA))
  #     colnames(returnObj) <- c(colnames(returnObj)[-length(colnames(returnObj))], paste("from", "0", "to", sizeclasses[i], sep=""))
  #   }
  #   else if(i == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
  #     returnObj <- cbind(returnObj, data.frame(x = NA))
  #     colnames(returnObj) <- c(colnames(returnObj)[-length(colnames(returnObj))], paste("above", sizeclasses[i-1], sep=""))
  #   }
  #   else{
  #     returnObj <- cbind(returnObj, data.frame(x = NA))
  #     colnames(returnObj) <- c(colnames(returnObj)[-length(colnames(returnObj))], paste("from", sizeclasses[i-1], "to", sizeclasses[i], sep=""))
  #   }
  # }
  # # delete NA line
  # returnObj <- returnObj[-1,]
  # 
  # # iterate over all samples and process each sample separately
  # for(i in 1:length(levels(factor(temp$sample)))){
  #   tempSample <- droplevels(subset(temp, temp$sample == levels(factor(temp$sample))[i]))
  #   
  #   # create a list to hold all measurements for one sample and the summary of all measurements of one sample
  #   tempobj <- list()
  #   
  #   # process each measurement separately and store results into the list
  #   # the values of each measurement should be retained
  #   for(j in 1:length(levels(factor(tempSample$measurement)))){
  #     tempMeasurement <- droplevels(subset(tempSample, tempSample$measurement == levels(factor(tempSample$measurement))[j]))
  #     
  #     # create data frame to store all values for each polymer
  #     measurementX <- data.frame(
  #       sample = rep(NA, length(levels(factor(tempMeasurement$className)))),
  #       measurement = rep(NA, length(levels(factor(tempMeasurement$className)))),
  #       polymer = rep(NA, length(levels(factor(tempMeasurement$className)))),
  #       fibres = rep(NA, length(levels(factor(tempMeasurement$className)))),
  #       fragments = rep(NA, length(levels(factor(tempMeasurement$className)))),
  #       spheres = rep(NA, length(levels(factor(tempMeasurement$className)))),
  #       pixels = rep(NA, length(levels(factor(tempMeasurement$className)))),
  #       particlesTotal = rep(NA, length(levels(factor(tempMeasurement$className))))
  #     )
  #     # add columns for sizeclasses
  #     for(l in 1:(length(sizeclasses)+1)){
  #       if(l == 1){ # for the first number it's 0 to <=x
  #         measurementX <- cbind(measurementX, data.frame(x = rep(NA, length(levels(factor(tempMeasurement$className))))))
  #         colnames(measurementX) <- c(colnames(measurementX)[-length(colnames(measurementX))], paste("from", "0", "to", sizeclasses[l], sep=""))
  #       }
  #       else if(l == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
  #         measurementX <- cbind(measurementX, data.frame(x = rep(NA, length(levels(factor(tempMeasurement$className))))))
  #         colnames(measurementX) <- c(colnames(measurementX)[-length(colnames(measurementX))], paste("above", sizeclasses[l-1], sep=""))
  #       }
  #       else{
  #         measurementX <- cbind(measurementX, data.frame(x = rep(NA, length(levels(factor(tempMeasurement$className))))))
  #         colnames(measurementX) <- c(colnames(measurementX)[-length(colnames(measurementX))], paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep=""))
  #       }
  #     }
  #     
  #     
  #     # iterate over all polymers and add a line for each 
  #     for(k in 1:length(levels(factor(tempMeasurement$className)))){
  #       
  #       # fill in measurementX for each polymer
  #       measurementX$sample[k] <- levels(factor(temp$sample))[i]
  #       measurementX$measurement[k] <- levels(factor(tempSample$measurement))[j]
  #       measurementX$polymer[k] <- levels(factor(tempMeasurement$className))[k]
  #       measurementX$fibres[k] <- nrow(tempMeasurement[which(tempMeasurement$form == fibre & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
  #       measurementX$fragments[k] <- nrow(tempMeasurement[which(tempMeasurement$form == fragment & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
  #       measurementX$spheres[k] <- nrow(tempMeasurement[which(tempMeasurement$form == sphere & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
  #       measurementX$pixels[k] <- nrow(tempMeasurement[which(tempMeasurement$form == pixel & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
  #       measurementX$particlesTotal[k] <- measurementX$fibres[k] + measurementX$fragments[k] + measurementX$pixels[k] + measurementX$spheres[k]
  #       # fill fields of sizeclasses
  #       for(l in 1:(length(sizeclasses)+1)){
  #         if(l == 1){ # for the first number it's 0 to <=x
  #           measurementX[,paste("from", "0", "to", sizeclasses[l], sep="")][k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength <= sizeclasses[l] & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
  #         }
  #         else if(l == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
  #           measurementX[,paste("above", sizeclasses[l-1], sep="")][k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > sizeclasses[l-1] & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
  #         }
  #         else{
  #           measurementX[,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")][k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > sizeclasses[l-1] & tempMeasurement$actualLength <= sizeclasses[l] & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
  #         }
  #       }  
  #       
  #     }# end for(k)
  #     
  #     # store measurementX into tempobj
  #     # here I gather all measurements and the sum of all together to write it in an excelfile
  #     # this is done for each sample (each sample gets its own excelfile now)
  #     tempobj[[j]] <- measurementX  
  #     
  #   }# end for(j)
  #   
  #   # now I have all measurements in tempobj
  #   # now I can sum all measurements together and write all data into an excelfile
  #   # each measurement and the summary gets their own sheet 
  #   measurementSum <- data.frame(
  #     sample = rep(NA, length(polymers)),
  #     measurement = rep(NA, length(polymers)),
  #     polymer = rep(NA, length(polymers)),
  #     fibres = rep(0, length(polymers)),
  #     fragments = rep(0, length(polymers)),
  #     spheres = rep(0, length(polymers)),
  #     pixels = rep(0, length(polymers)),
  #     particlesTotal = rep(0, length(polymers))
  #   )
  #   # add columns for sizeclasses
  #   for(l in 1:(length(sizeclasses)+1)){
  #     if(l == 1){ # for the first number it's 0 to <=x
  #       measurementSum <- cbind(measurementSum, data.frame(x = rep(0, length(polymers))))
  #       colnames(measurementSum) <- c(colnames(measurementSum)[-length(colnames(measurementSum))], paste("from", "0", "to", sizeclasses[l], sep=""))
  #     }
  #     else if(l == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
  #       measurementSum <- cbind(measurementSum, data.frame(x = rep(0, length(polymers))))
  #       colnames(measurementSum) <- c(colnames(measurementSum)[-length(colnames(measurementSum))], paste("above", sizeclasses[l-1], sep=""))
  #     }
  #     else{
  #       measurementSum <- cbind(measurementSum, data.frame(x = rep(0, length(polymers))))
  #       colnames(measurementSum) <- c(colnames(measurementSum)[-length(colnames(measurementSum))], paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep=""))
  #     }
  #   }
  #   
  #   # iterate over the possible polymers (that way it is in desired order and the table is similar across samples)
  #   for(j in 1:length(polymers)){
  #     
  #     for(k in 1:length(tempobj)){
  #       
  #       measurementSum$sample[j] <- levels(factor(temp$sample))[i]
  #       measurementSum$measurement[j] <- "sample sum"
  #       measurementSum$polymer[j] <- polymers[j]
  #       measurementSum$fibres[j] <- measurementSum$fibres[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$fibres) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$fibres}else{0}
  #       measurementSum$fragments[j] <- measurementSum$fragments[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$fragments) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$fragments}else{0}
  #       measurementSum$spheres[j] <- measurementSum$spheres[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$spheres) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$spheres}else{0}
  #       measurementSum$pixels[j] <- measurementSum$pixels[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$pixels) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$pixels}else{0}
  #       measurementSum$particlesTotal[j] <- measurementSum$fibres[j] + measurementSum$fragments[j] + measurementSum$pixels[j]
  #       # fill fields of sizeclasses
  #       for(l in 1:(length(sizeclasses)+1)){
  #         if(l == 1){ # for the first number it's 0 to <=x
  #           measurementSum[,paste("from", "0", "to", sizeclasses[l], sep="")][j] <- measurementSum[,paste("from", "0", "to", sizeclasses[l], sep="")][j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),][,paste("from", "0", "to", sizeclasses[l], sep="")]) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),][,paste("from", "0", "to", sizeclasses[l], sep="")]}else{0}
  #         }
  #         else if(l == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
  #           measurementSum[,paste("above", sizeclasses[l-1], sep="")][j] <- measurementSum[,paste("above", sizeclasses[l-1], sep="")][j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),][,paste("above", sizeclasses[l-1], sep="")]) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),][,paste("above", sizeclasses[l-1], sep="")]}else{0}
  #         }
  #         else{
  #           measurementSum[,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")][j] <- measurementSum[,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")][j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),][,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")]) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),][,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")]}else{0}
  #         }
  #       }  
  #       
  #     }# end for k
  #   } # end for j
  #   
  #   # add a bottom line with the sum of each column (requested feature)
  #   if(eocsum == TRUE){
  #     
  #     tempsumrow <- data.frame(
  #                               sample = "column sum",
  #                               measurement = "column sum",
  #                               polymer = "column sum",
  #                               fibres = sum(measurementSum$fibres),
  #                               fragments = sum(measurementSum$fragments),
  #                               spheres = sum(measurementSum$spheres),
  #                               pixels = sum(measurementSum$pixels),
  #                               particlesTotal = sum(measurementSum$particlesTotal)
  #                             )
  #     # add columns for sizeclasses
  #     for(l in 1:(length(sizeclasses)+1)){
  #       if(l == 1){ # for the first number it's 0 to <=x
  #         tempsumrow <- cbind(tempsumrow, data.frame(x = sum(measurementSum[,paste("from", "0", "to", sizeclasses[l], sep="")])))
  #         colnames(tempsumrow) <- c(colnames(tempsumrow)[-length(colnames(tempsumrow))], paste("from", "0", "to", sizeclasses[l], sep=""))
  #       }
  #       else if(l == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
  #         tempsumrow <- cbind(tempsumrow, data.frame(x = sum(measurementSum[,paste("above", sizeclasses[l-1], sep="")])))
  #         colnames(tempsumrow) <- c(colnames(tempsumrow)[-length(colnames(tempsumrow))], paste("above", sizeclasses[l-1], sep=""))
  #       }
  #       else{
  #         tempsumrow <- cbind(tempsumrow, data.frame(x = sum(measurementSum[,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")])))
  #         colnames(tempsumrow) <- c(colnames(tempsumrow)[-length(colnames(tempsumrow))], paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep=""))
  #       }
  #     }
  #     
  #     measurementSum <- rbind(measurementSum, tempsumrow)
  #   }
  #   
  #   # write results into excel file
  #   allMeasurements <- data.frame()
  #   for(j in 1:length(tempobj)){
  #     allMeasurements <- rbind(allMeasurements,tempobj[[j]])
  #   }
  #   
  #   returnObj <- rbind(returnObj, allMeasurements)
  #   writexl::write_xlsx(list(sample_summary = measurementSum, 
  #                            single_measurements = allMeasurements),
  #                       paste(PATH, levels(factor(temp$sample))[i], "_evaluated.xls", sep=""))
  #   
  # }# end for(i)
  # 
  # cat("Done.")
  # 
  # # return the whole data set as data frame
  # if(dataReturn == TRUE){
  #   return(returnObj)
  # }
  
}# end function

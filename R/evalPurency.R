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
#' @param dataReturn If set TRUE, a data frame will be returned containing the data of all measurement with 
#' the necessary information.
#' @param eocsum If TRUE (default) it adds a column sum at the end of each column of the summary panel.
#' @param labpreset A preset for most of the parameters (except: path, polymers, dataReturn, eocsum). 
#' Can be requested by other labs, to be implemented, that they don't have to be set manually all the time. 
#' @param colPol Column number where the polymer type is stated. In the TOEKI lab this is column 6 (Class Name). 
#' Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
#' @param colL Column number for the particle length. In the TOEKI lab this is column 17 (Length 5µ). 
#' Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
#' @param colReqPol Column number for the particle check, whether the particle is a polymer or not. 
#' In the TOEKI lab this is column 24 (Plastik? or Plastik ja/nein). Could also be provided as column name, but only 
#' in ASCII encoding (e.g., special character as . and ä = d).
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
#'
evalPurency <- function(path, 
                        polymers = c("PU", "EVAc", "PA", "PAN", "PBT", "PET", "PE", "PMMA", "PP", 
                                     "POM", "PS", "PVC", "PC", "ABS", "PPSU", "CA", "PEEK", "EVOH", 
                                     "PSU", "SI", "PLA", "PLAPBAT"),
                        sizeclasses = c(10, 20, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
                        dataReturn = FALSE,
                        eocsum = TRUE,
                        labpreset = FALSE,
                        colPol = 6, 
                        colL = 17,
                        colReqPol = 24, 
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
  
  # if a preset is set, load the respective preset (if available)
  # this is how a preset is created:
  # presets <- data.frame(
  #   labname = "Laforsch",
  #   colPol = 6,
  #   colL = 17,
  #   colReqPol = 24,
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
      #   Hilfsobjekt <- droplevels(subset(Hilfsobjekt, Hilfsobjekt$Plastik.ja.nein == "ja"))
      #   
      # }
      # else{
      #   Hilfsobjekt <- droplevels(subset(Hilfsobjekt, Hilfsobjekt$Plastik. == "ja"))
      # }
      # now with column numbers in the default form, but can also be provided als column name
      Hilfsobjekt <- droplevels(subset(Hilfsobjekt, Hilfsobjekt[colReqPol] == "ja"))
      
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
  
  # create a data frame to hold all data, to return it if dataReturn set TRUE
  returnObj <- data.frame(
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
      returnObj <- cbind(returnObj, data.frame(x = NA))
      colnames(returnObj) <- c(colnames(returnObj)[-length(colnames(returnObj))], paste("from", "0", "to", sizeclasses[i], sep=""))
    }
    else if(i == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
      returnObj <- cbind(returnObj, data.frame(x = NA))
      colnames(returnObj) <- c(colnames(returnObj)[-length(colnames(returnObj))], paste("above", sizeclasses[i-1], sep=""))
    }
    else{
      returnObj <- cbind(returnObj, data.frame(x = NA))
      colnames(returnObj) <- c(colnames(returnObj)[-length(colnames(returnObj))], paste("from", sizeclasses[i-1], "to", sizeclasses[i], sep=""))
    }
  }
  # delete NA line
  returnObj <- returnObj[-1,]
  
  # iterate over all samples and process each sample separately
  for(i in 1:length(levels(factor(temp$sample)))){
    tempSample <- droplevels(subset(temp, temp$sample == levels(factor(temp$sample))[i]))
    
    # create a list to hold all measurements for one sample and the summary of all measurements of one sample
    tempobj <- list()
    
    # process each measurement separately and store results into the list
    # the values of each measurement should be retained
    for(j in 1:length(levels(factor(tempSample$measurement)))){
      tempMeasurement <- droplevels(subset(tempSample, tempSample$measurement == levels(factor(tempSample$measurement))[j]))
      
      # create data frame to store all values for each polymer
      measurementX <- data.frame(
        sample = rep(NA, length(levels(factor(tempMeasurement$className)))),
        measurement = rep(NA, length(levels(factor(tempMeasurement$className)))),
        polymer = rep(NA, length(levels(factor(tempMeasurement$className)))),
        fibres = rep(NA, length(levels(factor(tempMeasurement$className)))),
        fragments = rep(NA, length(levels(factor(tempMeasurement$className)))),
        spheres = rep(NA, length(levels(factor(tempMeasurement$className)))),
        pixels = rep(NA, length(levels(factor(tempMeasurement$className)))),
        particlesTotal = rep(NA, length(levels(factor(tempMeasurement$className))))
      )
      # add columns for sizeclasses
      for(l in 1:(length(sizeclasses)+1)){
        if(l == 1){ # for the first number it's 0 to <=x
          measurementX <- cbind(measurementX, data.frame(x = rep(NA, length(levels(factor(tempMeasurement$className))))))
          colnames(measurementX) <- c(colnames(measurementX)[-length(colnames(measurementX))], paste("from", "0", "to", sizeclasses[l], sep=""))
        }
        else if(l == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
          measurementX <- cbind(measurementX, data.frame(x = rep(NA, length(levels(factor(tempMeasurement$className))))))
          colnames(measurementX) <- c(colnames(measurementX)[-length(colnames(measurementX))], paste("above", sizeclasses[l-1], sep=""))
        }
        else{
          measurementX <- cbind(measurementX, data.frame(x = rep(NA, length(levels(factor(tempMeasurement$className))))))
          colnames(measurementX) <- c(colnames(measurementX)[-length(colnames(measurementX))], paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep=""))
        }
      }
      
      
      # iterate over all polymers and add a line for each 
      for(k in 1:length(levels(factor(tempMeasurement$className)))){
        
        # fill in measurementX for each polymer
        measurementX$sample[k] <- levels(factor(temp$sample))[i]
        measurementX$measurement[k] <- levels(factor(tempSample$measurement))[j]
        measurementX$polymer[k] <- levels(factor(tempMeasurement$className))[k]
        measurementX$fibres[k] <- nrow(tempMeasurement[which(tempMeasurement$form == fibre & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$fragments[k] <- nrow(tempMeasurement[which(tempMeasurement$form == fragment & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$spheres[k] <- nrow(tempMeasurement[which(tempMeasurement$form == sphere & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$pixels[k] <- nrow(tempMeasurement[which(tempMeasurement$form == pixel & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$particlesTotal[k] <- measurementX$fibres[k] + measurementX$fragments[k] + measurementX$pixels[k] + measurementX$spheres[k]
        # fill fields of sizeclasses
        for(l in 1:(length(sizeclasses)+1)){
          if(l == 1){ # for the first number it's 0 to <=x
            measurementX[,paste("from", "0", "to", sizeclasses[l], sep="")][k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength <= sizeclasses[l] & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
          }
          else if(l == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
            measurementX[,paste("above", sizeclasses[l-1], sep="")][k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > sizeclasses[l-1] & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
          }
          else{
            measurementX[,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")][k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > sizeclasses[l-1] & tempMeasurement$actualLength <= sizeclasses[l] & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
          }
        }  
        
      }# end for(k)
      
      # store measurementX into tempobj
      # here I gather all measurements and the sum of all together to write it in an excelfile
      # this is done for each sample (each sample gets its own excelfile now)
      tempobj[[j]] <- measurementX  
      
    }# end for(j)
    
    # now I have all measurements in tempobj
    # now I can sum all measurements together and write all data into an excelfile
    # each measurement and the summary gets their own sheet 
    measurementSum <- data.frame(
      sample = rep(NA, length(polymers)),
      measurement = rep(NA, length(polymers)),
      polymer = rep(NA, length(polymers)),
      fibres = rep(0, length(polymers)),
      fragments = rep(0, length(polymers)),
      spheres = rep(0, length(polymers)),
      pixels = rep(0, length(polymers)),
      particlesTotal = rep(0, length(polymers))
    )
    # add columns for sizeclasses
    for(l in 1:(length(sizeclasses)+1)){
      if(l == 1){ # for the first number it's 0 to <=x
        measurementSum <- cbind(measurementSum, data.frame(x = rep(0, length(polymers))))
        colnames(measurementSum) <- c(colnames(measurementSum)[-length(colnames(measurementSum))], paste("from", "0", "to", sizeclasses[l], sep=""))
      }
      else if(l == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
        measurementSum <- cbind(measurementSum, data.frame(x = rep(0, length(polymers))))
        colnames(measurementSum) <- c(colnames(measurementSum)[-length(colnames(measurementSum))], paste("above", sizeclasses[l-1], sep=""))
      }
      else{
        measurementSum <- cbind(measurementSum, data.frame(x = rep(0, length(polymers))))
        colnames(measurementSum) <- c(colnames(measurementSum)[-length(colnames(measurementSum))], paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep=""))
      }
    }
    
    # iterate over the possible polymers (that way it is in desired order and the table is similar across samples)
    for(j in 1:length(polymers)){
      
      for(k in 1:length(tempobj)){
        
        measurementSum$sample[j] <- levels(factor(temp$sample))[i]
        measurementSum$measurement[j] <- "sample sum"
        measurementSum$polymer[j] <- polymers[j]
        measurementSum$fibres[j] <- measurementSum$fibres[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$fibres) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$fibres}else{0}
        measurementSum$fragments[j] <- measurementSum$fragments[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$fragments) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$fragments}else{0}
        measurementSum$spheres[j] <- measurementSum$spheres[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$spheres) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$spheres}else{0}
        measurementSum$pixels[j] <- measurementSum$pixels[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$pixels) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$pixels}else{0}
        measurementSum$particlesTotal[j] <- measurementSum$fibres[j] + measurementSum$fragments[j] + measurementSum$pixels[j]
        # fill fields of sizeclasses
        for(l in 1:(length(sizeclasses)+1)){
          if(l == 1){ # for the first number it's 0 to <=x
            measurementSum[,paste("from", "0", "to", sizeclasses[l], sep="")][j] <- measurementSum[,paste("from", "0", "to", sizeclasses[l], sep="")][j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),][,paste("from", "0", "to", sizeclasses[l], sep="")]) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),][,paste("from", "0", "to", sizeclasses[l], sep="")]}else{0}
          }
          else if(l == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
            measurementSum[,paste("above", sizeclasses[l-1], sep="")][j] <- measurementSum[,paste("above", sizeclasses[l-1], sep="")][j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),][,paste("above", sizeclasses[l-1], sep="")]) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),][,paste("above", sizeclasses[l-1], sep="")]}else{0}
          }
          else{
            measurementSum[,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")][j] <- measurementSum[,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")][j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),][,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")]) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),][,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")]}else{0}
          }
        }  
        
      }# end for k
    } # end for j
    
    # add a bottom line with the sum of each column (requested feature)
    if(eocsum == TRUE){
      
      tempsumrow <- data.frame(
                                sample = "column sum",
                                measurement = "column sum",
                                polymer = "column sum",
                                fibres = sum(measurementSum$fibres),
                                fragments = sum(measurementSum$fragments),
                                spheres = sum(measurementSum$spheres),
                                pixels = sum(measurementSum$pixels),
                                particlesTotal = sum(measurementSum$particlesTotal)
                              )
      # add columns for sizeclasses
      for(l in 1:(length(sizeclasses)+1)){
        if(l == 1){ # for the first number it's 0 to <=x
          tempsumrow <- cbind(tempsumrow, data.frame(x = sum(measurementSum[,paste("from", "0", "to", sizeclasses[l], sep="")])))
          colnames(tempsumrow) <- c(colnames(tempsumrow)[-length(colnames(tempsumrow))], paste("from", "0", "to", sizeclasses[l], sep=""))
        }
        else if(l == (length(sizeclasses)+1)){ # for the last number it's >x to infinite
          tempsumrow <- cbind(tempsumrow, data.frame(x = sum(measurementSum[,paste("above", sizeclasses[l-1], sep="")])))
          colnames(tempsumrow) <- c(colnames(tempsumrow)[-length(colnames(tempsumrow))], paste("above", sizeclasses[l-1], sep=""))
        }
        else{
          tempsumrow <- cbind(tempsumrow, data.frame(x = sum(measurementSum[,paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep="")])))
          colnames(tempsumrow) <- c(colnames(tempsumrow)[-length(colnames(tempsumrow))], paste("from", sizeclasses[l-1], "to", sizeclasses[l], sep=""))
        }
      }
      
      measurementSum <- rbind(measurementSum, tempsumrow)
    }
    
    # write results into excel file
    allMeasurements <- data.frame()
    for(j in 1:length(tempobj)){
      allMeasurements <- rbind(allMeasurements,tempobj[[j]])
    }
    
    returnObj <- rbind(returnObj, allMeasurements)
    writexl::write_xlsx(list(sample_summary = measurementSum, single_measurements = allMeasurements),
                        paste(PATH, levels(factor(temp$sample))[i], "_evaluated.xls", sep=""))
    
  }# end for(i)
  
  cat("Done.")
  
  # return the whole data set as data frame
  if(dataReturn == TRUE){
    return(returnObj)
  }
  
}# end function

######## evalPurrency() ###########
#' created by Marvin Kiene, use this function at own risk!
#' @description
#' Evaluate csv files, produced by purrency. It will count occurences of of fibres, fragments, spheres and pixels,
#' as well as size fractions (<10, 10-20, 20-50, 50-100, 100-150, 150-200,..., >500) for each polymer. Each file (i.e., each measurement)
#' is evaluated separately, as well as summarized for all files (i.e., one sample). 
#' 
#' @param path The path where the files can be found. All csv files in this folder will be evaluated. Also saves the
#' resulting files in this directory.
#' @param polymers A vector containing the abbreviations of polymers to be considered. Default vector contains 22 
#' polymers.
#' @param dataReturn If set TRUE, a data frame will be returned containing the data of all measurement with 
#' the necessary information.
#' @param colPol Column number where the polymer type is stated. In the TOEKI lab this is column 6 (Class Name). 
#' Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
#' @param colL Column number for the particle length. In the TOEKI lab this is column 17 (Length [5µ]). 
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
#'
evalPurency <- function(path, 
                        polymers = c("PU", "EVAc", "PA", "PAN", "PBT", "PET", "PE", "PMMA", "PP", 
                                     "POM", "PS", "PVC", "PC", "ABS", "PPSU", "CA", "PEEK", "EVOH", 
                                     "PSU", "SI", "PLA", "PLAPBAT"),
                        dataReturn = FALSE,
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
  
  # a set of polymers that may occur
  polymers <- polymers
  
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
  
  # The length is not correct for fibres.
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
  suppressWarnings(
    returnObj <- data.frame(
      sample = NA,
      measurement = NA,
      polymer = NA,
      fibres = NA,
      fragments = NA,
      spheres = NA,
      pixels = NA,
      particlesTotal = NA,
      upto10 = NA,
      from10to20 = NA,
      from20to50 = NA,
      from50to100 = NA,
      from100to150 = NA,
      from150to200 = NA,
      from200to250 = NA,
      from250to300 = NA,
      from300to350 = NA,
      from350to400 = NA,
      from400to450 = NA,
      from450to500 = NA,
      above500 = NA
    )
  )
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
      suppressWarnings(
        measurementX <- data.frame(
          sample = rep(NA, length(levels(factor(tempMeasurement$className)))),
          measurement = rep(NA, length(levels(factor(tempMeasurement$className)))),
          polymer = rep(NA, length(levels(factor(tempMeasurement$className)))),
          fibres = rep(NA, length(levels(factor(tempMeasurement$className)))),
          fragments = rep(NA, length(levels(factor(tempMeasurement$className)))),
          spheres = rep(NA, length(levels(factor(tempMeasurement$className)))),
          pixels = rep(NA, length(levels(factor(tempMeasurement$className)))),
          particlesTotal = rep(NA, length(levels(factor(tempMeasurement$className)))),
          upto10 = rep(NA, length(levels(factor(tempMeasurement$className)))),
          from10to20 = rep(NA, length(levels(factor(tempMeasurement$className)))),
          from20to50 = rep(NA, length(levels(factor(tempMeasurement$className)))),
          from50to100 = rep(NA, length(levels(factor(tempMeasurement$className)))),
          from100to150 = rep(NA, length(levels(factor(tempMeasurement$className)))),
          from150to200 = rep(NA, length(levels(factor(tempMeasurement$className)))),
          from200to250 = rep(NA, length(levels(factor(tempMeasurement$className)))),
          from250to300 = rep(NA, length(levels(factor(tempMeasurement$className)))),
          from300to350 = rep(NA, length(levels(factor(tempMeasurement$className)))),
          from350to400 = rep(NA, length(levels(factor(tempMeasurement$className)))),
          from400to450 = rep(NA, length(levels(factor(tempMeasurement$className)))),
          from450to500 = rep(NA, length(levels(factor(tempMeasurement$className)))),
          above500 = rep(NA, length(levels(factor(tempMeasurement$className))))
        )
      ) # end supressWarnings
      
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
        measurementX$upto10[k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength <= 10 & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$from10to20[k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > 10 & tempMeasurement$actualLength <= 20 & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$from20to50[k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > 20 & tempMeasurement$actualLength <= 50 & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$from50to100[k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > 50 & tempMeasurement$actualLength <= 100 & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$from100to150[k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > 100 & tempMeasurement$actualLength <= 150 & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$from150to200[k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > 150 & tempMeasurement$actualLength <= 200 & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$from200to250[k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > 200 & tempMeasurement$actualLength <= 250 & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$from250to300[k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > 250 & tempMeasurement$actualLength <= 300 & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$from300to350[k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > 300 & tempMeasurement$actualLength <= 350 & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$from350to400[k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > 350 & tempMeasurement$actualLength <= 400 & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$from400to450[k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > 400 & tempMeasurement$actualLength <= 450 & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$from450to500[k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > 450 & tempMeasurement$actualLength <= 500 & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
        measurementX$above500[k] <- nrow(tempMeasurement[which(tempMeasurement$actualLength > 500 & tempMeasurement$className == levels(factor(tempMeasurement$className))[k]),])
      }# end for(k)
      
      # store measurementX into tempobj
      # here I gather all measurements and the sum of all together to write it in an excelfile
      # this is done for each sample (each sample gets its own excelfile now)
      tempobj[[j]] <- measurementX  
      
    }# end for(j)
    
    # now I have all measurements in tempobj
    # now I can sum all measurements together and write all data into an excelfile
    # each measurement and the summary gets their own sheet 
    suppressWarnings(
      measurementSum <- data.frame(
        sample = rep(NA, length(polymers)+1),
        measurement = rep(NA, length(polymers)+1),
        polymer = rep(NA, length(polymers)+1),
        fibres = rep(0, length(polymers)+1),
        fragments = rep(0, length(polymers)+1),
        spheres = rep(0, length(polymers)+1),
        pixels = rep(0, length(polymers)+1),
        particlesTotal = rep(0, length(polymers)+1),
        upto10 = rep(0, length(polymers)+1),
        from10to20 = rep(0, length(polymers)+1),
        from20to50 = rep(0, length(polymers)+1),
        from50to100 = rep(0, length(polymers)+1),
        from100to150 = rep(0, length(polymers)+1),
        from150to200 = rep(0, length(polymers)+1),
        from200to250 = rep(0, length(polymers)+1),
        from250to300 = rep(0, length(polymers)+1),
        from300to350 = rep(0, length(polymers)+1),
        from350to400 = rep(0, length(polymers)+1),
        from400to450 = rep(0, length(polymers)+1),
        from450to500 = rep(0, length(polymers)+1),
        above500 = rep(0, length(polymers)+1)
      )
    ) # end supressWarnings
    
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
        measurementSum$upto10[j] <- measurementSum$upto10[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$upto10) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$upto10}else{0}
        measurementSum$from10to20[j] <- measurementSum$from10to20[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from10to20) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from10to20}else{0}
        measurementSum$from20to50[j] <- measurementSum$from20to50[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from20to50) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from20to50}else{0}
        measurementSum$from50to100[j] <- measurementSum$from50to100[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from50to100) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from50to100}else{0}
        measurementSum$from100to150[j] <- measurementSum$from100to150[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from100to150) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from100to150}else{0}
        measurementSum$from150to200[j] <- measurementSum$from150to200[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from150to200) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from150to200}else{0}
        measurementSum$from200to250[j] <- measurementSum$from200to250[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from200to250) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from200to250}else{0}
        measurementSum$from250to300[j] <- measurementSum$from250to300[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from250to300) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from250to300}else{0}
        measurementSum$from300to350[j] <- measurementSum$from300to350[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from300to350) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from300to350}else{0}
        measurementSum$from350to400[j] <- measurementSum$from350to400[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from350to400) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from350to400}else{0}
        measurementSum$from400to450[j] <- measurementSum$from400to450[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from400to450) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from400to450}else{0}
        measurementSum$from450to500[j] <- measurementSum$from450to500[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from450to500) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$from450to500}else{0}
        measurementSum$above500[j] <- measurementSum$above500[j] + if(length(tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$above500) != 0){tempobj[[k]][which(tempobj[[k]]$polymer == polymers[j]),]$above500}else{0}
        
      }# end for k
    } # end for j
    
    # add a bottom line with the sum of each column (requested feature)
    measurementSum$sample[length(polymers)+1] <- "column sum"
    measurementSum$measurement[length(polymers)+1] <- "column sum"
    measurementSum$polymer[length(polymers)+1] <- "column sum"
    measurementSum$fibres[length(polymers)+1] <- sum(measurementSum$fibres)
    measurementSum$fragments[length(polymers)+1] <- sum(measurementSum$fragments)
    measurementSum$spheres[length(polymers)+1] <- sum(measurementSum$spheres)
    measurementSum$pixels[length(polymers)+1] <- sum(measurementSum$pixels)
    measurementSum$particlesTotal[length(polymers)+1] <- sum(measurementSum$particlesTotal)
    measurementSum$upto10[length(polymers)+1] <- sum(measurementSum$upto10)
    measurementSum$from10to20[length(polymers)+1] <- sum(measurementSum$from10to20)
    measurementSum$from20to50[length(polymers)+1] <- sum(measurementSum$from20to50)
    measurementSum$from50to100[length(polymers)+1] <- sum(measurementSum$from50to100)
    measurementSum$from100to150[length(polymers)+1] <- sum(measurementSum$from100to150)
    measurementSum$from150to200[length(polymers)+1] <- sum(measurementSum$from150to200)
    measurementSum$from200to250[length(polymers)+1] <- sum(measurementSum$from200to250)
    measurementSum$from250to300[length(polymers)+1] <- sum(measurementSum$from250to300)
    measurementSum$from300to350[length(polymers)+1] <- sum(measurementSum$from300to350)
    measurementSum$from350to400[length(polymers)+1] <- sum(measurementSum$from350to400)
    measurementSum$from400to450[length(polymers)+1] <- sum(measurementSum$from400to450)
    measurementSum$from450to500[length(polymers)+1] <- sum(measurementSum$from450to500)
    measurementSum$above500[length(polymers)+1] <- sum(measurementSum$above500)
    
    
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

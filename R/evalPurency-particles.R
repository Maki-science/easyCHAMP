######## evalPurency.particles() ###########
#' Automated evaluation of Purency data.
#' @description
#' Instead of a summarized version of *.csv files from Purency like from evalPurency(), this function
#' provides a list of all particles. Blank correction is done with a most-similar-particle comparison.
#' 
#' @param path The path where the files can be found. All csv files in this folder will be evaluated. Also saves the
#' resulting files in this directory.
#' @param polymers A vector containing the abbreviations of polymers to be considered. Default vector contains 22 
#' polymers.
#' @param sizeclasses A vector containing desired sizeclasses to be evaluated. Default is c(10, 20, 50, 100, 150, 
#' 200, 250, 300, 350, 400, 450, 500). The function starts at 0 and then uses the set steps. 
#' It always uses values up to the provided higher number but excluding the former number (e.g., for
#' the default values, the function uses 0 to <= 10, >10 to <= 20, >20 to <= 50, ..., all >500).
#' However, since this function does not use just the length, but the length and width (or area), compared to evalPurency,
#' The size classes are set in area, considering the provided size class as a square (e.g. 500 x 500 µm = 250000 µm^2).
#' This is done, to simplify the setup. It is much easier to imagine a particle with the length of one edge, than a surface.
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
#' @param particleNumbers set TRUE if you would like to get an extra file with just plastic and non-plastic 
#' particle numbers for each file loaded.
#' 
#' @return If dataReturn = TRUE, the function returns a list object including all 
#'  raw and processed data of each processing step and the summary values.
#' 
#' @examples 
#' # For this example the path doesn't matter. 
#' # If you want to analyse your own data, set test = FALSE (or simply delete this parameter).
#' #mydata <- evalPurency.particles(path="//HERE/COMES/YOUR/PATH/", dataReturn = TRUE, test = TRUE)
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
                                  particleNumbers = FALSE,
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
                         particleNumbers = particleNumbers,
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
  # add an index for later identification of single particles -> makes removing particles easier
  vdata$index <- NA
  for(i in 1:nrow(vdata)){
    vdata$index[i] <- i
  }
  
  # the area calculated for fibers is incorrect. Therefore, we recalculate it with the actual length * width
  for(i in 1:nrow(vdata)){
    if(vdata$form[i] == config$fibre){
      vdata$area[i] <- vdata$actualLength[i] * vdata$width[i]
    }
  }
  
  # in contrast to evalPurency, this function uses the area of the particles. The size classes are set as
  # the provided size class (length of the longest edge) squared. This way, it is easier to imagine the particle
  # size and the setup by the user.
  for(i in 1:nrow(vdata)){
    for(j in 1:length(sizeclasses)){
      if(vdata$area[i] <= sizeclasses[1]^2){
        vdata$sizeClass[i] <- paste("from", "0", "to", sizeclasses[1], sep="")
      }
      else if(vdata$area[i] > sizeclasses[length(sizeclasses)]^2){
        vdata$sizeClass[i] <- paste("above", sizeclasses[length(sizeclasses)], sep="")
      }
      else if(j > 1 && vdata$area[i] > sizeclasses[j-1]^2 && vdata$area[i] <= sizeclasses[j]^2){
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
    
    ### check blank existence ####
    if(noBlank == FALSE){
      #### check for blank existence ####
      # check whether each sample has a blank to use for correction
      # otherwise throw a warning message
      sampleBlankChecklist <- c(rep(FALSE, length(levels(factor(dataMeasurements$sample)))))
      for(i in 1:length(levels(factor(unlist(strsplit(as.character(dataBlanks$sample), config$blankKey)))))){
        for(j in 1:length(levels(factor(dataMeasurements$sample)))){
          if(grepl(levels(factor(unlist(strsplit(as.character(dataBlanks$sample), config$blankKey))))[i], levels(factor(dataMeasurements$sample))[j], fixed = TRUE) == TRUE){
            sampleBlankChecklist[j] <- TRUE
          }
        }
      }
      
      for(i in 1:length(levels(factor(dataMeasurements$sample)))){
        if(sampleBlankChecklist[i] == FALSE){
          cat(paste("Warning: There is no blank for ", levels(factor(dataMeasurements$sample))[i], "\n", sep = ""))
        }
      }
    } # end if noBlank == FALSE
    
    #### blank processing #####
    # Take blanks of the same sample an check whether there are similar particles.
    # Usually, we would calculate the mean of the particle number for each type (polymer, shape, 
    # sizeclass, and (if matters) colour).
    # However, since we want to handle every particle individually, we need to do it differently.
    # If there are sufficiently similar particles over several blanks (same polymer, shape, sizeclass, and 
    # (if matters) colour), we calculate the mean values (length, width, area) and gather all colors in the 
    # respective field. The latter is done, to take the colours into account later on (if colourSep != FALSE)
    # If colourSep = FALSE, colours can be ignored.
    
    # create a new blank file to gather all processed blanks
    processedBlankS <- data.frame(matrix(ncol = 12, nrow = 0))
    colnames(processedBlankS) <- colnames(dataBlanks)
    # create a df for the removed and averaged blank particles for later output
    removedBlankParticles <- data.frame(matrix(ncol = 13, nrow = 0))
    colnames(removedBlankParticles) <- c(colnames(dataBlanks), "reason")# field to add reason (averaged with index,index,..., to index)
    
    # iterate over the amount of blank samples
    for(i in 1:length(levels(factor(unlist(sapply(strsplit(as.character(dataBlanks$sample), config$blankKey, fixed = TRUE), getElement, 1)))))){ 
      
      # check how many blanks are available for averaging
      levelsBlankFiles <- levels(factor(dataBlanks[which(unlist(sapply(strsplit(as.character(dataBlanks$sample), config$blankKey, fixed = TRUE), getElement, 1)) == levels(factor(unlist(sapply(strsplit(as.character(dataBlanks$sample), config$blankKey, fixed = TRUE), getElement, 1))))[i]),]$sample))
      nBlankFiles <- length(levelsBlankFiles)
      # as mainBlankFile, we use all particles from that sample. Every particle averaged, will be removed
      # This way, we make sure, that all particles from all files of one sample are considered.
      mainBlankFile <- dataBlanks[which(grepl(levels(factor(unlist(sapply(strsplit(as.character(dataBlanks$sample), config$blankKey, fixed = TRUE), getElement, 1))))[i],dataBlanks$sample , fixed = TRUE)),]
      
      # Now check whether there are sufficiently similar particles
      # sufficiently similar == same polymer, same shape, same sizeclass, and (if matters) 
      # same colour (i.e., selected colour or not -> e.g., "schwarz" or other)
      # This will be directed always from one blank. Thus, it is checked, whether there is a similar particle
      # (for each particle in the first blank) in the other blanks. If one similar is found (the most similar
      # in case of several findings), it is saved, and the next blank file is checked similarly. 
      # If all files are processed, a mean of the particle values is calculated (length, width, area), and the
      # colours of all particles averaged are written into the colour field (for later reference).
      # Then the original particles will be removed from the list, and the process proceeds to the next particle
      # in the list of the first blank file.
      
      if(nBlankFiles > 1){ # if there is just one file, there is no need for averaging
        while(nrow(mainBlankFile) >0){ # iterate over the particles of the first file
          j <- 1
          # create a df for similar particles. These can be saved in removedBlankParticles and averaged
          # before writing the averaged particle into processedBlank
          similarParticles <- data.frame(matrix(ncol = 12, nrow = 0))
          colnames(similarParticles) <- colnames(mainBlankFile)
          # already add the current particle
          similarParticles <- rbind(similarParticles, mainBlankFile[j,])
          
          ### find similar particles #####
          # iterate over the OTHER blank files and compare their particles with the current one
          for(k in 2:nBlankFiles){ # 1 is the mainBlankFile
            # gather all particles of the same polymer, sizeclass and shape (and colour if applicable)
            if(colourSep != FALSE && mainBlankFile$color[j] == colourSep){ # if colourSep is set, check that the selected colour has to overlap
              tempk <- dataBlanks[which(dataBlanks$sample == levelsBlankFiles[k] & 
                                        dataBlanks$className == mainBlankFile$className[j] &
                                        dataBlanks$form == mainBlankFile$form[j] &
                                        dataBlanks$sizeClass == mainBlankFile$sizeClass[j] &
                                        dataBlanks$color == colourSep &
                                        dataBlanks$index != mainBlankFile$index[j]),] # to make sure, not to average with the similar particle
            }
            else if(colourSep != FALSE && mainBlankFile$color[j] != colourSep){ # if colourSep is set, but particle is not of given colour
              tempk <- dataBlanks[which(dataBlanks$sample == levelsBlankFiles[k] & 
                                          dataBlanks$className == mainBlankFile$className[j] &
                                          dataBlanks$form == mainBlankFile$form[j] &
                                          dataBlanks$sizeClass == mainBlankFile$sizeClass[j] &
                                          dataBlanks$color != colourSep&
                                          dataBlanks$index != mainBlankFile$index[j]),]
            }
            else{ # colour is not important (colourSep == FALSE)
              tempk <- dataBlanks[which(dataBlanks$sample == levelsBlankFiles[k] & 
                                          dataBlanks$className == mainBlankFile$className[j] &
                                          dataBlanks$form == mainBlankFile$form[j] &
                                          dataBlanks$sizeClass == mainBlankFile$sizeClass[j]&
                                          dataBlanks$index != mainBlankFile$index[j]),]
            }
            
            if(nrow(tempk) > 0){ # if sufficiently similar particles exist
              # substract the values of the particle from the 1st blank file
              # Then we can see which particle is closest to the original one
              tempk$actualLength <- tempk$actualLength - mainBlankFile$actualLength[j]
              tempk$area <- tempk$area - mainBlankFile$area[j]
              tempk$width <- tempk$width - mainBlankFile$width[j]
              
              # now find the most similar particle:
              similarities <- rep(0, nrow(tempk))
              for(l in 1:nrow(tempk)){
                if(abs(tempk$actualLength[l]) == min(abs(tempk$actualLength))){
                  similarities[l] <- similarities[l] +1
                }
                if(abs(tempk$area[l]) == min(abs(tempk$area))){
                  similarities[l] <- similarities[l] +1
                }
                if(abs(tempk$width[l]) == min(abs(tempk$width))){
                  similarities[l] <- similarities[l] +1
                }
              } # end for l
              # -> now we have a ranking of similarities
              
              # check whether two particles have the same similarity
              # in that case, consider the color
              if(length(similarities) > 1){ # if there is more than one particle
                for(l in 1:nrow(tempk)){
                  if(similarities[l] == max(similarities[-l])){
                    if(tempk$color[l] == mainBlankFile$color[j]){
                      similarities[l] <- similarities[l] +1
                    }
                  }
                }
                # if still similar, take the first
                for(l in 1:nrow(tempk)){
                  if(similarities[l] == max(similarities[-l])){
                    similarities[l] <- similarities[l] +1
                  }
                }
              } # end if(length(similarities) > 1)
              
              # -> now we have the most similar particle identified
              # add the most similar particle to the list for averaging
              for(l in 1:nrow(tempk)){
                if(similarities[l] == max(similarities)){
                  similarParticles <- rbind(similarParticles, tempk[l,])
                }
              }

              
            } # end if nrow(tempk) > 0
            else{ # if no similar particles exist
              # add the particle as is to the processedBlank
              similarParticles <- rbind(similarParticles, c(rep(NA, 13)))
            }
          
          } # end k
          # -> now we have one most similar particle of each blank file from the same blank sample
          # so we can average those particles now to create a new one
          ##### average particle #####
          # However, if there was no similar particle (and we don't have to average), simply write the 
          # particle here
          if(length(similarParticles$sample[which(!is.na(similarParticles$sample))]) > 1){
            # create an averaged particle
            newBlankParticle <- data.frame(
                                          sample = similarParticles$sample[1],
                                          measurement = "averaged",
                                          className = similarParticles$className[1],
                                          length = mean(similarParticles$length, na.rm = TRUE),
                                          form = similarParticles$form[1],
                                          lengthFibre = mean(similarParticles$lengthFibre, na.rm = TRUE),
                                          color = paste(similarParticles$color[1:nrow(similarParticles)], collapse = ","),
                                          area = mean(similarParticles$area, na.rm = TRUE),
                                          width = mean(similarParticles$width, na.rm = TRUE),
                                          actualLength = mean(similarParticles$actualLength, na.rm = TRUE),
                                          sizeClass = similarParticles$sizeClass[1],
                                          index = nrow(vdata) + nrow(processedBlankS) + 1
                                          )
            
            # add the old particles to the removedBlankParticle df
            for(k in 1:nrow(similarParticles)){
              if(!is.na(similarParticles$sample[k])){ # if NA do nothing
                removedBlankParticles <- rbind(removedBlankParticles, 
                                               cbind(similarParticles[k,], 
                                                     data.frame(reason = paste("averaged with", paste(similarParticles$index[1:nrow(similarParticles)][-k], 
                                                                                  collapse = ","),
                                                           "to", newBlankParticle$index,
                                                           sep = " ")
                                                     )
                                                )
                                              )
                
                # remove this particle from the blanks and mainBlankFile
                dataBlanks <- dataBlanks[which(dataBlanks$index != similarParticles$index[k]),]
                mainBlankFile <- mainBlankFile[which(mainBlankFile$index != similarParticles$index[k]),]
                
              }
            }# end for k -> added removed particles to list
          }
          else{ # if there is no similar particle, just take the original one
            newBlankParticle <- similarParticles[1,]
            # remove this particle from the blanks
            dataBlanks <- dataBlanks[which(dataBlanks$index != similarParticles$index[1]),]
            mainBlankFile <- mainBlankFile[which(mainBlankFile$index != similarParticles$index[1]),]
          }

          # Then we have to add the new particle to the processedBlanks df
          processedBlankS <- rbind(processedBlankS, newBlankParticle)
          
        } # end while j (iteration over particles of mainBlankFile)
      } # end ifnblankfiles > 1
      else{
        # if just one blank file for this sample exists, just copy the content to processedBlankS
        processedBlankS <- rbind(processedBlankS, mainBlankFile)
      }

    } # end i
    
    obj$processedBlankS <- processedBlankS
    obj$removedBlankParticles <- removedBlankParticles
    # -> now the blanks are processed (averaged particles etc.)
    # colourSep is already considered: particles of the given colour are treated separately
    
    
    #### delete particles from samples that are very similar to blank particles. ####
    # In case of more than one fit, delete the closest.
    
    # create a new blank file to gather all processed blanks
    processedSamples <- data.frame(matrix(ncol = 12, nrow = 0))
    colnames(processedSamples) <- colnames(dataMeasurements)
    # create a df for the removed and averaged blank particles for later output
    removedSampleParticles <- data.frame(matrix(ncol = 13, nrow = 0))
    colnames(removedSampleParticles) <- c(colnames(dataMeasurements), "reason")# field to add reason (averaged with index,index,..., to index)
    
    # iterate over the samples
    for(i in 1:length(levels(factor(dataMeasurements$sample)))){
      
      # select blank particles of this sample
      tempBlanks <- processedBlankS[grepl(levels(factor(dataMeasurements$sample))[i], processedBlankS$sample),]
      
      # now, for each blank particle of this sample, find the most similar particle (within a decent range) and
      # remove it (and add to removedSampleParticles)
      for(j in 1:nrow(tempBlanks)){
        
        # find all particles that are similar to this one
        if(colourSep != FALSE && (tempBlanks$color[j] == colourSep && !is.na(tempBlanks$color[j]))){
          tempPart <- dataMeasurements[which(dataMeasurements$sample == levels(factor(dataMeasurements$sample))[i] &
                                               dataMeasurements$sizeClass == tempBlanks$sizeClass[j] &
                                               dataMeasurements$form == tempBlanks$form[j] &
                                               dataMeasurements$className == tempBlanks$className[j] &
                                               dataMeasurements$color == colourSep),]
        }
        else if(colourSep != FALSE && (tempBlanks$color[j] != colourSep || is.na(tempBlanks$color[j]))){
          tempPart <- dataMeasurements[which(dataMeasurements$sample == levels(factor(dataMeasurements$sample))[i] &
                                               dataMeasurements$sizeClass == tempBlanks$sizeClass[j] &
                                               dataMeasurements$form == tempBlanks$form[j] &
                                               dataMeasurements$className == tempBlanks$className[j] &
                                               dataMeasurements$color != colourSep),]
        }
        else{
          tempPart <- dataMeasurements[which(dataMeasurements$sample == levels(factor(dataMeasurements$sample))[i] &
                                               dataMeasurements$sizeClass == tempBlanks$sizeClass[j] &
                                               dataMeasurements$form == tempBlanks$form[j] &
                                               dataMeasurements$className == tempBlanks$className[j]),]
        }
        
        # expand range of sizeClass by 1.5, if no particle is similar
        if(nrow(tempPart) == 0){
          # do the same as before, but expand the range of sizeClass by 1.5
          # Therefore, define the acceptable range
          matches <- regmatches(tempBlanks$sizeClass[j], gregexpr("[[:digit:]]+", tempBlanks$sizeClass[j]))
          oldRange <- as.numeric(unlist(matches))
          newRange <- c(oldRange[1]*0.5, oldRange[2]*1.5)
          if(is.na(newRange[2])){ # write an unrealsitic high number (indicating all particles bigger than 500 - or whatever the user sets as upper limit)
            newRange[2] <- 10000 
          }
          # calculate the area for the new size range limits
          newRange <- newRange^2
          
          # now do selection again, but with area instead of sizeClass
          if(colourSep != FALSE && (tempBlanks$color[j] == colourSep && !is.na(tempBlanks$color[j]))){
            tempPart <- dataMeasurements[which(dataMeasurements$sample == levels(factor(dataMeasurements$sample))[i] &
                                                 dataMeasurements$area >= newRange[1] &
                                                 dataMeasurements$area <= newRange[2] &
                                                 dataMeasurements$form == tempBlanks$form[j] &
                                                 dataMeasurements$className == tempBlanks$className[j] &
                                                 dataMeasurements$color == colourSep),]
          }
          else if(colourSep != FALSE && (tempBlanks$color[j] != colourSep || is.na(tempBlanks$color[j]))){
            tempPart <- dataMeasurements[which(dataMeasurements$sample == levels(factor(dataMeasurements$sample))[i] &
                                                 dataMeasurements$area >= newRange[1] &
                                                 dataMeasurements$area <= newRange[2] &
                                                 dataMeasurements$form == tempBlanks$form[j] &
                                                 dataMeasurements$className == tempBlanks$className[j] &
                                                 dataMeasurements$color != colourSep),]
          }
          else{
            tempPart <- dataMeasurements[which(dataMeasurements$sample == levels(factor(dataMeasurements$sample))[i] &
                                                 dataMeasurements$area >= newRange[1] &
                                                 dataMeasurements$area <= newRange[2] &
                                                 dataMeasurements$form == tempBlanks$form[j] &
                                                 dataMeasurements$className == tempBlanks$className[j]),]
          }
        } # end if(nrow(tempPart) == 0)
        # -> now we should have particles that are sufficiently similar for blank correction
        
        if(nrow(tempPart) != 0){ # if there is still no particle sufficiently similar, do nothing
          # otherwise determine the most similar particle
          # substract the values of the blank from the particles
          # Then we can see which particle is closest to the original one
          tempPart$actualLength <- tempPart$actualLength - tempBlanks$actualLength[j]
          tempPart$area <- tempPart$area - tempBlanks$area[j]
          tempPart$width <- tempPart$width - tempBlanks$width[j]
          
          # now find the most similar particle:
          similarities <- rep(0, nrow(tempPart))
          for(l in 1:nrow(tempPart)){
            if(abs(tempPart$actualLength[l]) == min(abs(tempPart$actualLength))){
              similarities[l] <- similarities[l] +1
            }
            if(abs(tempPart$area[l]) == min(abs(tempPart$area))){
              similarities[l] <- similarities[l] +1
            }
            if(abs(tempPart$width[l]) == min(abs(tempPart$width))){
              similarities[l] <- similarities[l] +1
            }
            if(grepl(tempPart$color[l], tempBlanks$color[j])){
              similarities[l] <- similarities[l] +1
            }
          } # end for l
          # -> now we have a ranking of similarities
          
          # check whether two particles have the same similarity. Then, take the first
          if(length(similarities) > 1){ # if there is more than one particle
            for(l in 1:nrow(tempPart)){
              if(similarities[l] == max(similarities[-l])){
                similarities[l] <- similarities[l] +1
              }
            }
          } # end if(length(similarities) > 1)
          # -> now we have the most similar particle identified
          
          # remove that particle from tempPart and add it to removedSampleParticles
          for(l in 1:nrow(tempPart)){
            if(similarities[l] == max(similarities)){
              # add particle to removedSampleParticles
              removedSampleParticles <- rbind(removedSampleParticles, 
                                              cbind(dataMeasurements[which(dataMeasurements$index == tempPart$index[l]),], 
                                                    data.frame(reason = paste("removed with blank particle", 
                                                                              tempBlanks$index[j],
                                                                              sep = " ")
                                                                )
                                              )
                                              )
              # remove particle from dataMeasurements
              dataMeasurements <- dataMeasurements[which(dataMeasurements$index != tempPart$index[l]),]
              # remove particle from tempBlanks
              tempBlanks <- tempBlanks[which(tempBlanks$index != tempBlanks$index[j]),]
              
            }
          }# end for l iterate over tempPart

        }# end if(nrow(tempPart) != 0)

      } # end j (iterate over blank particles)
      
    } # end for i (iterate over the samples)
    
    # write dataMeasurements into processedSamples
    processedSamples <- dataMeasurements
    
    #### add corrected particle list to obj
    obj$processedSamples <- processedSamples
    obj$removedSampleParticles <- removedSampleParticles
    
  }
  else{ # noBlank == TRUE
    # without blanks, no processing must take place. Thus, just the raw data table is provided
    obj$processedSamples <- vdata
  }
  
  #### save data into *.xls
  if(test == FALSE){
    writexl::write_xlsx(list(processed_particle_list = obj$processedSamples, 
                             removed_sample_particles = obj$removedSampleParticles,
                             processed_blanks = obj$processedBlankS,
                             removed_blank_particles = obj$removedBlankParticles,
                             raw_samples = obj$samples,
                             raw_blanks = obj$blanks,
                             raw_data = obj$rawData),
                        paste(path, "processing data.xls", sep=""))
  }
  
  if(dataReturn == TRUE){
    return(obj)
  }

}

######## easyCHAMP.config.helper() ###########
#' Manages configuration of easyCHAMP, including labpresets
#' @description
#' Most settings of easyCHAMP can be overwritten by a labpreset. If a labpreset is
#' selected, it might overwrite or not overwrite the current function settings.
#' To manage which settings will be chosen and which not (depending on NAs in labpreset),
#' this function handles the configuration with labpresets correctly and returns a config data frame.
#' This function is thought to ensure correct configuration and to simplify changes and maintenance, 
#' as well as improving testing.
#' 
#' @param labpreset A preset for most of the parameters (except: path, polymers, dataReturn, eocsum). 
#' Can be requested by other labs, to be implemented, that they don't have to be set manually all the time. 
#' @param blankKey The key word to distinguish blanks from other measurements/samples. It is case sensitive to prevent
#' accidental matching. Defaults to "Blank".
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
#' @param startrow Number of rows that can be omitted from the *.csv files. Usually preprocessing software 
#' provides a bunch of meta data, that are not of interest here. Usually 40 rows can be skipped (more or less - 0 in case of siMPle).
#' This is automated now. However, automation can be disabled if a value is defined (e.g., for troubleshooting).
#' 
easyCHAMP.config.helper <- function(labpreset,
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
                             colArea,
                             colWidth,
                             fibre,
                             sphere,
                             fragment,
                             pixel,
                             startrow){
  
  #### lab presets ####
  # if a preset is set, load the respective preset (if available)
  # this is how a preset is created:
  # evalPurencyPresets <- data.frame(
  #   labname = "Laforsch",
  #   blankKey = "Blank",
  #   sep = ";",
  #   dec = ",",
  #   colPol = 6,
  #   colL = 17,
  #   colReqPol = 24,
  #   ReqPolKey = "ja",
  #   colShape = 25,
  #   colCol = 26,
  #   colLFib = 27,
  #   colArea = 4,
  #   colWidth = 18,
  #   fibre = "Faser",
  #   sphere = "Kugel",
  #   fragment = "Fragment",
  #   pixel = "Pixel",
  #   startrow = 40
  # )
  
  # check whether this is similar with easyCHAMP-config-helper.R
  # usethis::use_data(evalPurencyPresets, testdata.default, testdata.size, testdata.noeocsum,
  #                   overwrite = TRUE, internal = TRUE)
  
  config <- data.frame(
    labname = labpreset,
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
    startrow = startrow
  )
  
  # Check whether a labpreset was chosen
  if(labpreset != FALSE){
    if( length(evalPurencyPresets$labname[which(evalPurencyPresets$labname == labpreset)]) == 0){
      stop("You selected a labpreset, that does not exist. Please check this parameter for typo or request a new labpreset for your lab. \n")
    }
    
    if(!is.na(evalPurencyPresets$blankKey)){
      config$blankKey <- evalPurencyPresets$blankKey
    }
    if(!is.na(evalPurencyPresets$colPol)){
      config$colPol <- evalPurencyPresets$colPol[which(evalPurencyPresets$labname == labpreset)]
    }
    if(!is.na(evalPurencyPresets$colL)){
      config$colL <- evalPurencyPresets$colL[which(evalPurencyPresets$labname == labpreset)]
    }
    if(!is.na(evalPurencyPresets$colReqPol)){
      config$colReqPol <- evalPurencyPresets$colReqPol[which(evalPurencyPresets$labname == labpreset)]
    }
    if(!is.na(evalPurencyPresets$colShape)){
      config$colShape <- evalPurencyPresets$colShape[which(evalPurencyPresets$labname == labpreset)]
    }
    if(!is.na(evalPurencyPresets$colCol)){
      config$colCol <- evalPurencyPresets$colCol[which(evalPurencyPresets$labname == labpreset)]
    }
    if(!is.na(evalPurencyPresets$colLFib)){
      config$colLFib <- evalPurencyPresets$colLFib[which(evalPurencyPresets$labname == labpreset)]
    }
    if(!is.na(evalPurencyPresets$colArea)){
      config$colArea <- evalPurencyPresets$colArea[which(evalPurencyPresets$labname == labpreset)]
    }
    if(!is.na(evalPurencyPresets$colWidth)){
      config$colWidth <- evalPurencyPresets$colWidth[which(evalPurencyPresets$labname == labpreset)]
    }
    if(!is.na(evalPurencyPresets$fibre)){
      config$fibre <- evalPurencyPresets$fibre[which(evalPurencyPresets$labname == labpreset)]
    }
    if(!is.na(evalPurencyPresets$sphere)){
      config$sphere <- evalPurencyPresets$sphere[which(evalPurencyPresets$labname == labpreset)]
    }
    if(!is.na(evalPurencyPresets$fragment)){
      config$fragment <- evalPurencyPresets$fragment[which(evalPurencyPresets$labname == labpreset)]
    }
    if(!is.na(evalPurencyPresets$pixel)){
      config$pixel <- evalPurencyPresets$pixel[which(evalPurencyPresets$labname == labpreset)]
    }
    if(!is.na(evalPurencyPresets$startrow)){
      config$startrow <- evalPurencyPresets$startrow[which(evalPurencyPresets$labname == labpreset)]
    }
  }
  
  
  return(config)
}
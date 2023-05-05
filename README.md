
# evalPurency

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/evalPurency)](https://CRAN.R-project.org/package=evalPurency)
[![R-CMD-check](https://github.com/Maki-science/evalPurency/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Maki-science/evalPurency/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Maki-science/evalPurency/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Maki-science/evalPurency?branch=main)
<!-- badges: end -->

## Microplastic Finder from Purency
Microplastics can be found in any part of an environmental or nutritional sample. Today, FPA-based FTIR imaging allows a whole sample to be measured quickly. However, to analyse the measurement data, a powerful tool is needed to reliably identify the polymer particles contained in the sample. The analysis must provide objective, complete and robust results, which are presented in detail and transparently. The Microplastics Finder by Purency uses machine learning algorithms that identify more than 20 polymer types within minutes. In combination with FTIR imaging, it offers the fastest method to examine an entire sample for microplastics. Thanks to automation, which does not require time-consuming manual post-processing, the analysis results are optimally comparable and reproducible. Microplastics analysis becomes scalable and new types of questions about the presence and origin of microplastics can be answered. 
*Source: https://www.purency.ai/product1/microplastics-finder)*


## Purpose of this package
Commonly in microplastic research, a sample is splitted and filtered on several filters. These filters have to be measured separately via FTIR and can be then evaluated using the Purency software (https://www.purency.ai/). This produces .csv files for each measurement.

This package and its functions can be used to automate the single file handling and gathering the relevant data for each sample into one file. The processing requires only a few seconds instead of hours (of copying and pasting) in manual work. Furthermore, the chance of mistakes is reduced by generic computation.

It will count occurrences of fibres, fragments, spheres and pixels, as well as different (customisable) size fractions for each polymer. Each file (i.e., each measurement) is evaluated separately, as well as summarized for all files (i.e., one sample). Also the blank correction and correction for filter division is done by this function. For each sample a new excel file will be generated in the folder where the files are placed in. One file for each sample and one additional file, containing the data of different processing steps (also raw data) to allow a full track of the processing, if required. If desired, the function returns a list object to the R environment with all produced data sets, which can be used for further processing in R.

The default parameter settings are based on the requirements of the animal ecology I group (AG Laforsch) of the University of Bayreuth. However, the function is highly customisable, and you can change all parameters to your desire. Read further below for detailed instruction what and how you can/should change.


## Installation
After the corresponding paper is published, you should be able to download this package from CRAN (watch the badge on the top of this page). Just go to the upper tool bar of your RStudio and click *tools* > *Install Packages...* > tipe in *evalPurency* > click *install*.


You can also install the development version of evalPurency from [GitHub](https://github.com/). If not already installed, you need to have Rtools installed on your system to be able to compile the development version *(https://cran.r-project.org/bin/windows/Rtools/)*.
(devtools only has to be installed once)

``` r
install.packages("devtools") # just necessary if not already installed
devtools::install_github("Maki-science/evalPurency")
```
Once installed, you don't need to repeat this step each time! 
If you want to update to the newest version, just run the second line again.


## Workflow
The (pre-)Purency workflow usually produces several filters (or measurements) for each sample. We highly recommend to adopt a common file-naming procedure. The function uses everything of the .csv file names before the first '_' (underline) as sample name. The rest is considered as filter/measurement (e.g., SAMPLENAME_Filter1of3). This also allows for one blank for multiple filters/samples from one location (i.e., replicates), by naming the samples accordingly.

Here are some examples how to apply the naming for different blank application:

For each sample one or more own blank filters. You can still have several filters for each sample and/or blank, which are summed together before processing:

  - Samples: SAMPLENAME1_Filter1, SAMPLENAME2_Filter1
  - Blanks: SAMPLENAME1Blank_Filter1, SAMPLENAME2Blank_filter1
  
Samples should use the same blank(s):

  - Samples: SAMPLENAME1_Filter1, SAMPLENAME2_Filter1
  - Blank: SAMPLENAMEBlank_Filter1 (notice the missing number)
  
You have several blanks, but they should be averaged as combined blank for your samples (still, each blank can constitute several filters that are summed before averaging over the blanks):

  - Samples: SAMPLENAME1_Filter1, SAMPLENAME2_Filter1
  - Blanks: SAMPLENAME1Blank1_Filter1, SAMPLENAME1Blank2_filter1, SAMPLENAME2Blank1_Filter1, SAMPLENAME2Blank2_filter1 (note the number (or any other sign) behind 'Blank' but before '_')
  
You want to use one (or several averaged) blanks for all samples:

  - Samples: PREFIXSAMPLENAME1_Filter1, PREFIXSAMPLENAME2_Filter1 (set a prefix)
  - Blanks: PREFIXBlank_Filter1, PREFIXBLank_filter1 (use the same prefix but directly state 'Blank_')
  
You can also use a mix of all those versions, if necessary, though, we think this might be a bad sampling design.
  
If you don't have any blanks to process, you can skip the blank processing with setting *noBlank = TRUE*.

*WARNING: The naming of the files is an important prerequisite for a proper processing. Thus you need to cautiously set your file names!*

Put all files you want to process including their blank files into one folder. We recommend to put all files of a certain project/study or similar into one folder and process them at once.

To use the package function, just load the package as usual:
``` r
library(evalPurency)
```

Now simply run the follwing line, but change the path accordingly to the location where you stored your files to be processed. Make sure to use (or change) '/' and not '\\', since only windows uses backslashes. 

``` r
evalPurency(path="C:/users/MYNAME/Desktop/MYFILESTOBEPROCESSED/") # or similar
```
The '/' in the end is required!
Copy and paste this line into the console and press 'enter'.

*WARNING: This runs the function with its default configuration. You might need to adopt some further parameters. See below for details.*

*WARNING: Make sure that your csv file has the correct column separator (; in default) and decimal sign (, in default). Otherwise read further below how to change it in the function, or make sure to save the csv file properly.*

The function will warn you if there are missing values at some files. The respective files and columns are reported.
Once it finished the processing it will message that it is done.

It will usually need just very few seconds to process e.g. 100 files. An excel file for each sample is created in the selected folder where the sample results are summarized. For all files there is one additional file with the processing data. There you can find data sets of each processing step on separate sheets (also raw data), so you can trace back the calculation and/or load parts of the data into R if required without running the whole function again.

For compatibility the Excel files are of an old version. Therefore, there might occur a message about potential broken file that you have to confirm. However, the warning can be ignored. Note that the excel file will be coded with '.' as decimal sign!

## Additional parameters
The function is made fully dynamic and customisable. You can choose the options further below and combine them to your desire.

### Changing separator and decimal sign in csv-files
Depending on the habits of the country and/or system configuration, the csv files might be different. In default, the function uses ';' as separator, indicating a new column in the csv file, and ',' as a decimal sign. Change it to your liking by adding *sep = ";"* and *dec = ","* and change the sign within the quotes.
``` r
evalPurency(path="C:/users/MYNAME/Desktop/MYFILESTOBEPROCESSED/",
            sep = ";",
            dec = ",")
```

### Use not the recommended file naming
If you choose not to call your blank files as recommended (MYSAMPLENAMEBlank - with a capital *B*) you should change it accordingly. We chose to use case sensitivity here to prevent accidental choice of the wrong files as blanks. Here an example with a noncapital *b* (i.e., MYSAMPLENAMEblank):
``` r
evalPurency(path="C:/users/MYNAME/Desktop/MYFILESTOBEPROCESSED/",
            blankKey = "blank")
```

*WARNING: The rest of the naming recommendation, we would still recommend to adopt. However, feel free to adapt it to your liking as long as the division with the _ (underline), and the positioning of the blank in the sample name is provided.*

### Change evaluated polymers
At the current state, the function evaluates up to 22 polymers (see example).
If you would like to add polymers, or just evaluate some of them, you can overwrite the default setting by simply change the content of the 'c(...)' accordingly (make sure, that the content is not ending with a , like (... ,). 
Just delete or add the (un)desired polymers. 

*WARNING: If you have named the polymers differently in your Purency software, you should set these accordingly here.*

``` r
evalPurency(path="C:/users/MYNAME/Desktop/MYFILESTOBEPROCESSED/", 
            polymers = c("PU", "EVAc", "PA", "PAN", "PBT", "PET", "PE", "PMMA", "PP", 
                         "POM", "PS", "PVC", "PC", "ABS", "PPSU", "CA", "PEEK", "EVOH", 
                         "PSU", "SILICONE", "PLA", "PLAPBAT"))
```

### Change evaluated size classes
The size classes that should be evaluated by the function can be set manually. The function will dynamically switch to the desired size classes. It will sum the numbers from 0 to the first number (e.g., <=10. Then it will always exclude the lower number and include the upper number (e.g., >10 to <= 20, >20 to <= 50, ...). Finally, the highest number to infinite (e.g., >500).
``` r
evalPurency(path="C:/users/MYNAME/Desktop/MYFILESTOBEPROCESSED/", 
            sizeclasses = c(10, 20, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500))
```

### You analysed only parts of a sample?
In many cases it can happen, that only a part of a sample is measured, if there are too many particles. This might be different between sample and blanks (e.g., it might be that a filter breaks appart). Therefore, you can provide a division factor for each sample and blank separately, or even for each filter separately.

However, to keep it simple and prevent mistakes, you only set *setDivFactor* when calling the function. There are three options available: *setDivFactor = FALSE* will ignore this parameter (default). If *setDivFactor = "samplewise"*, this will cause the function to request the required values during the processing for each sample (i.e., all filters of one sample have the same factor). If set to *setDivFactor = "filterwise"*, a division factor will be requested for each filter separately. 

Just watch your R console and follow the instructions. 
``` r
evalPurency(path="C:/users/MYNAME/Desktop/MYFILESTOBEPROCESSED/", 
            setDivFactor = "samplewise")
```
If you only use a quarter of your sample the factor you should provide here is 0.25. If you did not divide a sample it is 1.

We implemented these two ways, to reduce the effort and possibility of mistakes in typing in the factors, if they are similar for all filters of a sample, but still provide the opportunity to choose the factors for each filter, in case there have been issues during the sample preparation, causing different factors for each filter.

*WARNING: Make sure to exactly follow the instructions. Keep the order similar to the provided order of the samples/blanks.*

*WARNING: If you use several blanks summed for all samples (like example 3 in the beginning) you can only chose a common division factor.*

### Keep the data in R as data frame
If you further want to proceed and analyse the data with R, you can set *dataReturn = TRUE*. The function will then return a data frame consisting of all measurements of all samples of the selected folder as well as the blanks and raw data.
``` r
results <- evalPurency(path="C:/users/MYNAME/Desktop/MYFILESTOBEPROCESSED/", 
                      dataReturn = TRUE)
# only use the fully processed data of all samples:
allSamples <- results$sampleSummary 
```
You can now proceed with this data and do your analyses or plotting as usual.


### Summary of each column at the bottom
The function adds a bottom line to the summary data, if desired. There, each column is summed for custom purpose (e.g., quality control of the function and preliminary steps). However, if you don't like this line to be in your data set, e.g., because you want to process the data further, you can skipt the calculation by adding *eocsum = FALSE* to the function call.
``` r
mydata <- evalPurency(path="C:/users/MYNAME/Desktop/MYFILESTOBEPROCESSED/", 
                      eocsum = FALSE)
```

## Use this package in another lab
The output of Purency can be customized to a certain extend and different labs may have different requirements and conventions on how to name things (also in the language this is done). Therefore, the default settings of this function apply for the animal ecology group of the University of Bayreuth. Unfortunately, there is no smart way to workaround. Thus, this function requires several parameters to preset the working environment of the lab.

To make it as generic and convenient to other labs working with this package, you can overwrite the default settings to your liking. In case you would prefer a complete preset for your lab, that you don't have to provide each parameter separately, we can implement a preset to your desire if you open an issue or send me a request. A preset can be added quite fast.

A preset can be loaded in the function call by adding the parameter *labpreset = "MYOWNLABNAME"*.

If no preset is available for your lab, so far, consider the following parameters to be adjusted (just add the necessary parameters as additional parameter in the function call):

  - colPol = 6, Column number where the polymer type is stated. In the TOEKI lab this is column 6 (Class Name). Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
  - colL = 17, Column number for the particle length. In the TOEKI lab this is column 17 (Length [5µ]). Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
  - colReqPol = 24, Column number for the particle check, whether the particle is a polymer or not. In the TOEKI lab this is column 24 (Plastik? or Plastik ja/nein). Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
  - ReqPolKey = "ja", key words that indicates whether it is a plastic particle or not in 'colReqPol'.
  - colShape = 25, Column number for the particle shape. In the TOEKI lab this is column 25 (Form). Could also be provided as column name, but only in ASCII encoding (e.g., special character = . and ä = d).
  - colCol = 26, Column number for the particle color In the TOEKI lab this is column 26 (Farbe). Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
  - colLFib = 27, Column number for the particle length in case of a fibre with corrected length (because of curvy shape). In the TOEKI lab this is column 27 (Länge). Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
  - fibre = "Faser", How fibres are called in colShape (Form). In the TOEKI lab it is 'Faser'.
  - sphere = "Kugel", How spheres are called in colShape (Form). In the TOEKI lab it is 'Kugel'.
  - fragment = "Fragment", How fragments are called in colShape (Form). In the TOEKI lab it is 'Fragment'.
  - pixel = "Pixel", How pixels are called in colShape (Form). In the TOEKI lab it is 'Pixel'.

I used column numbers here to workaround issues with special characters in some languages which R might has problems with. Numbers provide an unambiguous delimiter. However, we could use column names instead of numbers, as long as they are ASCII encoded or ASCII conform. Nonetheless, we would not recommend this, but rather use column numbers.

With an adopted preset, you can now run the function as mentioned before (just add the preset always to the function).


## Troubleshooting:

The function was written in Oktober 2022. I've used the current column numbers to address the required information. If these will be changed by any chance, this should be adopted to the function as well (see 'Use this package in another lab' above to read how).
If not, the results are not reliable any more! 

The source code can be accessed via github.
I've commented it quite sophisticated, and changes should be quite easy when you're a bit into programming. Otherwise open an issue or send me a request, and I will do my best to implement your desired features.

If there are any issues or wishes for changes, you can send me a mail to info@maki-science.org or open an issue here on github (https://github.com/Maki-science/evalPurency/issues).

  - If an error occurs or you get different results, each time the function runs, make sure that you have only the files in the respective folder that should be processed. If you re-run the function, you will have the previously created files there, so you need to delete them first, and then run the function again. 
  - Make sure you considered all points before, where a *WARNING* was provided.


## Citation
To cite evalPurency in publications use:

Marvin Kiene, Eva Vizsolyi Cseperke, Martin Löder and Christian Laforsch (2023). evalPurency: Automated Evaluation of Data of the Purency's Microplastic Finder. R package version 1.2.4.9004.
https://github.com/Maki-science/evalPurency


A BibTeX-entry for LaTeX-user is

  @Misc{,
    title = {evalPurency: Automated Evaluation of Purency Data},
    author = {Marvin Kiene and Eva {Cseperke Vizsolyi} and Martin Löder and Christian Laforsch},
    note = {R package version 1.2.5.9012},
    year = {2023},
    url = {https://github.com/Maki-science/evalPurency},
  }

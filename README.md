
# evalPurency

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/evalPurency)](https://CRAN.R-project.org/package=evalPurency)
<!-- badges: end -->

## Microplastic Finder from Purency
Microplastics can be found in any part of an environmental or nutritional sample. Today, FPA-based FTIR imaging allows a whole sample to be measured quickly. However, to analyse the measurement data, a powerful tool is needed to reliably identify the polymer particles contained in the sample. The analysis must provide objective, complete and robust results, which are presented in detail and transparently. The Microplastics Finder by Purency uses machine learning algorithms that identify more than 20 polymer types within minutes. In combination with FTIR imaging, it offers the fastest method to examine an entire sample for microplastics. Thanks to automation, which does not require time-consuming manual post-processing, the analysis results are optimally comparable and reproducible. Microplastics analysis becomes scalable and new types of questions about the presence and origin of microplastics can be answered. 
*Source: https://www.purency.ai/product1/microplastics-finder)*


## Purpose of this package
Commonly in microplastic research, a sample is splitted and filtered on several filters. These filters have to be measured separately via FTIR and can be then evaluated using the Purency software (https://www.purency.ai/). This produces .csv files for each measurement.

This package and its functions can be used to automate the single file handling and gathering the relevant data for each sample into one file.

It will count occurences of fibres, fragments, spheres and pixels, as well as size fractions (<10, 10-20, 20-50, 50-100, 100-150, 150-200,..., >500) for each polymer. Each file (i.e., each measurement) is evaluated separately, as well as summarized for all files (i.e., one sample). For each sample a new excel file will be generated
    in the folder where the files are placed.

I've written this package for the TOEKI working group of the university of bayreuth, to allow simple and (almost) bulletproof application. If you are interested in adopting the framework for another lab with different specifications, let me know and I will see what I can do. Ideas for implementation for broader usability are appreciated.

## Installation

You can install the development version of evalPurency from [GitHub](https://github.com/) with:
(devtools only has to be installed once)
``` r
install.packages("devtools") # just necessary of not already installed
devtools::install_github("Maki-science/evalPurency")
```
Once installed, you don't need to repeat this step each time! 
If you want to update, to the newest version, run the second line again.

## Workflow
The (pre-)Purency workflow usually produces several filters (measurements) for each sample. I highly recommend to adopt a common file-naming procedure. The function uses everything of the .csv file names before the first '_' as sample name. The rest is considered as measurement name (e.g., SAMPLENAME_MEASUREMENTX.csv).

Put all files you want to process into one folder. I recommend to put all files of a certain project/study or similar into one folder.

To use the package function, just load the package as usual:
``` r
library(evalPurency)
```

Now simply run the follwing line, but change the path accordingly to the location where you stored your files to be processed. Make sure to use (or change) '/' and not '\', since only windows uses backslashes. 

This is an example from the TOEKI student server:
``` r
evalPurency(path="//SERVERADDRESS/LSTOEK1_STUDENT/MYNAME/MYFILESTOBEPROCESSED/")
```
The '/' in the end is required!
Copy and paste this line into the console press 'enter'.

The function will warn you if there are missing values at some files. The respective files and columns are reported.
Once it finished the processing it will message that it is done.

It will usually need just very few seconds to process e.g. 100 files. An excel file for each sample is created in the selected folder. On the first sheet, a summary of the whole sample is provided with the counts of particle form and size fractions as well as the sum of each column as the last line. On the second sheet, each measurement is evaluated in case you want trace back the single measurements.

For compatibility the Excel files are of an old version. Therefore, there might occur a message about potential broken file that you have to confirm. However, the warning can be ignored.

Of course you can also use your local hard drive like so:
``` r
evalPurency(path="C:/users/MYNAME/Desktop/MYFILESTOBEPROCESSED/") # or similar
```

### Change evaluated polymers
At the current state, the function evaluates up to 22 polymers (see example).
If you would like to add polymers, or just evaluate some of them, you can overwrite the default setting by simply change the content of the 'c(...)' accordingly (make sure, that the content is not ending with a , like (... ,). 
Just delete or add the (un)desired polymers.
``` r
evalPurency(path="C:/users/MYNAME/Desktop/MYFILESTOBEPROCESSED/", 
            polymers = c("PU", "EVAc", "PA", "PAN", "PBT", "PET", "PE", "PMMA", "PP", 
                         "POM", "PS", "PVC", "PC", "ABS", "PPSU", "CA", "PEEK", "EVOH", 
                         "PSU", "SI", "PLA", "PLAPBAT"))
```

### Keep the data in R as data frame
If you further want to proceed and analyse the data with R, you can set *dataReturn = TRUE*. The function will then return a data frame consisting of all measurements of all samples of the selected folder.
``` r
mydata <- evalPurency(path="C:/users/MYNAME/Desktop/MYFILESTOBEPROCESSED/", 
                      dataReturn = TRUE)
```
You can now proceed with *mydata* and do your analyses or plotting as usual.



## Use this package in another lab
The output of Purency can be customized to a certain extend and different labs may have different requirements and conventions on how to name things (also in the language this is done). Therefore, the default settings of this function apply for the animal ecology group of the University of Bayreuth. Unfortunately, there is no smart way to workaround. Thus, this function requires several parameters to preset the working environment of the lab.

To make it as generic and convenient to other labs working with this package, you can overwrite the default settings to your liking. In case you would prefer a complete preset for your lab, that you don't have to provide each parameter separately, I can implement a preset set to your desire if you open an issue or send me a request.


Parameters to be adjusted (just add the necessary parameters as additional parameter in the function call):

  - colPol = 6, Column number where the polymer type is stated. In the TOEKI lab this is column 6 (Class Name). Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
  - colL = 17, Column number for the particle length. In the TOEKI lab this is column 17 (Length [5µ]). Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
  - colReqPol = 24, Column number for the particle check, whether the particle is a polymer or not. In the TOEKI lab this is column 24 (Plastik? or Plastik ja/nein). Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
  - colShape = 25, Column number for the particle shape. In the TOEKI lab this is column 25 (Form). Could also be provided as column name, but only in ASCII encoding (e.g., special character = . and ä = d).
  - colCol = 26, Column number for the particle color In the TOEKI lab this is column 26 (Farbe). Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
  - colLFib = 27, Column number for the particle length in case of a fibre with corrected length (because of curvy shape). In the TOEKI lab this is column 27 (Länge). Could also be provided as column name, but only in ASCII encoding (e.g., special character as . and ä = d).
  - fibre = "Faser", How fibres are called in colShape (Form). In the TOEKI lab it is 'Faser'.
  - sphere = "Kugel", How spheres are called in colShape (Form). In the TOEKI lab it is 'Kugel'.
  - fragment = "Fragment", How fragments are called in colShape (Form). In the TOEKI lab it is 'Fragment'.
  - pixel = "Pixel", How pixels are called in colShape (Form). In the TOEKI lab it is 'Pixel'.

I used column numbers here to workaround issues with special characters in some languages which R might has problems with. Numbers provide an unambiguous delimiter.

With an adopted preset, you can now run the function as mentioned before (just add the preset always to the function)


## Troubleshooting:

The function was written in Oktober 2022. I've used the current column numbers to address the required information. If these will be changed by any chance, this should be adopted to the function as well (see 'Use this package in another lab' above to read how).
If not, the results are not reliable any more! 

The source code can be accessed via github.
I've commented it quite sophisticated, and changes should be quite easy when you're a bit into programming. Otherwise open an issue or send me a request, and I can do my best to implement your desired features.

If there are any issues or wishes for changes, you can send me a mail to info@maki-science.org or open an issue here on github (https://github.com/Maki-science/evalPurency/issues).


## Citation
To cite evalPurency in publications use:

Marvin Kiene (2022). evalPurency: Automated Evaluation of Purency Data. R package version 1.1.1.0002.
https://github.com/Maki-science/evalPurency


A BibTeX-entry for LaTeX-user is

  @Misc{,
    title = {evalPurency: Automated Evaluation of Purency Data},
    author = {Marvin Kiene},
    note = {R package version 1.1.1.0002},
    year = {2022},
    url = {https://github.com/Maki-science/evalPurency},
  }
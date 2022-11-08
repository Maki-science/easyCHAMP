
# evalPurency

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/evalPurency)](https://CRAN.R-project.org/package=evalPurency)
<!-- badges: end -->


Microplastics can be found in any part of an environmental or nutritional sample. Today, FPA-based FTIR imaging allows a whole sample to be measured quickly. However, to analyse the measurement data, a powerful tool is needed to reliably identify the polymer particles contained in the sample. The analysis must provide objective, complete and robust results, which are presented in detail and transparently. The Microplastics Finder by Purency uses machine learning algorithms that identify more than 20 polymer types within minutes. In combination with FTIR imaging, it offers the fastest method to examine an entire sample for microplastics. Thanks to automation, which does not require time-consuming manual post-processing, the analysis results are optimally comparable and reproducible. Microplastics analysis becomes scalable and new types of questions about the presence and origin of microplastics can be answered. 
*Source: https://www.purency.ai/product1/microplastics-finder)*


## Purpose of this package
Evaluate a bulk of .csv files in one directory, produced by Purency (https://www.purency.ai/). 
It will count occurences of fibres, fragments, spheres and pixels, as well as size fractions (<10, 10-20, 20-50, 50-100, 100-150, 150-200,..., >500) for each polymer. Each file (i.e., each measurement) is evaluated separately, as well as summarized for all files (i.e., one sample). For each sample a new excel file will be generated
    in the folder where the files are placed.

I've written this package for the TOEKI working group of the university of bayreuth, to allow simple and (almost) bulletproof application. If you are interested in adopting the framework for another lab with different specifications, let me know and I will see what I can do. Ideas for implementation for broader usability are appreciated.

## Installation

You can install the development version of evalPurency from [GitHub](https://github.com/) with:
(devtools only has to be installed once)
``` r
# install.packages("devtools") # just necessary of not already installed (remove the '#' at the beginning of line)
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

It will usually need just very few seconds to process e.g. 100 files. An excel file for each sample is created in the selected folder. On the first sheet, a summary of the whole sample is provided with the counts of particle form and size fractions. On the second sheet, each measurement is evaluated in case you want trace back the single measurements.

The Excel files are of an old version. Therefore, there might occur a message that you have to confirm.

Of course you can also use your local hard drive like so:
``` r
evalPurency(path="C:/users/MYNAME/Desktop/MYFILESTOBEPROCESSED/") # or similar
```

### Change evaluated polymers
At the current state, the function evaluates up to 22 polymers (see example).
If you would like to add polymers, or just evaluate some of them, you can overwrite the default setting by simply change the content of the 'c(...)' accordingly. 
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


## Troubleshooting:

The function was written in Oktober 2022. I've used the current column names to address the required information. If these will be changed by any chance, this should be adopted to the function as well.
If not, the results are not reliable any more! 

The source code can be accessed via github.
I've commented it quite sophisticated, and changes should be quite easy when you're a bit into
programming.

If there are any issues or wishes for changes, you can send me a mail to info@maki-science.org or open an issue here on github (https://github.com/Maki-science/evalPurency/issues).

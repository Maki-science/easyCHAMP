
# easyCHAMP

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/easyCHAMP)](https://CRAN.R-project.org/package=easyCHAMP)
[![R-CMD-check](https://github.com/Maki-science/easyCHAMP/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Maki-science/easyCHAMP/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Maki-science/easyCHAMP/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Maki-science/easyCHAMP?branch=main)
<!-- badges: end -->

## Go to [pkgdown website](https://maki-science.github.io/easyCHAMP/) for how to use this package
If you already know the package, but need more information, check out the package vignettes or go tp the [pkgdown website](https://maki-science.github.io/easyCHAMP/) (or the upper toolbar, in case you are already there). Click on *Get started*, or view the website articles to study all options. We've tried to gather all information there in a (hopefully) simple and userfriendly way. Checkout the article *Troubleshooting*, for potential issues that may occur, if you do not setup the functions properly.


## Software before application of this package
Microplastics (or microparticles) can be found in any part of an environmental or nutritional sample. Today, FPA-based FTIR imaging allows a whole sample to be measured quickly. However, to analyse the measurement data, a powerful tool is needed to reliably identify the polymer particles contained in the sample. The analysis must provide objective, complete and robust results, which are presented in detail and transparently. Various software (like the MicroplasticsFinder, MicroparticlesAI, or siMPle) use machine learning algorithms that identify more than 20 polymer types within minutes. In combination with FTIR imaging, it offers the fastest method to examine an entire sample for microplastics. Thanks to automation, which does not require time-consuming manual post-processing, the analysis results are optimally comparable and reproducible. Microplastics analysis becomes scalable and new types of questions about the presence and origin of microplastics can be answered. 
*Source: https://www.purency.ai/product1/microplastics-finder)*


## Purpose of this package
Commonly in microplastic research, a sample is splitted and filtered on several filters. These are scanned via µ-FTIR and the resulting data is analysed with various software (like the MicroplasticsFinder, MicroparticlesAI, or siMPle). These software produce *.csv files for each sample contaning all particles and their annotation, which is usually cured and refined by a human. Then, the files for each sample have to be corrected by blank results and summarized. And here is were we jump in: 

This package and its functions can be used to automate the blank processing, single file handling and gathering the relevant data for each sample into one file. The processing requires only a few seconds instead of hours (of copying and pasting) in manual work. Furthermore, the chance of mistakes is reduced by generic computation. By employing robust and conservative algorithms it is ensured, that the results are transparently and comparably produced and the amount of microplastic contamination is not overestimated.

(If desired) It will count occurrences of fibres, fragments, spheres and pixels, as well as different (customisable) size fractions for each polymer. Each file (i.e., each measurement) is evaluated separately, as well as summarized for all files (i.e., one sample). Also the blank correction and correction for filter division can be automated by this function. For each sample a new excel file will be generated in the folder where the files are placed in. One file for each sample and one additional file, containing the data of different processing steps (also raw data) to allow a full track of the processing, if required. If desired, the function returns a list object to the R environment with all produced data sets, which can be used for further processing (e.g., creating plots) in R.

The default parameter settings are based on the requirements of the animal ecology I group (AG Prof. Laforsch) of the University of Bayreuth. However, the function is highly customisable, and you can change all parameters to your desire. Read further below for detailed instruction what and how you can/should change. If desired, also presets can be incorporated in the package that a common workflow for a lab or collaboration can be ensured.


## Installation
After the corresponding paper is published, you should be able to download this package from CRAN (watch the badge on the top of this page). Just go to the upper tool bar of your RStudio and click *tools* > *Install Packages...* > tipe in *easyCHAMP* > click *install*, or run:
```
install.packages("easyCHAMP")
```


You can also install the development version of easyCHAMP from [GitHub](https://github.com/). If not already installed, you need to have [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed on your system to be able to compile the development version.
(devtools only has to be installed once)

```
install.packages("devtools") # just necessary if not already installed
devtools::install_github("Maki-science/easyCHAMP")
```
Once installed, you don't need to repeat this step each time! 
If you want to update to the newest version, just run the second line again.


## How it works
There are two main functions to be applied: ```easyCHAMP()``` for a blank corrected summary after your liking; and ```easyCHAMP.particles()``` for a particle-wise blank correction, with retention of single particles and their properties. 
Additionally, there are plenty of options for customization and settings of the *.csv files. 

Check out the package vignettes or go tp the [pkgdown website](https://maki-science.github.io/easyCHAMP/) (or the upper toolbar, in case you are already there). Click on *Get started*, or view the website articles to study all options. We've tried to gather all information there in a (hopefully) simple and userfriendly way.


## Citation
To cite easyCHAMP in publications use:

Marvin Kiene, Eva Vizsolyi Cseperke, Benedikt Hufnagl, Martin Löder and Christian Laforsch (2024). easyCHAMP: An automated Comparable and Harmonized Analyses of Micro-Particles. R package version 1.3.8.9021.
https://github.com/Maki-science/easyCHAMP


A BibTeX-entry for LaTeX-user is

  @Misc{,
    title = {easyCHAMP: An automated Comparable and Harmonized Analyses of Micro-Particles},
    author = {Marvin Kiene and Eva {Cseperke Vizsolyi} and Benedikt Hufnagl and Martin Löder and Christian Laforsch},
    note = {R package version 1.3.8.9021},
    year = {2024},
    url = {https://github.com/Maki-science/easyCHAMP},
  }

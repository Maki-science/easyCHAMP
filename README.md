
# easyCHAMP

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/easyCHAMP)](https://CRAN.R-project.org/package=easyCHAMP)
[![R-CMD-check](https://github.com/Maki-science/easyCHAMP/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Maki-science/easyCHAMP/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Maki-science/easyCHAMP/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Maki-science/easyCHAMP?branch=main)
<!-- badges: end -->

*Go to [pkgdown website](https://maki-science.github.io/easyCHAMP/articles/easyCHAMP.html) for getting started*



## Software prior of using this package
This package works on data that is produced with third-party software and focuses on microplastic analyses (but can also be used for various particles). 
Microplastics (or microparticles) can be found in any part of an environmental or nutritional sample. Today, FPA-based FTIR imaging allows a whole sample to be measured quickly. However, to analyse the measurement data, a powerful tool is needed to reliably identify the polymer particles contained in the sample. The analysis must provide objective, complete and robust results, which are presented in detail and transparently. Various software (like the MicroplasticsFinder, MicroparticlesAI, or siMPle) use machine learning algorithms that identify more than 20 polymer types within minutes. In combination with FTIR imaging, it offers the fastest method to examine an entire sample for microplastics. Thanks to automation, which does not require time-consuming manual post-processing, the analysis results are optimally comparable and reproducible. Microplastics analysis becomes scalable and new types of questions about the presence and origin of microplastics can be answered. 
*Source: https://www.purency.ai/product1/microplastics-finder)*


## Purpose of this package
Commonly in microplastic research, a sample is splitted and filtered on several filters. These are scanned via µ-FTIR and the resulting data is analysed with various software (like the MicroplasticsFinder, MicroparticlesAI, or siMPle). These software produce *.csv files for each sample contaning all particles and their annotation, which is usually cured and refined by a human. Then, the files for each sample have to be corrected with blanks and summarized, which can be a laborious and detious manual or semi-automated work that is also prone by human mistake. 
And here is were we jump in: 

This package and its functions can be used to automate sample/blank averaging, the blank processing, single file handling and gathering the relevant data for each sample into one file. The processing requires only a few seconds instead of hours (of copying and pasting) in manual work. Furthermore, the chance of mistakes is reduced by generic computation. *By employing robust and conservative algorithms it is ensured, that the results are transparently and comparably produced and the amount of microplastic contamination is not overestimated.*

(If desired) It will count occurrences of fibres, fragments, spheres and pixels, as well as different (customisable) size fractions for each polymer. Each file (i.e., each measurement) is evaluated separately, as well as summarized for all files (i.e., one sample). Also the blank correction and correction for filter division can be automated by this function. For each sample a new excel file will be generated in the folder where the files are placed in. One file for each sample and one additional file, containing the data of different processing steps (also raw data) to allow a full track of the processing. If desired, the function returns a list object to the R environment with all produced data sets, which can be used for further processing (e.g., creating plots) in R.

The default parameter settings are based on the requirements of the animal ecology I group (AG Prof. Laforsch) of the University of Bayreuth. However, the function is highly customisable, and you can change all parameters to your desire. Read further at the [pkgdown website](https://maki-science.github.io/easyCHAMP/articles/easyCHAMP.html) for detailed instruction what and how you can/should change. If desired, also presets can be incorporated in the package that a common workflow for a lab or collaboration can be ensured.


## How it works
There are two main functions to be applied: ```easyCHAMP()``` for a blank corrected summary after your liking; and ```easyCHAMP.particles()``` for a particle-wise blank correction, with retention of single particles and their properties (e.g., for physical simulations). Very briefly, if everything is set, it is sufficient to call one of these functions and provide a path where the files can be found. However, the naming of the files should follow a [certain structure](https://maki-science.github.io/easyCHAMP/articles/easyCHAMP.html#naming-workflow), to ensure proper processing of the samples and blanks. This way, the way how the files are processed is highly customisable and potentially all scenarios are covered by the alogrithm.
Additionally, there are plenty of options for customization and settings of the *.csv files. 

Check out the package vignettes or go to the [pkgdown website](https://maki-science.github.io/easyCHAMP/). Click on *Get started*, or view the website articles to study all options. We've tried to gather all information there in a (hopefully) simple and userfriendly way.
The following topics can be found at the [pkgdown website](https://maki-science.github.io/easyCHAMP/):

  * Get started:
    + [How to install](https://maki-science.github.io/easyCHAMP/articles/easyCHAMP.html#installation)
    + [How files should be named](https://maki-science.github.io/easyCHAMP/articles/easyCHAMP.html#naming-workflow)
    + [Two main processing ways](https://maki-science.github.io/easyCHAMP/articles/easyCHAMP.html#processing-possibilities)
      - [A polymer-shape-size wise summary](https://maki-science.github.io/easyCHAMP/articles/easyCHAMP.html#processing-a-summary-with-blank-correction)
      - [A particle-wise processing](https://maki-science.github.io/easyCHAMP/articles/easyCHAMP.html#perform-particle-wise-processing)
  * Customisations:
    + [Changing file specifications (e.g., decimal sign)](https://maki-science.github.io/easyCHAMP/articles/file-custom.html#changing-separator-and-decimal-sign-in--csv-files)
    + [Use different naming styles](https://maki-science.github.io/easyCHAMP/articles/file-custom.html#dont-use-the-recommended-file-naming)  + [Change evaluated polymers (or other substances)](https://maki-science.github.io/easyCHAMP/articles/option-custom.html#change-evaluated-polymers)
    + [Change sizeclasses](https://maki-science.github.io/easyCHAMP/articles/option-custom.html#change-evaluated-size-classes)
    + [Not interested in particle shape?](https://maki-science.github.io/easyCHAMP/articles/option-custom.html#Not-interested-in-shape?)
    + [What if only parts of the samples are analysed?](https://maki-science.github.io/easyCHAMP/articles/option-custom.html#you-analysed-only-parts-of-a-sample---integrate-division-factors)
    + [*easyCHAMP.particles() only:* A certain particle colour should be analysed separately (e.g., using spiked standard particles)!](https://maki-science.github.io/easyCHAMP/articles/option-custom.html#one-colour-of-your-particles-should-be-separated-from-the-rest-easychamp-particles-only)
    + [*easyCHAMP.particles() only:* All particles are the same, I don't want to set it manually!](https://maki-science.github.io/easyCHAMP/articles/option-custom.html#automatically-fill-missing-values-with-provided-default)
    + [Get results into R environment for further use (e.g., creating graphs)](https://maki-science.github.io/easyCHAMP/articles/output-custom.html#keep-the-data-in-r-as-data-frame)
    + [How to get column sums in each file](https://maki-science.github.io/easyCHAMP/articles/output-custom.html#summary-of-each-column-at-the-bottom)
    + [How to get an extra file about the total particle numbers in each sample](https://maki-science.github.io/easyCHAMP/articles/output-custom.html#get-total-particle-numbers)
   * [Troubleshooting: a list of common issues and mistakes](https://maki-science.github.io/easyCHAMP/articles/troubleshooting.html#troubleshooting)
    

## Citation
To cite easyCHAMP in publications use:

Marvin Kiene, Eva Vizsolyi Cseperke, Benedikt Hufnagl, Martin Löder and Christian Laforsch (2025). easyCHAMP: An automated Comparable and Harmonized Analyses of Micro-Particles. R package version 1.3.8.9021.
https://github.com/Maki-science/easyCHAMP


A BibTeX-entry for LaTeX-user is

  @Misc{,
    title = {easyCHAMP: An automated Comparable and Harmonized Analyses of Micro-Particles},
    author = {Marvin Kiene and Eva {Cseperke Vizsolyi} and Benedikt Hufnagl and Martin Löder and Christian Laforsch},
    note = {R package version 1.3.8.9021},
    year = {2025},
    url = {https://github.com/Maki-science/easyCHAMP},
  }

# evalPurency 1.3.8.9021
  - since some researches might not be interested in shape of particles, we included a possibility to ignore that feature. The parameter 'colShape' can now be set to 'none'. This will cause the functions to create an extra column 'shape' and the shape is set according to the parameter 'fragment'. This was a quick-and-dirty (but properly working) implementation of this feature, since the actual ignorance of shape is not as easy as this was originially thought as central feature of this package. Therefore, it is strongly embedded in the current code structure and cannot be easily jumped over.

# evalPurency 1.3.7.9021
  - The startrow (how to read in the files) is now automatically extracted for each file separately. Therefore, errors regarding the reading of the files should be gone now, even if one file has a different number of rows until the real data starts. Even though this increases the processing time by a few seconds (not all data is read), the user does not have to worry about this point anymore. However, automation can be disabled if a value is defined (e.g., for troubleshooting).

# evalPurency 1.3.7.9020
  - Adjusted the handling of colours to skip the colour check if colours are not to be separated (this prevents a warning popping up all the time for cases with no colour separation - which is most of the time)

# evalPurency 1.3.7.9019
  - Changes in the function feedback: Since *evalPurency.particles()* also takes use of colour, we have included a note, if colour is not set. However, since this is done by a helper function, shared with *evalPurency()*, this note is also thrown for *evalPurency()* for each file having no colour defined. We changed the code now, so that this note is just thrown once for consideration.
  - Minor addon (requested by Martin): the *processing_data.csv* now contains an additional sheet, reporting the provided division factors.
  - Bugfix: We found a bug in the rounding behaviour of the algorithm. Therefore, in the blank correction, the final result was not rounded, but the digits were cut of. We fixed this flaw and it should always be rounded to the upper integer number (to provide conservative values).
  - Issue found: The names should always be uniquely identifyable. For instance, having a blank called 'TESTBlank_filterx', and samples called 'TEST_filtery' and 'somethingelseTEST_filterz' would have a match for 'TEST' in both samples, even though they are thought be separately blank-corrected. Therefore, the 'test' is not clearly identifyable and the second sample should not include test, or the blank must be renamed more precisely. In such a case this leads otherwise to blank-correction for both of these samples. We have adjusted the vignettes and description on the naming procedure and added a *Warning* to this section informing about this issue.
    - Testsuite: Since we changed the output with an additional sheet in the *processing_data.csv*, we updated the testsuite accordingly.

# evalPurency 1.3.7.9018
  - minor addon (requested by Sarmite): Now, you can set default values for form and colour if it is not set in the data. This could be done in excel, of course, but now you can do it just on-the-flow for all files you have.
  
# evalPurency 1.3.6.9018
  - minor addon: There is now a test suite available for *evalPurency.particles()* to improve maintenance and quality control.
  
# evalPurency 1.3.5.9018
  - Bugfix: fixed an issue with *evalPurency.particles()* for more than one sample-blank pair at once. Previously it jumped over every second sample.
  
# evalPurency 1.3.5.9017
  - new feature: a new function *evalPurency.particles()* is now ready for use. Testing and description is still not properly done. Thus consider it as *Beta* so far. With this function, it is possible to have a particle wise processing, to keep each particle with its traits strackable, while still allowing a conservative blank correction. These particle lists are valuable, e.g., for simulations of particle physical properties or environmental distributions.
  
# evalPurency 1.2.5.9017

  - fixed bug: expanded the issue with no particles in blank measures. Now the function throws a warning and internally sets *noBlank = TRUE*. Thus no errors should occur anymore.
  - new feature: following a wish (Anja), you can now set *particleNumbers = TRUE*, if you wish to get an extra *.xls-file providing the total particle numbers and plastic-particle numbers for each file.
  
# evalPurency 1.2.5.9016

  - fixed an issue, which caused problems in processing files without any measured plastic particles. Additionally, a warning is thrown in such a case, if this was not intended (e.g., mistakenly empty column).
  
# evalPurency 1.2.5.9015

  - apparently it happens, that Purency produces a file, that has special characters or is differently encoded (I didn't figure out yet). This causes evalPurency to have issues with reading the files (since it is usually just one out of several files that is somewhat different). Before the function applied 'ASCII' encoding. With this update I switched to 'latin1'. 
  - Some restructuring of the code was applied in preparation of an additional functionality coming soon.

# evalPurency 1.2.5.9014

  - following a request (Eva), the function now retains the information on area and width of the particles and provides it in the processing data on the raw data sheet.
  
# evalPurency 1.2.5.9013

  - fixed a bug that caused an error message under rare circumstances. However, the calculation was not affected by that.

# evalPurency 1.2.5.9012

  - fixed a bug that led to the division of blank particles by the number of blank filters (even of one blank), that should be summed together. By that I extended the code (to make it work even better) so one can use several filters of one blank to sum and several blanks for one or more samples to be averaged. Find more about that in the updated readme.
  
# evalPurency 1.2.5.9011

  - we extended the functionality for the division factors to allow also a filterwise setup for the factors. Now you can choose the parameter *setDivFactor* to be *TRUE*, which is equal to *samplewise*, which now requests the factors samplewise (i.e., all filters of one sample have the same factor), or to *filterwise*, which requests the factors filterwise.

# evalPurency 1.2.5.9010

  - the function now checks whether the provided forms are found in the provided data. If there are discrepancies in a field, the respective line will be excluded and a Warning message will appear. Those unknown values caused wierd output tables.

# evalPurency 1.2.5.9009

  - added a minor feature: you can now set *noBlank = TRUE* to skip the blank processing. Thus, you don't require a blank in your folder.
  - added the parameter startrow. This should only be changed if you work with an experimental Purency version that changed the way how the excelfiles are saved.

# evalPurency 1.2.4.9009

  - a bug is fixed now that caused an error if missing values occured in the data (which was indicated by a warning message before). Now those rows containing missing data, are excluded from further processing.

# evalPurency 1.2.4.9008

  - added sep and dec as additional parameters that can be customized to the users' csv-file specifications
  - The division factors are now requested stepwise for each sample/blank instead of all samples at once
  - a bug is fixed now, that sometimes produced a minor discrepancy between the real sum and the shown total particle number.
  - Additionally to the total sample summary, the sample files also contain sheets for each particle shape with a shape-wise summary of the numbers.

# evalPurency 1.2.4.9005

  - added a new major feature: now the function can also perform the blank correction for the data provided (read the readme or description to find out more)
  - added a new minor feature: you can now choose with an additional parameter whether to incorporate a division factor (if you just used parts of your sample) for samples and blanks separately. The function will request these factors during processing to avoid confusion and mistakes during function call
  - Silicone particles have been not evaluated so far, since they were implemented as 'SI'. However, the Purency configuration in our lab was 'SILICONE'. This has been corrected and should be evaluated now.

# evalPurency 1.1.3.9004
  
  - changed the way how the column summary is calculated to support the following feature
  - added a new parameter. Now *eocsum* can be set TRUE (detault) or FALSE to get this line or skip this calculation.
  - added a new parameter. Now labpreset can be set accordingly for different labs, that not all parameters have to be changed/set manually for other labs. Labpresets can now be requested and implemented fast.
  - a new parameter was added. Now the user can set the desired size classes that should be evaluated. Now, virtually every parameters is programmed dynamically and can be customised.

# evalPurency 1.1.1.9003

  - added customizable parameters to adopt the function in other labs with other naming conventions and different preset

# evalPurency 1.1.1.9002

  - updated readme
  - content of measurement in sample summary is now "sample sum" instead of "sum"
  
# evalPurency 1.1.1.9001

  - evalPurency() now adds a row in the sample summary with the column sums (requested by Eva)

# evalPurency 1.1.0.9001

  - now reports when processing done

# evalPurency 1.1.0.9000

  - reworked as a package for better maintainability and issue handling

# evalPurency 1.0.0.9000

  - full functional version
  - only as a function available

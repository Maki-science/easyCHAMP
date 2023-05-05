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

# evalPurency 1.2.4.9005

  - added a new major feature: now the function can also perform the blank correction for the data provided (read the readme or description to find out more)
  - added a new minor feature: you can now chose with an additional parameter whether to incorporate a division factor (if you just used parts of your sample) for samples and blanks separately. The function will request these factors during processing to avoid confusion and mistakes during function call
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

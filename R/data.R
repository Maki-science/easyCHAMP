#' sample data set as a result of Purency's Microplastic Particle Finder
#'
#' Reflects the data after they have been read into the environment by evalPurency. They are used for testing purpose,
#' as well as for examples or testing by users.
#'
#' @format A data frame with 2313 rows and 9 variables:
#' \describe{
#'   \item{sample}{The name of the sample. A sample comprises one to several single measurements, that are summed up during the processing of evalPurency.}
#'   \item{measurement}{The name of the single measurement.}
#'   \item{className}{Contains the name of the polymer, described in this row.}
#'   \item{length}{The length of the particle measured by Purency.}
#'   \item{form}{Which form(shape) the particle has (sphere, fragment, pixel, fibre).}
#'   \item{color}{Color of the particle (not used, yet).}
#'   \item{lengthFibre}{If particle is a fibre, the lenght has to be measured manually. In that case, the length is stated here.}
#'   \item{area}{The area of the particle measured by Purency.}
#'   \item{width}{The width of the particle measured by Purency.}
#' }
#' @source \url{https://github.com/Maki-science/evalPurency}
"purencySampleData"



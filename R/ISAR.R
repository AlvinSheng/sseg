#' Homogeneous ISAR
#'
#' @param x Multitype point pattern
#' @param i Target species
#' @param r r vector
#' @param intensity Optional vector of intensities, see details.
#' @param CSR Include CSR? Only for homogeneous case.
#'
#' @details This function computes the ISAR function. The default action
#' is to compute the homogeneous version, that assumes all component patterns
#' are homogeneous (intensity is constant).
#'
#' For inhomogeneous computation the \code{intensity} can be given. It should be
#' a vector giving the type-wise intensity at each point, or a list of im-objects giving the
#' intensity fields per mark. See \code{density.ppp} or \code{density_pplist} how to generate one.
#'
#' CSR is not usually a good hypothesis: It assumes everything is homogeneous Poisson and independent.
#'
#' @seealso \code{\link{intensity_optimal}}
#'
#' @examples
#' data(lansing, package = "spatstat")
#' # inhomogeneous
#' i1 <- ISAR(lansing, i = "hickory")
#' l <- intensity_optimal(lansing)
#' i2 <- ISAR(lansing, i = "hickory", intensity = l$intensity)
#' plot(i1)
#' lines(i2)
#'
#' @import spatstat
#' @export

ISAR <- function(x, i, r, intensity, CSR=FALSE){
  if(!missing(intensity)) ISAR_inhom(x, i, r, intensity)
  else ISAR_homog(x, i, r, CSR)
}

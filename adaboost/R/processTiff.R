#' Reads and plots raster files of type .tif(GeoTIFF image format)
#'
#' The function reads raster or gidded data from a .tif file which is a GeoTIFF image
#' format containing additional georeferencing information for raster metadata.
#' This is redenred as pixels, where each pixel represents an area on Earth's surface.
#'
#' The function reads a Geotiff file and returns a SpatialGridDataFrame object after
#' further processing. A SpatialGridDataFrame is a SpatialGrid object extended with a
#' data slot occupied by a data.frame object filled with a single band of data
#' representing elevation in metres. RasterLayer object can be coerced to
#' SpatialGridDataFrame.
#'
#' It plots a Digital Surface Model(DSM) from the raster object which is a rectangular
#' grid showing specific features on the surface. The raster data frame and raster object
#' returned by the function can be fed into any machine learning algorithm for analysis of
#' feature extraction and classification.
#'
#' @usage processTiff(fileRas)
#'
#' @param fileRas GeoTIFF image format(.tif)
#' @return A list with components: raster DataFrame and raster object
#'
#' @importFrom raster raster plot
#' @importFrom sp plot summary
#' @importFrom stringr str_detect
#' @importFrom utils object.size
#' @importFrom methods as
#'
#' @export
#'
processTiff <- function(fileRas) {
  #Valid GeoTIFF format is .tif
  validTypes <- c("tif")
  checkType <- stringr::str_detect(fileRas,".tif")
  if (isFALSE(checkType)) stop("readTif: DataFile must of type .tif")

  raster::raster(fileRas) -> RasObject
  raster::plot(RasObject, main = "Digital Surface Model from raster data")
  object.size(RasObject)
  rasDf <- methods::as(RasObject, "SpatialGridDataFrame")
  sp::summary(rasDf)
  rasterDf <- methods::as(rasDf, "data.frame")
  returnList <- list("rasterDf" = rasterDf, "rasterTif" = RasObject)
}

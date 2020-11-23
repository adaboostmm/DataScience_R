#' Reads and height normalizes LiDAR files of type (.las or .laz or .LAS or .LAZ)
#'
#' The LAS file format is a public file format approved by the American Society
#' of Photogrammetry and Remote Sensing (ASPRS) Board on November 14 2011.
#' This binary file format is an alternative to the proprietary systems which
#' many companies use. The standard LAS format allows the files to be operating
#' system agnostic and thus providing a standard basis for further data analysis.
#'
#' The LAS file is intended to contain LIDAR (or other) point cloud data records.
#' The data will generally be put into LAS file format from software provided by
#' LiDAR hardware vendors. The LiDAR software combines GPS, IMU, and laser pulse
#' range data to produce X, Y, and Z point data. This open format allows different
#' LIDAR hardware and software tools to output data in a standard format.
#'
#' After reading the LAS file it is very important to normalize the point cloud dataset
#' with respect to height so that the height metrics of building, other structure or
#' trees are converted to a number relative to ground. LiDAR software measures any length
#' in meters.
#'
#' Height normalization of LAS file requires Classification of ground points.
#' Usually LiDAR software puts ground points as Classification value 0. Sometimes
#' this value may be absent. If ground Classification is absent in data, it can be
#' simulatedn using one of the available algorithms, Cloth Simulation Filtering (CSF).
#'
#' After the ground points are classified, point cloud LiDAR data can be height normalized
#' using one of the available algorithms, Triangulated Irregular Network(TIN).
#'
#' It is still an open research item as which algorithms are optimal for height normalization
#' of LiDAR files due to the complexity of the file format and any reasonable LAS file is
#' several gigabytes in size containing several billions of point cloud data.
#'
#' @usage readFile(fileName)
#'
#' @param fileName A File in (ASPRS) standard format(.las or .laz or .LAS or .LAZ)
#' @return A las object of class: LAS (LASF)
#'
#' @importFrom lidR readLAS lasnormalize tin
#' @importFrom stringr str_detect
#' @export
#'
#' @examples
#'#=======================================================================#
#'# Reading a LAS file
#'#=======================================================================#
#' dFile <- system.file("extdata", "project.las", package = "adaboost")
#' out <- readFile(dFile)
#'
readFile <- function(fileName) {
  checkType <- stringr::str_detect(fileName,".las|.laz|.LAS|.LAZ")
  if (isFALSE(checkType))
    {
      stop("readFile: DataFile must of type .las or .laz or .LAS or .LAZ")
    }
  readLas <- lidR::readLAS(fileName)
  if (readLas@data$Classification[1] == 0)
    {
      readLas <- lidR::lasground(readLas, lidR::csf(), last_returns = FALSE)
    }
  readLas <- lidR::lasnormalize(readLas, lidR::tin())
}


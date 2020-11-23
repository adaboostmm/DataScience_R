#' Prints S4 slots data and header from LiDAR files of type(.las or .laz or .LAS or .LAZ)
#'
#' This function reads a las object of class:LAS(LASF), prints the available S4 slots.
#' It prints a few important variables from S4 slot data and S4 slot header. It also
#' displays the coordinate Reference system(CRS) for the data file.
#'
#' @usage lasSlotPrint(lasObject)
#'
#' @param lasObject A las object of class:LAS (LASF) to be analyzed
#' @return Prints slots details from a las object
#'
#' @importFrom lidR readLAS
#' @importFrom methods slotNames
#'
#' @export
#' @examples
#'#=======================================================================#
#'# Prints slots from LiDAR file
#'#=======================================================================#
#' dFile <- system.file("extdata", "lasc04765150.las", package = "adaboost")
#' lasOut <- readFile(dFile)
#' lasSlotPrint(lasOut)
#'
lasSlotPrint <- function(lasObject) {
  slotName <- lapply(methods::slotNames(lasObject), function(x) base::paste(x, ","))
  slotData <- lapply(names(lasObject@data), function(x) paste(x, ",") )

  cat("LiDAR data comes with 4 slots => ", unlist(slotName), "\n",
      paste(rep("-",70), collapse = ""), "\n",
      "Slot data has following important variables => ", unlist(slotData)[1:4], "\n",
      unlist(slotData)[5:8], "\n",
      paste(rep("-",70), collapse = ""), "\n",
      "Important values from the Slot Header => ", "\n",
      paste(rep("-",70), collapse = ""), "\n",
      "File Creation Year = ", lasObject@header@PHB$`File Creation Year`, "\n",
      "Number of point records = ", lasObject@header@PHB$`Number of point records`, "\n",
      "Min X = ", lasObject@header@PHB$`Min X`, "\n",
      "Max X = ", lasObject@header@PHB$`Max X`, "\n",
      "Min Y = ", lasObject@header@PHB$`Min Y`, "\n",
      "Max Y = ", lasObject@header@PHB$`Max Y`, "\n",
      "Min Z = ", lasObject@header@PHB$`Min Z`, "\n",
      "Max Z = ", lasObject@header@PHB$`Max X`, "\n",
      paste(rep("-",70), collapse = ""), "\n",
      "Slot proj4string has the following Coordinates reference information => ", "\n",
      paste(rep("",70), collapse = ""), "\n",as.character(lasObject@proj4string),  "\n",
      paste(rep("-",70), collapse = ""), "\n"
      )
}

#' Extracts the contents of LAS(LASF) class object and creates tibbles from S4 slots data and header.
#'
#' This function extracts attributes from point cloud S4 slots Data and Header.
#' Some of the important attributes of Slot Data are: X, Y, Z values(these determine
#' point cloud coordinates), Standard GPS time, Intensity to store magnitude of laser
#' pulse return, Return Number for a given output laser pulse, Number of Returns for a
#' given laser pulse, Classification representing class attribute of the point
#' (Ground, Vegetation, Building etc.)
#'
#' It provides a lookup table to map standard Classification Key(0-9) to its meaning.
#' Attributes from S4 slot data is stored in a tibble with well defined column names
#' to facilitatefor any future analysis. Any LiDAR file can potentially contains billions
#' of point clouds, so S4 slot Data will have billions of attribute entries that represent
#' all point clouds.
#'
#' Point cloud S4 slot Header provides some of the important metadata:
#' File Signature LASF, File Source ID (e.g. original flight line number),
#' File Creation Year, Number of Point Clouds in the dataset, Min and Max
#' values of  of X,Y,Z coordinates. There will be only one entry in the slot
#' Header per file.
#'
#' This function provides answers to the following commonly asked questions for LiDAR data:
#' How many points are there in the Point Cloud Data Set?
#' What are the distinct return Nos?
#' What are the distinct No of Returns?
#' What type of Classification does his dataset contain?
#' What are the top 5 Intensity values?
#'
#' @usage lasSlotExtract(lasObject, fileName)
#'
#' @param lasObject A las object of class:LAS (LASF) to be analyzed
#' @param fileName A Fie in  (ASPRS) standard .las or .laz or .LAS or .LAZ format
#' @return A list with 9 components: tibbles, colnames, las metadata
#'
#' @importFrom lidR readLAS
#' @importFrom methods slotNames
#' @importFrom dplyr distinct %>% n top_n
#' @export
#'
lasSlotExtract <- function(lasObject, fileName) {
  ReturnNumber <- 0L
  NumberOfReturns <- 0L
  Classification <- 0L
  ClassificationKey <- "Unclassified"
  Intensity <- 0L
  X <- 0L
  Y <- 0L
  Z <- 0L
  gpstime <- 0L
  ScanAngle <- 0L

  lookup <- c("Unclassified",
              "Ground", "Low Vegetation", "Medium Vegetation",
              "High Vegetation", "Building", "Low Point (noise) ",
              "Model Key-point (mass point)", "Water",
              "Reserved", "Reserved", "Overlap Point",
              rep("Reserved", 19))

  dataTibble <-
    dplyr::tibble(
      fileId = rep(fileName, nrow(lasObject@data)),
      X = ifelse(!is.null(lasObject@data$X), lasObject@data$X, X),
      Y = ifelse(!is.null(lasObject@data$Y), lasObject@data$Y, Y),
      Z = ifelse(!is.null(lasObject@data$Z), lasObject@data$Z, Z),
      gpstime = ifelse(!is.null(lasObject@data$gpstime), lasObject@data$gpstime, gpstime),
      Intensity = ifelse(!is.null(lasObject@data$Intensity), lasObject@data$Intensity, Intensity),
      ReturnNumber = ifelse(!is.null(lasObject@data$ReturnNumber), lasObject@data$ReturnNumber, ReturnNumber),
      Classification = ifelse(!is.null(lasObject@data$Classification), lasObject@data$Classification, Classification),
      ScanAngle = ifelse(!is.null(lasObject@data$ScanAngle), lasObject@data$ScanAngle, ScanAngle),
      NumberOfReturns = ifelse(!is.null(lasObject@data$NumberOfReturns), lasObject@data$NumberOfReturns, NumberOfReturns),
      ClassificationKey =  ifelse(!is.null(lasObject@data$Classification), lookup[lasObject@data$Classification], ClassificationKey)
    )

  headerTibble <-
    dplyr::tibble(
      fileId = fileName, fileYear = lasObject@header@PHB$`File Creation Year`,
      noOfPoints = lasObject@header@PHB$`Number of point records`,
      minX = lasObject@header@PHB$`Min X`, maxX = lasObject@header@PHB$`Max X`,
      minY = lasObject@header@PHB$`Min Y`, maxY = lasObject@header@PHB$`Max Y`,
      minZ = lasObject@header@PHB$`Min Z`, maxZ = lasObject@header@PHB$`Max Z`)

  dbColnamesData <- c("fileid", "x" , "y", "z", "gpstime", "intensity", "returnnumber",
                    "classification", "scanangle", "numberofreturns", "classificationkey")

  dbColnamesHeader <- c("fileid", "fileyear", "noofpoints","minx", "maxx",
                      "miny", "maxy",
                      "minz", "maxz")

  dataTibble %>%
    base::nrow() -> totalPts

  dataTibble %>%
    dplyr::distinct(ReturnNumber) -> returnNo

  dataTibble %>%
    dplyr::distinct(NumberOfReturns) -> noOfReturns

  dataTibble %>%
    dplyr::select (Classification, ClassificationKey) %>%
    dplyr::group_by(Classification, ClassificationKey ) %>%
    dplyr::summarize(count = dplyr::n()) -> classificationMap

  dataTibble %>%
    dplyr::arrange(dplyr::desc(Intensity)) %>%
    dplyr::select(Intensity) %>%
    dplyr::top_n(5) -> top5Intensity

  lasMetadata <- list("dataTibble" = dataTibble, "headerTibble" = headerTibble,
                      "dbColnamesData" = dbColnamesData,
                      "dbColnamesHeader" = dbColnamesHeader,
                      "returnNo" = returnNo,
                      "noOfReturns" = noOfReturns,
                      "classificationMap" = classificationMap,
                      "totalPts" = totalPts,
                      "top5Intensity" = top5Intensity)
}

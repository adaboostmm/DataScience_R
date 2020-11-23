#' Algorithm for tree density core and Forest Classification
#'
#' This function calculates a list of the following important attributes related
#' to trees in a LAS file:
#' Area of XY coordinates, Tree coordinates(X, Y, height), Total no of trees,
#' Tree density score, Tree classification score and Rumple Index.
#' All of these attributes can be fed into a machine learning
#' algorithm to predict whether the file represents any of these categories:
#' (very dense forest of tall trees, dense forest of tall trees, tall sparse trees,
#' very dense bush, small sparse trees).
#'
#' This function reads a LAS object and extracts the tree coordinates from Canopy
#' Height Model(CHM). It caclulates Rumple Index using an existing algorithm.
#' Rumple Index is the ratio of canopy surface over ground surface; for a perfectly
#' flat surface, rumple_index = 1 and for rough surface, rumple_index > 1.
#'
#' It assigns a tree density score for a LAS file based on this algorithm:
#' Each tree is a vector with 3 components:
#' (X value, Y value and Height which is Z value).
#' This function prepares a tree tibble for all the trees vectors.
#' Calculates a tree distance matrix using L2 norm; sorts the columns.
#' Keep only those trees which are within 15 meter radius, calculate the column mean.
#' Output the tree density score for the LAS file as a mean of all column means of
#' the neighboring trees that are within 15 meters of distance.
#'
#' This function assigns a tree classification score for a LAS file using this method:
#' It calculate a quantile rank of the tree heights and tree classification score is
#' percentage of tall trees with quantile rank == 4 with respect to tree height.
#'
#' It extracts values of minimum and maximum of X coordinates for LAS file,
#' calculate rangeX. It extracts values of minimum and maximum of Y coordinates,
#' calculate rangeY. Then it calculates the area of the recangle XY as rangeX*rangeY.
#'
#' @usage treeDensityIndex(lasObject)
#'
#' @param lasObject A las object of class:LAS (LASF) to be analyzed
#' @return A list with 6 components: Area of XY coordinates, Tree coordinates,
#'     Total no of trees, Tree density score, Tree classification score,
#'     Rumple Index.
#'
#' @importFrom raster focal
#' @importFrom dplyr %>% mutate count ntile
#' @importFrom stats dist
#' @importFrom tibble tibble
#' @importFrom lidR grid_canopy rumple_index
#' @importFrom rLiDAR FindTreesCHM
#'
#' @export
#' @examples
#'#=======================================================================#
#'# Extracts tree attributes
#'#=======================================================================#
#' dFile <- system.file("extdata", "project.las", package = "adaboost")
#' lasOut <- readFile(dFile)
#' scoreOutput <- treeDensityIndex(lasOut)
#' RumpleIndex <- scoreOutput["RumpleIndex"]
#' treedensityScore <- scoreOutput["densityScore"]
#' treeclassificationScore <- scoreOutput["treeFreq"]
#' treeCount <- scoreOutput["treeCount"]
#' areaXY <- scoreOutput["areaXY"]
#' paste("RumpleIndex =", RumpleIndex)
#' paste("Total No of Trees =", treeCount)
#' paste("Tree Density Score =", treedensityScore)
#' paste("Tree Classification Score =", treeclassificationScore)
#' paste("Area of XY in square meter =", areaXY)
#'
treeDensityIndex <- function(lasObject) {
  height <- 0
  chm <- lidR::grid_canopy(lasObject, res = 0.5, lidR::p2r(0.2))
  rum <- lidR::rumple_index(chm)
  kernel <- matrix(1,3,3)
  chmT <- raster::focal(chm, w = kernel, fun = stats::median, na.rm = TRUE)
  treeList <- rLiDAR::FindTreesCHM(chmT, 3, 0.5)

  treeX <- treeList$x
  treeY <- treeList$y
  treeZ <- treeList$height

  treeTibble <- tibble::tibble(unlist(treeX), unlist(treeY), unlist(treeZ))
  treeDistance <- as.matrix(stats::dist(treeTibble, method = "euclidean",
                                        diag = TRUE, upper = FALSE))

  sortedTree <- apply(treeDistance, 2, function(x) sort(x)) # sort the col
  colMean <- apply(sortedTree, 2, function(x) mean(x[x < 15L]))
  densityScore <- base::mean(colMean)

  minXExtract <- lasObject@header@PHB$`Min X`
  maxXExtract <- lasObject@header@PHB$`Max X`
  minYExtract <- lasObject@header@PHB$`Min Y`
  maxYExtract <- lasObject@header@PHB$`Max Y`
  rangeX <- base::Map('-', maxXExtract, minXExtract)
  rangeY <- base::Map('-', maxYExtract, minYExtract)
  areaXY <- base::Map('*', rangeX, rangeY)

  treeZ %>%
    as.data.frame -> temp
  base::names(temp) <- "height"
  temp %>%
    dplyr::mutate(quantile_rank = dplyr::ntile(temp$height,4)) -> temp
  treeFreq <- base::sum(temp$quantile_rank == 4) / dplyr::count(temp)
  noOfTrees <- dplyr::count(temp)

  treeAttr <- list ("RumpleIndex" = rum, "treeCoordinates" = treeList, "densityScore" = densityScore,
        "treeCount" = noOfTrees, "treeFreq" = treeFreq, "areaXY" = areaXY)
}

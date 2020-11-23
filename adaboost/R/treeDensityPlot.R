#' 3D Scatter plot of trees within LAS rectangle
#'
#' This function reads the tree coordinates(X, Y, height) and creates a 3D scatter plot
#' of the trees. This plot helps to visualize, whether it is any of the following
#' categories:
#' (very dense forest of tall trees, dense forest of tall trees, tall sparse trees,
#' very dense bush, small sparse trees).
#'
#' @usage treeDensityPlot(treeX,treeY,treeZ)
#'
#' @param treeX R numeric vector
#' @param treeY R numeric vector
#' @param treeZ R numeric vector
#' @return Plot of tree density
#'
#' @importFrom scatterplot3d scatterplot3d
#'
#' @export
#' @examples
#'#=======================================================================#
#'# 3D scatter plot of trees from a LAS file
#'#=======================================================================#
#' dFile <- system.file("extdata", "project.las", package = "adaboost")
#' lasOut <- readFile(dFile)
#' scoreOutput <- treeDensityIndex(lasOut)
#' treeCoordinates <- scoreOutput["treeCoordinates"]
#' treeX <- treeCoordinates$treeCoordinates$x
#' treeY <- treeCoordinates$treeCoordinates$y
#' treeZ <- treeCoordinates$treeCoordinates$height
#' treeDensityPlot(treeX,treeY,treeZ)
#'
treeDensityPlot <- function(treeX,treeY,treeZ){
  scatterPlot <- scatterplot3d::scatterplot3d(treeX, treeY, treeZ, type = "h", color = "green",
                                              col.axis = "red", col.grid = "purple", col.lab = "blue",
                                              xlab = "Tree X Coord",
                                              ylab = "Tree Y Coord", zlab = "Tree Height")

}

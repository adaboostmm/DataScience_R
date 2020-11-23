#' Creates plots for Digital Elevation Model(DEM), Canopy Height Model(CHM) and TreeTops on CHM
#'
#' Digital Elevation Model(DEM) refers to the bare earth model with respet to vertical datum.
#' Vertical datum refers a surface of zero elevation to refer heights in LiDAR file.
#' Adaboost package has function readFile that reads and processes a LiDAR file, it returns a
#' LAS object that is already height normalized with respect to vertical datum.
#' Raw LiDAR points are not spaced on a regular grid as they are collected by zigzagging laser
#' pulses. Hence, point cloud is converted to grid or raster by resampling x,y coordinates of
#' LiDAR data.Then inverse distance weighted average(IDW) is computed and interpolated over
#' no pixels(no data).
#'
#' DEM is a raster or grid of elevation values that can be rendered to show the landscape.
#' Goal is to Achieve smallest pixel size while minimizing no of pixels with no shots in them.
#' Smaller pixels are desirable as they capture all extraordinary details in the change of
#' elevation or the shape of the surface.
#'
#' This function filters out non-ground points along with trees to build DEM using one of the
#' available algorithms from raster grid. Experiment shows that optimum result is obtained by
#' using search radius between .5 meter(m) to 1m.
#'
#' Canopy Height Model(CHM) regers to the distance between the ground and the top of the trees.
#' It represents the top of earth's surface which includes the trees that sit on the Earth, hence
#' CHM is also called Digital Surface Model(DSM).
#' It finds the height of the highest point and then replaces each point with 8 points around the
#' original to allow for virtual ‘emulation’ of the fact that each lidar point is more realistically
#' a disc.
#'
#' This function uses one of the existing algorithms grid_canopy based on basic triangulation and
#' rasterization. Points-to-raster(p2r) algorithm with a resolution of 0.5 meters is used.
#' It replacing each point by a 20-cm radius circle of 8 points.
#'
#' The function uses one of the available tree detection algorithm using rastor-based method that
#' shows clear plots of tree tops on CHM.
#'
#' @usage lasVisualize(lasObject, option)
#'
#' @param lasObject A las object of class:LAS (LASF) to be visualized
#' @param option A string that indicates the type of the plot
#' @return A plot based on the options provided
#'
#' @importFrom rasterVis gplot histogram
#' @importFrom raster focal plot terrain hillShade
#' @importFrom sp plot
#' @importFrom ggplot2 geom_point geom_tile scale_fill_gradient coord_equal ggtitle theme
#' @importFrom lidR height.colors p2r knnidw grid_canopy plot
#' @importFrom lidR dsmtin tree_detection grid_terrain
#' @export
#'
#' @examples
#'#=======================================================================#
#'# Visualize CHM or DEM or TreeTop on CHM
#'#=======================================================================#
#' dFile <- system.file("extdata", "project.las", package = "adaboost")
#' lasOut <- readFile(dFile)
#' lasVisualize(lasOut, "plotChm")
#' lasVisualize(lasOut, "plotDem")
#' lasVisualize(lasOut, "plotTreeTop")
#'
lasVisualize <- function(lasObject, option) {
  lasChm <- function(lasObject) {
    col <- lidR::height.colors(50)
    chm1 <- lidR::grid_canopy(lasObject, res = 0.5, lidR::p2r(0.2))
    kernel <- matrix(1,3,3)
    chmT <- raster::focal(chm1, w = kernel, fun = stats::median, na.rm = TRUE)
    value <- NULL
    ggObject <-
      rasterVis::gplot(chmT) +
      ggplot2::geom_tile(ggplot2::aes(fill = value)) +
      ggplot2::scale_fill_gradient(low = 'yellow', high = 'green') +
      ggplot2::coord_equal() +
      ggplot2::ggtitle("Digital Surface Model(Canopy Height Model)") +
      ggplot2::xlab("X axis RasterLayer") +
      ggplot2::ylab("Y axis RasterLayer") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(color="red", size=12, face="bold.italic"),
        axis.title.x = ggplot2::element_text(color="green", size=14, face="bold"),
        axis.title.y = ggplot2::element_text(color="#993333", size=14, face="bold")
      )
    print(ggObject)
  }

  lasTreetops <- function(lasObject) {
    col <- lidR::height.colors(50)
    chm1 <- lidR::grid_canopy(lasObject, res = 0.5, lidR::p2r(0.2))
    kernel <- matrix(1,3,3)
    chmT <- raster::focal(chm1, w = kernel, fun = stats::median, na.rm = TRUE)
    ttops <- lidR::tree_detection(chmT, lidR::lmf(5))
    raster::plot(chmT, col = lidR::height.colors(30),
                 main = "Tree Tops on Canopy Height Model",
                 xlab = "X axis RasterLayer",
                 ylab = "Y axis RasterLayer"
    )
    sp::plot(ttops, add = TRUE)
  }

  lasDem <- function(lasObject) {
    chm <- lidR::grid_canopy(lasObject, res = 0.5, lidR::p2r(0.3))
    ker <- matrix(1,3,3)
    lidarDem <- raster::focal(chm, w = ker, fun = mean, na.rm = TRUE)
    raster::plot(lidarDem,
                 main = "Digital Elevation MoDel")
  }

  txt <- c("plotChm", "plotTreeTop", "plotDem")
  if (option %in%  txt) {
    if (option == "plotChm") lasChm(lasObject)
    if (option == "plotDem") lasDem(lasObject)
    if (option == "plotTreeTop") lasTreetops(lasObject)
  }
  else cat("Invalid options.
           Valid options are: plotChm,plotDem,plotTreeTop")
}

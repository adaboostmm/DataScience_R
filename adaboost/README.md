
<!-- README.md is generated from README.Rmd. Please edit that file -->
adaboost
========

The goal of adaboost is to enable scientists and users to perform a wide array of exploratory data analysis for Light Detection and Ranging(LiDAR) data. It provides a collection of functions that users can follow through to gain understanding of LiDAR semantics. Adaboost package incorporates all preliminary data extraction and visulization steps for LiDAR files of type (.las or .laz or .LAS or .LAZ), provides functions to use a columnar storage like MonetDBLite to load large data files into database tables, presents a list of tree scores associated to the file that can be used as input for a Machine Learning Model related to feature extraction or vegetation segmentation.

Installation
------------

You can install the package adaboost as:

``` r
install.packages("adaboost")
```

Example
-------

Users will gain a complete understanding of the package by using the demonstration in the included vignette.

This is a basic example to demonstrate all the steps:

Note: This can be very time consuming for large files, hence you will need to setup a multi core cloud computing environment before trying out some of the examples using large LAS files.

``` r
library(adaboost)

smallFile <- "LASexample.las"
smallLas <- readFile(smallFile) # read the file
lasMetadata <- lasSlotExtract(smallLas, smallFile)

# Extract individual components
totalPts <- lasMetadata["totalPts"] # total point clouds 
returnNo <- lasMetadata["returnNo"] # type of returns 
noOfReturns <- lasMetadata["noOfReturns"] # total no of returns

# Type of Classiofication: ground, vegetation, building etc
classificationMap <- lasMetadata["classificationMap"] 
# Compositional information of surface material, vegetation gives high intensity
top5Intensity <- lasMetadata["top5Intensity"]
lasSlotPrint(smallLas) #prints metadata from S4 slots for the file

# Read a file with good vegetation information in it
gFile <- "project.las"
smallLas <- readFile(gFile)
lasMetadata <- lasSlotExtract(smallLas, gFile)
lasVisualize(smallLas, "plotDem") # build digital elevation model(DEM)
lasVisualize(smallLas, "plotChm") # build digital surface model(DSM) or Canopy Height Model(CHM)
lasVisualize(smallLas, "plotTreeTop") # tree tops on Canopy Height Model(CHM)

# Put the S4 slot data into a tibble
dataTibble <- lasMetadata["dataTibble"]$dataTibble
dataColName <- lasMetadata["dbColnamesData"]$dbColnamesData

# Try loading this into a table in MonetDBLite
loadMonetDB(dataTibble, dataColName, "slotdata" )
MonetDBLite::monetdblite_shutdown()

# Extract all tree attributes; these can potentially serve as input to future 
# Machine learning(ML) algorithm for feature extraction or vegetation segmentation

scoreOutput <- treeDensityIndex(smallLas)
rumpleIndex <- scoreOutput["RumpleIndex"] # Rumple Index
treeCoordinates <- scoreOutput["treeCoordinates"] # Tree coord(X, Y, Height)
treeDensityScore <- scoreOutput["densityScore"] # Tree Density Score
treeCount <- scoreOutput["treeCount"] # Total no of Trees
treeFreq <- scoreOutput["treeFreq"] # count(trees taller than 75th percentile of tree height)
areaXY <- scoreOutput["areaXY"] # rectangular area of the LAS file

# 3D Scatter plot of trees within LAS rectangle
treeX <- treeCoordinates$treeCoordinates$x
treeY <- treeCoordinates$treeCoordinates$y
treeZ <- treeCoordinates$treeCoordinates$height
treeDensityPlot(treeX, treeY, treeZ)
```

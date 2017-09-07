\name{country.regionTRAC}
\alias{country.regionTRAC}
\title{
Longitude and latitude tracking on administrative boundary map and subset of adminstrative boundary map
}
\description{
Plotting longitude and latitude on adminstrative boundary map of a country or a particular region of a country. This is particularly useful to track any point, visualize spreadness of points, identifying points fall outside the area of interest etc. It is also possible to map subset of any particular regions of interest.  

The function reads shapefile using readOGR function of the package ``rgdal" which requires both the directory location of shapefiles and the name of the shapefile (without the .shp extension). Reading shapefile using readOGR function creates an object of class ``SpatialPolygonsDataFrame"  which has been used to produce administrative boundary map of different regions.  
}

\usage{
country.regionTRAC(shapedir = "WorkingDir", shapename, boundary.label, spc.level, 
long.vector = NULL, lat.vector = NULL, cex = 0.8, point.col = "red", point.pch = 19, 
point.cex = 0.5, point.trc = FALSE)
}

\arguments{
  \item{shapedir}{
Directory path where the shape files were kept. If not specified, it assumes that the shape files are in the current working directory. Default is "WorkingDir".
}
  \item{shapename}{
Name of shape file excluding extension (.shp). Character. 
}
  \item{boundary.label}{
Boundary label vector name supplied from the shape file. See info.shape() to know this variable. Character. See details below.  
}
  \item{spc.level}{
Vector of area names for which sub-map will be produced. Vector of character(s). See details below.
}
  \item{long.vector}{
Vector of longitude. Default is NULL.
}
  \item{lat.vector}{
Vector of latitude. Default is NULL.
}
  \item{cex}{
Size of area label. Default is 0.8. See also par(). 
}
  \item{point.col}{
Color of longitude-latitude point(s). Default is "red".
}
  \item{point.pch}{
Symbol of longitude-latitude point(s).Default is 19. See also par(). 
}
  \item{point.cex}{
Size of longitude-latitude point(s). Default is 0.5. See also par(). 
}
  \item{point.trc}{
Logical. If TRUE corresponding values of longitude and latitude will also be shown in the map.
}
}

\details{ Object of class ``SpatialPolygonsDataFrame"  has different slots to hold polygons with attributes. A shapefile usually contains country subdivision or administrative division (province, district, upazila etc). The largest administrative subdivision is called ``first administrative level" followed by "second administrative level" and so on. If we want administrative boundary map of first administrative level having administrative areas ``X", ``Y" and ``Z", we need to specify, in boundary.label parameter, which column (column name) contains ``X", ``Y" and ``Z". 

Map of certain portion of country subdivision, for example areas ``X" and ``Z", can also be obtained by specifying the parameter spc.level.  See examples below. 

The above information of country subdivision columns and area names can be viewed by info.shape function. Use same spelling as viewed by info.shape() function.      
}
\references{
\url{www.gadm.org}, version 2.8, November 2015.

Pineda, M. C., Lopez-Legentil, S., & Turon, X. (2011). The whereabouts of an ancient wanderer: global phylogeography of the solitary ascidian Styela plicata. PLoS One, 6(9), e25495. DOI: 10.1371/journal.pone.0025495.

Zhao, K., Tung, C. W., Eizenga, G. C., Wright, M. H., Ali, M. L., Price, A. H., ... & McClung, A. M. (2011). Genome-wide association mapping reveals a rich genetic architecture of complex traits in Oryza sativa. Nature communications, 2, 467. doi:10.1038/ncomms1467. 

Bivand, R. S., Pebesma, E. J., Gomez-Rubio, V., & Pebesma, E. J. (2008). Applied spatial data analysis with R (Vol. 747248717). New York: Springer.

\url{http://www.caliper.com/Support/answer11.htm}

\url{http://www.fao.org/geonetwork/srv/en/metadata.show?id=12691&currTab=simple}

\url{http://ungiwg.org/coreDB}
}

\author{
Mazharul Islam, Jahidur Rahman Khan and Hasinur Rahaman Khan
}



\seealso{info.shape}
\examples{

filePATH <- system.file("Bangladesh", package="mapReasy")

country.regionTRAC(shapedir=filePATH, shapename="BGD_adm1",boundary.label="NAME_1",
spc.level=c("Khulna","Dhaka","Rangpur","Sylhet"))
}

\keyword{longitude-latitude track}

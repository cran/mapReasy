\name{map.country}
\alias{map.country}

\title{
Produce map, color map and disease map of a country
}
\description{
Produce map of a country including simple map, color-shaded map and disease maping. 

The function reads shapefile using readOGR function of the package ``rgdal" which requires both the directory location of shapefiles and the name of the shapefile (without the .shp extension). Reading shapefile using readOGR function creates an object of class ``SpatialPolygonsDataFrame"  which has been used to produce administrative boundary map of different regions.  
}
\usage{
map.country(shapedir = "WorkingDir", shapename, boundary.label = NULL, 
spe.vector = NULL, cex = 0.8, col = "lightblue")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shapedir}{
Directory path where the shape files were kept. If not specified, it assumes that the shape files are in the current working directory. Character. Default is "WorkingDir".
}
  \item{shapename}{
Name of shape file excluding extension (.shp). Character. 
}
  \item{boundary.label}{
Boundary label vector name supplied from the shape file. See info.shape() to know this input. Default is NULL.  See details below. 
}
  \item{spe.vector}{
Vector of region (names of areas). Default is NULL.  See details below. 
}
  \item{cex}{
Size of region name. Default is 0.8.
}
  \item{col}{
Color of shaded areas. Default is "lightblue". See color10() for more colors, particularly disease mapping.
}
}
\details{ Object of class ``SpatialPolygonsDataFrame"  has different slots to hold polygons with attributes. A shapefile usually contains country subdivision or administrative division (province, district, upazila etc). The largest administrative subdivision is called ``first administrative level" followed by "second administrative level" and so on. If we want administrative boundary map of first administrative level having administrative areas ``X", ``Y" and ``Z", we need to specify, in boundary.label parameter, which column (column name) contains ``X", ``Y" and ``Z". 

The above information of country subdivision columns and area names can be viewed by info.shape function. Use same spelling as viewed by info.shape() function.      
}

\references{
\url{www.gadm.org}, version 2.8, November 2015.

Pineda, M. C., Lpez-Legentil, S., & Turon, X. (2011). The whereabouts of an ancient wanderer: global phylogeography of the solitary ascidian Styela plicata. PLoS One, 6(9), e25495. DOI: 10.1371/journal.pone.0025495.

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

map.country(shapedir= filePATH, shapename="BGD_adm1")

}

\keyword{Mapping}
\keyword{Disease mapping}
\keyword{Color mapping}
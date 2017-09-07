\name{plotonMAP}
\alias{plotonMAP}
\title{
Comparison of different factors on administrative map with bar and pie charts
}
\description{
Two different types of plots (bar and pie charts) can be produced to make comparison of different factors on administrative map. 

The function reads shapefile using readOGR function of the package ``rgdal" which requires both the directory location of shapefiles and the name of the shapefile (without the .shp extension). Reading shapefile using readOGR function creates an object of class ``SpatialPolygonsDataFrame"  which has been used to produce administrative boundary map of different regions. 
}

\details{ Object of class ``SpatialPolygonsDataFrame"  has different slots to hold polygons with attributes. A shapefile usually contains country subdivision or administrative division (province, district, upazila etc). The largest administrative subdivision is called ``first administrative level" followed by "second administrative level" and so on. If we want administrative boundary map of first administrative level having administrative areas ``X", ``Y" and ``Z", we need to specify, in boundary.label parameter, which column (column name) contains ``X", ``Y" and ``Z". 

The above information of country subdivision columns and area names can be viewed by info.shape function. Use same spelling as viewed by info.shape() function.      
}

\usage{
plotonMAP(shapedir = "WorkingDir", shapename, boundary.label, all.label = FALSE, 
lab.cex = 0.8, plot.type = "bar", plot.data, plot.col, plot.label.col = "blue", 
pie.radius = 0.8, plot.size = c(0.5, 0.5))
}

\arguments{
  \item{shapedir}{
Directory path where the shape files were kept. If not specified, it assumes that the shape files are in the current working directory. Character. Default is "WorkingDir".
}
  \item{shapename}{
Name of shape file excluding extension (.shp). Character. 
}
  \item{boundary.label}{
Boundary label vector name supplied from the shape file. See info.shape() to know this variable. Character. See details below.  
}
  \item{all.label}{
Logical. If TRUE all area label will be shown.
}
  \item{lab.cex}{
Size of area label. Default is 0.8. 
}
  \item{plot.type}{
Type of plot. Default is bar chart. Another argument is "pie", which produce pie chart. 
}
  \item{plot.data}{
A data frame of variable of interest. See example for more details.
}
  \item{plot.col}{
Specify colors of respective plot.
}
  \item{plot.label.col}{
Label color of area. Default is "blue".
}
  \item{pie.radius}{
Radius of pie chart. Default is 0.8.
}
  \item{plot.size}{
Vector of size of plot. Default is  c(0.5, 0.5).
}
}

\author{
Mazharul Islam, Jahidur Rahman Khan and Hasinur Rahaman Khan
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


\seealso{map.bubble}
\examples{

filePATH <- system.file("Bangladesh", package="mapReasy")

dat <- data.frame(region=c("Dhaka","Khulna","Sylhet"),
baseline=c(10,20,30),endline=c(20,20,15))

plotonMAP(shapedir=filePATH, shapename="BGD_adm1",boundary.label="NAME_1",all.label
=TRUE,plot.type="bar",plot.data=dat,plot.col=c("red","green"),plot.label.col
="red",plot.size=c(0.6,0.6))

}

\keyword{Bar chart on map}
\keyword{Pie chart on map}
\keyword{Comparison on map}
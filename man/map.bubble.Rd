\name{map.bubble}
\alias{map.bubble}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Two way bubble plot
}
\description{
Bubble plot can be produced. It is also possible to produce two way bubble plot to visualize 2 different variables at the same time on this plot. One variable is shown by shaded region and other by bubble. This is mainly visualizing corresponding impact on different regions. For example, viewing ``no of potential customers" and ``message coverage" at different administrative areas. 

The function reads shapefile using readOGR function of the package ``rgdal" which requires both the directory location of shapefiles and the name of the shapefile (without the .shp extension). Reading shapefile using readOGR function creates an object of class ``SpatialPolygonsDataFrame"  which has been used to produce administrative boundary map of different regions.  
}
\usage{
map.bubble(shapedir = "WorkingDir", shapename, boundary.label, spe.vector 
= NULL, cex=0.8,col="lightblue",bubcex,bubpch=1,bublwd=2,bubcol="green")
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
Boundary label vector name supplied from the shape file. See info.shape() to know this input. Character. See details.  
}
  \item{spe.vector}{
Vector of character(names of areas) for which color shades will be implied. Default is NULL;  which gives only bubble plot. Specify this parameter to visualize one variable of interest (let, X).  
}
  \item{cex}{
Size of area label. Default is 0.8. See also par(). 
}
  \item{col}{
Colors of shaded regions. Specify this parameter for predefined spe.vector argument (for vaiable X). Default is "lightblue". See color10() for sequence of colors.   
}
  \item{bubcex}{
Bubble size (ususlly proportion to another variable of interest, say Y). See examples below.  
}
  \item{bubpch}{
Symbol of Bubble. Default is 1. See also par(). 
}
  \item{bublwd}{
Line width of bubble. Default is 2. See also par().
}
  \item{bubcol}{
Color of bubble. Default is "green". 
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

map.bubble(shapedir=filePATH, shapename="BGD_adm1",boundary.label="NAME_1",cex=0.8,
bubcol="green",bubpch=1,bublwd=2,bubcex=c(17,12,12,18,23,18,29)/3)
}

\keyword{Bubble plot on diseases map}
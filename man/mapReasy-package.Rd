\name{mapReasy-package}
\alias{mapReasy-package}
\alias{mapReasy}
\docType{package}
\title{
Producing Administrative Boundary Map with Additional Features Embedded
}
\description{
Produce administrative map, visualize and compare different factors on administrative map, tracking latitude and longitude, bubble plot.

The package provides some handy functions to produce different administrative maps easily. Functions are included to obtain colorful visualization of different regions of interest or sub-divisional administrative map at different levels. Main objective of the package is to increase feasibility of mapping disease pattern across different regions (disease mapping) with appropriate colors having intensity coherent with magnitude of prevalence (Lawson, 2001; DOI: 10.1002/sim.933). 

In many surveys, information on location of sample are collected. Sometimes it is of interest to quick look at the spreadness of the collected sample, check if any observation falls outside of the survey area and identify them. This package provides unique ways to perform these tasks easily. Besides, some additional features are available to make ad-lib comparison of different factors across regions through the maps. Visual presentation of two different variables on a particular map using two way bubble plot is an important feature. Simple bar chart and pie chart can be produced on maps to compare several factors. 

}
\details{
Produce country subdivision or administrative division map with colorful indication, partial mapping, tracking latitude and longitude on map, comparing different factors on map, disease mapping and bubble plot.

}
\author{
Mazharul Islam, Jahidur Rahman Khan and Hasinur Rahaman Khan

Maintainer: Hasinur Rahaman Khan <hasinurkhan@gmail.com>
}
\references{
\url{www.gadm.org}, version 2.8, November 2015.

Pineda, M. C., Lopez-Legentil, S., & Turon, X. (2011). The whereabouts of an ancient wanderer: global phylogeography of the solitary ascidian Styela plicata. PLoS One, 6(9), e25495. DOI: 10.1371/journal.pone.0025495.

Zhao, K., Tung, C. W., Eizenga, G. C., Wright, M. H., Ali, M. L., Price, A. H., ... & McClung, A. M. (2011). Genome-wide association mapping reveals a rich genetic architecture of complex traits in Oryza sativa. Nature communications, 2, 467. doi:10.1038/ncomms1467. 

Bivand, R. S., Pebesma, E. J., Gomez-Rubio, V., & Pebesma, E. J. (2008). Applied spatial data analysis with R (Vol. 747248717). New York: Springer.

Lawson, A. B. (2001). Disease map reconstruction. Statistics in medicine, 20(14), 2183-2204.

\url{http://www.caliper.com/Support/answer11.htm}

\url{http://www.fao.org/geonetwork/srv/en/metadata.show?id=12691&currTab=simple}

\url{http://ungiwg.org/coreDB}
}


\keyword{Disease mapping}
\keyword{longitude-latitude track}
\keyword{Bubble plot on diseases map}
\keyword{Comparison of factors on map}

\examples{
color10(5)

filePATH <- system.file("Bangladesh", package="mapReasy")
map.country(shapedir= filePATH, shapename="BGD_adm1",boundary.label="NAME_1")

map.country(shapedir= filePATH, shapename="BGD_adm1",boundary.label="NAME_1",
spe.vector=c("Dhaka","Sylhet","Khulna"),col=color10(3))
           
}

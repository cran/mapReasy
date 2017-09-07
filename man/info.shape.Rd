\name{info.shape}
\alias{info.shape}
\title{Extract information of shape file}
\description{
Extract and provide data features, different fields, country subdivisions and area names from shape file. This function is particularly useful to know "boundary.label" parameter input and spelling of current names of different areas. 
}

\usage{
info.shape(shapedir, shapename)
}
\arguments{
  \item{shapedir}{
Directory path where the shape files were kept. If not specified, it assumes that the shape files are in the current working directory. Character. Default is "WorkingDir".}
  \item{shapename}{
Name of shape file excluding extension (.shp). Character. 
}
}

\value{
List of values including boundary label parameter input, names of areas
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

\note{Shape files can be used academic purpose only}

\seealso{map.country}

\examples{
filePATH <- system.file("Bangladesh", package="mapReasy") 

info<-info.shape(shapedir= filePATH, shapename="BGD_adm1")

info
}

\keyword{Shape file information}

\name{color10}
\alias{color10}
\title{
Generate sequence of colors
}
\description{
A sequence of colors will be generated which have pattern of increasing intensity. 
}
\usage{
color10(n)
}
\arguments{
  \item{n}{
No of areas needed to be colored. A minimum value of 1 to the maximum value of 10 are allowed.
}
}
\details{
In many scenario, including disease mapping, different areas are needed to be colored with intensity proportion to frequency (or percentage). A set of such colors will be provided to shade different regions. 
}
\value{
Vector
}

\author{
Mazharul Islam, Jahidur Rahman Khan and Hasinur Rahaman Khan
}

\seealso{map.country}
\examples{
color10(5)
}
\keyword{colors}
\keyword{sequence of colors}
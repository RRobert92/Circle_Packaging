\name{RRobert92}
\title{Circle packing in a circle}
\description{
Generic function for the estimation of a circle packing in a circle.

The circle pacaging is based on equation: Circle_no = (pi * R^2)/(P * r^2).

Where P = pi/(0.7175*(R/r)^0.0529 ); P was established based on power trendline of circle density from http://www.packomania.com/
}
\usage{
Circle_Fit(x, y, type)
}
\arguments{
  \item{x}{Radius of a big circle as a positive numeric value.}
  \item{y}{Radius of a small circle as a positive numeric value.}
  \item{type}{A type of output to generate.}
}
\value{
If type missing (the default), the text (type = "text") is generated.
If type is (type = "data"), a dataframe with results is generated.
}
\examples{
Circle_Fit(0.45, 0.025, "text")
}

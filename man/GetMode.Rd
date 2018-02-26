\name{GetMode}
\alias{GetMode}
\title{Mode, orderly}
\usage{
GetMode(x, order, type)
}
\arguments{
\item{x}{object to be ordered.}
\item{order}{Order of modes.}
\item{type}{Type of returned value. Count, Value.}
}
\value{
The most frequent value or number of a random variable.
}
\description{
The most frequent value of a random variable. You can see the number or value of mode.
}
\examples{
# The number of second mode
GetMode(iris$Species, order=2, type='count')
# The value of first mode
GetMode(iris$Species, order=1, type='value')
}

\name{fcl}
\alias{fcl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The first columns of a list of two-column matrices
}
\description{
If we have a list of two-column matrices, it sets the elements of each column, consecutively. 
}
\usage{
fcl(b, warning = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{
a list of two-column matrices.
}
  \item{warning}{
logical. If \samp{TRUE}, fcl will displayes warning message 'number of columnns of all matrices in b must be two'. 
}
}
\details{
If even one of a matrices have a less or more than two columns, this function does not work correctly.
}
\value{
A list.
  \item{d1 }{a list of two components: a vector of the first columns of all matrices and a vector of the second columns of all matrices}
}
\author{
Neda Daneshgar and Majid Sarmad.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
unlist
}
\examples{
b=list(matrix(1:10,ncol=2),matrix(5:8,ncol=2),matrix(2:5,ncol=2)) ## a list of 3 two-column matrices
fcl(b)

}


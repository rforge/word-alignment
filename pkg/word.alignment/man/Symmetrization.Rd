\name{Symmetrization}
\alias{Symmetrization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculating heuristic word alignment named symmetrization
}
\description{
It calculates \bold{f} to \bold{e} and \bold{e} to \bold{f} alignments using IBM model 1, as well as heuristic alignment such as intersection, union or grow-diag.
}
\usage{
Symmetrization(file1, file2, method = c("union", "intersection", "grow-diag"), nrec = -1, iter = 4, lus = FALSE, lut = TRUE, la = TRUE, first = 1, last = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file1}{
the name of  source language file in training set.
}
  \item{file2}{
the name of  target language file in training set.
}
  \item{method}{
method of heuristic alignment (union, intersection or grow-diag alignment)
}
  \item{nrec}{
number of sentences to be read.If  -1, it considers all sentences.
}
  \item{iter}{
number of  iteration for IBM model 1.
}
  \item{lus}{
logical. If \samp{TRUE}, it will convert the first character of source language's  sentences.
}
  \item{lut}{
logical. If \samp{TRUE}, it will convert the first character of target language's  sentences.
}
  \item{la}{
logical. \samp{TRUE} means that one of the two languages is a right-to-left, so internet connection is necessary.
}
  \item{first}{
the first sentence that we want to have its word alignment.
}
  \item{last}{
the last sentence that we want to have its word alignment.
}
}
\details{
Here, word alignment is not only a map of target language to source language and it is considered as a heuristic alignment such as union or intersection or grow-diag alignment.
}
\value{
A list.
\item{time }{A number. (in second/minute/hour)}
\item{method }{method of heuristic alignment (union, intersection or grow-diag alignment).}
\item{alignment }{A list of alignment for each sentence pair.}
}
\references{
Koehn P. (2010), "Statistical machine translation",
Cambridge University , New York.

http://statmt.org/europarl/v7/bg-en.tgz
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
Note that we have a memory restriction and just special computers with high cpu and big ram can allocate the vectors of this function. Of course, it depends on corpus size.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
word_alignIBM1
}
\examples{
#Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
#so the aforementioned unzip files have been exported to http://www.um.ac.ir/~sarmad/... .

S1=Symmetrization ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
'http://www.um.ac.ir/~sarmad/word.a/euro.en',nrec=200,lus=TRUE,method='grow-diag')
}
\name{word-alignment-package}
\alias{word-alignment-package}
\alias{word-alignment}
\docType{package}
\title{
Finding word alignment using IBM model 1 for a given parallel corpus
}
\description{
For a given Sentence-Aligned Parallel Corpus, it aligns words in every sentence pair.  Further, it builds a suggested dictionary of two languages based on given corpus.
}
\details{
\tabular{ll}{
Package: \tab word-alignment\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-11-15\cr
License: \tab What license is it under?\cr
}
}
\author{
Neda Daneshgar and Majid Sarmad.

Maintainer: Neda Daneshgar <ne_da978@stu-mail.um.ac.ir>
}
\note{To run it, internet connection is needed.}
\references{
P. Koehn, "Statistical machine translation",
Cambridge University , New York, 2010.

http://statmt.org/europarl/v7/bg-en.tgz
}

\keyword{ package }
\seealso{
NLP

}
\examples{
#Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
#so the aforementioned unzip files have been exported to http://www.um.ac.ir/~sarmad/... .

word_alignIBM1('http://www.um.ac.ir/~sarmad/europarl-v7.bg-en.en',
'http://www.um.ac.ir/~sarmad/europarl-v7.bg-en.bg',nrec=2000,lu2=TRUE,lang2='BULGARIAN')
}
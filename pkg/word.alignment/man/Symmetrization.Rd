\name{Symmetrization}
\alias{Symmetrization}

\alias{print.symmet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculating Symmetric Word Alignment
}
\description{
It calculates source-to-target and target-to-source alignments using IBM model 1, as well as symmetric word alignment models such as intersection, union or grow-diag.
}
\usage{
Symmetrization(file_train1, file_train2, 
               nrec = -1, iter = 4, ul_s = FALSE, ul_t = TRUE, 
               intrnt = TRUE, method = c("union", "intersection", "grow-diag")) 
               


\method{print}{symmet}(x, ...) 
}

\arguments{
  \item{file_train1}{
the name of  source language file in training set.
}
  \item{file_train2}{
the name of  target language file in training set.
}
  \item{nrec}{
number of sentences to be read.If  -1, it considers all sentences.
}
  \item{iter}{
number of  iteration for IBM model 1.
}
  \item{ul_s}{
logical. If \samp{TRUE}, it will convert the first character of source language's  sentences. When source language is a right-to-left, it can be \samp{FALSE}.
}
  \item{ul_t}{
logical. If \samp{TRUE}, it will convert the first character of target language's  sentences. When target language is a right-to-left, it can be \samp{FALSE}.
}
  \item{intrnt}{
logical. \samp{TRUE} means that one of the two languages is a right-to-left, so internet connection is necessary.
}
  \item{method}{
symmetric word alignment method (union, intersection or grow-diag alignment).
}
  
   \item{x}{
an object of class \code{"symmet"}.
  }
   \item{\dots}{ further arguments passed to or from other methods. }
}
\details{
Here, word alignment is not only a map of target language to source language and it is considered as a symmetric alignment such as union or intersection or grow-diag alignment.
}
\value{
\code{Symmetrization} returns an object of class \code{"symmet"}.

An object of class \code{"symmet"} is a list containing the following components:

\item{time }{A number. (in second/minute/hour)}
\item{method }{symmetric word alignment method (union, intersection or grow-diag alignment).}
\item{alignment }{A list of alignment for each sentence pair.}
}
\references{
Koehn P. (2010), "Statistical Machine Translation.",
Cambridge University, New York.

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

\dontrun{

S1 = Symmetrization ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                    'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                    nrec = 200, ul_s = TRUE, method = 'grow-diag'
                    intrnt = FALSE)
}
}
\name{fix.gold}
\alias{fix.gold}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructing a Crosstabulate Matrix of Source Language Words vs Target Language Words of a Given Sentence Pair
}
\description{
It constructs a crosstabulate matrix of source language words vs target language words of a given sentence pair to be filled by an expert (Sure|Possible : 1|2) or based on an external word-alignment software (3).
}
\usage{
fix.gold(sou, tar, nrec = -1, mlen = 40, lus = FALSE, lut = TRUE, kk)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sou}{
the name of source language file in test set.
}
  \item{tar}{
the name of target language file in test set.
}
  \item{nrec}{
number of sentences to be read. If  -1, it considers all sentences.
}
  \item{mlen}{
a maximum length of sentences. Long sentences will be removed to save the time.
}
  \item{lus}{
logical. If \samp{TRUE}, it will convert the first character of source language's  sentences.
}
  \item{lut}{
logical. If \samp{TRUE}, it will convert the first character of target language's  sentences.
}
  \item{kk}{
an integer. The number of which sentence pair that we want to crosstab its matrix. 
}
}
\details{
If we want to evaluate our word-alignment results, the matrix that is constructed by this function will be filled by an expert with codes 1 or 2 for Sure or Possible alignments, While if we want to evaluate alignment based on an external word-alignment software or even another method, this matrix is filled by an expert with code '3'.
}
\references{
Holmqvist M. and Ahrenberg L. (2011). "A Gold Standard for English-Swedish Word Alignment." NODALIDA 2011 Conference Proceedings, pp. 106 - 113

Och F. and Ney H.(2003). "A Systematic Comparison Of Various Statistical Alignment Models." 2003 Association for Computational Linguistics, J03-1002, 29(1).
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
In case of non-ascii problem, you can use \samp{consExcel} function instead.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
consExcel
}
\examples{
\dontrun{
 fix.gold('http://www.um.ac.ir/~sarmad/word.a/source1.txt',
 'http://www.um.ac.ir/~sarmad/word.a/target1.txt',nrec=5,kk=3)
 }
}

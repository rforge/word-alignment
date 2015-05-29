\name{mydictionary}
\alias{mydictionary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Building a Suggested Dictionary
}
\description{
It builds a suggested dictionary of two languages based on given Sentence-Aligned Parallel Corpus.
}
\usage{
mydictionary(file1,file2,iter = 10,nrec = -1,lus = FALSE,lut = TRUE,prob = 0.9,l1 = "Farsi",l2 = "English",la = TRUE,dtfile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file1}{
the name of source language file in training set.
}
  \item{file2}{
the name of target language file in training set.
}
  \item{iter}{
number of  iteration for IBM model 1. The higher iteration builds more precise dictionary than the lower one.
}
  \item{nrec}{
number of sentences to be read.If  -1, it considers all sentences.
}
  \item{lus}{
logical. If \samp{TRUE}, it will convert the first character of target language's  sentences.
}
  \item{lut}{
logical. If \samp{TRUE}, it will convert the first character of source language's sentences. 
}
  \item{prob}{
to build mydictionary we need this probability. The higher probability builds more precise dictionary than the lower one.
}
  \item{l1}{
source language's name in mydictionary.
}
  \item{l2}{
traget language's name in mydictionary.

}
  \item{la}{
logical. \samp{TRUE} means that one of the two languages is a right-to-left, so internet connection is necessary.
}
  \item{dtfile}{
if \samp{NULL}, we did not save data.table (dd1) already and we have to run it. If an address exists,  means that data.table(dd1)  was saved  and we use this saved data.table and we do not need to calculate it, again.
}
}
\details{
The results depend on the corpus. As an example, we used English-Persian parallel corpus named Mizan which consists of more than 1,000,000 sentence pairs with a size about 170 Mb. If all sentences are considered, it takes  1.391593 hours using a computer with cpu: hpcompack-i73930 and Ram: 8*8 G = 64 G and the suggested dictionary is not very good. But for the 10,000 first sentences it would be perfect, while it just take 1.356784 mins using an ordinary computer. The results have been reported in 

\code{http://www.um.ac.ir/~sarmad/word.a/mydictionary.pdf}
}

\value{
A list.
%%  If it is a LIST, use
\item{time }{A number. (in second/minute/hour)}
 \item{number_input }{An integer.}
 \item{iterIBM1 }{An integer.}
 \item{dictionary }{A matrix.}
}
\references{
Supreme Council of Information and Communication Technology. (2013). Mizan English-Persian Parallel Corpus. Tehran, I.R. Iran. Retrieved from http://dadegan.ir/catalog/mizan.

http://statmt.org/europarl/v7/bg-en.tgz
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
Note that we have a memory restriction and just special computers with high cpu and big ram can allocate the vectors of this function. Of course, it depends on corpus size.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
#Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
#so the aforementioned unzip files have been exported to http://www.um.ac.ir/~sarmad/... .

\dontrun{
mydictionary('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
'http://www.um.ac.ir/~sarmad/word.a/euro.en',n=2000,lus=TRUE,
lang1='BULGARIAN',la = FALSE)
}
}

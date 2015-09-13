\name{mydictionary}
\alias{mydictionary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Building a Suggested Dictionary
}
\description{
It builds a suggested dictionary of two languages based on given sentence-aligned parallel corpus.
}
\usage{
mydictionary (file_train1,file_train2, 
              nrec = -1, iter = 10, prob = 0.9, 
              minlen=5, maxlen = 40, ul_s = FALSE, ul_t = TRUE, 
              lang1 = 'Farsi', lang2 = 'English', intrnt = TRUE, 
              dtfile = NULL, f1 = 'fa', e1 = 'en')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_train1}{
the name of source language file in training set.
}
  \item{file_train2}{
the name of target language file in training set.
}
   \item{nrec}{
number of sentences to be read.If  -1, it considers all sentences.
}
   \item{iter}{
number of  iteration for IBM model 1. The higher iteration builds more precise dictionary than the lower one.
}
  \item{prob}{
to build mydictionary we need this probability. The higher probability builds more precise dictionary than the lower one.
}
  \item{minlen}{
a minimum length of sentences.
}
  \item{maxlen}{
a maximum length of sentences.
}
  \item{ul_s}{
logical. If \samp{TRUE}, it will convert the first character of target language's  sentences. When source language is a right-to-left, it can be \samp{FALSE}.
}
  \item{ul_t}{
logical. If \samp{TRUE}, it will convert the first character of source language's sentences. When target language is a right-to-left, it can be \samp{FALSE}. 
}
  \item{lang1}{
source language's name in mydictionary.
}
  \item{lang2}{
traget language's name in mydictionary.
}
  \item{intrnt}{
logical. \samp{TRUE} means that one of the two languages is a right-to-left, so internet connection is necessary.
}
  \item{dtfile}{
if \samp{NULL}, we did not save data.table (dd1) already and we have to run it. If an address exists,  means that data.table(dd1)  was saved  and we use this saved data.table and we do not need to calculate it, again.
}
\item{f1}{
it is an abbreviation of source language (default = \samp{'fa'}).
}
  \item{e1}{
it is an abbreviation of target language (default = \samp{'en'}).
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

%% ~Make other sections like Warning with \section{Warning }{....} ~S
\examples{
#Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
#so the aforementioned unzip files have been exported to http://www.um.ac.ir/~sarmad/... .

\dontrun{

mydictionary ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
              'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
              nrec = 2000, ul_s = TRUE, lang1 = 'BULGARIAN', 
              intrnt = FALSE)
}
}
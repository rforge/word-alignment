\name{word_alignIBM1}
\alias{word_alignIBM1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finding word alignment using IBM model 1 for a given parallel corpus.
}
\description{
For a given Sentence-Aligned Parallel Corpus, it aligns words in every sentence pair.  Further, it builds a suggested dictionary of two languages based on given corpus.
}
\usage{
word_alignIBM1(filename1, filename2, nrec = -1, maxlength = 40, lu1 = TRUE, lu2 = FALSE, iter = 10, lang1 = "English", lang2 = "Farsi", prob = 0.9, first = 1, last = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename1}{
path of the target language.
}
  \item{filename2}{
path of the source language.
}
  \item{nrec}{
number of sentences to be read.If  -1, it considers all sentences.
}
  \item{maxlength}{
a maximum length of sentences. Long sentences will be removed to save the time.
}
  \item{lu1}{
 logical. If \samp{TRUE}, it will convert the first character of target language's  sentences.
}
  \item{lu2}{
logical. If \samp{TRUE}, it will convert the first character of source language's sentences. 
}
  \item{iter}{
number of  iteration for IBM model 1.
}
  \item{lang1}{
traget language's name in mydictionary.
}
  \item{lang2}{
source language's name in mydictionary.
}
  \item{prob}{
to build mydictionary we need this probability. The higher probability builds more precise dictionary than the lower one.
}
  \item{first}{
the first sentence that we want to have its word alignment.
}
  \item{last}{
the last sentence that we want to have its word alignment.
}
}
\details{
Here, word alignment is a map of target language to source language. 

To run this function, it is essential to use RmTokenizer function. So, internet connection is necessary.

The results depend on the corpus. As an example, we used English-Persian parallel corpus named Mizan which consists of more than 1,000,000 sentence pairs with a size about 170 Mb. If all sentences are considered, it takes 4.071 hours using a computer with cpu: hpcompack-i73930 and Ram: 8*8 G = 64 G and word alignment is good but the suggested dictionary is not very good. But for the 10,000 first sentences, the suggested dictionary would be almost good, although the word alignment might not be good. In fact, it is sensitive to the original translation type (lexical or conceptual). The results have been reported in 

\code{http://www.um.ac.ir/~sarmad/example-word_alignIBM1.pdf}

}
\value{
A list.
%%  If it is a LIST, use
  \item{number_input_sentences}{An integer.}
  \item{number_used_sentences}{An integer.}
  \item{time }{A number. (in second/minute/hour)}
  \item{iteribm1 }{An integer.}
 \item{word_align }{A list of word alignment for every sentence pair.}
\item{mydictionary }{A matrix.}
}
\references{
P. Koehn, "Statistical machine translation",
Cambridge University , New York, 2010.

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

word_alignIBM1('http://www.um.ac.ir/~sarmad/europarl-v7.bg-en.en',
'http://www.um.ac.ir/~sarmad/europarl-v7.bg-en.bg',nrec=3000,lu2=TRUE,lang2='BULGARIAN')
}

\name{preparData}
\alias{preparData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initial Preparations of Bitext before Word Alignment and Evaluation
}
\description{
For a given Sentence-Aligned Parallel Corpus, it prepars sentence pairs as an input for \samp{word_alignIBM1} and \samp{Evaluation1} functions in this package.
}
\usage{
preparData (file1, file2, 
            nrec = -1, maxlen = 40, ul_s = FALSE, 
            ul_t = TRUE, all = FALSE, intrnt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file1}{
the name of source language file.
}
  \item{file2}{
the name of target language file.
}
  \item{nrec}{
number of sentences to be read.If  -1, it considers all sentences.
}
  \item{maxlen}{
a maximum length of sentences. Long sentences will be removed to save the time.
}
  \item{ul_s}{
logical. If \samp{TRUE}, it will convert the first character of source language's  sentences. When source language is a right-to-left, it can be \samp{FALSE}.
}

  \item{ul_t}{
logical. If \samp{TRUE}, it will convert the first character of target language's  sentences. When target language is a right-to-left, it can be \samp{FALSE}.
}

  \item{all}{
logical. If \samp{TRUE}, it considers the third argument (\samp{lower = TRUE}) in \samp{culf} function.
}  
  \item{intrnt}{
logical. \samp{TRUE} means that one of the two languages is a right-to-left, so internet connection is necessary.
}
}
\details{
It balances between source and target language as much as possible. For examples, it removes extra blank sentences and equalization sentence pairs. It also removes long sentences to save the time and using \samp{culf} function it converts the first letter of each sentence into lowercase, as well as it removes all punctuation characters by \samp{RmTokenizer} function. Moreover, if \samp{word_align = FALSE}, this function divide each sentence into its words.   
}
\value{
A list.
%%  If it is a LIST, use
  
  if \samp{word_align = TRUE}
  \item{initial }{An integer.}
  \item{used }{An integer.}
  \item{sen_pair }{A matrix.}
  if \samp{word_align = FALSE}
  \item{length1 }{An integer.}
  \item{source.tok }{A list of words for each source sentence.}
  \item{target.tok }{A list of words for each target sentence.}
}
\references{
Koehn P. (2010), "Statistical machine translation",
Cambridge University , New York.
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
Note that if there are not a lot of proper nouns in your text string, we suggest you to set \samp{all=TRUE} to convert all text to lowercase.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Evaluation1, culf, RmTokenizer, word_alignIBM1
}
\examples{
\dontrun{

aa1 = preparData ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                  'http://www.um.ac.ir/~sarmad/word.a/euro.en', 
                   nrec = 20, intrnt = FALSE)
}
}

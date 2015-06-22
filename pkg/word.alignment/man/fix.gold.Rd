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
fix.gold (tst.set_sorc, tst.set_trgt, 
          method = c("gold","aligns"), nrec = -1, 
          maxlen = 40, ul_s = FALSE, ul_t = TRUE, num)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tst.set_sorc}{
the name of source language file in test set.
}
  \item{tst.set_trgt}{
the name of target language file in test set.
}
  \item{method}{
it consists of two arguments. If \samp{"gold"}, it considers the message corresponding to gold standard (i.e. "Now, press 'Enter' to continue and edit the matrix to enter Sure/Possible alignments (Sure=1,Possible=2)"). If \samp{"aligns"}, it 
it considers the message corresponding to another alignment (i.e. "Now, press 'Enter' to continue and edit the matrix to enter 3 for alignments").
}
  \item{nrec}{
number of sentences to be read. If  -1, it considers all sentences.
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
  \item{num}{
an integer. The number of which sentence pair that we want to crosstab its matrix. 
}
}
\details{
If we want to evaluate our word-alignment results, the matrix that is constructed by this function will be filled by an expert with codes 1 or 2 for Sure or Possible alignments, While if we want to evaluate alignment based on an external word-alignment software or even another method, this matrix is filled by an expert with code 3.
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
 
 fix.gold ('http://www.um.ac.ir/~sarmad/word.a/source1.txt',
           'http://www.um.ac.ir/~sarmad/word.a/target1.txt',
            nrec = 5, num = 3)
 }
}

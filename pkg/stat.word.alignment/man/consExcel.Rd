\name{consExcel}
\alias{consExcel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create an excel file from two sentences
}
\description{
It creates an excel file from two sentences of two languages to help the user for constructing a gold standard such that he/she can set 1 or 2 for sure or possible alignments.
}
\usage{
consExcel(source1, target1, gold = TRUE, aligns = FALSE, out1 = 'gold.xlsx', out2 = 'align.xlsx', nrec = -1, mlen = 40, lus = FALSE, lut = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{source1}{
the name of source language file in test set.
}
  \item{target1}{
the name of target language file in test set.
}
  \item{gold}{
logical. If \samp{TRUE}, it creates a separated excel file of test set to fill up its sheets with 1|2 for sure|possible alignment.
}
  \item{aligns}{
logical. If \samp{TRUE}, it creates a separated excel file of test set to fill up its sheets with '3' as an alignment.
}
  \item{out1}{
the name of the excel file for gold standard.
}
  \item{out2}{
the name of the excel file for alignment.
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
}
\details{
The first step for evaluation of word alignment quality is creating a gold standard. This function makes an excel file with \samp{nrec} sheets of a test set consists of source and target languages. Each sheet consists of the words of the target sentence as its rows and the words of the source sentence as its columns. To create a gold standard, it can be filled by Sure/Possible alignments (Sure=1, Possible=2).

Sometimes, the user calculates word alignments using another software or even aother method and he/she wants to evaluate such alignment with this package. So, this function can help him/her in this way, it creates a separated excel file in 'out2.xlsx' (as a default: 'D:/align.xlsx') and it can be filled by number '3' for alignments.
}
\value{
One or two excel file in 'out1' or 'out2' file.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Holmqvist M. and Ahrenberg L. (2011). "A Gold Standard for English-Swedish Word Alignment." NODALIDA 2011 Conference Proceedings, pp. 106 - 113

Och F. and Ney H.(2003). "A Systematic Comparison Of Various Statistical Alignment Models." 2003 Association for Computational Linguistics, J03-1002, 29(1).
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
If you have not the non-ascii problem, you can use \samp{fix.gold} function instead.
}

\seealso{
fix.gold
}%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
## Not run:

consExcel('http://www.um.ac.ir/~sarmad/word.a/source1.txt','http://www.um.ac.ir/~sarmad/word.a/target1.txt',nrec=5)

consExcel('http://www.um.ac.ir/~sarmad/word.a/source1.txt','http://www.um.ac.ir/~sarmad/word.a/target1.txt', nrec=5, aligns= TRUE )

consExcel('http://www.um.ac.ir/~sarmad/word.a/source1.txt','http://www.um.ac.ir/~sarmad/word.a/target1.txt', nrec=5, gold=FALSE, aligns= TRUE )

## End(Not run)
}

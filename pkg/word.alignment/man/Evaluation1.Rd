\name{Evaluation1}
\alias{Evaluation1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluation of word alignment quality
}
\description{
It evaluates word-alignment quality based on precision, recall and AER criteria.
}
\usage{
Evaluation1(file1, file2, source1, target1, method = c("fix", "Excel"), excel1, excel2, align1 = c("myalign", "an.align"), dtfile = NULL, nrec = -1, nlen = -1, mlen = 40, lus = FALSE, lut = TRUE, iter = 3, la = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file1}{
the name of source language file in training set.
}
  \item{file2}{
the name of target language file in training set. 
}
  \item{source1}{
the name of source language file in test set.
}
  \item{target1}{
the name of target language file in test set.
}
  \item{method}{
it consists of two argumants. If \samp{'fix'}, it uses \samp{fix.gold} function to achieve gold standard. If \samp{'Excel'}, it uses \samp{consExcel} function to achieve gold standard.
}
  \item{excel1}{
the name of the excel file for gold standarad.
}
  \item{excel2}{
the name of the excel file for alignment.
}
  \item{align1}{
it consists of two argumants. If \samp{'myalign'}, the user applies the \samp{word_alignIBM1} function in this package. If \samp{'an.align'}, the user applies another software or even another method to word alignment.
}
  \item{dtfile}{
if \samp{'NULL'}, we did not save data.table (dd1) already and we have to run it. if an address exists,  means that data.table(dd1)  was saved  and we use this saved data.table and we do not need to calculate it, again.
}
  \item{nrec}{
number of sentences in the training set to be read. If  -1, it considers all sentences.
}
  \item{nlen}{
number of sentences in the test set to be read. If  -1, it considers all sentences.
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
  \item{iter}{
number of  iteration for IBM model 1.
}
  \item{la}{
logical. \samp{TRUE} means that one of the two languages is a right-to-left, so internet connection is necessary.
}
}
\value{
A list.
%%  If it is a LIST, use
 \item{Recall }{An integer.}
 \item{Precision}{An integer.}
 \item{AER }{An integer.}
}
\references{
Koehn P.(2010). "Statistical machine translation",
Cambridge University , New York.

Och F. and Ney H.(2003)."A Systematic Comparison Of Various Statistical Alignment Models." 2003 Association for Computational Linguistics, J03-1002, 29(1).

Wang X. "Evaluation of Two Word Alignment Systems", Final Thesis, Department of Computer and Information Science.
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
Note that we consider two auxiliary functions (\samp{fix.gold} or \samp{consExcel}) to help the user to enter gold standard (reference alignment) or alignment, easily.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
fix.gold, consExcel
}

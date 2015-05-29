\name{Evaluation1}
\alias{Evaluation1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluation of Word Alignment Quality
}
\description{
It evaluates word-alignment quality based on precision, recall and AER criteria.
}
\usage{
Evaluation1(file1,file2,sou,tar,method =c("fix","Excel"),e1='g.xlsx',e2='a.xlsx',agn=c("myagn","an.agn"),dtfile=NULL,nrec= -1,nlen= -1,mlen=40,lus=FALSE,lut=TRUE,iter=3,la=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file1}{
the name of source language file in training set.
}
  \item{file2}{
the name of target language file in training set. 
}
  \item{sou}{
the name of source language file in test set.
}
  \item{tar}{
the name of target language file in test set.
}
  \item{method}{
it consists of two arguments. If \samp{'fix'}, it uses \samp{fix.gold} function to achieve gold standard. If \samp{'Excel'}, it uses \samp{consExcel} function to achieve gold standard.
}
  \item{e1}{
the name of the excel file for gold standarad.
}
  \item{e2}{
the name of the excel file for alignment.
}
  \item{agn}{
it consists of two arguments. If \samp{'myagn'}, the user applies the \samp{word_alignIBM1} function in this package. If \samp{'an.agn'}, the user applies another software or even another method to word alignment.
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
\details{
To evaluate word-alignment quality, we need to a reference alignment (a gold standard for the word-alignment) of a test set. Two methods to enter this gold standard is considered. When \samp{method = 'fix'}, means that \samp{fix.gold} function is called and the user should press any key to continue and edit the matrix to enter Sure/Possible alignments (Sure=1,Possible=2). Furthermore, when the user applies another software or even another method to word-alignment, he/she should set \samp{align1 = 'an.align'}, and he/she based on another word-alignment's  results should press any key to continue and edit the matrix to enter '3' for alignment. (Note that for each sentence pair, one matrix is created.) 

If \samp{method = 'Excel'}, means that the created excel file of \samp{consExcel} is used. In this method, the aforementioned excel file should be completed by an expert with codes 1 or 2 for Sure or Possible alignments first and then this excel file named excel1 (as a default, 'g.xlsx') is set as an input argument. Moreover, to evaluate word-alignment quality using another software or even another method, the user can be used excel2 file (as a default, 'a.xlsx') that had been completed by '3' for alignments.    
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
%% ~Make other sections like Warning with \section{Warning }{....} ~
\note{
Note that we have a memory restriction and just special computers with high cpu and big ram can allocate the vectors of this function. Of course, it depends on corpus size.
}
\seealso{
fix.gold, consExcel
}

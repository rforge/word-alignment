\name{word.alignment-package}
\alias{word.alignment-package}
\alias{word.alignment}
\docType{package}
\title{
Finding word alignment using IBM model 1 for a given parallel corpus and its evaluation
}
\description{
For a given Sentence-Aligned Parallel Corpus, it aligns words for each sentence pair. It considers many-to-one alignment in \samp{word_alignIBM1} function and symmetrization alignment in \samp{Symmetrization} function. Moreover, it evaluates a word alignment resulted from \samp{word_alignIBM1} function or from another software or even another method. It also builds a suggested dictionary of two languages using the given corpus.
}
\details{

\tabular{ll}{
Package: \tab stat.word.alignment\cr
Type: \tab Package\cr
Version: \tab 1.0.1\cr
Date: \tab 2015-05-20\cr
License: \tab GPL (>= 2)\cr
}

}
\author{
Neda Daneshgar and Majid Sarmad.

Maintainer: Neda Daneshgar <ne_da978@stu-mail.um.ac.ir>
}
\references{
Koehn P. (2010), "Statistical machine translation",
Cambridge University , New York.

Lopez A (2008). "Statistical Machine Translation." ACM Computing Surveys, 40(3).

Peter F. , Brown J., (1990). "A Statistical Approach to Machine Translation." Computational Linguistics, 16(2), 79(85).

Supreme Council of Information and Communication Technology. (2013). Mizan English-Persian Parallel Corpus. Tehran, I.R. Iran. Retrieved from http://dadegan.ir/catalog/mizan.

http://statmt.org/europarl/v7/bg-en.tgz

Och F. and Ney H.(2003). "A Systematic Comparison Of Various Statistical Alignment Models." 2003 Association for Computational Linguistics, J03-1002, 29(1).

Wang X. "Evaluation of Two Word Alignment Systems", Final Thesis, Department of Computer and Information Science.
}

\keyword{ package }
\seealso{
NLP
}
\examples{
#Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
#so the aforementioned unzip files have been exported to http://www.um.ac.ir/~sarmad/... .

ww = word_alignIBM1('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
'http://www.um.ac.ir/~sarmad/word.a/euro.en',nrec=2000,lus=TRUE)

ss=Symmetrization ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
'http://www.um.ac.ir/~sarmad/word.a/euro.en',nrec=50,lus=TRUE,method='intersection')
}

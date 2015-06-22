\name{word_alignIBM1}
\alias{word_alignIBM1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finding One-to-Many Word Alignment Using IBM Model 1 for a Given Parallel Corpus
}
\description{
For a given sentence-aligned parallel corpus, it aligns words in each sentence pair. Moreover, it calculates expected length and vocabulary size of each language (source and taget language) and it shows word translation probability as a table.
}
\usage{
word_alignIBM1 (file_train1, file_train2,
                f1 = 'fa',e1 = 'en',nrec = -1, 
                iter = 4, maxlen = 40,ul_s = FALSE, 
                ul_t = TRUE, sym = FALSE, input = FALSE, 
 		            dtfile = NULL, display = c("word1","number"),
                intrnt = TRUE, first = 1, last = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_train1}{
the name of source language file in training set.
}
  \item{file_train2}{
the name of the target language file in training set.
}
 \item{f1}{
it is an abbreviation of source language (default = \samp{'fa'}).
}
 \item{e1}{
it is an abbreviation of target language (default = \samp{'en'}).
}  
 \item{nrec}{
number of sentences to be read. If  -1, it considers all sentences.
}
  \item{iter}{
number of iteration for IBM model 1.
}
  \item{maxlen}{
a maximum length of sentences. Long sentences will be removed to save the time.
}
  \item{ul_s}{
logical. If \samp{TRUE}, it will convert the first character of source language's  sentences. When source language is a right-to-left, it should be \samp{FALSE}.
}
  \item{ul_t}{
logical. If \samp{TRUE}, it will convert the first character of target language's  sentences. When target language is a right-to-left, it should be \samp{FALSE}.
}
  \item{sym}{
logical. If \samp{TRUE}, the output can be used by \samp{Symmetrization} function.
}
  \item{input}{
logical. If \samp{TRUE}, the output can be used by \samp{mydictionary} and \samp{Evaluation1} functions.
}
  \item{dtfile}{
to run this function for the first time, it must be assigned to \samp{NULL}. In this case, the function will automatically save required data.table (it is necessary for obtaining MLE of IBM model1's parameters.) with a name which is combination of \samp{f1}, \samp{e1}, \samp{nrec} and \samp{iter} as "f1.e1.nrec.iter.RData".
Note that, the abovementioned name must not been changed. For the next times, it is sufficient to set \samp{dtfile} by any character, e.g. "a", "textfile" or "myproject".  
 }
 \item{display}{
it consists of two arguments. If \samp{'word1'}, alignments are exhibited as words and when \samp{'number'} is considered, alignments exhibits as numbers.
}
  \item{intrnt}{
logical. \samp{TRUE} means that one of the two languages is a right-to-left, so internet connection is necessary.
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

The results depend on the corpus. As an example, we used English-Persian parallel corpus named Mizan which consists of more than 1,000,000 sentence pairs with a size about 170 Mb. If all sentences are considered, it takes about 1.105531 hours using a computer with cpu: hpcompack-i73930 and Ram: 8*8 G = 64 G and word alignment is good. But for the 10,000 first sentences, the word alignment might not be good. In fact, it is sensitive to the original translation type (lexical or conceptual). The results have been reported in 

\code{http://www.um.ac.ir/~sarmad/word.a/example_wordalignIBM1.pdf}
}
\value{
A list.
%%  If it is a LIST, use
 
 if  \samp{sym = TRUE}
 \item{ef}{A list of integer vectors.} 
 
 if  \samp{input = TRUE}
 \item{dd1}{A data table}
 
 if \samp{sym = FALSE}  and  \samp{input = FALSE}
 \item{number_input_sentences}{An integer.}
 \item{number_used_sentences}{An integer.}
 \item{time }{A number. (in second/minute/hour)}
 \item{iteribm1 }{An integer.}
 \item{expended_l_source }{A non-negative real number.}
 \item{expended_l_target }{A non-negative real number.}
 \item{VocabularySize_source }{An integer.}
 \item{VocabularySize_target }{An integer.}
 \item{word_translation_prob }{A data table.}
 \item{word_align }{A list of one-to-many word alignment for each sentence pair.
 It starts with \samp{first} upto \samp{last} sentence pairs. Furthermore, it displays word alignments for the last six sentence pairs.}
}
\references{
Koehn P. (2010), "Statistical machine translation",
Cambridge University , New York.

Lopez A (2008). "Statistical Machine Translation."ACM Computing Surveys, 40(3).

Peter F. , Brown J., (1990). "A Statistical
Approach to Machine Translation." Computational Linguistics, 16(2), 79(85).

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

\seealso{
Evaluation1, Symmetrization, mydictionary
}
\examples{
#Since the extraction of  bg-en.tgz in Europarl corpus is time consuming, 
#so the aforementioned unzip files have been exported to http://www.um.ac.ir/~sarmad/... .

\dontrun{

word_alignIBM1 ('http://www.um.ac.ir/~sarmad/word.a/euro.bg',
                'http://www.um.ac.ir/~sarmad/word.a/euro.en',
                 nrec = 3000, ul_s = TRUE,intrnt = FALSE)
}
}

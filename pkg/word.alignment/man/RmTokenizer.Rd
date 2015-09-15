\name{RmTokenizer}
\alias{RmTokenizer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Punctuation Characters and Tokenization
}
\description{
It removes all punctuation characters from a given text and splits it into separated words.
}
\usage{
RmTokenizer(text, intrnt = TRUE, split = FALSE, rmBlank = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
an object.
}
  \item{intrnt}{
logical. \samp{TRUE} means that one of the two languages is a right-to-left, so internet connection is necessary.
}
  \item{split}{
logical. If \samp{TRUE}, it will split the text into separated words.
}
  \item{rmBlank}{
logical. If \samp{TRUE}, it removes blank words in the text, unless blanks located at the beginning or end of a sentence.
}
}
\details{
This function also considers numbers as a separated word. 

Assume that there is a text containing some sentences, by applying this function on a whole text, output will be a character string and if we set \samp{split = TRUE}, result is one component list. If we want to tokenize by sentences, we have to split each sentence first and then apply \samp{RmTokenizer} on each the sentence. (For more details, see examples below.)
}
\value{
A vector or a list of characters or a character string. If the input is a matrix and \samp{split = TRUE} , the output is a list of character vectors. (For more details, see examples below.)
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
When \samp{Rmtokenizer} removes punctuations, it creates blanks instead of them and if the user wants to remove these blanks, he/she can set \samp{rmBlank=TRUE} and of course he/she should notice that this function can not remove blanks in the first or the last of each object.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
MC_tokenizer (tm), whitespace_tokenizer (NLP)
}
\examples{
b1='Word-alignment is used by phrase-based systems to extract phrase pairs[2].'
 
b2='There are several methods for word alignment: generative and discriminative.'

b3='For the first time, Brown et. al [1] introduced IBM models.'

m1=matrix(c(b1,b2,b3))
RmTokenizer(m1,intrnt = FALSE)
RmTokenizer(m1,split = TRUE,intrnt = FALSE)
RmTokenizer(m1,rmBlank = TRUE, intrnt = FALSE)
RmTokenizer(m1,rmBlank = TRUE, split=TRUE,intrnt = FALSE)

# m2 is a text with multiple sentences.
m2='It is based on IBM models. The problem is that we do not have word-aligned data' 
RmTokenizer(m2,intrnt = FALSE) # It is one vector
RmTokenizer(m2,split = TRUE, intrnt = FALSE) # It is an object list

s1=strsplit(m2,'[.]')
RmTokenizer(s1, intrnt = FALSE) #It is a list with 3 objects

#l1 is a list
l1=list('A vector or a list of characters.','(For more details, see examples below.)' )

RmTokenizer(l1,intrnt = FALSE)         # It is not a list.
RmTokenizer(l1,split=TRUE,intrnt = FALSE) # It is a list.
RmTokenizer(l1,split=TRUE,rmBlank=TRUE,intrnt=FALSE) #Not removing blank at the first.
}
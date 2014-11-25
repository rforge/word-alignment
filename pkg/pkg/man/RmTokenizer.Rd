\name{RmTokenizer}
\alias{RmTokenizer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove punctuation characters and tokenization
}
\description{
It removes all punctuation characters from a given text and splits it into separated words.
}
\usage{
RmTokenizer(text, split = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
an object.
}
  \item{split}{
logical. If \samp{TRUE}, RmTokenizer will split the text into separated words.
}
}
\details{
This function also considers numbers as a separated word.
}
\value{
A vector or a list of characters. If the input is a matrix and \samp{split = TRUE} , the output is a list of character vectors. (For more details, see examples below.)
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
To run this function, internet access is needed.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
tokenizer (tm), whitespace-tokenizer (NLP)
}
\examples{
b1='Word alignment is used by phrase-based systems to extract phrase pairs from trining data and build tables of possible translations of phrase [2].'
 
b2='There are several methods for word alignment that are divided into two groups: generative word alignment and discriminative word alignment models.'

b3='The former is usually based on IBM models.'

b4='For the first time, Brown et. al [1] introduced IBM models.'

m1=matrix(c(b1,b2,b3,b4))
RmTokenizer(m1)

#l1 is a list
l1=list('A vector or a list of characters.', 'If the input is a matrix and split is TRUE, the output is a list of character vectors.', '(For more details, see examples below.)' )

RmTokenizer(l1)         # It is not a list.
RmTokenizer(l1,split=T) # It is a list.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

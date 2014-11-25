
\name{culf}
\alias{culf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make a string's first n characters lowercase
}
\description{
Converts uppercase to lowercase letters for the first n characters  of a character string.
}
\usage{
culf(x, n = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a character string.
}
  \item{n}{
an integer. Number of characters that we want to convert.
}
}
\details{
If we have a text of several sentences and we want to convert to lowercase the first n letters of every sentence separately, then we have to split text to sentences and apply the function for every sentence. (see the examples below)
 
If we have a list, it works correctly.
}
\value{
A character string.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
tolower
}
\examples{
# x is a list

x=list('This paper discribes the word alignment based on IBM model 1 for an English-Persian Parallel
Corpus (Mizan).','ALIGNMENT is a link between English and Persian words.')

culf(x, n=8) ## culf(x, n=8) is not a list
x='Machine translation (MT) is the automatic translation of a text in one language into another. Statistical Machine Translation (SMT) is one of the methods of MT that is based on statistical models.'

culf(x)   ## only run for the first sentence

u1=unlist(strsplit(x, ". ", fixed = TRUE))
sapply(1:length(u1),function(x)culf(u1[x])) ## run for all sentences
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

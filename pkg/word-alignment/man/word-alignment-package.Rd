\name{word-alignment-package}
\alias{word-alignment-package}
\alias{word-alignment}
\docType{package}
\title{
Finding word alignment using IBM model 1 for a given parallel corpus
}
\description{
For a given Sentence-Aligned Parallel Corpus, it aligns words in every sentence pair.  Further, it builds a suggested dictionary of two languages based on given corpus.
}
\details{
\tabular{ll}{
Package: \tab word-alignment\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-11-15\cr
License: \tab What license is it under?\cr
}
}
\author{
Neda Daneshgar and Majid Sarmad.

Maintainer: Neda Daneshgar <ne_da978@stu-mail.um.ac.ir>

}
\note{To run it, internet connection is needed.}
\references{
P. Koehn, "Statistical machine translation",
Cambridge University , New York, 2010.

Supreme Council of Information and Communication Technology. (2013). Mizan English-Persian Parallel Corpus. Tehran, I.R. Iran. Retrieved from http://dadegan.ir/catalog/mizan.
}
\keyword{ package }
\seealso{
NLP

}
\examples{
#It is as a result of Mizan English-Persian parallel corpus (only the 10000 first sentence pairs).

$number_input_sentences
[1] 10000

$number_used_sentences
[1] 9446

$time
Time difference of 1.848863 mins

$iteribm1
[1] 10

$word_align
$word_align[[1]]
 [1] "afterwards ÈÚÏÇ" "in ÏÑ"           "the null"        "autumn ÇÆíÒ"    "of ÇÒ"           "1919 1919"       "this ãİŞæÏ"      "first Çæáíå"    
 [9] "draft ÇÆíÒ"     "and æ"           "some ÈÚÖí"       "of ÇÒ"           "the null"        "notes 1919"      "were ÔÏäÏ"       "lost ÇÆíÒ"     

$word_align[[2]]
 [1] "so áĞÇ"          "it Çíä"          "was ÔÏ"          "built 20"        "again ãÌÏÏÇ"     "with ÈÇ"         "heavy 20"        "repugnance 20"  
 [9] "in ÏÑ"           "London 20"       "in ÏÑ"           "the null"        "winter ÒãÓÊÇä"   "of ÇÒ"           "1919 1919"       "20 20"          
[17] "from ÎÇØÑååÇ"     "memory 20"       "and æ"           "my íÇÏÏÇÔÊåÇí"    "surviving 20"    "notes íÇÏÏÇÔÊåÇí"

$word_align[[3]]
 [1] "the null"           "record ÇÔÊÈÇåÇÊí"   "of null"            "events ÇÔÊÈÇåÇÊí"   "was ÈæÏ"            "not ä˜ÑÏå"          "dulled ÇÔÊÈÇåÇÊí"  
 [8] "in æŞÇíÚ"           "me ãä"              "and æ"              "perhaps ÔÇíÏ"       "few ÇÔÊÈÇåÇÊí"      "actual ÇÔÊÈÇåÇÊí"   "mistakes ÇÔÊÈÇåÇÊí"
[15] "crept ÇÔÊÈÇåÇÊí"    "in æŞÇíÚ"           "except ÇÔÊÈÇåÇÊí"   "in æŞÇíÚ"           "details ÇÔÊÈÇåÇÊí"  "of null"            "dates æŞÇíÚ"       
[22] "or ÇÕáí"            "numbers ÇÔÊÈÇåÇÊí" 

$word_align[[4]]
 [1] "but áí˜ä"            "the null"            "outlines ÊÚáŞÇÊ"     "and æ"               "significance ÊÚáŞÇÊ" "of ÊÃËíÑ"           
 [7] "things ÊÚáŞÇÊ"       "had İÑæÑİÊå"         "lost ÊÚáŞÇÊ"         "edge ÊÚáŞÇÊ"         "in ÏÑ"               "the null"           
[13] "haze ÊÚáŞÇÊ"         "of ÊÃËíÑ"            "new ÌÏíÏ"            "interests ÊÚáŞÇÊ"   

$word_align[[5]]
 [1] "dates æŞÇíÚ"      "and æ"            "places ÊÇÑíÎåÇ"    "are ÇÓÇãí"        "correct ÊÇÑíÎåÇ"   "so ÕÍíÍ"          "far ÊÇÑíÎåÇ"       "as íÇÏ"          
 [9] "my ãä"            "notes íÇÏÏÇÔÊåÇí"  "preserved ÊÇÑíÎåÇ" "them ÇÔÎÇÕ"       "but ÇãÇ"          "the null"         "personal ÊÇÑíÎåÇ"  "names ÇÓÇãí"     
[17] "are ÇÓÇãí"        "not æŞÇíÚ"       


$mydictionary
      English     Farsi    
 [1,] "against"   "Úáíå"   
 [2,] "Ageyl"     "ÚŞíá"   
 [3,] "Akaba"     "ÚŞÈå"   
 [4,] "Arab"      "ÚÑÈ"    
 [5,] "Bair"      "ÈÆÑ"    
 [6,] "Beni"      "Èäí"    
 [7,] "best"      "ÈåÊÑíä" 
 [8,] "Billi"     "Èíáí"   
 [9,] "bread"     "äÇä"    
[10,] "British"   "ÇäáíÓ" 
[11,] "but"       "ÇãÇ"    
[12,] "Cairo"     "ŞÇåÑå"  
[13,] "Damascus"  "ÏãÔŞ"   
[14,] "desert"    "ÕÍÑÇ"   
[15,] "Egypt"     "ãÕÑ"    
[16,] "eighty"    "åÔÊÇÏ"  
[17,] "Emir"      "ÇãíÑ"   
[18,] "fifty"     "äÌÇå"  
[19,] "first"     "Çæáíä"  
[20,] "five"      "äÌ"    
[21,] "four"      "åÇÑ"   
[22,] "God"       "ÎÏÇæäÏ" 
[23,] "Harb"      "ÍÑÈ"    
[24,] "Hejaz"     "ÍÌÇÒ"   
[25,] "Howeitat"  "ÍæíØÇÊ" 
[26,] "ibn"       "ÇÈä"    
[27,] "Jemal"     "ÌãÇá"   
[28,] "Jerusalem" "ÇæÑÔáíã"
[29,] "last"      "ÂÎÑíä"  
[30,] "Maan"      "ãÚÇä"   
[31,] "Medina"    "ãÏíäå"  
[32,] "miles"     "ãíáí"   
[33,] "new"       "ÌÏíÏ"   
[34,] "or"        "íÇ"     
[35,] "peace"     "ÕáÍ"    
[36,] "Rabegh"    "ÑÈíÛ"   
[37,] "red"       "ŞÑãÒ"   
[38,] "right"     "ÑÇÓÊ"   
[39,] "second"    "Ïæã"    
[40,] "Sherif"    "ÔÑíİ"   
[41,] "six"       "ÔÔ"     
[42,] "Suez"      "ÓæÆÒ"   
[43,] "Syria"     "ÓæÑíå"  
[44,] "ten"       "Ïå"     
[45,] "thousand"  "åÒÇÑ"   
[46,] "three"     "Óå"     
[47,] "Turkey"    "ÊÑ˜íå"  
[48,] "twenty"    "ÈíÓÊ"   
[49,] "two"       "Ïæ"     
[50,] "Wadi"      "æÇÏí"   
[51,] "white"     "ÓİíÏ"   
[52,] "world"     "ÌåÇä"   
}

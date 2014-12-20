\name{word_alignIBM1}
\alias{word_alignIBM1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finding word alignment using IBM model 1 for a given parallel corpus.
}
\description{
For a given Sentence-Aligned Parallel Corpus, it aligns words in every sentence pair.  Further, it builds a suggested dictionary of two languages based on given corpus.
}
\usage{
word_alignIBM1(filename1, filename2, nrec = -1, skipped = 0, maxlength = 40, lu1 = TRUE, lu2 = FALSE, iter = 10, lang1 = "English", lang2 = "Farsi", prob = 0.9, first = 1, last = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename1}{
path of the target language.
}
  \item{filename2}{
path of the source language.
}
  \item{nrec}{
number of sentences to be read.If  -1, it considers all sentences.
}
  \item{skipped}{
an integer. the number of sentences to be ignored from the beginning.
}
  \item{maxlength}{
a maximum length of sentences. Long sentences will be removed to save the time.
}
  \item{lu1}{
 logical. If \samp{TRUE}, it will convert the first character of target language's  sentences.
}
  \item{lu2}{
logical. If \samp{TRUE}, it will convert the first character of source language's sentences. 
}
  \item{iter}{
number of  iteration for IBM model 1.
}
  \item{lang1}{
traget language's name in mydictionary.
}
  \item{lang2}{
source language's name in mydictionary.
}
  \item{prob}{
to build mydictionary we need this probability. The higher probability builds more precise dictionary than the lower one.
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

To run this function, it is essential to use RmTokenizer function. So, internet connection is necessary.

The results depend on the corpus. As an example, we used English-Persian Parallel corpus named Mizan which consists of more than 1,000,000 sentence pairs with a size about 170 Mb. If all sentences are considered, it takes 4.071 hours using a computer with cpu: hpcompack-i73930 and Ram: 8*8 G = 64 G and word alignment is good but the suggested dictionary is not very good. But for the 10,000 first sentences, the suggested dictionary would be almost good, although the word alignment might not be good. In fact, it is sensitive to the original translation type (lexical or conceptual).


}
\value{
A list.
%%  If it is a LIST, use
  \item{number_input_sentences}{An integer.}
  \item{number_used_sentences}{An integer.}
  \item{time }{A number. (in second/minute/hour)}
  \item{iteribm1 }{An integer.}
 \item{word_align }{A list of word alignment for every sentence pair.}
\item{mydictionary }{A matrix.}
}
\references{
P. Koehn, "Statistical machine translation",
Cambridge University , New York, 2010.

Supreme Council of Information and Communication Technology. (2013). Mizan English-Persian Parallel Corpus. Tehran, I.R. Iran. Retrieved from http://dadegan.ir/catalog/mizan.
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
Note that we have a memory restriction and just special computers with high cpu and big ram can allocate the vectors of this function. Of course, it depends on corpus size.
}

\examples{
word_alignIBM1('path of English language','path of Persian language')

#It is as a result of Mizan English-Persian Parallel corpus (all sentence pairs).

$number_input_sentences
[1] 1021593

$number_used_sentences
[1] 1006273

$time
Time difference of 8.600841 hours

$iteribm1
[1] 10

$word_align
$word_align[[1]]
 [1] "afterwards »⁄œ«" "in œ—"           "the null"        "autumn Å«∆Ì“"    "of «“"           "1919 1919"       "this «Ì‰"        "first «Ê·ÌÂ"     "draft 1919"     
[10] "and Ê"           "some »⁄÷Ì"       "of «“"           "the null"        "notes Ì«œœ«‘ Â«"  "were ‘œ‰œ"       "lost „›ﬁÊœ"     

$word_align[[2]]
 [1] "so ·–«"          "it «Ì‰"          "was ‘œ"          "built 1919"      "again „Ãœœ«"     "with »«"         "heavy 1919"      "repugnance 1919" "in œ—"          
[10] "London 1919"     "in œ—"           "the null"        "winter “„” «‰"   "of «“"           "1919 1919"       "20 20"           "from «“"         "memory Œ«ÿ—ÂÂ«"  
[19] "and Ê"           "my Œ«ÿ—ÂÂ«"       "surviving 1919"  "notes Ì«œœ«‘ Â«Ì"

$word_align[[3]]
 [1] "the —«"             "record —Œœ«œÂ«Ì"    "of œ—"              "events Êﬁ«Ì⁄"       "was »Êœ"            "not ‰ò—œÂ"          "dulled —Œœ«œÂ«Ì"    "in œ—"             
 [9] "me „‰"              "and Ê"              "perhaps ‘«Ìœ"       "few —Œœ«œÂ«Ì"       "actual —Œœ«œÂ«Ì"    "mistakes «‘ »«Â« Ì" "crept —Œœ«œÂ«Ì"     "in œ—"             
[17] "except —Œœ«œÂ«Ì"    "in œ—"              "details —Œœ«œÂ«Ì"   "of œ—"              "dates —Œœ«œÂ«Ì"     "or —Œœ«œÂ«Ì"        "numbers —Œœ«œÂ«Ì"  

$word_align[[4]]
 [1] "but ·Ìò‰"            "the null"            "outlines  ⁄·ﬁ« "     "and Ê"               "significance  ⁄·ﬁ« " "of «“"               "things  ⁄·ﬁ« "      
 [8] "had »Êœ"             "lost  ⁄·ﬁ« "         "edge  ⁄·ﬁ« "         "in œ—"               "the null"            "haze  ⁄·ﬁ« "         "of «“"              
[15] "new ÃœÌœ"            "interests  ⁄·ﬁ« "   

$word_align[[5]]
 [1] "dates  «—ÌŒÂ«"    "and Ê"           "places  «—ÌŒÂ«"   "are «‘Œ«’"       "correct ’ÕÌÕ"    "so  «"           "far  «"          "as  «"           "my „‰"          
[10] "notes Ì«œœ«‘ Â«Ì" "preserved Õ›Ÿ"   "them «‘Œ«’"      "but «„«"         "the —«"          "personal  «—ÌŒÂ«" "names «”«„Ì"     "are «‘Œ«’"       "not «„«"        


$mydictionary
       English                 Farsi                        
  [1,] "31"                    "”ÌÊÌò„"                       
  [2,] "34"                    "”ÌÊçÂ«—„"                     
  [3,] "71"                    "71"                         
  [4,] "79"                    "79"                         
  [5,] "86"                    "86"                         
  [6,] "88"                    "88"                         
  [7,] "a"                     "«««"                        
  [8,] "absinthe"              "¬»”Ì‰ "                     
  [9,] "aha"                   "¬Â«««"                      
 [10,] "alas"                  "«›”Ê‹‰"                      
 [11,] "alohomora"             "¬·ÊÂÊ„Ê—«"                  
 [12,] "aROOOOO"               "«—ÊÊÊÊÊ"                    
 [13,] "awesome"               "«ı»Â Â"                       
 [14,] "bah"                   "Å«Â"                        
 [15,] "battistino"            "»« Ì” Ì‰"                   
 [16,] "beccccccc"             "òòòòò"                      
 [17,] "bezukhov"              "»“ÊŒÊ›"                      
 [18,] "blighter"              "Ã⁄¯·ﬁ"                        
 [19,] "booououh"              "¬ÊÊÊÊÊ"                     
 [20,] "break"                 "›—Ê"                         
 [21,] "bueno"                 "»Ê∆‰Ê"                      
 [22,] "bye"                   "Œœ«›Ÿ"                      
 [23,] "carmagnolet"           "ò«—„«‰ÌÊ·Â"                 
 [24,] "caveyard"              "€«—” «‰"                    
 [25,] "chains"                "»« Ê”·"                     
 [26,] "cHAUCER"               "ç«Ê”—"                      
 [27,] "coincidence"           "« ›«ÂÌ"                     
 [28,] "corbiere"              "òÊ—»ÌÌ—"                     
 [29,] "cORINETTE"             "òÊ—Ì‰ "                      
 [30,] "cough"                 "«ÂÂ"                         
 [31,] "courage"               "··«"                        
 [32,] "coward"                "»ÌÕ„Ì¯ "                       
 [33,] "damnloafers"           "Ê·ê—Â«Ì"                    
 [34,] "dEADLINE"              "«·⁄Ã·"                      
 [35,] "deceitful"             "›—Ì»ò«—‰œ"                  
 [36,] "deeper"                "Â„⁄„Ìﬁ —"                    
 [37,] "degenerates"           "‰”»Â«"                       
 [38,] "delirious"             "Â–ÌÊ‰"                      
 [39,] "derveeshio"            "œ—ÊÌ‘‘"                     
 [40,] "deviant"               "œÊÌ«‰ "                     
 [41,] "dishonourable"         "»Ì¬»—ÊÌ„"                    
 [42,] "dogs"                  "”ê«”"                       
 [43,] "dotard"                "Œ—›‹ "                      
 [44,] "dufond"                "œÊ›Ê‰"                      
 [45,] "edit"                  "ÊÌ—«Ì‘‘"                    
 [46,] "eglerio"               "«ê·—ÌÊ"                     
 [47,] "egoists"               "ŒÊœÅ—” Â«"                   
 [48,] "eh"                    "‹Â«‰"                       
 [49,] "england"               "«‰ê·” "                     
 [50,] "er"                    "Â«¬"                        
 [51,] "eSCAPE"                "êı—Ì“"                        
 [52,] "exactly"               "ÊﬁÌﬁ«"                       
 [53,] "excellent"             "«ÌÊ·"                       
 [54,] "expecto"               "«ò”Åò Ê"                    
 [55,] "finally"               "—ÊÌÂ„—› Â"                  
 [56,] "finistere"             "›Ì‰Ì” —"                    
 [57,] "fired"                 "œ—ò—œÌ"                     
 [58,] "gilthoniel"            " Ê‰Ì·"                      
 [59,] "gonzales"              "êÊ«‰“«·”"                   
 [60,] "good"                  "€ŒÊ»” "                     
 [61,] "good"                  "—Ê‘ò—"                      
 [62,] "gOODBYES"              "Õ«›ŸÌÂ«"                     
 [63,] "grinning"              "ÅÊ“Œ‰œ»—·»"                 
 [64,] "grubitssssssssssch"    "ê—Ê»Ìçççççççççççççççççççççç"
 [65,] "guido"                 "òÊ∆ÌœÊ"                     
 [66,] "gUIZOT"                "êÌ“Ê"                       
 [67,] "handsome"              "Ã–«»"                         
 [68,] "here"                  "‘ÊœÌœ"                      
 [69,] "hmm"                   "Â„„„„"                      
 [70,] "hogwarts"              "Â«êÊ«“ “Ì"                  
 [71,] "homework"              " ò«·Ì›„Ê‰Ê"                 
 [72,] "horrible"              "«› ÷«Õ‰œ"                   
 [73,] "humann"                "ÂÊ„«‰"                      
 [74,] "hyroQwhich"            "ÂÌ—Ê"                       
 [75,] "impervius"             "«Ì„Å—ÌÊ”"                   
 [76,] "inferi"                "«”›·«·”«›·Ì‰"                
 [77,] "injuries"              "»«“ò‰«„Ê‰"                  
 [78,] "iNTRODUCTION"          "„ﬁ‹‹œ„‹‹Â"                  
 [79,] "karkaroff"             "ò«—ò«—Ê›Â"                  
 [80,] "kIDNAP"                "ÌçÂ"                        
 [81,] "killing"               "ò‹‘ «—"                     
 [82,] "kingsweston"           "òÌ‰ê“Êˆ” ‰"                   
 [83,] "klimovna"              "ò·Ì„ÊÊ‰«"                   
 [84,] "kuriakore"             "òÊ—Ì«òÊ”"                   
 [85,] "lasers"                "·Ì“—"                       
 [86,] "laveuve"               "·«ÊÊÊ"                      
 [87,] "learn"                 "«„ÊŒ ‰"                     
 [88,] "lebeziatnikov"         "·Ì“Ì« ‰Ìò›"                 
 [89,] "lectureship"           "„—»Ìê—Ì"                     
 [90,] "lvidaj"                "«ÌÊÌœÌ"                     
 [91,] "macnair"               "„ò‰—Â"                      
 [92,] "mad"                   "Ã‰Ê‰Â"                      
 [93,] "mangedentelle"         "„«‰éÂ"                      
 [94,] "mangoes"               "«‰»ÂÂ«"                      
 [95,] "mattatoni"             "„« « Ê Ì"                   
 [96,] "maximilian"            "„Ì·Ì«‰"                     
 [97,] "mmm"                   "Â„„„„„„"                    
 [98,] "mulciber"              "„«·”»Ì—"                    
 [99,] "nambril"               "‰«„«—ÌÂ"                    
[100,] "no"                    "Œ”—"                        
[101,] "nonsense"              "„‹“Œ‹—›"                    
[102,] "null"                  "Null"                       
[103,] "on"                    "»«“Ì„Ìò‰Ì„"                   
[104,] "oxfam"                 "¬ò”›«„"                     
[105,] "pa"                    "»«Ã«‰"                      
[106,] "parbleu"               "«··«ÂÂ"                     
[107,] "pardieu"               "Å«œÌÊ"                      
[108,] "parkins"               "Å—ò‰Ì“"                     
[109,] "pathologists"          "‘‰«”«"                      
[110,] "paveyard"              "”‰ê” «‰"                    
[111,] "pooh"                  "çÌ“Â«∆Ì„ÌêÊ∆Ìœ"              
[112,] "poussagrive"           "ÅÊ”«ê—ÌÊ"                   
[113,] "poussagrive"           "ÿ—ﬁÂêÌ—"                     
[114,] "prohibited"            "„„‰Ê⁄"                        
[115,] "quick"                 "»Ã‰»"                        
[116,] "quincampoix"           "ò‰ò«‰ÅÊ«"                   
[117,] "ralston"               "—«·” Ê‰"                    
[118,] "really"                "œ«"                          
[119,] "rematch"               "»«“Ìˆ"                        
[120,] "riddikulus"            "—ÌœÌÊòÌÊ·Ê”"                
[121,] "rightonrightonrighton" "‹¬›—Ì‰"                     
[122,] "sabine"                "«¬Œ"                        
[123,] "sedyablyaka"           "»·Ì«ò«"                     
[124,] "see"                   "»»‰ÌÌœ"                     
[125,] "septembrist"           "”Å «„»—Ì"                   
[126,] "shoo"                  "ÅÌ‘ Â"                      
[127,] "silence"               "‹”«ò "                      
[128,] "silence"               "Å”«ò "                      
[129,] "sorry"                 "»»ŒÌœ"                      
[130,] "sorry"                 "„ «”›"                        
[131,] "soshamma"              "”Ê‘«„«"                     
[132,] "strangers"             "€—Ì»Â‰"                      
[133,] "tactless"              "»Ì„·«ÕŸÂ”"                    
[134,] "terrible"              "‘‰Ì⁄Â"                      
[135,] "thailand"              " «Ì·‰œ"                     
[136,] "theoc"                 " ∆Ê"                        
[137,] "trevor"                " —ˆÂÊ—"                        
[138,] "twenty"                "»Ì” "                       
[139,] "twenty"                "»Ì‹” "                      
[140,] "um"                    "«ÊÊÊ„"                      
[141,] "um"                    "«ÊÊÊÊ„"                     
[142,] "ummmmmm"               "«ÊÂÊÂÊÂÊ„"                  
[143,] "underpants"            "‘·Ê«—ÌÂ«"                    
[144,] "underwear"             "¬⁄„«ﬁ"                      
[145,] "unjust"                "«‰’«›Â«"                     
[146,] "unstable"              "‰«Å«Ìœ«—Ì‘"                 
[147,] "up"                    "ŒÌ“Ìœ"                      
[148,] "urens"                 "«Ê—‰”"                      
[149,] "ventregoulette"        "–«  «‰"                     
[150,] "vesenny"               "ÊÌ”Ì"                       
[151,] "well"                  "Ê«???"                        
[152,] "wHAM"                  "ê—„»"                       
[153,] "whoa"                  "Â‘‘‘"                         
[154,] "why"                   "çÿÊ—„ê—"                    
[155,] "why"                   "‰Œ—ÌœÌœ"                    
[156,] "witnesses"             "‘ÂÊœ‰œ"                     
[157,] "woo"                   "ÊÊÊÊ"                       
[158,] "woooooooooo"           "«ÊÊÊÊÊÊÊÊ"                  
[159,] "wostov"                "Ê«” Ê›" 
160,] "yes"                   "»·Â«"                       
[161,] "zouaves"               "“Ê¬ÊÂÂ«"                     
[162,] "zuss"                  "ÌÕ"                         

       
#It is as a result of Mizan English-Persian Parallel corpus (only the 10000 first sentence pairs).
  
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
 [1] "afterwards »⁄œ«" "in œ—"           "the null"        "autumn Å«∆Ì“"    "of «“"           "1919 1919"       "this „›ﬁÊœ"      "first «Ê·ÌÂ"    
 [9] "draft Å«∆Ì“"     "and Ê"           "some »⁄÷Ì"       "of «“"           "the null"        "notes 1919"      "were ‘œ‰œ"       "lost Å«∆Ì“"     

$word_align[[2]]
 [1] "so ·–«"          "it «Ì‰"          "was ‘œ"          "built 20"        "again „Ãœœ«"     "with »«"         "heavy 20"        "repugnance 20"  
 [9] "in œ—"           "London 20"       "in œ—"           "the null"        "winter “„” «‰"   "of «“"           "1919 1919"       "20 20"          
[17] "from Œ«ÿ—ÂÂ«"     "memory 20"       "and Ê"           "my Ì«œœ«‘ Â«Ì"    "surviving 20"    "notes Ì«œœ«‘ Â«Ì"

$word_align[[3]]
 [1] "the null"           "record «‘ »«Â« Ì"   "of null"            "events «‘ »«Â« Ì"   "was »Êœ"            "not ‰ò—œÂ"          "dulled «‘ »«Â« Ì"  
 [8] "in Êﬁ«Ì⁄"           "me „‰"              "and Ê"              "perhaps ‘«Ìœ"       "few «‘ »«Â« Ì"      "actual «‘ »«Â« Ì"   "mistakes «‘ »«Â« Ì"
[15] "crept «‘ »«Â« Ì"    "in Êﬁ«Ì⁄"           "except «‘ »«Â« Ì"   "in Êﬁ«Ì⁄"           "details «‘ »«Â« Ì"  "of null"            "dates Êﬁ«Ì⁄"       
[22] "or «’·Ì"            "numbers «‘ »«Â« Ì" 

$word_align[[4]]
 [1] "but ·Ìò‰"            "the null"            "outlines  ⁄·ﬁ« "     "and Ê"               "significance  ⁄·ﬁ« " "of  √ÀÌ—"           
 [7] "things  ⁄·ﬁ« "       "had ›—Ê—› Â"         "lost  ⁄·ﬁ« "         "edge  ⁄·ﬁ« "         "in œ—"               "the null"           
[13] "haze  ⁄·ﬁ« "         "of  √ÀÌ—"            "new ÃœÌœ"            "interests  ⁄·ﬁ« "   

$word_align[[5]]
 [1] "dates Êﬁ«Ì⁄"      "and Ê"            "places  «—ÌŒÂ«"    "are «”«„Ì"        "correct  «—ÌŒÂ«"   "so ’ÕÌÕ"          "far  «—ÌŒÂ«"       "as Ì«œ"          
 [9] "my „‰"            "notes Ì«œœ«‘ Â«Ì"  "preserved  «—ÌŒÂ«" "them «‘Œ«’"       "but «„«"          "the null"         "personal  «—ÌŒÂ«"  "names «”«„Ì"     
[17] "are «”«„Ì"        "not Êﬁ«Ì⁄"       


$mydictionary
      English     Farsi    
 [1,] "against"   "⁄·ÌÂ"   
 [2,] "Ageyl"     "⁄ﬁÌ·"   
 [3,] "Akaba"     "⁄ﬁ»Â"   
 [4,] "Arab"      "⁄—»"    
 [5,] "Bair"      "»∆—"    
 [6,] "Beni"      "»‰Ì"    
 [7,] "best"      "»Â —Ì‰" 
 [8,] "Billi"     "»Ì·Ì"   
 [9,] "bread"     "‰«‰"    
[10,] "British"   "«‰ê·Ì”" 
[11,] "but"       "«„«"    
[12,] "Cairo"     "ﬁ«Â—Â"  
[13,] "Damascus"  "œ„‘ﬁ"   
[14,] "desert"    "’Õ—«"   
[15,] "Egypt"     "„’—"    
[16,] "eighty"    "Â‘ «œ"  
[17,] "Emir"      "«„Ì—"   
[18,] "fifty"     "Å‰Ã«Â"  
[19,] "first"     "«Ê·Ì‰"  
[20,] "five"      "Å‰Ã"    
[21,] "four"      "çÂ«—"   
[22,] "God"       "Œœ«Ê‰œ" 
[23,] "Harb"      "Õ—»"    
[24,] "Hejaz"     "ÕÃ«“"   
[25,] "Howeitat"  "ÕÊÌÿ« " 
[26,] "ibn"       "«»‰"    
[27,] "Jemal"     "Ã„«·"   
[28,] "Jerusalem" "«Ê—‘·Ì„"
[29,] "last"      "¬Œ—Ì‰"  
[30,] "Maan"      "„⁄«‰"   
[31,] "Medina"    "„œÌ‰Â"  
[32,] "miles"     "„Ì·Ì"   
[33,] "new"       "ÃœÌœ"   
[34,] "or"        "Ì«"     
[35,] "peace"     "’·Õ"    
[36,] "Rabegh"    "—»Ì€"   
[37,] "red"       "ﬁ—„“"   
[38,] "right"     "—«” "   
[39,] "second"    "œÊ„"    
[40,] "Sherif"    "‘—Ì›"   
[41,] "six"       "‘‘"     
[42,] "Suez"      "”Ê∆“"   
[43,] "Syria"     "”Ê—ÌÂ"  
[44,] "ten"       "œÂ"     
[45,] "thousand"  "Â“«—"   
[46,] "three"     "”Â"     
[47,] "Turkey"    " —òÌÂ"  
[48,] "twenty"    "»Ì” "   
[49,] "two"       "œÊ"     
[50,] "Wadi"      "Ê«œÌ"   
[51,] "white"     "”›Ìœ"   
[52,] "world"     "ÃÂ«‰"   

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

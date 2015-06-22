RmTokenizer <-
function(text, intrnt = TRUE, split = FALSE, rmBlank=FALSE)
{
if(intrnt)
{
rr = as.character (read.delim('http://www.um.ac.ir/~sarmad/word.a/ol.txt', sep = '\n', encod = 'UTF-8', header = FALSE)[[1]])
text = gsub (rr[1], ' ', text)
text = gsub (rr[2], ' ', text)
text = gsub (rr[3], ' ', text)
}
text = gsub ("[[:punct:]]", " ", text)
    
if (rmBlank) {while (!identical (text, gsub ('  ', ' ', text))) text = gsub ('  ',' ',text)}

if (split) text = strsplit (text, ' ') # It is not needed for word_alignIBM1
return (text)
}

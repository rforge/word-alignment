RmTokenizer <-
function(text,split=FALSE)
{
    rr=as.character(read.delim('http://www.um.ac.ir/~sarmad/ol.txt',sep='\n',encod='UTF-8',h=F)[[1]])
    text=gsub(rr[1],' ',text)
    text=gsub(rr[2],' ',text)
    text=gsub(rr[3],' ',text)
    text=gsub("[[:punct:]]", " ", text)
    #while(!identical(text,gsub('  ',' ',text))) text=gsub('  ',' ',text)
   
    if (split) text=strsplit(text,' ') # It is not needed for word_alignIBM1
    return(text)
}

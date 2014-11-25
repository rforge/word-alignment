fcl <-
function(b,warning=F)
{
    cc=sapply(b,length)
       
    kk1=c(1,cumsum(cc[-length(cc)])+1)
    kk2=kk1+cc/2-1
    
    pp=eval(parse(text=paste('c(',paste(kk1,kk2,sep=':',collapse=','),')',sep='')))
    
    rm(kk1,kk2,cc)
    gc()
    d1=data.table(e=unlist(b)[pp],f=unlist(b)[-pp])    
    rm(pp)
    gc()
if(warning)    print('number of columnns of all matrices in b must be two')
    return(d1)
}

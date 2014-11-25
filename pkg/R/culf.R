culf <-
function(x,n=1)
{
      x=paste(tolower(substr(x,1,n)),substring(x,n+1),sep='')
return(x)
}

word_alignIBM1 <-
function(filename1,filename2,nrec=-1,maxlength=40,lu1=TRUE,lu2=FALSE,iter=10,lang1='English',lang2='Farsi',prob=0.9,first=1,last=5)
{

date1=as.POSIXlt(Sys.time(), "Iran")

#--------------------------Translation:f to e -----------------------------------
e=f=a=b=t=bb=c()
e=readLines(con <- file(filename1),encoding='UTF-8',n=nrec)
close(con)

f=readLines(con <- file(filename2),encoding='UTF-8',n=nrec)
close(con)

for(i in 1:length(e))
	if(e[i]==''){f[i+1]=paste(f[i],f[i+1]);f[i]=''}

for(i in 1:length(f))
	if(f[i]==''){e[i+1]=paste(e[i],e[i+1]);e[i]=''}

e=e[sapply(1:length(e),function(x)e[x]!='')]
f=f[sapply(1:length(f),function(x)f[x]!='')]

a=cbind(e,f)
a4=nrow(a)

a=matrix(a[vapply(1:nrow(a),function(x)length(unlist(strsplit(a[x,1],' '))),FUN.VALUE=0)<=maxlength],ncol=2)
a=matrix(a[vapply(1:nrow(a),function(x)length(unlist(strsplit(a[x,2],' '))),FUN.VALUE=0)<=maxlength],ncol=2)

#------------------------- Tokenization -----------------------------------------
   
if (lu1) a[,1]=sapply(1:nrow(a),function(x)culf(a[x,1]))
if (lu2) a[,2]=sapply(1:nrow(a),function(x)culf(a[x,2]))

a[,1]=RmTokenizer(a[,1])
a[,2]=RmTokenizer(a[,2])

a[,1]=a[,1][sapply(1:nrow(a),function(x)a[,1][x]!='')]
a[,2]=a[,2][sapply(1:nrow(a),function(x)a[,2][x]!='')]

#-------------------------------------------------------------------------------
a[,2]=paste('null',a[,2])
a3=nrow(a)

rm(e,f)
gc()

b=apply(a,1,function(x)cbind(Var1=rep.int((strsplit(as.character(x[1]),' ')[[1]])[strsplit(as.character(x[1]),' ')[[1]]!=''], 
	length((strsplit(as.character(x[2]),' ')[[1]])[strsplit(as.character(x[2]),' ')[[1]]!=''])), Var2=rep((strsplit(as.character(x[2]),' ')[[1]])[strsplit(as.character(x[2]),' ')[[1]]!=''], 
	each=length((strsplit(as.character(x[1]),' ')[[1]])[strsplit(as.character(x[1]),' ')[[1]]!='']))))


cc=sapply(b,length)
       
      dd1=data.table(g=rep(1:a3,(cc/2)),fcl(b),t=as.numeric(rep(1/(cc/2),(cc/2))))

     rm(b,cc)
gc()

iteration=0
for(iiiii in 1:iter)
{
iteration=iteration+1
dd1[,count0:=t/sum(t),by=paste(g,e)]
dd1[,t:=NULL]
dd1[,count:=sum(count0),by=paste(e,f)]
dd1[,total:=sum(count0),by=f]
dd1[,t:=count/total]
dd1[,count0:=NULL]
dd1[,count:=NULL]
dd1[,total:=NULL]
}

u1=unique(dd1[round(t,1)>prob,e,f])
fe=matrix(c(u1$e,u1$f),ncol=2)
colnames(fe)=c(lang1,lang2)
fe=fe[order(fe[,1]),]

a1=strsplit(a[,1],' ')
a2=strsplit(a[,2],' ')

a1=sapply(1:a3,function(x)a1[[x]][a1[[x]]!=''])
a2=sapply(1:a3,function(x)a2[[x]][a2[[x]]!=''])

le=sapply(a1,length)
lf=sapply(a2,length)

dd1[,i:=unlist(sapply(1:a3,function(x)rep(0:(lf[x]-1),rep(le[x],lf[x]))))]
dd1[,j:=unlist(sapply(1:a3,function(x)rep(1:le[x],lf[x])))]

d1=dd1[,i[which.max(t)],by=paste(g,j)][[2]]

c1=c(0,cumsum(le))
ef=sapply(1:length(a1),function(x)paste(a1[[x]],a2[[x]][d1[(c1[x]+1):c1[x+1]]+1]))

date2=as.POSIXlt(Sys.time(), "Iran")

mylist=list(number_input_sentences=a4,number_used_sentences=a3,time=date2-date1,iteribm1 = iteration,word_align=ef[c(first:last)],mydictionary=fe)
return(mylist)
}

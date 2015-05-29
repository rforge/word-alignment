Evaluation1 <-
function(file1,file2,sou,tar,method=c('fix','Excel'),e1='g.xlsx',e2='a.xlsx',agn=c('myagn','an.agn'),dtfile=NULL,nrec= -1,nlen= -1,mlen=40,lus=FALSE,lut=TRUE,iter=3,la=TRUE)
{

e = f = g = c()

method = match.arg(method)
agn = match.arg(agn)

p1 = preparData(sou, tar, nrec = nlen, mlen = mlen, lus = lus, lut = lut)
#----------------- Constructing a gold standard -----------------
if (method == 'fix')
{

sure = possible = c()
for (ll in 1 : p1$length)
{
mm = fix.gold (sou, tar, kk = ll)
sure = c (sure, outer (mm [-1, 1], mm [1, -1], paste)[mm [-1, -1] == 1])
possible = c (possible, outer (mm [-1, 1], mm [1, -1], paste) [mm [-1, -1] == 2])
}
}

if (method == 'Excel')
{
sure = possible = c()
for(ll in 1 : p1$length)
{
df1 <- read.xlsx (xlsxFile = e1, sheet = ll)
mm = as.matrix (df1)
sure = c (sure, outer (mm[-1,1], mm [1, -1], paste) [mm[-1, -1] == 1])
possible = c (possible, outer (mm [-1, 1], mm [1, -1], paste) [mm [-1, -1] == 2])
}
}
#----- Constucting a word alignment based on another softeware -----
if (agn == 'an.agn')
{
if(method == 'fix')
{
readline("Now, press any key to continue and edit the matrix to enter '3' for alignments.")
align = c()
for(ll in 1 : p1$length)
{
m = fix.gold(sou, tar, kk = ll)
align = c(align, outer(m[-1,1],m[1,-1],paste)[m[-1,-1] == 3])
}
}

if(method == 'Excel')
{
align = c()
for(ll in 1 : p1$length)
{
df1 <- read.xlsx(xlsxFile = e2, sheet = ll)
mm = as.matrix(df1)
align = c(align,outer(mm[-1,1],mm[1,-1],paste)[mm[-1,-1] == 3])
}
}
}

#-------- calculate word alignment based on my IBM model1 ---------

if(agn == 'myagn')
{
if(is.null(dtfile))
{
dd1 = word_alignIBM1(file1,file2,iter = iter,nrec = nrec, mlen = mlen, lus = lus, lut = lut, input = TRUE, la = la)
dd1[,g := NULL]
save(dd1,file = paste(dtfile,'RData',sep = '.'))
}

if(! is.null(dtfile))
{
load(dtfile)
}

align=c()

for(i in 1:p1$length) 
{
dd2 = dd1[e %in% p1[[3]][[i]] & f %in% p1[[2]][[i]],]
dd2 = dd2[,f[which.max(t)],by = e]
align = c(align, paste(dd2[[1]],dd2[[2]]))
}
}

#----------measure recall, precision and accuracy -----------
recall = sum(align %in% sure) / length(sure)
precision = sum(align %in% possible) / length(align)
AER = 1 - ((sum(align %in% sure) + sum(align %in% possible)) / (length(align) + length(sure)))
#############################################################
list2 = list(Recall = recall, Precision = precision, AER = AER)
#############################################################
return(list2)
}

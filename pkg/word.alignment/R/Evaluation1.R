Evaluation1 <-
function(file_train1, file_train2, tst.set_sorc, tst.set_trgt, f1 = 'Fa', e1 = 'En', method = c("fix","Excel"), agn = c("my.agn","an.agn"), excel1 = 'gold.xlsx', excel2 = 'align.xlsx', fixfile_gld = NULL, fixfile_agn = NULL, dtfile = NULL, nrec = -1, nlen = -1, maxlen = 40, ul_s = FALSE, ul_t = TRUE, iter = 3, intrnt = TRUE)
{

e = f = g = c()

method = match.arg(method)
agn = match.arg(agn)

p1 = preparData(tst.set_sorc, tst.set_trgt, nrec = nlen, maxlen = maxlen, ul_s = ul_s, ul_t = ul_t)
len = p1$length1
# ------------------------- using fix.gold function --------------------------------
if (method == 'fix')
{
#------------------ Constructing a gold standard for the first time ----------------
if (is.null(fixfile_gld))
{
sure = possible = c()
for (ll in 1 : len)
{
mm = fix.gold (tst.set_sorc, tst.set_trgt, method = "gold", num = ll)
save(mm,file = paste(ll,'RData',sep='.'))
sure = c (sure, outer (mm [1, -1], mm [-1, 1], paste)[mm [-1, -1] == 1])
possible = c (possible, outer (mm [1, -1], mm [-1, 1], paste) [mm [-1, -1] == 2])
}
}
#----------------- Constructing a gold standard for later time ---------------------
if( ! is.null(fixfile_gld))
{
sure = possible = c()
for (ll in 1 : len)
{
load(paste(ll,'RData',sep='.'))
sure = c (sure, outer (mm [1, -1], mm [-1, 1], paste)[mm [-1, -1] == 1])
possible = c (possible, outer (mm [1, -1], mm [-1, 1], paste) [mm [-1, -1] == 2])
}
}
#-------------- Constucting a word alignment using another softeware ---------------
if (agn == 'an.agn')
{
#-------------- for the first time ---------------
if (is.null (fixfile_agn))
{
align = c()
for(ll in 1 : len)
{
m = fix.gold(tst.set_sorc, tst.set_trgt, method = "aligns", num = ll)
save (m, file = paste('agn',ll,'RData',sep='.'))
align = c(align, outer(m[1,-1],m[-1, 1],paste)[m[-1,-1] == 3])
}
}
#---------------- for later times ----------------
if (! is.null (fixfile_agn))
{
align = c()
for(ll in 1 : len)
{
load ( paste('agn',ll,'RData',sep='.'))
align = c(align, outer(m[1, -1],m[-1,1],paste)[m[-1,-1] == 3])
}
}
}
}
# ------------------------- using consExcel function --------------------------------
if (method == 'Excel')
{
# ------------------------Constructing a gold standard ------------------------------
sure = possible = c()
for(ll in 1 : len)
{
df1 <- read.xlsx (xlsxFile = excel1, sheet = ll)
mm = as.matrix (df1)
sure = c (sure, outer (mm[1, -1], mm [-1, 1], paste) [mm[-1, -1] == 1])
possible = c (possible, outer (mm [1, -1], mm [-1, 1], paste) [mm [-1, -1] == 2])
}
#-------------- Constucting a word alignment using another softeware ---------------
if(agn == 'an.agn')
{
align = c()
for(ll in 1 : len)
{
df1 <- read.xlsx(xlsxFile = excel2, sheet = ll)
mm = as.matrix(df1)
align = c(align,outer(mm[1, -1],mm[-1, 1],paste)[mm[-1,-1] == 3])
}
}
}
#----- calculate word alignment based on my IBM model1 using word_alignIBM1 function -----s
if(agn == 'my.agn')
{
#---------------------------- for the first time -----------------------------------
if(is.null(dtfile))
{
dd1 = word_alignIBM1(file_train1,file_train2, iter = iter,nrec = nrec, maxlen = maxlen, ul_s = ul_s, ul_t = ul_t, input = TRUE, intrnt = intrnt)
save (dd1, iter,file = paste(f1, e1, nrec, iter,'RData', sep = '.'))
}
# ------------------------------ for later times ----------------------------------
if(! is.null(dtfile))
load(paste(f1, e1, nrec, iter,'RData', sep = '.'))

align=c()

for(i in 1:len) 
{
dd2 = dd1[e %in% p1[[2]][[i]] & f %in% p1[[3]][[i]],]
dd2 = dd2[,f[which.max(t)],by = e]
align = c(align, paste(dd2[[1]],dd2[[2]]))
}
}

#---------- recall, precision and accuracy measures-----------
recall = sum(align %in% sure) / length(sure)
precision = sum(align %in% possible) / length(align)
AER = 1 - ((sum(align %in% sure) + sum(align %in% possible)) / (length(align) + length(sure)))
#############################################################
list2 = list(Recall = recall, Precision = precision, AER = AER)
#############################################################
return(list2)
}

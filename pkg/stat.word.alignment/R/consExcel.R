consExcel <-
function(source1,target1,gold = TRUE,aligns = FALSE,out1 = 'D:/gold.xlsx',out2 = 'D:/align.xlsx',nrec = -1,mlen = 40,lus = FALSE,lut = TRUE)
{
p1 = preparData (source1, target1, nrec = nrec, mlen = mlen, lus = lus, lut = lut)

if (gold)
{
wb1 <- createWorkbook ("data")
for (j in 1 : p1$length1)
{
m1 = matrix (0, length (p1 [[2]][[j]]) + 1, length (p1 [[3]][[j]]) + 1)
m1 [2 : nrow (m1), 1] = p1 [[2]][[j]]; m1 [1, 2 : ncol (m1)] = p1 [[3]][[j]]; m1 [1, 1] = ''
addWorksheet (wb1, as.character(j))
writeData (wb1, sheet =j, m1)
saveWorkbook (wb1, out1, overwrite = TRUE)
 }
print ("Now, please edit 'out1.xlsx' to enter Sure/Possible alignments (Sure=1, Possible=2)")
}

if (aligns)
{
wb2 <- createWorkbook ("data")
for(j in 1 : p1$length1)
{
m1 = matrix (0, length (p1 [[2]][[j]]) + 1, length (p1 [[3]][[j]]) + 1)
m1 [2 : nrow (m1), 1] = p1 [[2]][[j]]; m1 [1, 2 : ncol (m1)] = p1 [[3]][[j]]; m1 [1,1]=''
addWorksheet (wb2, as.character(j))
writeData (wb2, sheet = j, m1)
saveWorkbook (wb2, out2, overwrite = TRUE)
 }
print("Now, please edit 'out2.xlsx' to enter '3' for alignments.")
}
}

Symmetrization <-
function (file_train1,file_train2, method = c ('union', 'intersection', 'grow-diag'), nrec = -1,iter = 4,ul_s = FALSE,ul_t = TRUE,intrnt = TRUE,first = 1, last = 5)
{
date1 = as.POSIXlt (Sys.time(), "Iran")

method = match.arg (method)

ef1 = word_alignIBM1 (file_train1, file_train2, nrec = nrec, iter = iter, display = 'number', sym = TRUE, intrnt = intrnt)

fe1 = word_alignIBM1 (file_train2, file_train1, nrec = nrec, iter = iter, display = 'number', sym = TRUE, intrnt = intrnt)
ll = length (fe1)

le = vapply (ef1, length, FUN.VALUE = 0)
lf = vapply (fe1, length, FUN.VALUE = 0)

word = preparData (file_train1, file_train2, nrec = nrec, ul_s = ul_s, ul_t = ul_t)
word2 = word [[3]]
word3 = word [[4]]

u1 = unlist (fe1); u1 [u1 == 0] = NA; fe1 = relist (u1, fe1)
u1 = unlist (ef1); u1 [u1 == 0] = NA; ef1 = relist (u1, ef1)

rm (u1)
gc ()

#---- position of matrix f to e (rows = the target language, columns = The source language)----
 
pos1 = sapply (1 : ll, function (x) (0 : (lf [x] - 1)) * le [x] + fe1 [[x]])

fe = sapply (1 : ll, function (x) pos1 [[x]] + seq (le [x] + 3, by = 2, length = lf [x]))#column's position in added matrix (2 rows and 2 columns are added in the marginal of initial matrix)
fe = sapply (1 : ll, function (x) fe [[x]][!is.na (fe [[x]])])

#---- position of matrix e to f (rows=the target language,columns=The source language)----
    
pos_row = sapply (1 : ll, function (x)(0 : (le[x] - 1)) * lf [x] + ef1 [[x]])

ef = sapply (1 : ll, function (x) pos_row [[x]] + seq (lf [x] + 3, by = 2, length = le [x])) #row's position in added matrix (2 rows and 2 columns are added in the marginal of initial matrix)
ef = sapply (1 : ll, function (x) (ef [[x]] - (ef1 [[x]] + 1)) / (lf [x] + 2) + 1 + ef1 [[x]] * (le [x] + 2)) # added rows and columns based on column's position
ef = sapply (1 : ll, function (x) ef [[x]][!is.na (ef [[x]])])
   
#----------------------------------------------------------------
#          Union Word Alignment without null
#----------------------------------------------------------------
if (method == 'union')
{
union = sapply (1 : ll, function (x) unique (c (ef [[x]], fe [[x]])))
pos_col = sapply (1 : ll, function (x) floor (union [[x]] / (le [x] + 2))) # column's number related to the source language in the matrix 
pos_row = sapply (1 : ll, function (x) union [[x]] - pos_col [[x]] * (le[x] + 2) - 1) # row's number related to the target language in the matrix 

align_un = sapply(1 : ll, function(x) paste (word3 [[x]][pos_row[[x]]], word2 [[x]][pos_col[[x]]]))
names(align_un) = 1 : ll

date2 = as.POSIXlt(Sys.time(), "Iran")

mylist = list(time = date2 - date1, method = method, alignment = c(align_un[first:last],tail(align_un)))
return (mylist)
}
#----------------------------------------------------------------
#         Intersection Word Alignment without null
#----------------------------------------------------------------

if (method == 'intersection')
{
intersection = sapply (1 : ll, function(x)fe [[x]][fe [[x]] %in% ef[[x]]])

pos_col = sapply (1 : ll, function (x) floor (intersection [[x]] / (le [x] + 2))) # column's number related to the source language in the matrix 
pos_row = sapply (1 : ll, function (x) intersection [[x]] - pos_col [[x]] * (le[x] + 2) - 1) # row's number related to the target language in the matrix 

align_in = sapply(1 : ll, function(x) paste ( word3 [[x]][pos_row[[x]]], word2 [[x]][pos_col[[x]]]))
names(align_in) = 1 : ll

date2 = as.POSIXlt(Sys.time(), "Iran")

mylist = list(time = date2 - date1, method = method, alignment = c(align_in[first:last],tail(align_in)))
return(mylist)
}
#----------------------------------------------------------------
#          GROW-DIAG Word Alignment without null
#----------------------------------------------------------------
if(method=='grow-diag')
{    
iii = sapply (1 : ll, function(x) squareN (fe [[x]],ef [[x]],(le [x] + 2)))

pos_col = sapply (1 : ll, function (x) floor (iii [[x]] / (le [x] + 2))) # column's number related to the source language in the matrix 
pos_row = sapply (1 : ll, function (x) iii [[x]] - pos_col [[x]] * (le[x] + 2) - 1) # row's number related to the target language in the matrix 

symmet = sapply(1 : ll, function(x) paste ( word3 [[x]][pos_row[[x]]], word2 [[x]][pos_col[[x]]]))
names(symmet) = 1 : ll

date2 = as.POSIXlt(Sys.time(), "Iran")

mylist = list(time = date2 - date1, method = method, alignment = c(symmet[first:last],tail(symmet)))
return(mylist)
}
}
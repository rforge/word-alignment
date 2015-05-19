word_alignIBM1 <-
function(file1,file2,nrec = -1,iter = 4,mlen = 40,lus = FALSE,lut = TRUE,map = c('ef1','fe1'),sym = FALSE,input = FALSE,dtfile = NULL,word1 = TRUE,number = FALSE,file3 = 'dd.RData',la = TRUE,first = 1,last = 5)
{
map = match.arg (map)

date1 = as.POSIXlt (Sys.time(), "Iran")
a = b = c ()
#-----------------------Translation:f to e ----------------------
aa = preparData (file1, file2, nrec = nrec, mlen = mlen, lus = lus, lut = lut, word_align = TRUE, map = 'ef', la = la)
a = aa[[3]]

if (is.null(dtfile))
{
if (map == 'fe1') 
{
aa = preparData (file1, file2, nrec = nrec, mlen = mlen, lus = lus, lut = lut, word_align = TRUE, map = 'fe', la = la )
a = aa [[3]]
}

b = apply (a, 1, function (x) {Vt1 = strsplit (as.character (x [1]), ' ') [[1]]; Vt2 = strsplit (as.character (x[2]), ' ') [[1]];
Vt1 = Vt1 [Vt1 != '']; Vt2 = Vt2 [Vt2 != '']; cbind (Var1 = rep.int (Vt1, length (Vt2)), Var2 = rep (Vt2, each = length (Vt1)))})

cc = vapply (b,length,FUN.VALUE=0)

#-------------------------- main code ---------------------------       
if (map == 'ef1')
{dd1 = data.table (g = rep (1 : aa [[2]], (cc/2)), f = unlist (sapply (b, function (x) x [,1])), e = unlist (sapply (b, function (x) x [,2])), t = as.numeric (rep (1 / (cc/2), (cc/2))))}

if (map == 'fe1')
{dd1 = data.table (g = rep (1 : aa [[2]], (cc/2)), f = unlist (sapply (b, function(x)x[,2])), e = unlist (sapply (b, function(x) x[,1])), t = as.numeric (rep (1 / (cc/2), (cc/2))))}

rm (b, cc)
gc ()

iteration = 0
for (iiiii in 1 : iter)
{
iteration = iteration + 1
dd1 [, count0 := t / sum(t), by = paste (g, e)]
dd1 [, t := NULL]
dd1 [, count := sum (count0), by = paste (e, f)]
dd1 [, total := sum (count0), by = f]
dd1 [, t := count/total]
dd1 [, count0 := NULL]
dd1 [, count := NULL]
dd1 [, total := NULL]
}
save (dd1,iteration, file = paste(dtfile, map, 'RData', sep = '.'))
if (input) return (dd1)
}
# -------------------- Using saved file ------------------------
if (! is.null (dtfile))
{
load(dtfile)
if (input) return (dd1)
}
#--------------------- Best alignment --------------------------
word = strsplit (a, ' ')
word = sapply (1 : (2 * aa [[2]]), function (x) word [[x]] [nzchar (word[[x]])])
word2 = word [1 : aa [[2]]]
word3 = word [ (aa [[2]] + 1) : (2 * aa [[2]])]

lf = vapply (word2, length, FUN.VALUE = 0)
le = vapply (word3, length, FUN.VALUE = 0) 

if (map == 'ef1')
{
dd1 [, i := unlist (sapply (1 : aa [[2]], function (x) rep (0 : (lf [x]-1), le [x])))]
dd1 [, j := unlist (sapply (1 : aa [[2]], function (x) rep (1 : (le[x]), each = lf [x])))]

d1 = dd1 [, i [ which.max (t)], by = paste (g, j)] [[2]]

c1 = c (0, cumsum (le))

if (word1)  ef = sapply (1 : aa[[2]], function (x) paste (word3 [[x]], word2[[x]] [d1 [ (c1 [x] + 1) : c1 [x + 1]] + 1]))

if (number) 
{
ef = sapply (1 : aa [[2]], function (x) d1 [ (c1 [x] + 1) : c1 [x + 1]])
if (sym) return (ef)
}
}

if (map == 'fe1')
{
dd1[, i := unlist (sapply (1 : aa [[2]], function (x) rep (0 : (le [x] - 1), each = lf [x])))]
dd1[, j := unlist (sapply (1 : aa [[2]], function (x) rep (1 : (lf [x]), le [x])))]

d1 = dd1 [, i [which.max (t)], by = paste (g,j)] [[2]]

c1 = c(0, cumsum (lf))

fe = sapply (1  :aa [[2]], function(x) d1 [(c1 [x] + 1) : c1 [x + 1]])

return (fe)
}
#------------- Expected Length of both languages----------------

ex1 = mean (lf) - 1 
ex2 = mean (le)

#------------- Vocabulary size of both languages----------------

v.s1 = length (unique (unlist (word2)))
v.s2 = length (unique (unlist (word3)))

#----------------- Word Translation Probability ----------------
dd2 = unique (dd1 [, t, by = paste (e, f)])

date2=as.POSIXlt(Sys.time(), "Iran")
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
mylist = list (number_input_sentences = aa [[1]], number_used_sentences = aa [[2]], time = date2 - date1,
iterIBM1 = iteration, expended_l_source = ex1, expended_l_target = ex2, VocabularySize_source = v.s1,
VocabularySize_target = v.s2, word_translation_prob = dd2, word_align = ef[first:last] )

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
return (mylist)
}

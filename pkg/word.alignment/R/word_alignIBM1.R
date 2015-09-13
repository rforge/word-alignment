word_alignIBM1 <-
function(file_train1, file_train2, nrec = -1, iter = 4, minlen = 5, maxlen = 40, ul_s = FALSE, ul_t = TRUE, intrnt = TRUE, display = c("word1","number"), dtfile = NULL, f1 = 'fa',e1 = 'en', sym = FALSE, input = FALSE)
{
display = match.arg (display)

date1 = as.POSIXlt (Sys.time(), "Iran")
a = b = count0 = count = total = i = j = e = f = g = c ()

#-----------------------Translation:f to e ----------------------
aa = preparData (file_train1, file_train2, nrec = nrec, minlen = minlen, maxlen = maxlen, ul_s = ul_s, ul_t = ul_t, intrnt = intrnt)

word4 = cbind (word2 = aa[[3]], word3 = aa[[4]])
word4 = word4 [apply (word4, 1, function(x) length(x[1] $ word2) * length (x[2] $ word3) != 0)]

word2 = word4[1 : (length(word4) / 2)]; word2 = sapply(1 : length(word2), function(x)c('null', word2[[x]]))
word3 = word4[(length(word4) / 2 + 1) : length(word4)]  

a = cbind( sapply(word2 , paste, collapse = ' '), sapply(word3, paste, collapse = ' '))

if (is.null(dtfile))
{
b = apply (a, 1, function (x) {Vt1 = strsplit (as.character (x [1]), ' ') [[1]]; Vt2 = strsplit (as.character (x[2]), ' ') [[1]];
Vt1 = Vt1 [Vt1 != '']; Vt2 = Vt2 [Vt2 != '']; cbind (Var1 = rep.int (Vt1, length (Vt2)), Var2 = rep (Vt2, each = length (Vt1)))})

cc = vapply (b,length,FUN.VALUE=0)

#-------------------------- main code ---------------------------       
dd1 = data.table (g = rep (1 : aa [[2]], (cc/2)), f = unlist (sapply (b, function (x) x [,1])), e = unlist (sapply (b, function (x) x [,2])), t = as.numeric (rep (1 / (cc/2), (cc/2))))

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
save (dd1,iteration, file = paste(f1, e1, nrec, iter, 'RData', sep = '.'))
if (input) return (dd1)
}
# -------------------- Using saved file ------------------------
if (! is.null (dtfile))
{
load(paste(f1, e1, nrec, iter, 'RData',sep='.'))
if (input) return (dd1)
}
#--------------------- Best alignment --------------------------
lf = vapply (word2, length, FUN.VALUE = 0)
le = vapply (word3, length, FUN.VALUE = 0) 

dd1 [, i := unlist (sapply (1 : aa [[2]], function (x) rep (0 : (lf [x]-1), le [x])))]
dd1 [, j := unlist (sapply (1 : aa [[2]], function (x) rep (1 : (le[x]), each = lf [x])))]

d1 = dd1 [, i [ which.max (t)], by = paste (g, j)] [[2]]

c1 = c (0, cumsum (le))

if (display == 'word1')  ef = sapply (1 : aa[[2]], function (x) paste (word3 [[x]], word2[[x]] [d1 [ (c1 [x] + 1) : c1 [x + 1]] + 1], sep = ':', collapse='    '))

if (display == 'number') 
{
ef = sapply (1 : aa [[2]], function (x) d1 [ (c1 [x] + 1) : c1 [x + 1]])
if (sym) return (ef)
}
#------------- Expected Length of both languages----------------

ex1 = mean (lf) - 1 
ex2 = mean (le)

#------------- Vocabulary size of both languages----------------

v.s1 = length (unique (unlist (word2)))
v.s2 = length (unique (unlist (word3)))

#----------------- Word Translation Probability ----------------
dd2 = unique (dd1 [, t, by = paste (e,f)])

names(ef) = 1:aa[[2]]

date2=as.POSIXlt(Sys.time(), "Iran")
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
mylist = list (n1 = aa [[1]], n2 = aa [[2]], time = date2 - date1,
iterIBM1 = iteration, expended_l_source = ex1, expended_l_target = ex2, VocabularySize_source = v.s1,
VocabularySize_target = v.s2, word_translation_prob = dd2, word_align = ef)

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
attr(mylist, "class") <- "alignment"
return (mylist)
}
print.alignment <-
function(obj) 
{
print(obj $ time)
cat("Number of input sentence pairs is", obj[[1]], "\n")
cat("Number of used sentence pairs is", obj[[2]], "\n")
cat("Number of iteration for IBM model 1 is ", obj[[4]], "\n")
cat("Word alignment for some sentence pairs are", "\n")
print (as.character(obj $ word_align[1 : 3]))
cat("            ", ".", "\n")
cat("            ", ".", "\n")
cat("            ", ".", "\n")
print (as.character(obj $ word_align[(length(obj $ word_align) - 2) : length(obj $ word_align) ] ))
}


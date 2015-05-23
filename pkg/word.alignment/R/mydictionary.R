mydictionary <-
function (file1, file2, iter = 10, nrec = -1, lus = FALSE, lut = TRUE, prob = 0.9, lang1 = 'Farsi', lang2 = 'English', la = TRUE, dtfile = NULL)
{
date1 = as.POSIXlt (Sys.time(), "Iran")

e = f = c()

if(is.null (dtfile))
{
dd1 = word_alignIBM1 (file1, file2, iter = iter, nrec = nrec, lus = lus, lut = lut, input = TRUE, la = la)
save(dd1, file = 'dt.RData')
}

if( ! is.null(dtfile))
load(dtfile)

u1 = unique (dd1 [round (t, 1) > prob, f, e])
fe = matrix (c (u1$f, u1$e), ncol = 2)
colnames(fe) = c (lang1, lang2)
fe = fe [order (fe [,1]),]

date2 = as.POSIXlt (Sys.time(), "Iran")
##################################################################
mylist = list (time = date2 - date1, number_input = nrec, iterIBM1 = iter, dictionary = fe)
##################################################################
return (mylist)
}

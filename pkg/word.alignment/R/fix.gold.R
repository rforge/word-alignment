fix.gold <-
function (source1, target1, nrec = -1, mlen = 40, lus = FALSE, lut = TRUE, kk)
{
p1 = preparData (source1, target1, nrec = nrec, mlen = mlen, lus = lus, lut = lut)

readline("Now, press any key to continue and edit the matrix to enter Sure/Possible alignments (Sure=1,Possible=2).")

mm = sapply (1 : p1$length, function (x) {m = matrix (0, length (p1 [[2]][[x]]) + 1, length (p1 [[3]][[x]]) + 1);
m [2 : nrow (m), 1] = p1 [[2]][[x]]; m [1, 2 : ncol(m)] = p1 [[3]][[x]]; m [1, 1] = ''; m})
fg = mm [[kk]]
fix (fg)
}

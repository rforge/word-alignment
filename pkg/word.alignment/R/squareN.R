squareN <-
function (fe, ef, nrow)
{
kk = function (x,y) c ((x - 1), (x - 1 + 2), (x - 1 - y) : (x - 1 - y + 2), (x - 1 + y) : (x - 1 + y + 2))
iii = fe [fe %in% ef]
i2 = length (iii)
if (i2 == 0) {iii = 0; return (iii)}
repeat
{
i2 = length (iii)
s = sapply (iii, kk, nrow)
if (i2 != 0) iii = as.numeric (names (table (c (iii, cbind (s,s) [matrix (c (s %in% ef, s %in% fe), ncol = 2 * length (iii))]))))
if (i2 == length (iii)) break
}
iii
}

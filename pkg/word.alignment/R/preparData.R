preparData <-
function(file1, file2, nrec = -1, minlen = 5, maxlen = 40, ul_s = FALSE, ul_t = TRUE, all = FALSE, intrnt = TRUE)
{
s_sen = t_sen = aa = t = c()

s_sen = readLines (con <- file(file1), encoding = 'UTF-8', n = nrec, warn = FALSE)
close(con)

t_sen = readLines (con <- file(file2), encoding = 'UTF-8', n = nrec, warn = FALSE)
close(con)

if (length(s_sen) == length(t_sen))
{
for (k1 in 1 : length (s_sen)) if (s_sen[k1] == '') {t_sen [k1+1] = paste (t_sen [k1], t_sen [k1+1]); t_sen [k1] = ''}

for (k2 in 1 : length (t_sen)) if (t_sen[k2] == '') {s_sen [k2+1] = paste (s_sen [k2], s_sen [k2+1]); s_sen [k2] = ''}
}

s_sen = s_sen [nzchar (s_sen)]
t_sen = t_sen [nzchar (t_sen)]

aa = cbind(s_sen,t_sen)
l1 = nrow(aa)

#------------------------- Tokenization --------------------------
  
aa = aa [apply (aa, 1, function(x) prod (vapply (strsplit (x, ' '), length, FUN.VALUE=0) >= minlen)& prod (vapply (strsplit (x, ' '), length, FUN.VALUE=0) <= maxlen) == 1) ,]
    
if (ul_s) aa[,1] = culf (aa [,1], lower = all)
if (ul_t) aa[,2] = culf (aa [,2], lower = all)

rm (s_sen, t_sen)
gc ()

aa = RmTokenizer (aa, intrnt = intrnt, split = TRUE)
l2 = length(aa) / 2

aa = sapply (1 : (2 * l2), function (x) aa [[x]] [nzchar (aa[[x]])])

aa1 = aa [1 : l2]
aa2 = aa [ (l2+1) : (2 * l2)]

aa = cbind (aa1, aa2)
aa = aa [apply(aa, 1, function(x) length (x[1] $ aa1) * length (x [2] $ aa2) != 0)]

l2 = length(aa) / 2

list1 = list (initial = l1, used = l2, sorc.tok = aa [1 : l2], trgt.tok = aa[ (l2 + 1) : (2 * l2)] )
return (list1)
}

preparData <-
function(file1, file2, nrec = -1, mlen = 40, lus = FALSE, lut = TRUE, all = FALSE, word_align = FALSE, la = TRUE)
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
  
aa = aa [apply (aa,1,function(x) prod (vapply (strsplit (x, ' '), length, FUN.VALUE=0) <= mlen) == 1) ,]
    
if (lus) aa[,1] = culf (aa [,1], lower = all)
if (lut) aa[,2] = culf (aa [,2], lower = all)

#------------Preparing Data For Word Alignment IBM 1 --------------

if (word_align) 
{
rm (s_sen, t_sen)
gc ()
aa = RmTokenizer (aa, la = la)
aa = matrix (aa [apply (aa, 1, function(x) (nchar (stripWhitespace (x[1])) > 1) & (nchar(stripWhitespace(x[2])) > 1))], ncol = 2)
l3 = nrow(aa)

aa [,1] = paste ('null', aa [,1])

list1 = list (initial=l1, used=l3, sen_pair = aa)
return (list1)
}

#-------Preparing Data For Evaluation of Word Alignment --------

aa = RmTokenizer (aa, la = la, split = TRUE, rmBlank = TRUE)
aa = sapply (aa, function (x) x [x != ''])
l2 = length (aa) / 2

list2 = list (length1 = l2, source.tok = aa [1 : l2], target.tok = aa [ (l2 + 1) : (2 * l2)])
return (list2)
}

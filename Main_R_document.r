setwd("C:\Users\pancr\OneDrive\Documents\R\Porject_1_Group_44")
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

split_punct
setwd("C:/Users/pancr/OneDrive/Documents/R/Porject_1_Group_44")
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

# find words containting
punctuation_marks <- c(",",".",";","!",":","?")

i=5

c <- grep(punctuation_marks[i],a,fixed = TRUE)
lc <- length(c)

#create empty string
ns <- rep("",2*lc+n)

#location of split words
rcl <- c+2*(1:lc)
ccl <- rcl-1
lcl <- rcl-2

#split words
wlr <- strsplit(a[c], punctuation_marks[i])
wrl <- ""
for (val in 1:lc) {wrl <- c(wrl,wlr[[val]])}

#location of unsplit words
nwl <- c(rcl,ccl,lcl) 


ns[ccl] <- punctuation_marks[i]
ns[lcl] <- wrl[seq(2,lc,2)]
ns[rcl] <- wrl[seq(3,lc+1,2)]
ns[-nwl] <- a[-c]

#ns[-nwl] <- an
rm(wrl)
print(ns)

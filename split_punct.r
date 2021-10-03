setwd("C:/Users/pancr/OneDrive/Documents/R/Porject_1_Group_44")
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

# find words containting
punctuation_marks <- c(",",";","!",":","\\?","\\.")

for (i in 1:6) {
    n <- length(a)
    c <- grep(punctuation_marks[i],a)
    lc <- length(c)

    if (lc>0){
        #create empty string
        ns <- rep("",2*lc+n)
        
        #location of split words
        rcl <- c+2*(1:lc)
        ccl <- rcl-1
        lcl <- rcl-2
    
        #split words
        wlr <- strsplit(a[c], punctuation_marks[i])
        for (val in 1:lc) {
            if (length(wlr[[val]])<2) {
                wlr[[val]] <- c(wlr[[val]],"")
                }}
        wrl <- matrix(unlist(wlr), ncol=2, byrow=TRUE)

        #location of unsplit words
        nwl <- c(rcl,ccl,lcl) 
        if (i==5){
            ns[ccl] <- "?"} 
            else if (i==6){
                ns[ccl] <- "."}
                else{
                    ns[ccl] <- punctuation_marks[i]}
        ns[lcl] <- wrl[,1]
        ns[rcl] <- wrl[,2]
        ns[-nwl] <- a[-c]
        a <- ns 
        rm(wrl)
        rm(wlr)
        }}

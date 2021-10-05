setwd("C:/Users/pancr/OneDrive/Documents/R/Porject_1_Group_44")
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

#assign a value to m
m <- 1000

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
a <- a[a != ""]

#part question 6

#remove capital letter
b <- tolower(a)

#create list of all different words in a
un <- unique(b)

#assign numbers to words
c <- match((b),(un))
#count thow often the words re-occur
count <- tabulate(c)

#find how often the m most frequent word re-occur (in bbb)
s_count <- sort(count)
bbb <- s_count[(length(count)-m+1):length(count)]

#find the most m commen used words
common_words_loc <- count %in% bbb
common_words <- un[common_words_loc]

b <- common_words

#question 7
a_index <- match(a_low, b)
column_matrix <- cbind(a_index[-length(a_index)],a_index[-1])
pair_index <- which(!is.na(rowSums(column_matrix)))
word_pairs <- column_matrix[pair_index,]

m <- length(b)

A <- matrix(0,m,m) # m is the number of common words aka length b
for (count in 1:nrow(word_pairs)){
  i <- word_pairs[count,1]
  j <- word_pairs[count,2]
  A[i,j] <- A[i,j]+1
}
A <- t(apply(A, 1, function(x)(x/sum(x)))) 
A <- gsub(NaN,1/length(b),A)
A <- t(matrix(A, ncol=length(b), byrow=TRUE))

#question 8
prev_word_index <-sample(1:length(b),1)
end_text_index <- c(prev_word_index)

for (temp in 2:50){
  chosen_word_index <- sample(1:length(b),1,prob=A[prev_word_index,])
  end_text_index <- c(end_text_index, chosen_word_index)
  prev_word_index <- chosen_word_index
}

print(b[end_text_index])
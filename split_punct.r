setwd("C:/Users/pancr/OneDrive/Documents/R/Porject_1_Group_44")
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

#assign a value to m
m <- 1000

# find words containting
punctuation_marks <- c(",",";","!",":","\\?","\\.")

split_punct <-function (a){
for(i in 1:6) {# find and seperate the 6 different characters one by one
    n <- length(a)
    c <- grep(punctuation_marks[i],a) #find all locations of the ith punctuation mark
    lc <- length(c)

    if (lc>0){
      # this makes sure only an attempt to sperate word from punctuation marks is done 
      # when the text contains these punctuations marks
        
        #create empty string able to contain all of the string after the speration.
        ns <- rep("",2*lc+n)
        
        # determin the location of split words
        # three places are reserved for every interpunction mark
        # one for the text connected to the right and left of the punctuations mark
        # and one for the puncutation mark it self
        rcl <- c+2*(1:lc)
        ccl <- rcl-1
        lcl <- rcl-2
    
        # split words with a punctuation mark in three
        wlr <- strsplit(a[c], punctuation_marks[i])

        # create an empty string right of the punctuation mark it it does not exist
        # so every word with an interpunction is split in the same format
        for (val in 1:lc) {
            if (length(wlr[[val]])<2) {
                wlr[[val]] <- c(wlr[[val]],"")
                }}

        # make a matrix containing the text on the left and right of the interpunction 
        wrl <- matrix(unlist(wlr), ncol=2, byrow=TRUE)

        # paste the interpunction marks used to split the words in the text at their propper locatoin
        # to avoid the placement of "//" in th text ? and . have to be placed in a different way
        # to do so the if statemetn is used
        if (i==5){
            ns[ccl] <- "?"} 
            else if (i==6){
                ns[ccl] <- "."}
                else{
                    ns[ccl] <- punctuation_marks[i]}
        
        # paste the word left of the interpunction marks in the propper location 
        ns[lcl] <- wrl[,1]
        # paste the word right of the interpunction marks in the propper location 
        ns[rcl] <- wrl[,2]

        # nwl contains all locations in the text reseved for words split by interpunction marks and interpunctions words
        nwl <- c(rcl,ccl,lcl) 
        # past all other words in the empty string
        ns[-nwl] <- a[-c]
        # save the new string of words to a
        a <- ns

        # reset variables needed for next next itteration
        ##### i think this can be deleted #####
        rm(wrl)
        rm(wlr)
        }}
# eliminate all empty entries in string a
a <- a[a != ""]
return(a)}

#part question 6

a_low <- tolower(a) #lowers capital letters in words
unique_words <- unique(a_low) #obtains unique words in a
index_vector <- match(a_low,unique_words) #creates a vector length of a which indicates the word's index in unique_words
freq <- tabulate(index_vector) #creates a vector length of b where each value corresponds to the frequency of the word with the same index in b

th <- 1 #initializing the threshold value to retain 1000 words
ncw <- length(unique_words) #initializing the number of common words with the given threshold value
desired_word_count <- 1000 #as requested in the Practical 1 document

while (ncw >= desired_word_count) {
  p_th <- th
  th <- th + 1
  p_ncw <- ncw
  ncw <- sum(freq >= th)
}

if (desired_word_count - ncw < p_ncw - desired_word_count) {
  m <- ncw
  threshold <- th
} else {
  m <- p_ncw
  threshold <- p_th
}

b <- unique_words[which(freq >= threshold)]

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

cat(b[end_text_index])

dif <- a==a_low
loc_cap <- grep(FALSE,dif)
cap_words <- a_low[dif]
index_cap_vector <- match(a_low,cap_words)
freq_cap <- tabulate(index_cap_vector)
#assuming that the previous 5 step is done and the matrix with properly split words is called a

#just a random "a" vector to work on
a <- c("An","NASA","omnishambles",",","in","a","headless","a","a","chicken","an","an","factory","tum","tee","tum","tee","tumpty","tum","wibble","wobble")

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


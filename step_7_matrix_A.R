#assuming a_low is the final a matrix, and b is the array of ~1000 common words

a_index <- match(a_low, b)
column_matrix <- cbind(a_index[-length(a_index)],a_index[-1])
pair_index <- which(!is.na(rowSums(column_matrix)))
word_pairs <- column_matrix[pair_index,]

A <- matrix(0,m,m) # m is the number of common words aka length b
for (count in 1:nrow(word_pairs)){
  i <- word_pairs[count,1]
  j <- word_pairs[count,2]
  A[i,j] <- A[i,j]+1
}

t(apply(A, 1, function(x)(x/sum(x)))) 

# Machine Problem
# by Marlon Vincent Abella

# 2. Define an R function that computes the factorial of given an integer argument.
# The output should be a vector of length 1.

factorial <- function(y) {
  x <- 1
  for(i in 1:y){
    x <-x*((1:y)[i])
  }
  print(x)
}


# 3. Define an R function that computes the determinant of a given matrix.
# The output should be a vector of length 1.

Determinant <- function(a,b) {
  if (dim(a)[1] == 1 && dim(a)[2] == 1)
    return(a[1,1])
  if (dim(a)[1] == 2 && dim(a)[2] == 2)
    return(a[1,1]*a[2,2]-a[1,2]*a[2,1])
  else
    n = 0
  for (i in 1:dim(a)[2]) {
    n = n + a[b,i]*(-1)^(b+i)*
      Determinant(a[-b,-i],b)
  }
  return(n)
}
 
# 7. Create a function that accepts a vector and an integer n and returns nth highest number 

Nth_High <- function(vector, n){
  TopN <- c()
  ind <- c()
  for (i in 1:n){
    largest <- match(max(vector), vector)
    ind[i] <- largest
    TopN[i] <- max(vector)
    vector <- vector[-largest]
  }
  matrix <- cbind(TopN, ind)
  return(matrix[n])
}

 
# 8. Create a function that computes the compound interest of an
# investment given the rate, time, and initial amount or principal.

Comp_Int <- function(rate,time,P) {
  prin <- P * (1+rate/100)^(0:(time-1))
  int  <- prin * rate/100
  totalInt <- sum(int)
  totalInt
}


# 9. Create a function isPrime(n) that accepts an integer and outputs a Boolean
# value (TRUE or FALSE) depending whether the integer is a prime number or not.

isPrime <- function(n) {
  ifelse(sum(n %% (1:n) == 0) > 2, FALSE, TRUE)
}







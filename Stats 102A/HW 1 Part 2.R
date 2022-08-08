# Our most notable prime numbers are 1, 2, 3

prime_factor <- function(x) {
  # create empty vector for loop
  # Our main focus is around the prime number 2
  vect <- c()
  i = 2
  # set up for Prime numbers or whole numbers possibilities
  # choosing the first 3 prime numbers to give "ERROR" result
  if(x <= 3) {
    cat('Already a Prime Number OR cannot be negative! \n', x, "Please choose another number. \n")
  }
  # Start with values greater than 2
  if(2 < x) {
  vect <- c(vect, x)
  }
  # Find out if divisible by 2 with no remainders
  while(x %% 2 == 0) {
    vect <- c(vect, 2)
    x = x %/% 2 
  }
  # This is to cover for prime numbers of 5, 7, 11, etc. using index + prime even number
  i <- (i + 2)
  while(x %% i == 0 ) {
    vect <- c(vect, i)
    x = x %% i + 2
  }
  return(vect)
}

# Check:
# I'm not sure how to remove the first element which returns my x-value.
prime_factor(4)
prime_factor(3)
prime_factor(2)

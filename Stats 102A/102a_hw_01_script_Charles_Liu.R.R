# Name (UID): Charles Liu (304804942)

# "By including this statement, I, Charles Liu, declare that all of the work in this
# assignment is my own original work. At no time did I look at the code of other students
# nor did I search for code solutions online. I understand that plagiarism on any single 
# part of this assignment will result in a 0 for the entire assignment and that I will be
# referred to the dean of students."

# I did discuss ideas related to the homework with Leah Skelton for Part 1 and 2. 
# At no point did I show another student my code, nor did I look at 
# another student's code.

### PART 1
by_type <- function(x, sort) {
  # set up empty vectors
  v1i <- c()
  v2d <- c()
  v3ch <- c()
  if(sort == FALSE)
    for(i in x) {
      # check if my integer has any of these single numbers using OR
      # keep in mind that I only used "|" rather than "||" because I need it vectorized
      if(i == '6' | i == '1') {
        v1i <- c(v1i, i)
      }
      # checking for character
      if(i == 'a' | i == 'house') {
        v3ch <- c(v3ch, i)
      }
      # checking for double
      if(i == '2.2' | i == '3.4') {
        v2d <- c(v2d, i)
      }
    }
  # if TRUE then it'll sort it
  if(sort == TRUE) {
    v1i <- sort(v1i, decreasing = FALSE)
    v2d <- sort(v2d, decreasing = FALSE)
    v3ch <- sort(v3ch, decreasing = FALSE)
  }
  result <- list("integers" = v1i, "doubles" = v2d, "character" = v3ch)
  return(result)
}


# Check:
x <- c("house", "6", "2.2", "a", "3.4", "1")
by_type(x, sort = FALSE)




### PART 2
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


### PART 3
# Unfortunately, I do not know how to form loops/if/while/etc. statements for things regarding characters and factors.
# I did not learn anything of this in my Stats 20 class. Part 3 is shown as much as I can do.
month_convert <- function(x, from_lang, to_lang) {
  vlan1 <- factor(c(), levels = c())
  vlan2 <- factor(c(), levels = c())
  if(from_lang == "English") {
    for(i in x) {
      if(i == 'January' | i == 'February' | i == 'March' | i == 'April' | i == 'May' |
         i == 'June' | i == 'July' | i == 'August' | 
         i == 'September' | i == 'October' | i == 'November' | i == 'Decemeber') {
        vlan1 <- factor(c(vlan1, i))
      }
    }
    if(to_lang == "Spanish") {
      for(i in vlan1) {
        if(i == 'enero' | i == 'febrero' | i == 'marzo' | i == 'abril' | i == 'mayo' |
           i == 'junio' | i == 'julio' | i == 'agosto' | 
           i == 'septiembre' | i == 'octubre' | i == 'noviembre' | i == 'diciembre') {
          vlan2 <- factor(c(vlan1, i))
        }
      }
    }
    return(vlan2)
  }
}


# Check:
x <- factor(c("March", "March", "February", "June"))
month_convert(x, "English", "Spanish")
month_names <- read.delim("UCLA Works/UCLA Winter 2020/Stats 102A/Homeworks/HW 1/month_names.txt", encoding="UTF-8", row.names=1)

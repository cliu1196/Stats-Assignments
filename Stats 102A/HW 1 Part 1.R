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


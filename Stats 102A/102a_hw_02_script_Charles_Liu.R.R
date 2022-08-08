# Stats 102A HW 2
## Part 1
Board <- list("nrow" = 10, "ncol" = 10, "Chutes" =  '98, 95, 93, 87, 64, 62, 56, 49, 47, 16',
              "Ladders" = '1, 4, 9, 21, 28, 36, 51, 71, 80', "Start Point" = '1', 
              "End Point" = '100')
Board



## Part 2
show_board <- function(miniboard = FALSE) {
  if(miniboard == FALSE) {
plot.new()
# Vertical Line
plot.window(xlim = c(0, 55), ylim = c(0, 55))
segments(x0 = seq(from = 5, to = 55, by = 5), x1 = seq(from = 5, to = 55, by = 5), 
         y0 = 0, y1 = 100)
# Horizontal Line
segments(x0 = 0, x1 = 100, 
         y0 = seq(from = 5, to = 55, by = 5), y1 = seq(from = 5, to = 55, by = 5))
text(x = seq(from = 7, to = 55, by = 5), y = 8, label = '1':'10')
text(x = seq(from = 7, to = 55, by = 5), y = 13, label = '20':'11')
text(x = seq(from = 7, to = 55, by = 5), y = 18, label = '21':'30')
text(x = seq(from = 7, to = 55, by = 5), y = 23, label = '40':'31')
text(x = seq(from = 7, to = 55, by = 5), y = 28, label = '41':'50')
text(x = seq(from = 7, to = 55, by = 5), y = 33, label = '60':'51')
text(x = seq(from = 7, to = 55, by = 5), y = 38, label = '61':'70')
text(x = seq(from = 7, to = 55, by = 5), y = 43, label = '80':'71')
text(x = seq(from = 7, to = 55, by = 5), y = 48, label = '81':'90')
text(x = seq(from = 7, to = 55, by = 5), y = 53, label = '100':'91')

arrows(x0 = 5, x1 = 19, 
       y0 = 4, y1 = 25, col = 'green', lwd = 2)
arrows(x0 = 20, x1 = 38, 
       y0 = 5, y1 = 14, col = 'green', lwd = 2)
arrows(x0 = 48, x1 = 52, 
       y0 = 5, y1 = 25, col = 'green', lwd = 2)
arrows(x0 = 5, x1 = 14, 
       y0 = 15, y1 = 30, col = 'green', lwd = 2)
arrows(x0 = 45, x1 = 24, 
       y0 = 15, y1 = 48, col = 'green', lwd = 2)
arrows(x0 = 30, x1 = 23, 
       y0 = 20, y1 = 28, col = 'green', lwd = 2)
arrows(x0 = 55, x1 = 37, 
       y0 = 30, y1 = 37, col = 'green', lwd = 2)
arrows(x0 = 53, x1 = 53, 
       y0 = 40, y1 = 55, col = 'green', lwd = 2)
arrows(x0 = 7, x1 = 7, 
       y0 = 42, y1 = 53, col = 'green', lwd = 2)

arrows(x0 = 18, x1 = 18, 
       y0 = 53, y1 = 40, col = 'red', lwd = 2)
arrows(x0 = 33, x1 = 33, 
       y0 = 53, y1 = 40, col = 'red', lwd = 2)
arrows(x0 = 43, x1 = 43, 
       y0 = 53, y1 = 40, col = 'red', lwd = 2)
arrows(x0 = 23, x1 = 5, 
       y0 = 38, y1 = 33, col = 'red', lwd = 2)
arrows(x0 = 13, x1 = 13, 
       y0 = 38, y1 = 12, col = 'red', lwd = 2)
arrows(x0 = 25, x1 = 43, 
       y0 = 33, y1 = 33, col = 'red', lwd = 2)
arrows(x0 = 48, x1 = 54, 
       y0 = 30, y1 = 10, col = 'red', lwd = 2)
arrows(x0 = 38, x1 = 33, 
       y0 = 30, y1 = 17, col = 'red', lwd = 2)
arrows(x0 = 28, x1 = 33, 
       y0 = 14, y1 = 8, col = 'red', lwd = 2)
}
  else {
    plot.new()
    plot.window(xlim = c(0, 50), ylim = c(0, 50))
    segments(x0 = seq(from = 5, to = 35, by = 5), x1 = seq(from = 5, to = 35, by = 5), 
             y0 = 0, y1 = 50)
    segments(x0 = 0, x1 = 50, 
             y0 = seq(from = 5, to = 40, by = 5), y1 = seq(from = 5, to = 40, by = 5))
    text(x = seq(from = 7, to = 35, by = 5), y = 8, label = '1':'6')
    text(x = seq(from = 7, to = 35, by = 5), y = 13, label = '12':'7')
    text(x = seq(from = 7, to = 35, by = 5), y = 18, label = '13':'18')
    text(x = seq(from = 7, to = 35, by = 5), y = 23, label = '24':'19')
    text(x = seq(from = 7, to = 35, by = 5), y = 28, label = '25':'30')
    text(x = seq(from = 7, to = 35, by = 5), y = 33, label = '36':'31')
    text(x = seq(from = 7, to = 35, by = 5), y = 38, label = '37':'42')
    
    arrows(x0 = 5, x1 = 13, 
           y0 = 5, y1 = 23, col = 'green', lwd = 2)
    arrows(x0 = 23, x1 = 18, 
           y0 = 10, y1 = 27, col = 'green', lwd = 2)
    arrows(x0 = 27, x1 = 33, 
           y0 = 22, y1 = 32, col = 'green', lwd = 2)
    arrows(x0 = 22, x1 = 27, 
           y0 = 30, y1 = 38, col = 'green', lwd = 2)
    
    arrows(x0 = 22, x1 = 32, 
           y0 = 18, y1 = 7, col = 'red', lwd = 2)
    arrows(x0 = 33, x1 = 33, 
           y0 = 30, y1 = 17, col = 'red', lwd = 2)
    arrows(x0 = 12, x1 = 18, 
           y0 = 33, y1 = 33, col = 'red', lwd = 2)
    arrows(x0 = 23, x1 = 12, 
           y0 = 38, y1 = 28, col = 'red', lwd = 2)
}
}

## TEST!!!!!!!!
show_board()



# Part 3
## Miniboard is done with TRUE!
show_board(miniboard = TRUE)



# Part 4
## For Ladders
func1 <- function(newposition) {
  Ladders <- rbind(c(1, 38), c(4, 14), c(9, 31), c(21, 42), c(28, 84), c(36, 44), 
                   c(51, 67), c(71, 91), c(80, 100))
  if(any(Ladders[, 1]) == newposition) {
    row1 <- Ladders[Ladders[,1] == 1]
    newval1 <- row1[2]
  }
  else {
    newval1 <- newposition
  }
  newval1
}

## For Chutes
func2 <- function(newposition) {
  Chutes <- rbind(c(98, 78), c(95, 75), c(93, 73),c(64, 60), c(62, 19), c(56, 53), 
                  c(49, 11), c(47, 26), c(16, 6))
  if(any(Chutes[, 1]) == newposition) {
    row2 <- Chutes[Chutes[,1] == 1]
    newval2 <- row2[2]
  }
  else {
    newval2 <- newposition
  }
  newval2
}

##GAME!
play_solo <- function(x, verbose) {
  # Using by rows for our Chutes or Ladders
  # c("land on", "moved to")
  # Possibly separate Chutes and Ladders
  Ladders <- rbind(c(1, 38), c(4, 14), c(9, 31), c(21, 42), c(28, 84), c(36, 44), 
                   c(51, 67), c(71, 91), c(80, 100))
  Chutes <- rbind(c(98, 78), c(95, 75), c(93, 73),c(64, 60), c(62, 19), c(56, 53), 
                  c(49, 11), c(47, 26), c(16, 6))
  
  newposition <- 0
  oldposition <- 0
  possiblemove <- 0
  turns <- 0
  moves <- 0
  ladders_tally <- 0
  chutes_tally <- 0
  
  if(verbose == TRUE) {
  
    while(all(oldposition < 100)) {
    cat("Turns:", turns + 1, "\n")
    cat("Started at:", oldposition, "\n")
    spin <- sample(1:6, 1)
    cat("Spin Landed On:", spin, "\n")
    possiblemove <- (oldposition + spin)
    if(possiblemove > 100) {
      while(possiblemove > 100) {
        cat("The number is over 100! \n", "Spin Again Please! \n")
        spin <- sample(1:6, 1)
        possiblemove <- oldposition + spin
      }
    }
    newposition <- possiblemove
    temp1 <- newposition
    newposition <- func1(newposition)
    if(temp1 != newposition && temp1 < newposition) {
      ladders_tally <- ladders_tally + 1
      cat("LADDERS!", "\n")
    }
    newposition <- func2(newposition)
    if(temp1 != newposition && temp1 > newposition) {
      chutes_tally <- chutes_tally + 1
      cat("CHUTES!", "\n")
    }
    oldposition <- newposition
    oldposition
    turns <- turns + 1
    cat("Ended at:", oldposition, "\n")
    moves <- append(moves, oldposition)
    cat("\n")
  }
  oldposition
  cat("YOU WIN after", turns, "Turns \n")
  cat("MOVES DONE:", moves, "\n")
  cat("Number of Ladders Done:", ladders_tally, "\n")
  cat("Number of Chutes Done:", chutes_tally, "\n")
  }
  else {
    while(all(oldposition < 100)) {
      spin <- sample(1:6, 1)
      possiblemove <- (oldposition + spin)
      if(possiblemove > 100) {
        while(possiblemove > 100) {
          spin <- sample(1:6, 1)
          possiblemove <- oldposition + spin
        }
      }
      newposition <- possiblemove
      temp1 <- newposition
      newposition <- func1(newposition)
      if(temp1 != newposition && temp1 < newposition) {
        ladders_tally <- ladders_tally + 1
      }
      newposition <- func2(newposition)
      if(temp1 != newposition && temp1 > newposition) {
        chutes_tally <- chutes_tally + 1
      }
      oldposition <- newposition
      oldposition
      turns <- turns + 1
      moves <- append(moves, oldposition)
    }
    oldposition
    cat("YOU WIN after", turns, "Turns \n")
  }
}



## TEST!!!!!!!!!!!
play_solo(player1, verbose = FALSE)



## Set up for board!
y <- matrix(nrow = 10, ncol = 10, byrow = TRUE, 100:1)
y
for(row in 1:10) {
  if(row %% 2 == 0) {
  y[row,] <- rev(y[row,])
  }  
}
y



## SPINNER
spin <- sample(1:6, 1)
spin



## Transition
Ladders <- rbind(c(1, 38), c(4, 14), c(9, 31), c(21, 42), c(28, 84), c(36, 44), 
                 c(51, 67), c(71, 91), c(80, 100))
Chutes <- rbind(c(98, 78), c(95, 75), c(93, 73),c(64, 60), c(62, 19), c(56, 53), 
                c(49, 11), c(47, 26), c(16, 6))
Ladders
row <- Ladders[Ladders[,1] == 71]
row[2]



## Game to 100 Exactly (OUTSIDE OF FUNCTION)
newposition <- 0
oldposition <- 0
possiblemove <- 0
turns <- 0
moves <- 0
ladders_tally <- 0
chutes_tally <- 0

while(all(oldposition < 100)) {
  cat("Turns:", turns + 1, "\n")
  cat("Started at:", oldposition, "\n")
  spin <- sample(1:6, 1)
  cat("Spin Landed On:", spin, "\n")
  possiblemove <- (oldposition + spin)
  if(possiblemove > 100) {
    while(possiblemove > 100) {
      cat("The number is over 100! \n", "Spin Again Please! \n")
      spin <- sample(1:6, 1)
      possiblemove <- oldposition + spin
    }
  }
  newposition <- possiblemove
  temp1 <- newposition
  newposition <- func1(newposition)
  if(temp1 != newposition && temp1 < newposition) {
    ladders_tally <- ladders_tally + 1
    cat("LADDERS!", "\n")
  }
  newposition <- func2(newposition)
  if(temp1 != newposition && temp1 > newposition) {
    chutes_tally <- chutes_tally + 1
    cat("CHUTES!", "\n")
  }
  oldposition <- newposition
  oldposition
  turns <- turns + 1
  cat("Ended at:", oldposition, "\n")
  moves <- append(moves, oldposition)
  cat("\n")
}
oldposition
cat("YOU WIN after", turns, "Turns \n")
cat("MOVES DONE:", moves, "\n")
cat("Number of Ladders Done:", ladders_tally, "\n")
cat("Number of Chutes Done:", chutes_tally, "\n")




# Part 5
replicate(10000, play_solo())

## 5a)
length(moves)
hist(moves, breaks = 50, rep(moves, 10000))

## 5b)
### I know it'll take about 7 turns for the minimum. I just can't figure out how to 
### code for the minimum number of turns due to my turn count being inside my while 
### loops.

## 5c)
max(moves, rep(moves, 10000))

## 5d)
median(moves, rep(moves, 10000))

## 5e)
mean(moves, trim = 0.5, rep(moves, 10000))

## 5f)
if(length(moves) > 100) {
  moves
}

## 5g)
if(length(moves) < 10) {
  moves
}

## 5h)
### Not quite sure how to do this one.

## 5i)
barplot(Chutes, xlab = "Chutes", ylab = "Frequency of Chutes")

## 5j)
barplot(Ladders, xlab = "Ladders", ylab = "Frequency of Ladders")
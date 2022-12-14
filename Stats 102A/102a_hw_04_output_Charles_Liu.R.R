# You will not simulate the entire game. You will simulate only the movement of pieces, 
# and will keep track of which squares the pieces land on.
library(R6)

# gameboard and decks -----------------------------------------------------

gameboard <- data.frame(
  space = 1:40, 
  title = c(
    "Go", "Mediterranean Avenue", "Community Chest", "Baltic Avenue",
    "Income Tax", "Reading Railroad", "Oriental Avenue", "Chance",
    "Vermont Avenue", "Connecticut Avenue", "Jail", "St. Charles Place",
    "Electric Company", "States Avenue", "Virginia Avenue",
    "Pennsylvania Railroad", "St. James Place", "Community Chest",
    "Tennessee Avenue", "New York Avenue", "Free Parking",
    "Kentucky Avenue", "Chance", "Indiana Avenue", "Illinois Avenue",
    "B & O Railroad", "Atlantic Avenue", "Ventnor Avenue", "Water Works",
    "Marvin Gardens", "Go to jail", "Pacific Avenue",
    "North Carolina Avenue", "Community Chest", "Pennsylvania Avenue",
    "Short Line Railroad", "Chance", "Park Place", "Luxury Tax",
    "Boardwalk"), stringsAsFactors = FALSE)

chancedeck <- data.frame(
  index = 1:15, 
  card = c(
    "Advance to Go", "Advance to Illinois Ave.",
    "Advance to St. Charles Place", "Advance token to nearest Utility",
    "Advance token to the nearest Railroad",
    "Take a ride on the Reading Railroad",
    "Take a walk on the Boardwalk", "Go to Jail", "Go Back 3 Spaces",
    "Bank pays you dividend of $50", "Get out of Jail Free",
    "Make general repairs on all your property", "Pay poor tax of $15",
    "You have been elected Chairman of the Board", 
    "Your building loan matures"), stringsAsFactors = FALSE)

communitydeck <- data.frame(
  index = 1:16, 
  card = c(
    "Advance to Go", "Go to Jail",
    "Bank error in your favor. Collect $200", "Doctor's fees Pay $50",
    "From sale of stock you get $45", "Get Out of Jail Free",
    "Grand Opera Night Opening", "Xmas Fund matures", "Income tax refund",
    "Life insurance matures. Collect $100", "Pay hospital fees of $100",
    "Pay school tax of $150", "Receive for services $25",
    "You are assessed for street repairs",
    "You have won second prize in a beauty contest",
    "You inherit $100"), stringsAsFactors = FALSE)

# RandomDice class --------------------------------------------------------

RandomDice <- R6Class(
  classname = "RandomDice",
  public = list(
    verbose = NA,
    initialize = function(verbose = FALSE){
      stopifnot(is.logical(verbose))
      self$verbose = verbose
    },
    roll = function() {
      outcome <- sample(1:6, size = 2, replace = TRUE)
      if(self$verbose) {
        cat("Dice Rolled:", outcome[1], collapse = ", ", outcome[2], "\n")
      }
      outcome
    }
  )
)

# Preset dice -------------------------------------------------------------

PresetDice <- R6Class(
  classname = "PresetDice",
  public = list(
    verbose = NA,
    preset_rolls = double(0),
    position = 1,
    initialize = function(rolls, verbose = FALSE){
      stopifnot(is.logical(verbose))
      stopifnot(is.numeric(rolls))
      self$preset_rolls = rolls
      self$verbose = verbose
    },
    roll = function(){
      if(self$position > length(self$preset_rolls)){
        stop("You have run out of predetermined dice outcomes.")
      }
      outcome <- c(self$preset_rolls[self$position], 
                   self$preset_rolls[self$position + 1])
      self$position <- self$position + 2
      if(self$verbose){
        cat("Dice Rolled:", outcome[1], collapse = ",", outcome[2], "\n")
      }
      outcome
    }
  )
)

# Chance and Community Decks ----------------------------------------------

CardDeck <- R6Class(
  classname = "CardDeck",
  public = list(
    verbose = NA,
    deck_order = double(0),
    deck = data.frame(),
    position = 1,
    initialize = function(deck, verbose = FALSE){
      stopifnot(is.data.frame(deck),
                is.numeric(deck[[1]]),
                is.character(deck[[2]]))
      self$deck_order <- sample(length(deck[[1]]))
      self$verbose <- verbose
      self$deck <- deck
    },
    draw = function(){
      if(self$position > length(self$deck_order)){
        # if we run out of cards, shuffle deck
        # reset the position to 1
        if(self$verbose){
          cat("Shuffling deck.\n")
        }
        self$deck_order <- sample(length(self$deck[[1]]))
        self$position <- 1
      }
      outcome <- c(self$deck_order[self$position])
      self$position <- self$position + 1
      if(self$verbose){
        cat("Card:", self$deck[outcome, 2], "\n")
      }
      outcome
    }
  )
)


# R6 Class Player ---------------------------------------------------------

Player <- R6Class(
  classname = "Player",
  public = list(
    pos = 1, # starting position
    jail_times = 0, # count the number of jail times
    double_times = 0, # count the number of getting double rolls
    verbose = TRUE,
    initialize = function(pos, jail_times, double_times, verbose = FALSE) {
      stopifnot(is.numeric(pos))
      stopifnot(is.numeric(jail_times))
      stopifnot(is.numeric(double_times))
      stopifnot(is.logical(verbose))
      self$pos = pos
      self$jail_times = jail_times
      self$double_times = double_times
      self$verbose = verbose
    }, 
    # takes the Player's movement
    move_fwd = function(n) {
      if(self$verbose) {
      self$pos <- self$pos + n
      if(self$pos > 40) {
        self$pos <- self$pos - 40
      }
      # tells where the Player moved to and by how many
      if(self$verbose) { 
        cat("Player Moved This Many:", n, "\n")
        self$pos <- self$pos + n
        cat("Player is Now at:", self$pos, "\n")
      }
      # Space 31 is where Jail space is located
      if(self$pos == 31) { 
        cat("You Will Go to Jail, Do Not Pass Go", "\n")
        self$pos <- 11 # Space of jail located
        self$jail_times <- self$jail_times + 1
      }
      }
    },
# Interesting note is that you cannot use "<-" within R6 but you have to use "=" for functions
    # If spaces make you jump a space
    Jump_to_Space = function(n) {
      if(self$verbose) {
        cat("Player at: ", self$pos, "-->", gameboard$title[self$pos], "\n")
        self$pos <- n
        }
      if(self$verbose) {
        cat("Player Now at: ", self$pos, "\n")
        }
    },
    # Tallying doubles to send you to jail if done 3 times in a row or straight jail
    Counts_of_Doubles_Rolled = function(n) {
      if(self$verbose) {
        self$double_times <- self$double_times + 1
        cat("Double count is now: ", self$double_times, "\n")
      }
      if(self$double_times == 3){
        cat("You Will Go to Jail, Do Not Pass Go", "\n")
        self$pos <- 11 # Space of jail located
        self$jail_times <- self$jail_times + 1
      }
    },
    # What happens if you land on the Chance deck card card (will take a while to code)
    Chancedeck_Draw = function(x) {
      if(self$verbose) {
        cat("Draw a Card from the Community Chest", x, "\n")
        x <- sample(chancedeck$index, 1)
      }
      if(self$verbose) {
        cat("Chance Card Picked: ", self$pos, chancedeck$card[x], "\n")
    # I've tried running position to position and was too hard for me. So, I found out
        # the easiest method is to add or subtract the position (+/-)
      }
      # "There are nine cards in the Chance deck that move the Player's token"
      # Choosing card 1
      if(x == 1) {
          self$pos <- self$pos + 1
      }
        else
      # Choosing card 2
      if(x == 2) {
      if(self$pos == 8) {
          self$pos <- self$pos + 17
      }
        else 
      if(self$pos == 23) {
          self$pos <- self$pos + 2
      }
        else 
      if(self$pos == 37) {
          self$pos <- self$pos - 12
      }
      }
        else 
      # Choosing card 3
      if(x == 3) {
      if(self$pos == 8) {
          self$pos <- self$pos + 4
      }
        else 
      if(self$pos == 23) {
          self$pos <- self$pos - 11
      }
        else 
      if(self$pos == 37) {
          self$pos <- self$pos - 25
      }
      }
        else 
      # Choosing card 4
      if(x == 4) {
      if(self$pos == 8) {
          self$pos <- self$pos + 5
      }
        else 
      if(pos == 23) {
          self$pos <- self$pos + 6
      }
        else 
      if(self$pos == 37) {
          self$pos <- self$pos + 2
      }
      } 
        else 
      # Choosing card 5
      if(x == 5) {
      if(self$pos == 8) {
          self$pos <- self$pos + 8
      }
        else 
      if(self$pos == 23) {
          self$pos <- self$pos + 3
      }
        else 
      if(self$pos == 37) {
          self$pos <- self$pos -31
      }
      } 
        else 
      # Choosing card 6
      if(x == 6) {
      if(self$pos == 8) {
          self$pos <- self$pos - 2
      }
        else 
      if(self$pos == 23) {
          self$pos <- self$pos - 17
      }
        else 
      if(self$pos == 37) {
          self$pos <- self$pos - 31
      }
      } 
        else
      # Choosing card 7
      if(x == 7) {
      if(self$pos == 8) {
          self$pos <- self$pos +32
      }
        else 
      if(self$pos == 23) {
          self$pos <- self$pos + 17
      }
        else 
      if(self$pos == 37) {
          self$pos <- self$pos + 3
      }
      } 
        else 
      # Choosing card 8
      if(x == 8) {
        self$pos <- 11
      } 
        else 
      # Choosing card 9
      if(x == 9) {
        self$pos <- self$pos - 3
      }
        else {
        self$pos <- self$pos
      }
    },
    # What happens if you draw a Community Chest deck card
    Communitydeck_Draw = function(x) {
      if(verbose) {
        cat("Drew community card")
        x <- sample(communitydeck$index,1)
      }
      if(verbose) {
        cat("Community Card Draw ", self$pos, communitydeck$card[x], "\n")
      }
      if(x == 1) {
        self$pos <- 1
      }
      if(x == 2) {
        self$pos <- 11
        self$jail_times <- self$jail_times + 1
      } 
      else {
        self$pos <- self$pos
    }
    }
  )
)

# R6 Class SpaceTracker ---------------------------------------------------

SpaceTracker <- R6Class(
  classname = "SpaceTracker",
  public = list(
    counts = rep(0, 40),
    verbose = TRUE,
    initialize = function(counts, verbose = FALSE) {
      stopifnot(is.numeric(counts))
      stopifnot(is.logical(verbose))
      self$counts = counts
      self$verbose = verbose
    },
    tally = function(x, verbose) {
      self$counts[x] <- self$counts[x] + 1
      if(self$verbose) {
        cat("Added tally to:", x, ": ", gameboard$title[x], "\n")
      }
    }
  )
)

# VERY BASIC turn taking example ------------------------------------------

take_turn <- function(Player, spacetracker){
  dice_rolls <- dice$roll()
  Player$move_fwd(sum(dice_rolls))
  spacetracker$tally(Player$pos)
}

######################################################################################
dice <- PresetDice$new(
  rolls = c(6,4, 5,3, 3,5, 4,4, 4,4, 2,2, 4,3, 4,4, 1,4,
            3,4, 1,2, 3,6, 5,4, 5,5, 1,2, 5,4, 3,3, 6,1,
            1,1, 2,3, 5,5, 5,4, 4,1, 2,2, 2,4),
  verbose = TRUE
)


######################################################################################

monopoly <- SpaceTracker$new(counts = rep(0,40), verbose = TRUE)
Player1 <- Player$new(pos = 1, jail_times = 0, double_times = 0, verbose = TRUE)
for(i in 1:20){
  cat("--- Turn", i,"---\n")
  take_turn(Player1, monopoly)
  cat("\n")
}

# My game will return NA is over 40 space

monopoly$counts

# I was unable to figure out how to keep track of the spaces for more than 1 game
# with more than 1 player

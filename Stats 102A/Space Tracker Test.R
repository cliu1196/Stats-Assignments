# Space Tracker Test
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
      # No Doubles rolled for jail
      if(Player$double_times == 0) {
        if(roll$double == FALSE)
          self$counts[x] <- self$counts[x] + 1
        else
          if(roll$double == TRUE) {
            self$counts[x] <- self$counts[x] + 1
            Player$jail_times <- Player$jail_times + 0
          }
      }
      # 1 Double has been rolled and need 2 more to jail
      if(Player$double_times == 1) {
        if(roll$double == FALSE)
          self$counts[x] <- self$counts[x] + 1
        else
          if(roll$double == TRUE) {
            self$counts[x] <- self$counts[x] + 1
            Player$jail_times <- Player$jail_times + 0
          }
      }
      # 2 Double has been rolled and need 1 more to jail
      if(Player$double_times == 2) {
        if(roll$double == FALSE)
          self$counts[x] <- self$counts[x] + 1
        else
          if(roll$double == TRUE) {
            self$counts[x] <- self$counts[x] + 1
            Player$jail_times <- Player$jail_times + 0
          }
      }
      # Player ends up in jail
      if(Player$double_times == 3) {
        if(roll$double == FALSE)
          self$counts[x] <- self$counts[x] + 1
        else
          if(roll$double == TRUE) {
            self$counts[x] <- self$counts[x] + 1
            Player$jail_times <- Player$jail_times + 0
          }
      }
    }
  )
)
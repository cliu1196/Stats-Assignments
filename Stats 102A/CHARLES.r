by_type <- function(x, sort = FALSE) {

  by_type <- vector(mode = "list", length = 3)
  if (sort == FALSE)
    { 
      for (i in 1:length(x))
      {
        if (typeof(x[i]) == "character")
        {
          #character
          #add it to a character vector within a list using append(by_type[3],x[i])
          append(by_type[3], x[i])
        }
        else
        {
          #number 
          #check if it's an integer or a double 
          typeof(x[i])
          if (typeof(x[i]) == "integer")
          {
            #add to integer vector
            append(by_type[1], x[i])
          }
          else 
          {
            typeof(x[i])
            if (typeof(x[i]) == "double")
            #add to double vector
            append(by_type[2], x[i])
          }
          
        }
      }
    }
    #print list 
    print(by_type)
    #check with: by_type(x = c("1","1.1","HI"))
      
    }
  
#do the same with the sort function. The difference should be that if sort = TRUE, add all
#of the values to the respective vectors, and then use the sort function with each vector.
x <- c("a", "1", "2.2", "house", "3.4", "6")
by_type(x)

# Part 4: Questions to Answer

Replace 'write your answer here' with your responses. Be sure your answers have been 'highlighted' using the triple hash `###` which makes the text large and bold.

1. Coercion: For each of the following, explain what type of output you will receive and why R is producing that output.
a. `c(0, TRUE)`
b. `c("F", F)`
c. `c(list(1), "b")`
d. `c(FALSE, 1L)`

### write your answer here

2. What is the difference between NULL, NA, and NaN?
  
### write your answer here
  
3. What is the difference between logical(0) and NULL? Write a command (other than `logical(0)`) that will produce logical(0) as the output. Write a command (other than `NULL`) that will produce NULL as the output.

### write your answer here



4. A vector `c(TRUE, FALSE)` is a logical vector. Other than `TRUE` or `FALSE`, what can you insert into the vector so that it increases to a length of 3 and remains a logical vector and does not get coerced into another class?
  
### write your answer here
  
5. What are the lengths of the following lists? Use bracket notation to subset them to the letters "h" and "i". Be sure to print the result so it shows the subset.

l1 <- list(letters[1:5], letters[3:9] , letters[4:7])
l1

l2 <- list( c(letters[1:5], letters[3:9]), letters[4:7] )
l2
```

### write your answer here

5. What are the lengths of the following lists? Use bracket notation to subset them to the letters "h" and "i". Be sure to print the result so it shows the subset.

```{r}
l1 <- list(letters[1:5], letters[3:9] , letters[4:7])
l1

l2 <- list( c(letters[1:5], letters[3:9]), letters[4:7] )
l2
```

6. What will `c(4:7) * c(2:4)` produce? Briefly, why?
  
  ### write your answer here
  
  7. Take a look at the following code chunks. What are some of the differences between `cat()` and `print()`?
  
  ```{r}
cat(5 + 6)
print(5 + 6)
```

```{r}
x8 <- cat(5 + 6)
y8 <- print(5 + 6)
x8
y8
```

```{r, error = TRUE}
cat(letters[1:3], letters[24:26])
print(letters[1:3], letters[24:26]) # Why are we getting the following error?
# Error in print.default(letters[1:3], letters[24:26]) : invalid 'digits' argument
cat(l1)
print(l1)
```

### write your answer here. Be sure to explain the print() error.

8. What happens to a factor when you reverse its levels?
  
  ```{r}
f1 <- factor(c("A","A","B","C","D","A","C"))
f1
levels(f1) <- rev(levels(f1))
f1
```

### write your answer here

9. How do f2 and f3 differ from the unmodified f1?
  
  ```{r}
f1 <- factor(c("A","A","B","C","D","A","C"))
f1
f2 <- factor(rev(c("A","A","B","C","D","A","C")))
f2
f3 <- factor(c("A","A","B","C","D","A","C"), levels = rev(c("A","B","C","D")))
f3
```

### write your answer here

10. What attributes does a data frame possess?
  
  ### write your answer here
  
  11. What does as.matrix() do when applied to a data frame with columns of different types? Create a simple data.frame with two columns: one numeric and one string. Use as.matrix and show the results.

### write your answer here
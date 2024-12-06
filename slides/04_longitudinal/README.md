# R and workflow

## Subsetting

Check out this chapter by Hadley Wickham on indexing:
https://adv-r.hadley.nz/subsetting.html. It is very intuitive and leads to a
deeper understanding how subsetting wirks in R (it also introduces the
differences between data frames and tibbles which is very useful to understand
when working with both).

### Basic stuff about subsetting

```r
x <- c(2.1, 4.2, 3.3, 5.4)

## Positive integers
x[c(3, 1)]
#> [1] 3.3 2.1
x[order(x)]
#> [1] 2.1 3.3 4.2 5.4

# Duplicate indices will duplicate values
x[c(1, 1)]
#> [1] 2.1 2.1

# Real numbers are silently truncated to integers
x[c(2.1, 2.9)]
#> [1] 4.2 4.2

## Negative integers
x[-c(3, 1)]
#> [1] 4.2 5.4

## Logical vectors
x[c(TRUE, TRUE, FALSE, FALSE)]
#> [1] 2.1 4.2
x[x > 3]
#> [1] 4.2 3.3 5.4

x[c(TRUE, FALSE)]
#> [1] 2.1 3.3
# Equivalent to
x[c(TRUE, FALSE, TRUE, FALSE)]
#> [1] 2.1 3.3

x[c(TRUE, TRUE, NA, FALSE)]
#> [1] 2.1 4.2  NA

## Named vectors
(y <- setNames(x, letters[1:4]))
#>   a   b   c   d 
#> 2.1 4.2 3.3 5.4
y[c("d", "c", "a")]
#>   d   c   a 
#> 5.4 3.3 2.1

# Like integer indices, you can repeat indices
y[c("a", "a", "a")]
#>   a   a   a 
#> 2.1 2.1 2.1

# When subsetting with [, names are always matched exactly
z <- c(abc = 1, def = 2)
z[c("a", "d")]
#> <NA> <NA> 
#>   NA   NA

y[factor("b")]
#>   a 
#> 2.1
# --> uses the underlying integer vector
```

### Subsetting with data frames

* Show that data frames are lists with vectors that can be of different types,
  but must have the same length

* Hence, each column is a list entry

* This implies that all list operations also work on data frames (especially
  `lapply()`)

* And list indexing also works on data frames

```r
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])

df[df$x == 2, ]
#>   x y z
#> 2 2 2 b
df[c(1, 3), ]
#>   x y z
#> 1 1 3 a
#> 3 3 1 c

# There are two ways to select columns from a data frame
# Like a list
df[c("x", "z")]
#>   x z
#> 1 1 a
#> 2 2 b
#> 3 3 c
# Like a matrix
df[, c("x", "z")]
#>   x z
#> 1 1 a
#> 2 2 b
#> 3 3 c

# There's an important difference if you select a single 
# column: matrix subsetting simplifies by default, list 
# subsetting does not.
str(df["x"])
#> 'data.frame':    3 obs. of  1 variable:
#>  $ x: int  1 2 3
str(df[, "x"])
#>  int [1:3] 1 2 3

# Subsetting a tibble with [ always returns a tibble:
df <- tibble::tibble(x = 1:3, y = 3:1, z = letters[1:3])

str(df["x"])
#> tibble [3 × 1] (S3: tbl_df/tbl/data.frame)
#>  $ x: int [1:3] 1 2 3
str(df[, "x"])
#> tibble [3 × 1] (S3: tbl_df/tbl/data.frame)
#>  $ x: int [1:3] 1 2 3
```


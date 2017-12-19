library(tidyverse)

typeof(letters)
typeof(1:10)

x <- list("a", "b", 1:10)
length(x)

# Important Types of Atomic Vector

## Logical

1:10 %% 3 == 0
c(T, T, F, NA)

## Numeric

typeof(1)
typeof(1L)
1.5L

x <- sqrt(2) ^ 2
x
x - 2

near(x, 2)

c(-1, 0, 1) / 0

## Character

x <- "This is a reasonably long string."
pryr::object_size(x)

y <- rep(x, 1000)
pryr::object_size(y)

## Missing Values

NA
NA_integer_
NA_real_
NA_character_

## Exercises
#1
is.finite(x)
!is.infinite(x)
#2
#3
#4
#5
#parse_integer parse_double parse_logical

# Using Atomic Vectors

## Coercion

x <- sample(20, 100, replace = T)
y <- x > 10
sum(y)
mean(y)

if(length(x)) {}
if(length(x) > 0) {} # as the same but easy to understand how coded

typeof(c(T, 1L))
typeof(c(1L, 1.5))
typeof(c(1.5, "a"))

## Test Functions

## Scalars and Recycling Rules

sample(10) + 100
runif(10) > 0.5

1:10 + 1:2
1:10 + 1:3

tibble(x = 1:4, y = 1:2)
tibble(x = 1:4, y = rep(1:2, 2))
tibble(x = 1:4, y = rep(1:2, each = 2))

## Naming Vectors

c(x = 1, y = 2, z = 4)
set_names(1:3, c("a", "b", "c"))

## Subsetting

x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]

x[c(1, 1, 5, 5, 5, 2)]

x[c(-1, -3, -5)]
x[c(1, -1)]
x[0]

x <- c(10, 3, NA, 5, 8, 1, NA)
x[!is.na(x)]
x[x %% 2 == 0]

x <- c(abc = 1, def = 2, xyz = 5)
x[c("abc", "def")]

## Exercises

#1
mean(is.na(x))
sum(!is.finite(x))
#2
?is.vector
#3
?set_names
#4
##4-1
x[[3]]
x[3]
##4-2
x[x %in% seq(2, 10000, 2)]
##4-3
x[-length(x)]
##4-4
x[x %% 2 ==0 & !is.na(x)]
#5
x[-which(x >0)]
x[x <= 0]
#6
x[4]
x["bcg"]

# Recursive Vectors (Lists)

x <- list(1, 2, 3)
x
str(x)
x_named <- list(a = 1, b = 2, c = 3)
str(x_named)

y <- list("a", 1L, 1.5, T)
str(y)

z <- list(list(1, 2), list(3, 4))
str(z)

## Visualizing Lists
x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))

## Subsetting

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
a

str(a[1:2])
str(a[4])

str(y[[1]])
str(y[[4]])
a$a
a[["a"]]
a["a"]

## List of Condiments

## Attributes

x <- 1:10
attr(x, "greeting")
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)
x

as.Date
methods("as.Date")

getS3method("as.Date", "default")

getS3method("as.Date", "numeric")
getS3method("print", "default")

# Augmented Vectors

## Factors

x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)
attributes(x)
x
str(x)

## Date and Date-Times

x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
attributes(x)

x <- lubridate::ymd_hm("1970-01-01 01:00")
x
unclass(x)
typeof(x)
attributes(x)

attr(x, "tzone") <- "US/Pacific"
x
attr(x, "tzone") <- "US/Eastern"
attributes(x)
x

y <- as.POSIXlt(x)
typeof(y)
attributes(y)

## Tibbles

tb <- tibble(x = 1:5, y = 5:1)
typeof(tb)
attributes(tb)

df <- data.frame(x = 1:5, y = 5:1)
typeof(df)
attributes(df)

## Exercises
#1
hms::hms(3600)
typeof(hms::hms(3600))
attributes(hms::hms(3600))
#2
tb2 <- tibble(x = 1:5, y = 4:1)
#3
?tibble

## tip

lst(n = 5, x = runif(n))

# You can splice-unquote a list of quotes and formulas
lst(!!! list(n = rlang::quo(2 + 3), y = quote(runif(n))))

a <- 1:5
tibble(a, b = a * 2)
tibble(a, b = a * 2, c = 1)
tibble(x = runif(10), y = x * 2)

lst(n = 5, x = runif(n))

# tibble never coerces its inputs
str(tibble(letters))
str(tibble(x = list(diag(1), diag(2))))

# or munges column names
tibble(`a + b` = 1:5)

# You can splice-unquote a list of quotes and formulas
tibble(!!! list(x = rlang::quo(1:10), y = quote(x * 2)))

# data frames can only contain 1d atomic vectors and lists
# and can not contain POSIXlt
## Not run: 
tibble(x = tibble(1, 2, 3))
tibble(y = strptime("2000/01/01", "%x"))

## End(Not run)
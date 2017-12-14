library(tidyverse)

# When Should You Write a Function?

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
df$a <- (df$a - min(df$a, na.rm = T)) / 
  (max(df$a, na.rm = T) - min(df$a, na.rm = T))
df$b <- (df$b - min(df$b, na.rm = T)) /
  (max(df$b, na.rm = T) - min(df$b, na.rm = T))
df$c <- (df$c - min(df$c, na.rm = T)) /
  (max(df$c, na.rm = T) - min(df$c, na.rm = T))
df$d <- (df$d - min(df$d, na.rm = T)) /
  (max(df$d, na.rm = T) - min(df$d, na.rm = T))
df

(df$a - min(df$a, na.rm = T)) /
  (max(df$a, na.rm = T) - min(df$a, na.rm = T))

x <- df$a
(x - min(x, na.rm = T)) / 
  (max(x, na.rm = T) - min(x, na.rm = T))

rng <- range(x, na.rm = T)
(x - rng[1]) / (rng[2] - rng[1])

rescale01 <- function(x) {
  rng <- range(x, na.rm = T)
  (x - rng[1])/(rng[2]-rng[1])
}
rescale01(c(0, 5, 10))
rescale01(df$a)


rescale01(c(-10, 0, 10))
rescale01(c(1,2,3, NA, 5))

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

x <- c(1:10, Inf)
rescale01(x)

rescale01 <- function(x) {
  rng <- range(x, na.rm = T, finite = T)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)

## Exercises

#1
x <- c(seq(1,10), NA, seq(10, 1))
rescale01(x)
rescale02 <- function(x) {
  rng <- range(x, na.rm = F, finite = F)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale02(x)

#2
rescale01 <- function(x) {
  for (i in 1:length(x)) {
    x[i] <- ifelse(x[i] == -Inf, 0,
                   ifelse(x[i] == Inf, 1, x[i]))
  }
  rng <- range(x, na.rm = T)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)
x
#3

get_coe <- function(x) {
  mean(is.na(x))
  x / sum(x, na.rm = T)
  sd(x, na.rm = T) / mean(x, na.rm = T)
}

get_coe(rescale01(x))

#4
skewness <- function(x) {
  n <- length(x)
  v <- var(x)
  m <- mean(x)
  third.moment <- (1/(n - 2)) * sum((x - m)^3)
  third.moment/(var(x)^(3/2))
}

skewness(mpg$cty)

variance <- function(x) {
  n <- length(x)
  m <- mean(x)
  (sum((x-m)^2))/(n-1)
}

variance(mpg$cty)

#5
both_na <- function(vec1, vec2) {
  sum(is.na(vec1) & is.na(vec2))
}
both_na(mpg$cty, mpg$hwy)
x <- c(1,2,3, NA, 5,6,7)
y <- c(1,2,4, NA, 5,6,7)
both_na(x, y)

#6
is_directory <- function(x) file.info(x)$isdir
is_readable <- function(x) file.access(x, 4) == 0

#7

# Functions Are for Humans and Computers

col_mins <- function(x, y) {}
rowMaxes <- function(y, x) {}
remove(col_mins, rowMaxes)

## Exercises

#1 
f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}
f2 <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}
f3 <- function(x) {
  rep(y, length.out = length(x))
}

#2
#3
rnorm
MASS::mvrnorm

# Conditional Execution

has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(F, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}
has_name(mpg)

## Conditions

if (c(T, F)) {}
if (NA) {}

identical(0L, 0)
identical(0, 0)

x <- sqrt(2) ^ 2
x
x == 2
x - 2
#x == NA

## Multiple Conditions

if (this) {
  # do that
} else if (that) {
  # do something else
} else {
  #
}

function(x, y, op) {
  switch(op,
         plus = x + y,
         minus = x - y,
         times = x * y,
         divide = x / y,
         stop("Unknown op!")
         )
}

## Code Style

# Good
if (y < 0 && debug) {
  message("Y is negative")
}

if (y == 0) {
  log(x)
} else {
  y ^ x
}

# Bad

if (y < 0 && debug)
  message("Y is negative")

if (y == 0) {
  log(x)
}
#else {
#  y ^ x
#}

y <- 10
x <- if (y < 20) "Too low" else "Too high"

if (y < 20) {
  x <- "Too low"
} else {
  x <- "Too high"
}

## Exercises

#2
library(lubridate)
now()
vignette("lubridate")
parse_date(now())

str_split(parse_character(now()))

say_hello <- function() {
  ment <- str_sub(parse_character(now()), 12,19)
  if (ment > "12:00:00" && ment <= "18:00:00") {
    print("Good afternoon")
  } else if (ment < "12:00:00" && ment >= "06:00:00") {
    print("Good morning")
  } else if (ment > "18:00:00" && ment <= "21:00:00") {
    print("Good evening")
  } else {
    print("Good night")
  }
}
say_hello()

#3

fizzbuzz <- function(n) {
  if (n %% 3 == 0 && n %% 5 == 0) {
    return("fizzbuzz")
  } else if (n %% 3 == 0) {
    return("fizz")
  } else if (n %% 5 ==0) {
    return("buzz")
  } else {
    return(n)
  }
}

#4

if (temp <= 0) {
  "freezing"
} else if (temp <= 10) {
  "cold"
} else if (temp <= 20) {
  "cool"
} else if (temp <= 30) {
  "warm"
} else {
  "hot"
}

temp <- sample(-30:30, 20)
cut(temp, breaks = c(-Inf, 0, 10, 20, 30, Inf), labels = c("freezing", "cold", "cool", "warm", "hot"))

#5
switch(k)
?switch

center <- function(temp, type) {
  switch(type, 
         mean = mean(temp),
         median = median(temp),
         trimmed = mean(temp, trim=.1))
}
center(temp, "median")

#6
switch(x,
       a = ,
       b = "ab",
       c = ,
       d = "cd"
       )
x <- "e"


# Function Arguments

# Compute confidence interval around mean using normal approximation

mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

x <- runif(100)
mean_ci(x)
mean_ci(x, conf = 0.99)

#good example
mean(1:10, na.rm = T)

#bad example
mean(x = 1:10, , F)
mean(, TRUE, x = c(1:10, NA))

# Good
average <- mean(feet / 12 + inches, na.rm = T)
average<-mean(feet/12+inches,na.rm=T)

## Choosing Names
## Checking Values

wt_mean <- function(x, w) {
  sum(x * w) / sum(x)
}
wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}
wt_sd <- function(x, w) {
  sqrt(wt_var(x, w))
}
wt_mean(1:6, 1:3)

wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("'x' and 'w' must be the same length", call. = F)
  }
  sum(w * x) / sum(x)
}
wt_mean(1:6, 1:3)

wt_mean <- function(x, w, na.rm = F) {
  if (!is.logical(na.rm)) {
    stop("'na.rm' must be logical")
  }
  if (length(na.rm) != 1) {
    stop("'na.rm' must be length 1")
  }
  if (length(x) != length(w)) {
    stop("'x' and 'w' must be the same length", call. = F)
  }
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(x)
}

wt_mean <- function(x, w, na.rm = F) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(x)
}
wt_mean(1:6, 6:1, na.rm = "foo")

# Dot-Dot-Dot (...)

sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
str_c("a", "b", "c", "d", "e", "f")

commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output")

x <- c(1, 2)
sum(x, na.mr = T)

## Lazy Evaluation

## Exercises
#2
rule2 <- function(..., pad = "-+") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  padchar <- nchar(pad)
  cat(title, " ",
      stringr::str_dup(pad, width %/% padchar),
      # if not multiple, fill in the remaining characters
      stringr::str_sub(pad, 1, width %% padchar),
      "\n", sep = "")
}
rule2("Important output")
rule2("Title", pad ="-+")

#3
?sum

#4

# Return Values

## Explicit Return Statements

complicated_function <- function(x, y, z) {
  if (length(x) == 0 || length(y) == 0) {
    return(0)
  }
  
  # Complicated code here
}

f <- function() {
  if (x) {
    # Do
    # something
    # that
    # takes
    # many
    # lines
    # to
    # express
  } else {
    # return something short
  }
}

f <- function() {
  if (!x) {
    return(something_short)
  }
  
  # Do
  # something
  # that
  # takes
  # many
  # lines
  # to
  # express
}

# Writing Pipeable Functions

show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}
show_missings(mtcars)

x <- show_missings(mtcars)
class(x)
dim(x)

mtcars %>% 
  show_missings() %>% 
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings()

# Environment

f <- function(x) {
  x + y
}

y <- 100
f(10)
y <- 1000
f(10)

`+` <- function(x, y) {
  if (runif(1) < 0.1) {
    sum(x, y)
  } else {
    sum(x, y) * 1.1
  }
}
table(replicate(100, 1 + 2))

  
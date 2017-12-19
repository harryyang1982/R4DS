library(tidyverse)

# For Loops

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10), 
  d = rnorm(10)
)

median(df$a)
median(df$b)
median(df$c)
median(df$d)

output <- vector("double", ncol(df))
for (i in seq_along(df)) {
  output[[i]] <- median(df[[i]])
}

output <- vector("double", 4)

y <- vector("double", 0)
seq_along(y)
1:length(y)

## Exercises

#1
#a
output <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  output[[i]] <- mean(mtcars[[i]])
}

#b
install.packages("nycflights13")
flights <- nycflights13::flights

output <- vector("character", ncol(flights))
for (i in seq_along(flights)) {
  output[i] <- class(flights[[i]])
}
output

#c
output <- vector("double", ncol(iris))
for (i in seq_along(iris)) {
  output[i] <- length(unique(iris[[i]]))
}
output

#d
means <- c(-10, 0, 10, 100)
output <- tibble(a=rep(NA, 10), b=rep(NA, 10), c=rep(NA, 10), d=rep(NA, 10))
for (i in seq_along(means)) {
  output[i] <- rnorm(10, mean = means[[i]])
}
output

#2

out <- ""
out <- str_c(letters, collapse = "")
out

?sd
?rnorm

x <- sample(100)
sd(x)


x <- runif(100)
cumsum(x)

#3
#4
output <- vector("integer", 0)
for (i in seq_along(x)) {
  output <- c(output, lengths(x[[i]]))
}
output

# For Loop Variations

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

rescale01 <- function(x) {
  rng <- range(x, na.rm = T)
  (x - rng[1]) / (rng[2] - rng[1])
}

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}
df

## Looping Patterns

results <- vector("list", length(x))
names(results) <- names(x)

for (i in seq_along(x)) {
  name <- names(x)[[i]]
  value <- x[[i]]
}

## Unknown Output Length

means <- c(0, 1, 2)
output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
output
str(output)

out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
out
str(out)
str(unlist(out))

## Unknown Sequence Length

while (condition) {
  # body
}

for (i in seq_along(x)) {
  # body
}

i <- 1
while (i <= length(x)) {
  # body
  i <- i + 1
}

flip <- function() sample(c("T", "H"), 1)
flips <- 0
nheads <- 0

while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips

## Exercises

#1
files <- dir(".", pattern = "\\.csv$", full.names = T)

out <- vector("list", length(files))
for (i in seq_along(files)) {
  out[[i]] <- read_csv(files[[i]])
}
out

#2
for (nm in names(x)) {
  return(nm)
}
nm

#3
show_mean(iris)
iris

show_mean <- function(df) {
  for (i in seq_along(df)) {
    cat("#>", names(df[i]), ":", mean(df[[i]], na.rm=T), "\n")
  }
}
show_mean(iris)

#show_mean <- function(df, digits = 2) {
  # Get max length of any variable in the dataset
#  maxstr <- max(str_length(names(df)))
  #for (nm in names(df)) {
   # if (is.numeric(df[[nm]])) {
    #  cat(str_c(str_pad(str_c(nm, ":"), maxstr + 1L, side = "right"),
     #           format(mean(df[[nm]]), digits = digits, nsmall = digits),
      #          sep = " "),
       #   "\n")
    #}
  #}
#}

#4
trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}
mtcars
trans[["disp"]]
trans[["am"]]

# For Loops Versus Functionals

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output <- vector("double", length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}

col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[[i]] <- mean(df[[i]])
  }
  output
}
col_mean(df)

col_median <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[[i]] <- median(df[[i]])
  }
  output
}
col_median(df)

col_sd <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[[i]] <- sd(df[[i]])
  }
  output
}

col_sd(df)

f1 <- function(x) abs(x - mean(x)) ^1
f2 <- function(x) abs(x - mean(x)) ^2
f3 <- function(x) abs(x - mean(x)) ^3

f <- function(x, i) abs(x - mean(x)) ^i

col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}

col_summary(df, median)
col_summary(df, mean)
col_summary(df, var)
col_summary(df, sd)

## Exercises

#1
?apply
apply(df, 2, mean)
apply(df, 1, mean)

#2
col_summary <- function(df, fun) {
  out <- vector("double", length(df))
    for (i in seq_along(df)) {
      if (is.numeric(df[[i]])) {
        out[i] <- fun(df[[i]])
      }
    } 
  out
}
col_summary(df, median)
col_summary(df, mean)

# The Map Functions
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

map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

df %>% map_dbl(mean)
df %>% map_dbl(median)
df %>% map_dbl(sd)
df %>% map_dbl(var)

map_dbl

map_dbl(df, mean, trim = 0.5)

z <- list(x = 1:3, y = 4:5)
map_int(z, length)

## Shortcuts
### Verbose way
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))
### Concise way
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

### Traditional way
models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)
### String way
models %>% 
  map(summary) %>% 
  map_dbl("r.squared")

x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2)

## Base R

x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06),
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78),
  c(0.93, 0.21, 0.65, 0.13, 0.27),
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)
threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()
x2 %>% sapply(threshold) %>% str()

## Exercises

#1a
mtcars %>% map_dbl(mean)
#1b
nycflights13::flights %>% map(class)
#1c
iris %>% map(unique)
#1d
rn_df <- c(-10, 0, 10, 100)
rn_df %>% map(~rnorm(10, mean = ., sd = 1))

map(c(-10, 0, 10, 100), rnorm, n = 10)

#2
mpg$cyl <- as.factor(mpg$cyl)
mpg$trans <- as.factor(mpg$trans)

fact_ver <- mpg %>% map_lgl(is.factor)
str(fact_ver)

#3
map(1:5, runif)

#4
map(-2:2, rnorm, n =5)
map_dbl(-2:2, rnorm, n = 5)

#5
mtcars %>% map(x, function(df) lm(mpg ~ wt, data = df))
mtcars %>% map(~lm(mpg ~ wt, data=.))

# Dealing with Failure

safe_log <- safely(log)
str(safe_log(10))
str(safe_log("a"))

x <- list(1, 10, "a")
y <- x %>% map(safely(log))
str(y)

y <- y %>% transpose()
str(y)

is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]
y$result[is_ok] %>% flatten_dbl()

x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))

x <- list(1, -1)
x %>% map(quietly(log)) %>% str()

# Mapping over Multiple Arguments

mu <- list(5, 10, -3)
mu %>% 
  map(rnorm, n = 5) %>% 
  str()

sigma <- list(1, 5, 10)
seq_along(mu) %>% 
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>% 
  str()

map2(mu, sigma, rnorm, n = 5) %>% str()

map2 <- function(x, y, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], y[[i]], ...)
  }
  out
}

n <- list(1, 3, 5)
args <- list(n, mu, sigma)
args %>% 
  pmap(rnorm) %>% 
  str()

args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>% 
  pmap(rnorm) %>% 
  str()

params <- tribble(
  ~mean, ~sd, ~n,
  5,      1,   1,
  10,     5,   3,
  -3,    10,   5)

params %>% 
  pmap(rnorm) %>% 
  str()

## Invoking Different Functions

f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1),
  list(sd = 5),
  list(lambda = 10)
)
invoke_map(f, param, n = 5) %>% str()

sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)
sim_tibble <- sim %>% 
  mutate(sim = invoke_map(f, params, n = 10))

sim_tibble$params

# Walk

x <- list(1, "a", 3)
x %>% 
  walk(print)

plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")
pwalk(list(paths, plots), ggsave, path = tempdir())

pdfFonts()

# Other Patterns of For Loops

## Predicate Functions

iris %>% 
  keep(is.factor) %>% 
  str()

iris %>% 
  discard(is.factor) %>% 
  str()

x <- list(1:5, letters, list(10))
x
x %>% 
  some(is_character)

x %>% 
  every(is_vector)

x <- sample(10)

x %>% 
  detect(~ . > 5)

x %>% 
  detect_index(~ . > 5)

?detect_index

x %>% 
  head_while(~ . > 5)
x %>% 
  tail_while(~ . > 5)

## Reduce and Accumulate

dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)

dfs %>% reduce(full_join)

vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)
vs %>% reduce(intersect)

(x <- sample(10))
x
x %>% accumulate(`+`)

## Exercises

#1 
mtcars %>% every(is_vector)

output <- vector("double", ncol(mtcars))
output
for (i in seq_along(mtcars)) {
  if(!is_vector(mtcars[[i]])) print(F) 
  print(T)
}

every2 <- function(.x, .p, ...) {
  for (i in .x) {
    if (!.p(i, ...)) {
      # If any is FALSE we know not all of then were TRUE
      return(FALSE)
    }
  }
  # if nothing was FALSE, then it is TRUE
  TRUE  
}

#2

col_sum2 <- function(df, f, ...) {
  map(keep(df, is.numeric), f, ...)
}

col_sum2(df, mean)

#3
col_sum3 <- function(df, f) {
  is_num <- sapply(df, is.numeric)
  df_num <- df[, is_num]
  
  sapply(df_num, f)
}

df <- tibble(
  x = 1:3,
  y = 3:1,
  z = c("a", "b", "c")
)

col_sum3(df, mean)
col_sum3(df[1:2], mean)
col_sum3(df[1], mean)
col_sum3(df[0], mean)


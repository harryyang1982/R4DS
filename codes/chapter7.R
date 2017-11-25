vignette("tibble")
library(tidyverse)

# Creating Tibbles

str(iris)
as_tibble(iris)

tibble(
  x = 1:5,
  y = 1, 
  z = x^2 + y
)

tb <- tibble(
  `:)` = "smile",
  ` ` = "space",
  `2000` = "number"
)
tb

tribble(
  ~x, ~y, ~z,
  #--/--/----
  "a", 2, 3.6,
  "b", 1, 8.5
)

# Tibbles Versus data.frame

tibble(
  a = lubridate:: now() + runif(1e3) * 86400,
  b = lubridate:: today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = T)
)


nycflights13::flights %>% 
  print(n = 10, width = Inf)

nycflights13::flights %>% 
  View()

## Subsetting

df <- tibble(
  x = runif(5),
  y = rnorm(5)
)

df$x
df[["x"]]

df[[1]]

df %>% .$x
df %>%  .[["x"]]

# Interacting with Older Code

class(as.data.frame(tb))

## exercises

#1
as_tibble(mtcars)
mtcars

#2
df <- data.frame(abc = 1, xyz = "a")
df
df$x
df[, "xyz"]
df[, c("abc", "xyz")]

tb <- tibble(abc = 1, xyz = "a")
tb
tb$x
tb[, "xyz"]
tb[, c("abc", "xyz")]

#3

var <- as_tibble("mpg")
var
package?tibble

var$value
var[["value"]]

#4
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
##a
annoying[[1]]
annoying$`1`
##b
annoying %>% 
  ggplot(aes(x = `1`, y = `2`)) +
  geom_point()
##c
annoying %>% 
  mutate(`3` = `2` / `1`)
##d
annoying %>% 
  rename(one = `1`, two = `2`)

#5
?tibble::enframe
enframe(1:3)
enframe(c(a = 5, b = 7))


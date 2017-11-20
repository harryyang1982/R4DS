library(tidyverse)
library(nycflights13)

glimpse(flights)

data(flights)

# Exercises

#1-a
flights %>% 
  filter(arr_delay >= 120)
#1-b
flights %>% 
  filter(dest == "IAH" | dest == "HOU")
#1-c
flights %>% 
  filter(carrier %in% c("UA", "DL", "AA"))

#1-d
flights %>% 
  filter(month %in% c(7, 8, 9))

#1-e
flights %>% 
  filter(arr_delay >=60 & dep_delay==0)

#1-f
flights %>% 
  filter(dep_delay >= 60 & arr_delay <= 30)

#1-g
flights %>% 
  filter(dep_time >= 0 & dep_time <= 600)

#2
?between

#2-a
flights[between(flights$arr_delay, 120, Inf),]

#2-d
flights[between(flights$month, 7, 9),]

#2-e
flights[between(flights$arr_delay, 60, Inf) & flights$dep_delay == 0,] %>% 
  na.omit()

#2-f
k <- flights[between(flights$dep_delay, 60, Inf) & between(flights$arr_delay, -Inf, 30), ] %>% 
  na.omit()

#2-g
flights[between(flights$dep_time, 0, 600),]

#3
flights %>% 
  count(is.na(dep_time))

flights %>% 
  filter(is.na(dep_time))

#4
NA^0
NA|T
F&NA
NA*0

# Arrange Rows with arrange()
flights %>% 
  arrange(year, month, day)

flights %>% 
  arrange(desc(arr_delay))

df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))

# Exercises
#1
df %>% 
  arrange(!is.na(x))
#2
flights %>% 
  arrange(desc(dep_delay))
flights %>% 
  arrange(dep_delay)
#3
flights %>% 
  arrange(flight)

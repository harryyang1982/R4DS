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

flights %>% 
  select(year, month, day)

flights %>% 
  select(year:day)

flights %>% 
  select(-(year:day))

flights %>% 
  rename(tail_num = tailnum)

flights %>% 
  select(time_hour, air_time, everything())

# Exercises

#1
flights %>% 
  select(dep_time, dep_delay, arr_time, arr_delay)

flights %>% 
  select(dep_time:arr_delay, -sched_dep_time, -sched_arr_time)

flights %>% 
  select(starts_with("dep"), starts_with("arr"))

#2
flights %>% 
  select(dep_time, dep_time, dep_time)

vars <- c(
  "year", "month", "day", "dep_delay", "arr_delay"
)

#3
flights %>% 
  select(one_of(vars))

#4
flights %>% 
  select(contains("TIME"))

str(flights)

# Add New Variables with mutate()


flights_sml <- flights %>% 
  select(year:day, 
         ends_with("delay"),
         distance,
         air_time)

flights_sml %>% 
  mutate(gain = arr_delay - dep_delay,
         speed = distance / air_time * 60)

flights_sml %>% 
  mutate(gain = arr_delay - dep_delay,
         hours = air_time / 60,
         gain_per_hour = gain / hours)

flights %>% 
  transmute(gain = arr_delay - dep_delay,
            hours = air_time / 60,
            gain_per_hour = gain / hours)

flights %>% 
  transmute(dep_time,
            hour = dep_time %/% 100,
            minute = dep_time %% 100)

# Useful Creation Functions

(x <- 1:10)
lag(x)
lead(x)

x - lag(x)
x != lag(x)

x
cumsum(x)
cummean(x)

y <- c(1,2,2,NA,3,4)
min_rank(y)
min_rank(desc(y))

row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)

# Exercise

#1
flights %>% 
  transmute(dep_hour = dep_time %/% 100, 
            dep_minute = dep_time %% 100, 
            sched_dep_hour = sched_dep_time %/% 100,
            sched_dep_minute = sched_dep_time %% 100) %>% 
  transmute(dep_time = dep_hour * 60 + dep_minute, 
            sched_dep_time = sched_dep_hour * 60 + sched_dep_minute) %>% 
  ggplot(aes(dep_time)) +
  geom_bar()

#2
flights %>% 
  transmute(air_time, 
            com_time = arr_time - dep_time)

#3
flights %>% 
  select(dep_time, sched_dep_time, dep_delay)

#4
k <- flights %>% 
  mutate(delay_sum = arr_delay + dep_delay) 

n <- min_rank(k$delay_sum)

flights %>% 
  mutate(ranking = n) %>% 
  arrange(desc(n)) %>% 
  head(10)

#5
1:3 + 1:9

#6
readr("https://stat.ethz.ch/R-manual/R-devel/library/base/html/Trig.html")

# Grouped Summaries with summarize()

summarize(flights, delay = mean(dep_delay, na.rm = T))

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = T))

flights %>% 
  group_by(year, month, day) %>% 
  summarise(delay = mean(dep_delay, na.rm = T))

## Combining Multiple Operations with the Pipe

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = T),
                   delay = mean(arr_delay, na.rm = T))

delay <- filter(delay, count > 20, dest!= "HNL")
delay

ggplot(delay, aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = F)

delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = T),
    delay = mean(arr_delay, na.rm = T)
  ) %>% 
  filter(count > 20, dest != "HNL")
delay == delays


## Missing Values

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = T))

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

## Counts


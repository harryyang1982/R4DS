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

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(delays, aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = T),
    n = n()
  )

ggplot(delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

delays %>% 
  filter(n > 25) %>% 
  ggplot(aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = T) / sum(AB, na.rm = T),
    ab = sum(AB, na.rm = T)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(aes(x = ab, y = ba)) +
  geom_point() +
  geom_smooth(se = F)

batters %>% 
  arrange(desc(ba))

## Useful Summary Functions

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    # average delay:
    avg_delay1 = mean(arr_delay),
    # average positive delay:
    avg_delay2 = mean(arr_delay[arr_delay > 0])
  )

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time),
    last_dep = last(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

not_cancelled %>% 
  count(dest)

not_cancelled %>% 
  count(tailnum, wt = distance)

not_cancelled %>% 
  count(tailnum, wt = air_time)

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))

mean(not_cancelled$arr_delay > 60)

## Grouping by Multiple Variables

daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year <- summarise(per_month, flights = sum(flights)))

## Ungrouping

daily %>% 
  ungroup() %>% 
  summarise(flights = n())

# Exercises

#1
flights %>% 
  mutate(early_late = ifelse(arr_delay == 15, "late",
                             ifelse(arr_delay == -15, "early", "soso"))) %>% 
  group_by(tailnum) %>% 
  summarise(late_sum = mean(early_late =="late", na.rm=T), early_sum = mean(early_late=="early", na.rm=T))
  
flights %>% 
  group_by(tailnum) %>% 
  summarise(late_sum = mean(arr_delay == 15, na.rm=T), early_sum = mean(arr_delay == -15, na.rm=T)) %>% 
  filter(late_sum == 0.5, early_sum == 0.5)

flights %>% 
  group_by(tailnum) %>% 
  summarise(ten_late = mean(arr_delay == 10, na.rm=T))

flights %>% 
  summarise(ten_late = mean(arr_delay == 10, na.rm=T))

flights %>% 
  group_by(tailnum) %>% 
  summarise(late_sum = mean(arr_delay == 30, na.rm=T), early_sum = mean(arr_delay == -30, na.rm=T)) %>% 
  filter(late_sum >= 0.5, early_sum <= 0.5)

flights %>% 
  group_by(tailnum) %>% 
  summarise(late_sum = mean(arr_delay == 200, na.rm=T), on_time = mean(arr_delay == 0, na.rm=T)) %>% 
  filter(late_sum <= 0.01, on_time >= 0.99)

#2
not_cancelled %>% 
  count(dest)

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(n())

not_cancelled %>% 
  count(tailnum, wt = distance)

not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(n = sum(distance))

#3
flights %>% 
  filter((is.na(arr_delay)))

#4
cancelled <- flights %>% 
  filter((is.na(arr_delay)))

cancelled_day <- cancelled %>% 
  group_by(year, month, day) %>% 
  count() %>% 
  arrange(desc(n))

cancelled_month <- cancelled_day %>% 
  group_by(year, month) %>% 
  summarise(n=sum(n))

#5
flights %>% 
  group_by(carrier, dest) %>% 
  summarise(n())

flights %>% 
  group_by(dest) %>% 
  summarise(delay_mean = mean(arr_delay, na.rm=T)) %>% 
  arrange(desc(delay_mean)) %>% 
  head(10)

flights %>% 
  group_by(carrier) %>% 
  summarise(delay_mean = mean(arr_delay, na.rm=T)) %>% 
  arrange(desc(delay_mean)) %>% 
  head(10)

flights %>% 
  group_by(carrier, dest) %>% 
  summarise(delay_mean = mean(arr_delay, na.rm=T)) %>% 
  arrange(desc(delay_mean)) %>% 
  head(20) %>% 
  ggplot(aes(x=dest, y=delay_mean)) +
  geom_col(aes(fill=carrier))

#6
flights %>% 
  group_by(carrier) %>% 
  summarise(n=n_distinct(dep_delay > 100))

#7
flights %>% 
  count(dest, sort = T)

# Grouped Mutates (and Filters)

flights_sml %>% 
  group_by(year, month, day) %>% 
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)

popular_dests

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

# Exercises

#2
worst_tail <- flights %>% 
  filter(arr_delay > 0, dep_delay > 0) %>% 
  group_by(tailnum) %>% 
  summarise(delay_mean = mean(arr_delay+dep_delay)) %>% 
  arrange(desc(delay_mean)) %>% 
  head(10)

#3
flights %>% 
  filter(dep_delay > 0) %>% 
  group_by(hour) %>% 
  summarise(delay = sum(dep_delay>0, na.rm=T) / sum(dep_delay, na.rm=T)) %>% 
  arrange(delay)

#4
flights %>% 
  group_by(dest) %>% 
  filter(arr_delay <0) %>% 
  summarise(minus_delay = mean(arr_delay, na.rm=T))

flights %>% 
  filter(arr_delay >0) %>% 
  mutate(delay_prop = arr_delay / sum(arr_delay, na.rm=T)) %>% 
  group_by(tailnum) %>% 
  summarise(delay_p = mean(delay_prop))
  
#5
not_cancelled %>% 
  filter(arr_delay > 10) %>% 
  mutate(lag_delay = lag(arr_delay)) %>% 
  ggplot(aes(arr_delay, lag_delay)) +
  geom_point() +
  geom_smooth()

#6
not_cancelled %>% 
  group_by(tailnum) %>% 
  mutate(speed = distance / air_time) %>% 
  summarise(mean_speed = mean(speed)) %>% 
  arrange(mean_speed) %>% 
  head(10)
  
library(tidyverse)
library(nycflights13)

airlines
airports
planes
weather

planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)

flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)

flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)

## Exercises

#2
Lahman::Batting
babynames::babynames
nasaweather::atmos
fueleconomy::vehicles
diamonds

Lahman::Master
Lahman::Salaries
Lahman::AwardsManagers

# Mutating Joins

flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2

flights2 %>% 
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")

flights2 %>% 
  left_join(airlines, by = "carrier")

flights2 %>% 
  select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

## Understanding Joins

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)

y <- tribble(
  ~key, ~val_x,
  1, "y1",
  2, "y2",
  4, "y3"
)

## Inner Join

x %>% 
  inner_join(y, by = "key")

## Duplicate Keys

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)

left_join(x, y, by = "key")

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)

left_join(x, y, by = "key")

## Defining the Key Columns

flights2 %>% 
  left_join(weather)

flights2 %>% 
  left_join(planes, by = "tailnum")

flights2 %>% 
  left_join(airports, c("dest" = "faa"))

flights2 %>% 
  left_join(airports, c("origin" = "faa"))

## Exercises

#1.

airports %>% 
  semi_join(flights, c("faa" = "dest")) %>% 
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point(aes(color = alt)) +
  coord_quickmap()

#2.
flights %>% 
  left_join(airports, by = c("dest" = "faa"))

#3.
flights %>% 
  left_join(planes, "tailnum") %>% 
  filter(arr_delay > 0) %>% 
  group_by(tailnum) %>% 
  mutate(flight_age = year.x - year.y) %>% 
  summarise(flight_age = mean(flight_age, na.rm = T), arr_delay = mean(arr_delay, na.rm=T)) %>% 
  ggplot(aes(flight_age, arr_delay)) +
  geom_col()

#4.
flights %>% 
  select(year:day, hour, origin, dest, arr_delay, tailnum, carrier) %>% 
  left_join(weather) %>% 
  na.omit(arr_delay) %>% 
  mutate(delay_rate = ifelse(arr_delay < 0, "low",
                             ifelse(arr_delay < 30, "middle", "much"))) %>% 
  group_by(delay_rate) %>% 
  summarise(temp = mean(temp, na.rm = T), dewp = mean(dewp, na.rm = T), humid = mean(humid, na.rm = T), wind_speed = mean(wind_speed, na.rm = T))

glimpse(weather)
?weather

flight3 <- flights %>% 
  select(year:day, hour, origin, dest, arr_delay, tailnum, carrier) %>% 
  left_join(weather)

flight3

summary(glm(arr_delay ~ temp + dewp + humid + wind_speed + wind_gust, data = flight3))

#5.

flights %>% 
  select(year:day, hour, origin, dest, arr_delay, tailnum, carrier) %>% 
  left_join(weather) %>% 
  filter(year == 2013, month == 6, day == 13) %>% 
  ggplot(aes(arr_delay)) +
  geom_bar()

## Filtering Joins

top_dest <- flights %>% 
  count(dest, sort = T) %>% 
  head(10)
top_dest

flights %>% 
  filter(dest %in% top_dest$dest)

flights %>% 
  semi_join(top_dest)

flights %>% 
  anti_join(top_dest)

flights %>% 
  anti_join(planes, by = "tailnum") %>% 
  count(tailnum, sort = T)

## Exercises

#1. 

flights %>% 
  anti_join(planes, by = "tailnum")

#2.

planes %>% 
  count(tailnum, sort=T) %>% 
  tail(10)

flights %>% 
  semi_join(planes, by = "tailnum") %>% 
  count(tailnum, sort = T) %>% 
  filter(n >= 100)

#3.
data(vehicles, package = "fueleconomy")
data(common, package = "fueleconomy")

vehicles %>% 
  semi_join(common, by = "make")

#4.
glimpse(flights)

flights4 <- flights %>% 
  group_by(year, month, day, hour) %>% 
  summarise(sum_delay = sum(arr_delay, na.rm = T), mean_delay = mean(arr_delay, na.rm = T)) %>% 
  arrange(year, month, day, hour)

flights5 <- flights4 %>% 
  semi_join(weather)

#5.

flights %>% 
  anti_join(airports, by = c('dest' = 'faa'))

airports %>% 
  anti_join(flights, by = c('faa' = 'dest'))

# Join Problems

airports %>%  count(alt, lon) %>%  filter(n > 1)

# Set Operations

df1 <- tribble(
  ~x, ~y,
  1, 1,
  2, 1
)
df2 <- tribble(
  ~x, ~y,
  1, 1,
  1, 2
)

intersect(df1, df2)

union(df1, df2)

setdiff(df1, df2)
setdiff(df2, df1)

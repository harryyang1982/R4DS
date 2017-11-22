#Chapter 5 Exploratory Data Analysis

library(tidyverse)

diamonds %>% 
  ggplot() +
  geom_bar(aes(x = cut))

diamonds %>% 
  count(cut)

diamonds %>% 
  group_by(cut) %>% 
  summarise(n=n())

ggplot(diamonds) +
  geom_histogram(aes(x=carat), binwidth = 0.5)

diamonds %>% 
  count(cut_width(carat, 0.5))

diamonds %>% 
  count(cut_interval(carat, 11))

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

ggplot(smaller, aes(carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1)

## Typical Values

ggplot(smaller, aes(carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(faithful, aes(x = eruptions)) +
  geom_histogram(binwidth = 0.25)

## Unusual Values

ggplot(diamonds) +
  geom_histogram(aes(x = y), binwidth = 0.5)

ggplot(diamonds) +
  geom_histogram(aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  arrange(y)
unusual

### Exercises

#1
ggplot(diamonds) +
  geom_histogram(aes(x = x), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 30))

ggplot(diamonds) +
  geom_histogram(aes(x = z), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 30))

#2
ggplot(diamonds) +
  geom_histogram(aes(x = price), binwidth = 10) +
  coord_cartesian(ylim = c(0, 30))

#3
diamonds %>% 
  filter(carat == 0.99) %>% 
  count()

diamonds %>% 
  filter(carat == 1) %>% 
  count()

#4
ggplot(diamonds) +
  geom_histogram(aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 30))

ggplot(diamonds) +
  geom_histogram(aes(x = y), binwidth = 0.5) +
  ylim(0, 30)

ggplot(diamonds) +
  geom_histogram(aes(x = y)) +
  ylim(0, 30)

## Missing Values

diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(diamonds2, aes(x = x, y = y)) +
  geom_point()

ggplot(data = diamonds2, aes(x = x, y = y)) +
  geom_point(na.rm = T)

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(aes(sched_dep_time)) +
  geom_freqpoly(
    aes(color = cancelled),
    binwidth = 1/4
  )

ggplot(diamonds2, aes(x=y)) +
  geom_bar()

ggplot(diamonds2, aes(x=y)) +
  geom_histogram()


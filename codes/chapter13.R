## Prerequisites

library(tidyverse)
library(lubridate)
library(nycflights13)

# Creating Date/Times

today()
now()
Sys.timezone()
locale("ko")

# From Strings

ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")
dmy("31-January-2017")

ymd(20170131)
ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")
ymd(20170131, tz = "UTC")

## From Individual Components

flights %>% 
  select(year, month, day, hour, minute)

flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(
    departure = make_datetime(year, month, day, hour, minute)
  )

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(
      year, month, day, sched_dep_time
    ),
    sched_arr_time = make_datetime_100(
      year, month, day, sched_arr_time
    )
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt

flights_dt %>% 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86490)

flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 600)

## From Other Types

as_datetime(today())
as_date(now())

as_datetime(60 * 60 * 10)
as_date(365 * 10 + 2)

## Exercises

#1
ymd(c("2010-10-10", "bananas"))

#2
today(tzone = "Asia/Seoul")

#3
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14"

mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)

# Date-Time Components
## Getting Components

datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)
month(datetime)
day(datetime)
mday(datetime)

yday(datetime)
wday(datetime)

month(datetime, label = T)
wday(datetime, label = T, abbr = F)
wday(datetime, label = T, abbr = T)

flights_dt %>% 
  mutate(wday = wday(dep_time, label = T)) %>% 
  ggplot(aes(wday)) +
  geom_bar()

flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm = T), n = n()) %>% 
  ggplot(aes(minute, avg_delay)) +
  geom_line()

sched_dep <- flights_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = T),
    n = n())

ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()

ggplot(sched_dep, aes(minute, n)) +
  geom_line()

## Rounding

flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()

## Setting Components

(datetime <- ymd_hms("2016-07-08 12:34:56"))
year(datetime) <- 2020
datetime
month(datetime) <- 01
datetime
hour(datetime) <- hour(datetime) + 1
datetime

update(datetime, year = 2020, month = 2, mday = 2, hour = 2)

ymd("2015-02-01") %>% 
  update(mday = 30)
ymd("2015-02-01") %>% 
  update(hour = 400)

flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)

## Exercises

#1
flights_ft <- flights %>% 
  mutate(flight_time = make_datetime_100(year, month, day, flight)) %>% 
  select(origin, dest, flight_time)

flights_ft %>% 
  ggplot(aes(flight_time)) +
  geom_freqpoly(binwidth = 86490)

#2
flights_dt %>% 
  mutate(dep_time_ = sched_dep_time + dep_delay * 60) %>% 
  filter(dep_time_ != dep_time) %>% 
  select(dep_time_, dep_time, sched_dep_time, dep_delay)

#3
flights %>% 
  mutate(flight_dur = arr_time - dep_time,
         diff = flight_dur - air_time) %>% 
  select(origin, dest, flight_dur, air_time, diff) %>% 
  ggplot(aes(diff)) +
  geom_freqpoly()

flights_loc <- flights %>% 
  mutate(flight_dur = arr_time - dep_time,
         diff = flight_dur - air_time) %>% 
  select(origin, dest, flight_dur, air_time, diff)

flights_loc

#4
flights_dt %>%
  mutate(sched_dep_hour = hour(sched_dep_time)) %>%
  group_by(sched_dep_hour) %>%
  summarise(dep_delay = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay, x = sched_dep_hour)) +
  geom_point() +
  geom_smooth()

flights_dt %>% 
  mutate(dep_hour = hour(update(sched_dep_time, yday = 1))) %>% 
  group_by(dep_hour) %>% 
  summarise(dep_delay = mean(dep_delay)) %>% 
  ggplot(aes(dep_hour, dep_delay)) +
  geom_point() +
  geom_smooth()

#5
flights_dt %>% 
  mutate(dep_day = wday(dep_time, label = T)) %>% 
  group_by(dep_day) %>% 
  summarise(delay = mean(dep_delay, na.rm = T)) %>% 
  ggplot(aes(dep_day, delay)) +
  geom_col()

#6

diamonds$carat
table(diamonds$carat)

flights$sched_dep_time
ggplot(diamonds, aes(x = carat)) + 
  geom_density()

ggplot(diamonds, aes(x = carat %% 1 * 100)) +
  geom_histogram(binwidth = 1)

ggplot(flights_dt, aes(x = minute(sched_dep_time))) +
  geom_histogram(binwidth = 1)

#7
flights_dt %>%
  mutate(early = dep_delay < 0,
         minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarise(early = mean(early)) %>%
  ggplot(aes(x = minute, y = early)) +
  geom_point()

flights_dt %>%
  mutate(early = dep_delay < 0,
         minute = minute(sched_dep_time) %% 10) %>%
  group_by(minute) %>%
  summarise(early = mean(early)) %>%
  ggplot(aes(x = minute, y = early)) +
  geom_point()

# Time Spans

h_age <- today() - ymd(19791014)
h_age

as.duration(h_age)
y_age <- today() - ymd(19821106)
y_age

as.duration(y_age)

dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(3)
dyears(1)

2 * dyears(1)
dyears(1) + dweeks(12) + dhours(15)

tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)

tomorrow
last_year

one_pm <- ymd_hms(
  "2016-03-12 13:00:00",
  tz = "America/New_York"
)
one_pm
one_pm + ddays(1)

## Periods

one_pm
one_pm + days(1)

Sys.getenv("TZ")
Sys.setenv(TZ="Asia/Seoul")

seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
weeks(3)
years(1)
weeks(1)

10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)

ymd("2016-01-01") + dyears(1)
ymd("2016-01-01") + years(1)

one_pm + ddays(1)
one_pm + days(1)

flights_dt %>% 
  filter(arr_time < dep_time)

flights_dt <- flights_dt %>% 
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
  )

flights_dt %>% 
  filter(overnight, arr_time < dep_time)

## Intervals

dyears(1) / ddays(365)
years(1) / days(1)
next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)
(today() %--% next_year) %/% days(1)

# Summary

## Exercises
#1.
months(3)
dmonths(3)

#2.
#3.
ymd("2015-01-01") + months(0:11)
ymd(paste0(year(today()),"01","01", sep="-")) + months(0:11)
ymd(str_c(year(today()), "01", "01", sep="-")) + months(0:11)

#4.
your_age <- function(birthday) {
  (ymd(birthday) %--% today()) %/% years(1)
}
your_age("1982-11-06")

today() - ymd(19821106)

age <- function(bday) {
  (ymd(bday) %--% today()) %/% years(1)
}
age("19821106")

#5.
(today() %--% (today() + years(1)) / months(1))

# Time Zones

length(OlsonNames())
head(OlsonNames())

x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York")
x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen")
x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland")
x1
x2
x3
x1 - x2
x1 - x3

x4 <- c(x1, x2, x3)
x4
x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a
x4a - x4
x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b
x4b - x4

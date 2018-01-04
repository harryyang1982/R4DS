## Prerequisites
library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)

# Why Are Low-Quality Diamonds More Expensive?

ggplot(diamonds, aes(cut, price)) + 
  geom_boxplot()
ggplot(diamonds, aes(color, price)) +
  geom_boxplot()
ggplot(diamonds, aes(clarity, price)) +
  geom_boxplot()

## Price and Carat

ggplot(diamonds, aes(carat, price)) +
  geom_hex(bins = 50)
ggplot(diamonds, aes(carat, price)) +
  geom_point()

diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(lcarat, lprice)) +
  geom_hex(bins = 50)

mod_diamonds <- lm(lprice ~ lcarat, data = diamonds2)

grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamonds, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) +
  geom_hex(bins = 50) +
  geom_line(data = grid, color = "red", size = 1)

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamonds, "lresid")

ggplot(diamonds2, aes(lcarat, lresid)) +
  geom_hex(bins = 50)

ggplot(diamonds2, aes(cut, lresid)) + 
  geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) +
  geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) +
  geom_boxplot()

## A More Complicated Model

mod_diamonds2 <- lm(
  lprice ~ lcarat + color + cut + clarity,
  data = diamonds2
)

grid <- diamonds2 %>%
  data_grid(cut, lcarat = -0.515, color = "G", clarity = "SI1") %>%
  add_predictions(mod_diamonds2)

ggplot(grid, aes(cut, pred)) +
  geom_point()

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamonds2, "lresid2")

ggplot(diamonds2, aes(lcarat, lresid2)) +
  geom_hex(bins = 50)

diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamonds2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)

## Exercises

#1
#2
#3
unique(diamonds2$lresid)
#4
diamonds2 %>% 
  add_predictions(mod_diamonds2) %>%
  add_residuals(mod_diamonds2) %>%
  summarise(sq_err = sqrt(mean(resid^2)),
            abs_err = mean(abs(resid)),
            p975_err = quantile(resid, 0.975),
            p025_err = quantile(resid, 0.025))

# What Affects the Number of Daily Flights?

daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())
daily

ggplot(daily, aes(date, n)) +
  geom_line()

## Day of Week
daily <- daily %>% 
  mutate(wday = wday(date, label = T))
ggplot(daily, aes(wday, n)) +
  geom_boxplot()

mod <- lm(n ~ wday, data = daily)

grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")
grid

ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "red", size = 4)

daily <- daily %>% 
  add_residuals(mod)

daily %>% 
  ggplot(aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line()

ggplot(daily, aes(date, resid, color = wday)) +
  geom_ref_line(h = 0) +
  geom_line()

daily %>% 
  filter(resid < -100)

daily %>% 
  ggplot(aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line(color = "grey50") +
  geom_smooth(se = F, span = 0.20)

## Seasonal Saturday Effect

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n)) +
  geom_point() +
  geom_line() +
  scale_x_date(
    NULL,
    date_breaks = "1 month",
    date_labels = "%b"
  )

term <- function(date) {
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall"))
}

daily <- daily %>% 
  mutate(term = term(date))
daily

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, color = term)) +
  geom_point(alpha = 1/3) +
  geom_line() +
  scale_x_date(
    NULL,
    date_breaks = "1 month",
    date_labels = "%b"
  )

daily %>% 
  ggplot(aes(wday, n, color = term)) +
  geom_boxplot()


mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha = 0.75)

grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")
grid

ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "red") +
  facet_wrap(~ term)

mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) +
  geom_hline(yintercept = 0, size = 2, color = "white") +
  geom_line()

##as same as above
daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line()

## Computed Variables

compute_vars <- function(data) {
  data %>% 
    mutate(
      term = term(date),
      wday = wday(date, label = T)
    )
}

wday2 <- function(x) wday(x, label = T)
mod3 <- lm(n ~ wday2(date) * term(date), data = daily)

## Time of Year: An Alternative Approach

library(splines)
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(date, pred, color = wday)) +
  geom_line() +
  geom_point()

seq_range(daily$date, n = 13)

## Exercises

#1
#2
daily %>% 
  top_n(3, resid)

#3
k <- daily %>% 
  mutate(wday_term = ifelse(wday == "Sat", str_c(wday, term, sep = "-"), as.character(wday)))
mod <- lm(n ~ wday_term, data = k)

k %>% 
  gather_residuals(sat_term = mod, all_interact = mod2) %>% 
  ggplot(aes(date, resid, color = model)) +
  geom_line()

k2 <- daily %>%
  mutate(wday2 = 
           case_when(.$wday == "Sat" & .$term == "summer" ~ "Sat-summer",
                     .$wday == "Sat" & .$ term == "fall" ~ "Sat-fall",
                     .$wday == "Sat" & .$term == "spring" ~ "Sat-spring",
                     TRUE ~ as.character(.$wday)))
mod4 <- lm(n ~ wday2, data = k2)
k2 %>% 
  gather_residuals(sat_term = mod4, all_interact = mod2) %>% 
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75)

#4
#7
flights %>% 
  mutate(date = make_date(year, month, day),
         wday = wday(date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(dist_mean =  mean(distance),
            dist_median = median(distance)) %>%
  ggplot(aes(y = dist_mean, x = wday)) +
  geom_point()

flights %>% 
  mutate(date = make_date(year, month, day),
         wday = wday(date, label = TRUE)) %>%
  group_by(wday, hour) %>%
  summarise(dist_mean =  mean(distance),
            dist_median = median(distance)) %>%
  ggplot(aes(y = dist_mean, x = hour, colour = wday)) +
  geom_point() + 
  geom_line()

glimpse(flights)

#8
monday_first <- function(x) {
  forcats::fct_relevel(x, levels(x)[-1])  
}
daily %>% 
  mutate(wday = wday(date, label = TRUE)) %>% 
  ggplot(aes(monday_first(wday), n)) + 
  geom_boxplot() +
  labs(x = "Day of Week", y = "Number of flights")


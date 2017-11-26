library(tidyverse)

table1
table2
table3
table4a
table4b

table1 %>% 
  mutate(rate = cases / population * 10000)

table1 %>% 
  count(year, wt = cases)

table1 %>% 
  ggplot(aes(year, cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country))

## Exercises

#2
table2 %>% 
  group_by(country, year) %>% 
  summarise(rate = count[type == "cases"] / count[type == "population"] * 10000)

table4 %>% 
  mutate(type = c(rep("cases", 3), rep("population", 3))) %>% 
  group_by(country) %>% 
  summarise(rate_1999 = `1999`[type =="cases"] / `1999`[type == "population"] * 10000,
            rate_2000 = `2000`[type =="cases"] / `2000`[type == "population"] * 10000)
  
#3
table2 %>% 
  filter(type == "cases") %>% 
  ggplot(aes(year, count)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country))

# Spreading and Gathering

## Gathering

table4a %>% 
  gather(year, cases, `1999`:`2000`)

table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")

table4b %>% 
  gather(year, population, `1999`:`2000`)

table4a <- table4a %>% 
  gather(year, cases, `1999`:`2000`)
table4b <- table4b %>% 
  gather(year, population, `1999`:`2000`)
left_join(table4a, table4b)

## Spreading

table2 %>% 
  spread(type, count)

table2 %>% 
  spread(key = type, value = count)

## Exercises

#1

stocks <- tibble(
  year = c(2015, 2015, 2016, 2016),
  half = c(1, 2, 1, 2),
  return = c(1.88, 0.59, 0.92, 0.17)
)

stocks
stocks %>% 
  spread(year, return) %>% 
  gather(year, return, `2015`:`2016`)

stocks

#2
table4a %>% 
  gather(1999, 2000, key = "year", value = "cases")
table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")

#3
people <- tribble(
  ~name,              ~key,       ~value,
  #------------------/-----------/-------
  "Phillip Woods",   "age",        45,
  "Phillip Woods",   "height",    186,
  "Phillip Woods",   "weight",        50,
  "Jessica Cordero", "age",      37,
  "Jessica Cordero", "height",  156
)

people %>% 
  spread(key, value)

#4
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes", NA, 10,
  "no", 20, 12
)
preg
preg %>% 
  gather(gender, count, `male`:`female`) %>% 
  spread(pregnant, count) %>% 
  select(gender, yes, no)

## Separating and Pull

table3 %>% 
  separate(rate, into = c("cases", "population"))

table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")

table3 %>% 
  separate(rate, into = c("cases", "population"), convert = T)

table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)

table3 %>% 
  separate(year, into = c("century", "year"), sep = -3)

## Unite

table5
table5 %>% 
  unite(new, century, year)

table5 %>% 
  unite(new, century, year, sep = "") %>% 
  mutate(new = parse_double(new)) %>% 
  separate(rate, into = c("cases", "population"), convert = T)

## Exercises

#1.

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "merge")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), fill = "left")

#2.
?unite
?separate

#3
?extract

table5
table5 %>% 
  extract(rate, into="cases")


# Missing Values

stocks <- tibble(
  year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr = c(1, 2, 3, 4, 2, 3, 4),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)
stocks

stocks %>% 
  spread(year, return)

stocks %>% 
  spread(year, return) %>% 
  gather(year, return, `2015`:`2016`, na.rm = T)

stocks %>% 
  complete(year, qtr)

treatment <- tribble(
  ~person,            ~treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,          10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)

treatment %>% 
  fill(person)

## Exercises

#1.
stocks %>% 
  spread(year, qtr, fill = T)

stocks %>% 
  complete(year, qtr)


# Case Study

who

who1 <- who %>% 
  gather(key, cases, new_sp_m014:newrel_f65, na.rm=T
  )
who1

who1 %>% 
  count(key)

who2 <- who1 %>% 
  mutate(key = str_replace(key, "newrel", "new_rel"))

who2

who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep="_")
who3

who3 %>% 
  count(new)

who4 <- who3 %>% 
  select(-new, -iso2, iso3)

who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1)

who5

who %>% 
  gather(code, value, new_sp_m014:newrel_f65, na.rm = T) %>% 
  mutate(code = str_replace(code, "newrel", "new_rel")) %>% 
  separate(code, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

## Exercises

#4
who5 %>% 
  group_by(country, year, sex) %>% 
  summarise(cases = sum(cases)) %>% 
  ggplot(aes(x = year, y = cases)) +
  geom_col(aes(fill = sex), position = "dodge") +
  scale_x_continuous(limits = c(1995, 2013))


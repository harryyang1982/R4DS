library(tidyverse)

# Creating Factors

x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")
sort(x1)
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
y1
y2 <- factor(x2, levels = month_levels)
y2

y2 <- parse_factor(x2, levels = month_levels)
factor(x1)

f1 <- factor(x1, levels = unique(x1))
f1

f2 <- x1 %>% 
  factor() %>% 
  fct_inorder()
f2
levels(f2)

# General Social Survey

gss_cat
gss_cat %>% 
  count(race)

gss_cat %>% 
  ggplot(aes(race)) +
  geom_bar()

ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = F)

## Exercises

#1
ggplot(gss_cat, aes(rincome)) +
  geom_bar() +
  scale_x_discrete(drop = T)

ggplot(gss_cat, aes(rincome)) +
  geom_bar() +
  scale_x_discrete(drop = F)

gss_cat %>% 
  count(rincome)

#2
gss_cat %>% 
  count(relig) %>% 
  arrange(desc(n))

gss_cat %>% 
  count(partyid) %>% 
  arrange(desc(n))

#3
gss_cat %>% 
  group_by(denom, relig) %>% 
  count() 

gss_cat %>% 
  ggplot(aes(relig)) +
  geom_bar(aes(fill = denom))

# Modifying Factor Order

relig <- gss_cat %>% 
  group_by(relig) %>% 
  summarise(
    age = mean(age, na.rm = T),
    tvhours = mean(tvhours, na.rm = T),
    n = n()
  )

ggplot(relig, aes(tvhours, relig)) + geom_point()

ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

relig %>% 
  mutate(relig = fct_reorder(relig, tvhours)) %>% 
  ggplot(aes(tvhours, relig)) +
  geom_point()

rincome <- gss_cat %>% 
  group_by(rincome) %>% 
  summarise(age = mean(age, na.rm = T),
            tvhours = mean(tvhours, na.mr = T),
            n = n())
ggplot(rincome, aes(age, fct_reorder(rincome, age))) + 
  geom_point()

ggplot(rincome, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()

by_age <- gss_cat %>% 
  filter(!is.na(age)) %>% 
  group_by(age, marital) %>% 
  count() %>% 
  mutate(prop = n / sum(n))

by_age

ggplot(by_age, aes(age, prop, color = marital)) +
  geom_line(na.rm = T)

ggplot(by_age, aes(age, prop, color = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(color = "marital")

gss_cat %>% 
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>% 
  ggplot(aes(marital)) +
  geom_bar()

## Exercises

#1

gss_cat %>% 
  group_by(relig) %>% 
  summarise(age = mean(age, na.rm = T), tvhours = median(age, na.rm =T)) %>% 
  ggplot(aes(relig, tvhours)) +
  geom_point()

gss_cat %>% 
  group_by(relig) %>% 
  summarise(age = mean(age, na.rm = T), tvhours = mean(age, na.rm =T)) %>% 
  ggplot(aes(relig, tvhours)) +
  geom_point()

gss_cat %>% 
  group_by(rincome) %>% 
  summarise(tvhours = mean(tvhours, na.rm = T)) %>% 
  ggplot(aes(rincome, tvhours)) +
  geom_col()

#2
gss_cat
glimpse(gss_cat)

levels(gss_cat$marital)
levels(gss_cat$race)
levels(gss_cat$rincome)
levels(gss_cat$partyid)
levels(gss_cat$relig)

#3

# Modifying Factor Levels

gss_cat %>% 
  count(partyid)

gss_cat %>% 
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"     = "Strong republican",
                              "Republican, weak"       = "Not str republican",
                              "Independent, near rep"  = "Ind,near rep",
                              "Independent, near dem"  = "Ind,near dem",
                              "Democrat, weak"         = "Not str democrat",
                              "Democrat, strong"       = "Strong democrat"
                              )) %>% 
  count(partyid)

gss_cat %>% 
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"     = "Strong republican",
                              "Republican, weak"       = "Not str republican",
                              "Independent, near rep"  = "Ind,near rep",
                              "Independent, near dem"  = "Ind,near dem",
                              "Democrat, weak"         = "Not str democrat",
                              "Democrat, strong"       = "Strong democrat",
                              "Other"                  = "No answer",
                              "Other"                  = "Don't know",
                              "Other"                  = "Other party"
  )) %>% 
  count(partyid)

gss_cat %>% 
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat"))) %>% 
  count(partyid)

gss_cat %>% 
  mutate(relig = fct_lump(relig)) %>% 
  count(relig)

gss_cat$relig

gss_cat %>% 
  mutate(relig = fct_lump(relig, n = 10)) %>% 
  count(relig, sort = T) %>% 
  print(n = Inf)

gss_cat %>% 
  count(relig, sort = T)

data(gss_cat)
View(gss_cat)

## Exercise
#1
gss_cat %>% 
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat"))) %>% 
  group_by(year) %>% 
  count(partyid) %>% 
  ggplot(aes(year, n)) +
  geom_line(aes(color = partyid))

#2
gss_cat %>% 
  mutate(rincome = fct_collapse(rincome,
                                other = c("No answer", "Don't know", "Refused"),
                                high = c("$25000 or more", "$20000 - 24999"),
                                mid = c("$15000 - 19999", "$10000 - 14999", "$8000 to 9999"),
                                low = c("$7000 to 7999", "$6000 to 6999", "$5000 to 5999", "$4000 to 4999", "$3000 to 3999", "$1000 to 2999", "Lt $1000"))) %>% 
  group_by(year) %>% 
  count(rincome) %>% 
  ggplot(aes(year, n)) +
  geom_line(aes(color = rincome))


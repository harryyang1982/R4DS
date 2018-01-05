## Prerequisites
library(tidyverse)

# Label

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = F) +
  labs(
    title = paste(
      "Fuel efficiency generally decreases with",
      "engine size"
    )
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) + 
  geom_smooth(se = F) +
  labs(
    title = paste("fuel efficiency generally decreases with", "engine size"),
    subtitle = paste("Two seaters (sports cars) are an exception",
                     "because of their light weight"),
    caption = "Data from fueleconomy.gov"
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = F) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    color = "Car type"
  )

df <- tibble(
  x = runif(10),
  y = runif(10)
)
df
ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(sum(x[i] ^ 2, i == 1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  )

## Exercises
#2
ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point(aes(colour = class)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Subcompact cars show the greatest sensitivity to engine size",
    caption = "Data from fueleconomy.gov"
  )

# Annotations

best_in_class <- mpg %>% 
  group_by(class) %>% 
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_text(aes(label = model), data = best_in_class)

best_in_class

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_label(
    aes(label = model),
    data = best_in_class,
    nudge_y = 2,
    alpha = 0.5
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) +
  ggrepel::geom_text_repel(
    aes(label = model),
    data = best_in_class
  )

class_avg <- mpg %>% 
  group_by(class) %>% 
  summarise(
    displ = median(displ),
    hwy = median(hwy)
  )

ggplot(mpg, aes(displ, hwy, color = class)) +
  ggrepel::geom_label_repel(aes(label = class),
                            data = class_avg,
                            size = 6,
                            label.size = 0,
                            segment.color = NA) +
  geom_point() +
  theme(legend.position = "none")

label <- mpg %>% 
  summarise(
    displ = max(displ),
    hwy = max(hwy),
    label = paste(
      "Increasing engine size is \nrelated to",
      "decreasing fuel economy."
    )
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(
    aes(label = label),
    data = label,
    vjust = "top",
    hjust = "right"
  )

label <- tibble(
  displ = Inf,
  hwy = Inf,
  label = paste(
    "Increasing engine size is \nrelated to",
    "decreasing fuel economy."
  )
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(
    aes(label = label),
    data = label,
    vjust = "top",
    hjust = "right"
  )

"Increasing engine size related to decreasing fuel ecconomy." %>% 
  stringr::str_wrap(width = 40) %>% 
  writeLines()

# Scales

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete()

## Axis Ticks and Legend Keys

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL)

presidential %>% 
  mutate(id = 33 + row_number()) %>% 
  ggplot(aes(start, id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id, color = party)) +
  scale_x_date(
    NULL,
    breaks = presidential$start,
    date_labels = "'%y"
  )

presidential

## Legend Layout

base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class))

base + theme(legend.position = "left")
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = F) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(
      nrow = 1,
      override.aes = list(size = 4)
    )
  )

## Replacing a Scale

ggplot(diamonds, aes(carat, price)) +
  geom_bin2d()

ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv)) +
  scale_color_brewer(palette = "Set1")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_color_brewer(palette = "Set1")

presidential %>% 
  mutate(id = 33 + row_number()) %>% 
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(
    values = c(Republican = "red", Democratic = "blue")
  )

df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)
ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed()

ggplot(df, aes(x, y)) +
  geom_hex() +
  viridis::scale_fill_viridis() +
  coord_fixed()

## Exercises

#1
ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed()

#2
#3
presidential %>% 
  mutate(id = 33 + row_number()) %>% 
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(
    values = c(Republican = "red", Democratic = "blue")
  )

presidential

#3tip
library(hms)
years <- lubridate::make_date(seq(year(min(presidential$start)),
                                  year(max(presidential$end)),
                                  by = 4), 1, 1)

presidential %>%
  mutate(id = 33 + row_number(),
         name_id = stringr::str_c(name, " (", id, ")"),
         name_id = factor(name_id, levels = name_id)) %>%
  ggplot(aes(start, name_id, colour = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = name_id)) +
  scale_colour_manual(values = c(Republican = "red", Democratic = "blue")) +
  scale_y_discrete(NULL) +
  scale_x_date(NULL, breaks = years, date_labels = "'%y") +
  theme(panel.grid.minor = element_blank())

#4
ggplot(diamonds, aes(carat, price)) +
  geom_point(aes(color = cut), alpha = 1/20) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(
      nrow = 1,
      override.aes = list(alpha = 1)
    )
  )

# Zooming

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 7), ylim = c(10, 30))

mpg %>% 
  filter(displ >= 5, displ <= 7, hwy >= 10, hwy <= 30) %>% 
  ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()

suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class == "compact")

ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point()

ggplot(compact, aes(displ, hwy, color = drv)) +
  geom_point()

x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_color_discrete(limits = unique(mpg$drv))

ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale + y_scale + col_scale

ggplot(compact, aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale + y_scale + col_scale


# Themes

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = F) +
  theme_bw()

# Saving Your Plots

ggplot(mpg, aes(displ, hwy)) +
  geom_point()

## Figure Sizing


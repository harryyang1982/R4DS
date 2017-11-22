# chapter 1 - Data Visualization with ggplot2

library(tidyverse)
mpg

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy))

# 1.1 Exercises

ggplot(data=mpg)
glimpse(mpg)

?mpg

ggplot(mpg) +
  geom_point(aes(hwy, cyl))

mpg %>% 
  group_by(cyl) %>% 
  summarise(mean_hwy=mean(hwy)) %>% 
  ggplot() +
  geom_col(aes(cyl, mean_hwy))

ggplot(mpg, aes(class, drv)) +
  geom_point()

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy, color=class))


ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, size=class))

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy, alpha=class))

ggplot(mpg) +
  geom_point(aes(displ, hwy, shape=class))

ggplot(mpg) +
  geom_point(aes(displ, hwy), color="blue")

# 1-2 Exercises

ggplot(mpg) +
  geom_point(aes(displ, hwy, color="blue"))

str(mpg)

ggplot(mpg) +
  geom_point(aes(cty, hwy, color=cty))

ggplot(mpg) +
  geom_point(aes(cty, hwy, size=cty))

ggplot(mpg) +
  geom_point(aes(cty, hwy, shape=cty))

ggplot(mpg) +
  geom_point(aes(cty, displ, stroke=displ))

ggplot(mpg) +
  geom_point(aes(cty, displ, color=displ <5))

ggplot(mpg) +
  geom_point(aes(displ, hwy, color=class=="2seater"))

# Common Problems

ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy))

# Facets

ggplot(mpg) +
  geom_point(aes(displ, hwy)) +
  facet_wrap(~class, nrow=2)

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy)) +
  facet_grid(drv ~ cyl)

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy)) +
  facet_grid(. ~ cyl)

# 1-3 Exercises

#1
ggplot(data=mpg) +
  geom_point(aes(displ, y=hwy)) +
  facet_wrap(~cty)
#2
ggplot(mpg) +
  geom_point(aes(drv, cyl)) +
  facet_grid(drv~cyl)

#3
ggplot(mpg) +
  geom_point(aes(displ, hwy)) +
  facet_grid(drv~.)

ggplot(mpg) +
  geom_point(aes(displ, hwy)) +
  facet_grid(.~cyl)

#4

ggplot(mpg) +
  geom_point(aes(displ, hwy)) +
  facet_wrap(~class, nrow=2)

?facet_wrap

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(c("cyl", "drv"))
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~ cyl + drv)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class, scales = "free")

ggplot(economics_long, aes(date, value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", nrow = 2, strip.position = "bottom") +
  theme(strip.background = element_blank(), strip.placement = "outside")

# Geometric Objects

ggplot(mpg) +
  geom_point(aes(displ, hwy))

ggplot(mpg) +
  geom_smooth(aes(displ, hwy))

ggplot(mpg) +
  geom_smooth(aes(displ, hwy, linetype=drv))

ggplot(mpg) +
  geom_point(aes(displ, hwy, color=drv)) +
  geom_smooth(aes(displ, hwy, linetype=drv))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color=drv)) +
  geom_smooth(aes(linetype=drv))

ggplot(mpg) +
  geom_smooth(aes(displ, hwy))

ggplot(mpg) +
  geom_smooth(aes(displ, hwy, group=drv))

ggplot(mpg) +
  geom_smooth(aes(displ, hwy, color=drv),
              show.legend=F)

ggplot(mpg) +
  geom_point(aes(displ, hwy)) +
  geom_smooth(aes(displ, hwy))

ggplot(mpg, aes(displ, hwy)) +
  geom_point() + geom_smooth(method="lm")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color=class)) +
  geom_smooth()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color=class)) +
  geom_smooth(
    data=filter(mpg, class=="subcompact"),
    se=F
  )

# 1-4 Exercises
#2
ggplot(mpg, aes(x=displ, y=hwy, color=drv)) +
  geom_point() + geom_smooth(se=F)
#3
#4
#5

ggplot(mpg, aes(displ, hwy)) +
  geom_point() + geom_smooth()

ggplot() +
  geom_point(data=mpg, aes(displ, hwy)) + geom_point(data=mpg, aes(displ, hwy))

#6
p <- ggplot(data=mpg)
p + geom_point(aes(displ, hwy)) + geom_smooth(aes(displ, hwy), se=F)
p + geom_point(aes(displ, hwy)) + geom_smooth(aes(displ, hwy, group=drv), se=F)
p + geom_point(aes(displ, hwy, color=drv)) + geom_smooth(aes(displ, hwy, color=drv), se=F)
p + geom_point(aes(displ, hwy, color=drv)) + geom_smooth(aes(displ, hwy), color="blue", se=F)
p2 <- p + geom_point(aes(displ, hwy)) + geom_smooth(aes(displ, hwy))
p + geom_point(aes(displ, hwy, color=drv)) + geom_smooth(aes(displ, hwy, linetype=drv), se=F)
p + geom_point(aes(displ, hwy, color=drv))

# Statistcal Transformations

ggplot(diamonds) +
  geom_bar(aes(x=cut))

ggplot(data=diamonds) +
  stat_count(aes(x=cut))

demo <- tribble(
  ~a, ~b,
  "bar_1", 20,
  "bar_2", 30,
  "bar_3", 40
)
demo

ggplot(data=demo) +
  geom_bar(aes(x=a, y=b), stat="identity")
ggplot(data=demo) +
  geom_col(aes(x=a, y=b))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin=min,
    fun.ymax=max,
    fun.y = median
  )

# 1-5 Exercises
#1
?stat_summary()

#2
#3
#4
ggplot(data=mpg) +
  stat_smooth(aes(x=cty, y=displ))
?stat_smooth

#5
ggplot(data=diamonds) +
  geom_bar(aes(x=cut, y=..prop.., group=1))
ggplot(data = diamonds) +
  geom_bar(aes(x=cut, fill=color, y=..prop.., group=1))

?geom_bar
??geom_bar

# Position Adjustments
ggplot(diamonds) +
  geom_bar(aes(cut, color=cut))

ggplot(diamonds) +
  geom_bar(aes(x=cut, fill=cut))

ggplot(diamonds) +
  geom_bar(aes(cut, fill=clarity))

ggplot(data=diamonds, aes(x=cut, fill=clarity)) +
  geom_bar(alpha=1/5, position="identity")
ggplot(data=diamonds, mapping=aes(x=cut, color=clarity) +
         geom_bar(fill=NA, position="identity"))

ggplot(diamonds) +
  geom_bar(aes(x=cut, fill=clarity),
           position="fill")

ggplot(diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = clarity),
    position = "dodge"
  )

ggplot(mpg) +
  geom_point(
    aes(x=displ, y=hwy)
  )

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy),
             position="jitter")

# 1-5 Exercises

ggplot(mpg, aes(x=cty, y=hwy)) +
  geom_point()

ggplot(mpg, aes(x=cty, y=hwy)) +
  geom_point(position = "jitter")

?geom_jitter

ggplot(mpg, aes(x=cty, y=hwy)) +
  geom_jitter()

ggplot(mpg, aes(x=cty, y=hwy)) +
  geom_count()

ggplot(mpg, aes(displ, hwy, group=displ)) +
  geom_boxplot()

data(mpg)
head(mpg)

ggplot(mpg, aes(manufacturer, hwy)) +
  geom_boxplot()

# Coordinate Systems

ggplot(mpg, aes(x=class, y=hwy)) +
  geom_boxplot()

ggplot(mpg, aes(x=class, y=hwy)) +
  geom_boxplot() +
  coord_flip()

nz <- map_data("nz")
ggplot(nz, aes(long, lat, group=group)) +
  geom_polygon(fill = "white", color="black")

ggplot(nz, aes(long, lat, group=group)) +
  geom_polygon(fill = "white", color="black") +
  coord_quickmap()

bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x=cut, fill=cut),
    show.legend = F,
    width=1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

# 1.6 Exercises

#1
ggplot(diamonds) +
  geom_bar(aes(x=cut, fill=cut)) + coord_polar()

#3
?coord_map

#4

ggplot(data=mpg, mapping = aes(x = cty, y= hwy)) +
  geom_point() +
  geom_abline() +
  coord_fixed()

?geom_abline

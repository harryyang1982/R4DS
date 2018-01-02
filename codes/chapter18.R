library(tidyverse)
library(modelr)
options(na.action = na.warn)

# A Simple Model

ggplot(sim1, aes(x, y)) +
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) +
  geom_abline(
    aes(intercept = a1, slope = a2),
    data = models, alpha = 1/4
  ) +
  geom_point()

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}

model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(models, rank(dist) <= 10)
  )

ggplot(models, aes(a1, a2)) +
  geom_point(
    data = filter(models, rank(dist) <= 10),
    size = 4, color = "red"
  ) +
  geom_point(aes(color = -dist))

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(
    data = filter(grid, rank(dist) <= 10),
    size = 4, color = "red"
  ) +
  geom_point(aes(color = -dist))

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(grid, rank(dist) <= 10)
  )

best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])

sim1_mod <- lm(y ~ x, data =sim1)
coef(sim1_mod)

## Exercises

sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)
sim1a

sim1a_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1a)
}

models2 <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1a_dist))

models2

ggplot(sim1a, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(models2, rank(dist) <= 10)
  )

ggplot(models2, aes(a1, a2)) +
  geom_point(
    data = filter(models, rank(dist) <= 10),
    size = 4, color = "red"
  ) +
  geom_point(aes(color = -dist))

grid2 <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1a_dist))

grid2 %>% 
  ggplot(aes(a1, a2)) +
  geom_point(
    data = filter(grid, rank(dist) <= 10),
    size = 4, color = "red"
  ) +
  geom_point(aes(color = -dist))

ggplot(sim1a, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(grid, rank(dist) <= 10)
  )

lm(y ~ x, data = sim1a)

ggplot(sim1a, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = 6.633, slope = 1.396)
  )

simt <- function(i) {
  tibble(
    x = rep(1:10, each = 3),
    y = x * 1.5 + 6 + rt(length(x), df = 2),
    .id = i
  )
}

lm_df <- function(.data) {
  mod <- lm(y ~ x, data = .data)
  beta <- coef(mod)
  tibble(intercept = beta[1], slope = beta[2])
}

sims <- map(1:100, simt) %>%
  map_df(lm_df)

sims

ggplot(sims, aes(x = intercept, y = slope)) +
  geom_point()

##2

make_prediction <- function(model, df) {
  
  # Get class of model object
  model_class <- class(model)[1]
  
  # Different prediction logic dependent on model type
  if (model_class == "gam") {
    
    # Seems to be generic
    x <- unname(predict(model, df))
    
    # A one dimensional matrix? A little odd, drop
    attr(x, "dim") <- NULL
    
  } else if (model_class == "randomForest.formula") {
    
    # Can be generic, but use name space for when the package is not loaded
    x <- unname(randomForest:::predict.randomForest(model, df))
    
  } else if (model_class == "ksvm") {
    
    # Not generic and returns a matrix
    x <- unname(kernlab::predict(model, df))[, 1]
    
  } else if (model_class == "gbm") {
    
    # Use a vector but needs an extra argument, comes from model object
    x <- gbm::predict.gbm(
      model, 
      df, 
      n.trees = length(model$trees)
    )
    
  } else if (model_class == "lm") {
    
    x <- unname(predict(model, df))
    
  } else {
    
    stop("Model not recognised.", call. = FALSE)
    
  }
  
  return(x)
  
}

measure_distance <- function(mod, data) {
  diff <- data$y - make_prediction(mod, data)
  mean(abs(diff))
}

best <- optim(c(0, 0), measure_distance, data = sim1)

#3
model1 <- function(a, data) {
  a[1] + data$x * a[2] + a[3]
}

# Visualizing Models

## Predictions

grid <- sim1 %>% 
  data_grid(x)

grid
grid <- grid %>% 
  add_predictions(sim1_mod)
grid
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(
    aes(y = pred),
    data = grid,
    color = "red",
    size = 1
  )

## Residuals

sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1

ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth = 0.5)

ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

## Exercises
#1
sim1 <- sim1 %>% 
  mutate(resid = loess(y ~ x, sim1)$resid)

ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth = 0.5)
ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

#2
?gather_predictions

# Formulas and Model Families

df <- tribble(
  ~y, ~x1, ~x2,
  4,   2,   5,
  5,   1,   6)

df
model_matrix(df, y~x1)

model_matrix(df, y ~ x1 - 1)

model_matrix(df, y ~ x1 + x2)

## Categorical Variables

df <- tribble(
  ~ sex, ~ response,
    "male", 1,
    "female", 2,
    "male", 1
)
df
model_matrix(df, response ~ sex)

ggplot(sim2) +
  geom_point(aes(x, y))

mod2 <- lm(y ~ x, data = sim2)
grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)

ggplot(sim2, aes(x)) +
  geom_point(aes(y = y)) +
  geom_point(
    data = grid,
    aes(y = pred),
    color = "red",
    size = 4
  )

tibble(x = 'e') %>% 
  add_predictions(mod2)

## Interactions (Continuous and Categorical)

ggplot(sim3, aes(x1, y)) +
  geom_point(aes(color = x2))

sim3

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2)
grid

ggplot(sim3, aes(x1, y, color = x2)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~model)

sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, color = x2)) +
  geom_point() +
  facet_grid(model ~ x2)

## Interactions (Two Continuous)

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5),
    x2 = seq_range(x2, 5)
  ) %>% 
  gather_predictions(mod1, mod2)

seq_range(c(0.0123, 0.923423), n = 5)
seq_range(c(0.0123, 0.923423), n = 5, pretty = T)

x1 <- rcauchy(100)
seq_range(x1, n = 5)
seq_range(x1, n = 5, trim = 0.10)
seq_range(x1, n = 5, trim = 0.25)
seq_range(x1, n = 5, trim = 0.50)

x2 <- c(0, 1)
seq_range(x2, n = 5)
seq_range(x2, n = 5, expand = 0.10)
seq_range(x2, n = 5, expand = 0.25)
seq_range(x2, n = 5, expand = 0.50)

ggplot(grid, aes(x1, x2)) +
  geom_tile(aes(fill = pred)) +
  facet_wrap(~ model)

ggplot(grid, aes(x1, pred, color = x2, group = x2)) +
  geom_line() +
  facet_wrap(~ model)

ggplot(grid, aes(x2, pred, color = x1, group = x1)) +
  geom_line() +
  facet_wrap(~ model)

## Transformations

df <- tribble(
  ~y, ~x,
   1,  1,
   2,  2,
   3,  3)

model_matrix(df, y ~ x^2 + x)
model_matrix(df, y ~ I(x^2) + x)
model_matrix(df, y ~ poly(x, 2))

library(splines)
model_matrix(df, y ~ ns(x, 2))

sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

ggplot(sim5, aes(x, y)) +
  geom_point()

mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) +
  geom_point() +
  geom_line(data = grid, color = "red") +
  facet_wrap(~ model)

## Exercises

#1
mod1 <- lm(y~x - 1, data = sim2)
mod2 <- lm(y~x, data = sim2)
mod1$coefficients
mod2$coefficients

grid1 <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod1)
grid2 <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
grid1
grid2
grid3 <- sim2 %>% 
  data_grid(x) %>% 
  gather_predictions(mod1, mod2)

ggplot(sim2, aes(x)) +
  geom_point(aes(y=y)) +
  geom_point(data=grid3, aes(y = pred), color = "red", size = 4) +
  facet_grid(~model)

#2
model_matrix(sim3, y ~ x1 + x2)
model_matrix(sim3, y ~ x1 * x2)
model_matrix(sim4, y ~ x1 + x2)
model_matrix(sim4, y ~ x1 * x2)

model_matrix(sim3, y ~ x2 * I(x1 ^ 2))

#3
#4
ggplot(sim4, aes(x1, y)) +
  geom_point() +
  geom_line(data = grid, color = "red") +
  facet_wrap(~ model)

sim4
grid <- sim4 %>% 
  data_grid(x1, x2) %>% 
  gather_residuals(mod1, mod2)

ggplot(sim4, aes(x1, x2)) +
  geom_smooth(data = grid, aes(color = model)) +
  facet_grid(~ model)

table(grid$model)

# Missing Values

df <- tribble(
  ~x, ~y,
  1, 2.2,
  2, NA,
  3, 3.5,
  4, 8.3, 
  NA, 10
)
mod <- lm(y ~ x, data = df)
mod <- lm(y ~ x, data = df, na.action = na.exclude)
nobs(mod)

# Other Model Families

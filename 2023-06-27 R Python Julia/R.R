# package management

# install.packages("microbenchmark")
library(microbenchmark)
library(readr)
library(dplyr)
library(ggplot2)
library(glue)
library(ggforce)

# read some data from csv
diamonds_csv <- read_csv("data/diamonds.csv") # read the diamonds data

# preview some data
diamonds_csv

# benchmark some data
mbm_read_csv <- microbenchmark(read_csv = diamonds_csv <- read_csv("data/diamonds.csv"), times=10L)
autoplot(mbm_read_csv)
round(mean(mbm_read_csv$time)/10e5) # time in ms

# Looking at parts of tabular data
diamonds_csv$color
diamonds_csv[2]
diamonds_csv[1,2]
diamonds_csv |>
  select(contains("co"))
diamonds_csv[2,]
diamonds_csv |>
  select(where(is.numeric))

# group and summarise
diamonds_csv |>
  group_by(cut) |>
  summarise(carat = mean(carat), price = mean(price))

# bench summarise
mbm_summarise <- microbenchmark(
  
  summarise = diamonds_csv |>
    group_by(cut) |>
    summarise(carat = mean(carat), price = mean(price))
  
)
autoplot(mbm_summarise)

# plot
diamonds_csv |>
  ggplot() +
  geom_point(aes(x = carat, y = price), color="steelblue4") +
  facet_grid(vars(color), vars(cut)) +
  theme_classic()

multiplot <- function(){
  print(diamonds_csv |>
    ggplot() +
    geom_point(aes(x = carat, y = price), color="steelblue4") +
    facet_grid(vars(color), vars(cut)) +
    theme_classic())
}

graph0 <- microbenchmark(multiplot(), times = 10L)
autoplot(graph0)

# something more maths intensive

a = runif(1e7)
autoplot(microbenchmark(sum(a)))

# monte carlo pi

runs <- 10000
mc_mbm <- microbenchmark(
  baseR = (sum(runif(runs,min=-0.5,max=0.5)^2 + runif(runs,min=-0.5,max=0.5)^2 <= 0.5^2)/runs)*4,
  tidyverse = {
    
    mc_pi <- tibble(
    x =  runif(runs,min=-0.5,max=0.5),
    y =  runif(runs,min=-0.5,max=0.5)
  ) |>
    mutate(in_circle= x^2 + y^2 <=0.25) 
  
  mc_pi_v <- 4*sum(mc_pi$in_circle)/runs

  }
)
print(mc_mbm)
autoplot(mc_mbm)

dd = tibble(x=0, y=0, r=0.5)

mc_pi |>
  ggplot() +
  geom_point(aes(x=x,y=y, colour = in_circle)) +
  theme(legend.position="none") +
  labs(title = glue("Pi ≈ {mc_pi_v} by Monte Carlo estimate"),
       subtitle = glue("From {runs} runs")) +
  geom_circle(data = dd, aes(x0=x,y0=y,r=r))

mc.pi <- (sum(runif(runs,min=-0.5,max=0.5)^2 + runif(runs,min=-0.5,max=0.5)^2 <= 0.5^2)/runs)*4




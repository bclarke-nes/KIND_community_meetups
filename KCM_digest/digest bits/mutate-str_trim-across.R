library(tidyverse)

# take some data with white space
data <- tibble(
  a = c("one ", " two ", "three  (double space)"),
  b = c("one ", " two ", "three  (double space)"),
  c = c("one ", " two ", "three  (double space)")
  )

# across all columns use mutate(across(everything()... ----

# to trim leading and trailing whitespace use str_trim
data %>%
  mutate(across(everything(), str_trim))

# to trim and remove internal doublespaces
data %>%
  mutate(across(everything(), str_squish))

# across a vector of columns use mutate(across(all_of(cols)... ----
cols <- c("a", "b")

data %>%
  mutate(across(all_of(cols), str_trim))

data %>%
  mutate(across(everything(), str_squish))

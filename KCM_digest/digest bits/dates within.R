df <- tibble(day = seq(ymd('2022-04-07'),ymd('2023-03-22'),by='weeks'))

start <- ymd("2022-06-01")
end <- ymd("2022-08-01")

df %>%
  filter(day %within% interval(start, end))

library(pacman)
p_load(tidyverse, gutenbergr, tidytext)

# get frankenstein
gutenberg_download(84)

# get the words in order
corpus <- gutenberg_download(84) %>%
  unnest_tokens(word, text) %>%
  select(!gutenberg_id) 

# find out which words occur and how often
freq <- corpus %>%
  count(word, sort=T) %>%
  mutate(f = n/sum(n))

# then find out which words follow which words
next_words <- corpus %>%
  select(word) %>%
  mutate(next_word = lead(word)) %>%
  group_by(word) %>%
  add_count(next_word, sort=T) %>%
  distinct()

# look at the distribution!
next_words %>%
  count(word, sort=T) %>%
  ggplot() +
  geom_histogram(aes(x=n))

# pick the best 10 following words for each word
best_words <- next_words %>%
  distinct() %>%
  slice_max(n, n = 10) %>%
  arrange(desc(n))

# calc p(next_word | word)
p_words <- best_words %>%
  group_by(word) %>%
  mutate(p = n/sum(n)) %>%
  arrange(p)

# get the next word from the current word using those probabilities
next_word <- function(word) {
  test <- p_words %>%
    filter(word == {{word}}) %>%
    pull(next_word, p)
  
  unname(sample(test, 1, replace=T, prob=names(test)) )
}

# pick a starting word based on frequency
sample(freq$word, 1, replace=T, prob=freq$f)

# put both first word and next word snippets into a function
n_words <- function(n) {
  out <- vector("character", n)
  out[1] <- sample(freq$word, 1, replace=T, prob=freq$f)
  for(i in 2:n) {
    out[i] <- next_word(out[i-1])
  }
  glue::glue_collapse(out, " ")
}

# generate some words
n_words(30)

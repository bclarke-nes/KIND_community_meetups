library(pacman)
p_load(tidyverse, gutenbergr, tidytext)

book_learnin <- function(gut_n, n_out, to_lower=TRUE, strip_punct=TRUE) {

  corpus <- gutenberg_download(gut_n) %>%
  unnest_tokens(word, text, to_lower=to_lower, strip_punct=strip_punct) %>%
  select(!gutenberg_id) 

  
?unnest_tokens
# find out which words occur and how often
freq <- corpus %>%
  count(word, sort=T) %>%
  mutate(f = n/sum(n))

# find out which words follow which words
p_words <- corpus %>%
  select(word) %>%
  mutate(next_word = lead(word)) %>%
  group_by(word) %>%
  add_count(next_word, sort=T) %>%
  distinct() %>%
  slice_max(n, n = 10) %>%
  mutate(p = n/sum(n)) 

group_keys <- unlist(group_keys(p_words))

list(freq = freq, p_words = p_words, group_keys = group_keys)

}

select_groups <- function(dd, gr, ...) dd[sort(unlist(attr(dd, "groups")$.rows[ gr ])), ]

next_word <- function(df, group_word) {
  test <- df$p_words %>% select_groups(which(df$group_keys %in% group_word)) %>%
    pull(next_word, p)
  
  unname(sample(test, 1, replace=T, prob=names(test)) )
}

n_words <- function(df, n) {
  out <- vector("character", n)
  out[1] <- sample(df$freq$word, 1, replace=T, prob=df$freq$f)
  for(i in 2:n) {
    out[i] <- next_word(df, out[i-1])
  }
  glue::glue_collapse(out, " ")
}

build_sentence <- function(df) {
  # approach shamelessly stolen from whoever chatGPT took it from
  # find the first word
  first <- df$freq %>%
    filter(str_detect(word, "^[A-Z]")) %>%
    pull(word, f)
  
  # start building a sentence around that first word
  sentence <- unname(sample(first, 1, replace=T, prob=names(first)))
  
  while (!any(grep("[.!?]$", sentence))) {
  # add next words based on sampling probability until end of sentence
    next_word <- df$p_words %>% 
      select_groups(which(df$group_keys %in% sentence[length(sentence)])) %>%
      pull(next_word, p) %>%
      sample(., 1, replace=T, prob=names(.))
    sentence <- c(sentence, next_word)
  }
  out <- paste(unname(sentence), collapse=" ")
  out <- str_replace_all(out, "\\s(?=[\\.,!?;])", "")
  out
}

frankenstein_2 <- book_learnin(84, to_lower=FALSE, strip_punct=FALSE) # frankenstein
n_words(frankenstein, 50)
build_sentence(frankenstein_2)

moby_dick <- book_learnin(2701) # moby_dick
n_words(moby_dick, 50)

moby_dick_2 <- book_learnin(2701, to_lower=FALSE, strip_punct=FALSE)
build_sentence(moby_dick_2)

middlemarch <- book_learnin(145) # middlemarch
n_words(middlemarch, 50)

austen <- book_learnin(31100) # Austen
n_words(austen, 50)

# big <- book_learnin(1:500)
# big <- read_rds("big.rds")
# n_words(big, 50)

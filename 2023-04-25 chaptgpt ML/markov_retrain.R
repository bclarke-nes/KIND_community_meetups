
book_learnin <- function(gut_n, n_out) {

  corpus <- gutenberg_download(gut_n) %>%
  unnest_tokens(word, text) %>%
  select(!gutenberg_id) 

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


frankenstein <- book_learnin(84) # frankenstein
n_words(frankenstein, 50)

moby_dick <- book_learnin(2701) # moby_dick
n_words(moby_dick, 50)

middlemarch <- book_learnin(145) # middlemarch
n_words(middlemarch, 50)

austen <- book_learnin(31100) # Austen
n_words(austen, 50)

big <- book_learnin(1:500)
n_words(big, 50)

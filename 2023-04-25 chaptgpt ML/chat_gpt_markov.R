# prompt ----

# write me some r code to retrieve gutenberg texts, build a markov chain model from those texts, and return full sentences based on those training texts

# unedited response ----

# Load required packages
library(tidytext)
library(dplyr)
library(stringr)

# Define function to retrieve Gutenberg texts
get_gutenberg_text <- function(book_id) {
  url <- paste0("http://www.gutenberg.org/cache/epub/", book_id, "/pg", book_id, ".txt")
  text <- readLines(url, warn = FALSE)
  text <- text[-(1:45)] # Remove Gutenberg header and footer
  text <- paste(text, collapse = "\n")
  return(text)
}

# Get the text of two books from Gutenberg
book1 <- get_gutenberg_text(11) # Alice's Adventures in Wonderland
book2 <- get_gutenberg_text(74) # The Adventures of Tom Sawyer

# Combine the texts into one corpus
corpus <- paste(book1, book2, sep = "\n\n")

# Create a tidy data frame from the corpus
tidy_corpus <- tibble(text = corpus) %>%
  unnest_tokens(word, text, token = "words")

# Create a Markov chain model
markov_model <- tidy_corpus %>%
  pairwise_count(word, sort = TRUE) %>%
  group_by(word1) %>%
  mutate(prob = n/sum(n)) %>%
  ungroup()

# Define function to generate sentences from the model
generate_sentence <- function(markov_model, starting_word = NULL) {
  if (is.null(starting_word)) {
    # Choose a random starting word
    starting_word <- markov_model %>%
      filter(str_detect(word1, "^[A-Z]")) %>%
      sample_n(1) %>%
      pull(word1)
  }
  sentence <- starting_word
  while (!str_detect(sentence, "[.?!]$")) {
    next_word <- markov_model %>%
      filter(word1 == sentence[length(sentence)]) %>%
      sample_n(1, weight = prob) %>%
      pull(word2)
    sentence <- c(sentence, next_word)
  }
  sentence <- paste(sentence, collapse = " ")
  return(sentence)
}

# Generate a sentence using the Markov chain model
sentence <- generate_sentence(markov_model)
print(sentence)

# edits needed ----

# pairwise_count namespace and syntax
# chatGPT hallucinated a widyr function - not in namespace, different syntax. This was likely partly based on the use of the widyr package e.g. https://www.tidytextmining.com/ngrams.html#counting-and-correlating-pairs-of-words-with-the-widyr-package - needs column to work out groups
# workaround using lead() added

# `sample_n()` now obsolete, and again syntax problems. Replaced with slice_sample():

# unnest_tokens strips punctuation and changes all to lower-case by default. Yet the sentences functions later depend on both upper-case and punctuation to work.
??diffr
# edited version ----

# Load required packages
library(tidytext)
library(dplyr)
library(stringr)

# Define function to retrieve Gutenberg texts
get_gutenberg_text <- function(book_id) {
  url <- paste0("http://www.gutenberg.org/cache/epub/", book_id, "/pg", book_id, ".txt")
  text <- readLines(url, warn = FALSE)
  text <- text[-(1:(1+ which(grepl("START OF THE PROJECT GUTENBERG EBOOK", text[1:100]))))] # Remove Gutenberg header
  text <- text[1:((which(grepl("END OF THE PROJECT GUTENBERG EBOOK", text)))-1)] #and footer
  text <- paste(text, collapse = "\n")
  return(text)
}

# Get the text of two books from Gutenberg
book1 <- get_gutenberg_text(11) # Alice's Adventures in Wonderland
book2 <- get_gutenberg_text(84) # Frankenstein again

# Combine the texts into one corpus
corpus <- paste(book1, book2, sep = "\n\n")

# Create a tidy data frame from the corpus
tidy_corpus <- tibble(text = corpus) %>%
  unnest_tokens(word, text, token = "words", to_lower=F, strip_punct=F)

# Create a Markov chain model
markov_model <- tidy_corpus %>%
  rename(word1 = word) %>%
  mutate(word2 = lead(word1))   %>%
  add_count(word1) %>%
  group_by(word1) %>%
  mutate(prob = n/sum(n)) %>%
  ungroup()

# Define function to generate sentences from the model
generate_sentence <- function(markov_model, starting_word = NULL) {
  if (is.null(starting_word)) {
    # Choose a random starting word
    starting_word <- markov_model %>%
      filter(str_detect(word1, "^[A-Z]")) %>%
      sample_n(1) %>%
      pull(word1)
  }
  sentence <- starting_word
  while (all(!str_detect(sentence, "[.?!]$"))) {
    next_word <- markov_model %>%
      filter(word1 == sentence[length(sentence)]) %>%
      sample_n(1, weight = prob) %>%
      pull(word2)
    sentence <- c(sentence, next_word)
  }
  sentence <- paste(sentence, collapse = " ")
  return(sentence)
}

# Generate a sentence using the Markov chain model
sentence <- generate_sentence(markov_model)
print(sentence)

# visualise diffs
diffr::diffr("chat_gpt_markov_pre.R", "chat_gpt_markov_post.R")

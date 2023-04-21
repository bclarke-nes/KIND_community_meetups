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
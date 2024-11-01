library(dplyr)
library(ggplot2)
library(janeaustenr)
library(stringr)
library(tidytext)

# Load the books, where each row is a line of text.
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
      ignore_case = TRUE
    )))
  ) %>%
  ungroup()

# Split the text into individual words.
tidy_books <- original_books %>%
  unnest_tokens(word, text)

# Remove stop words.
data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

# Compute word counts.
word_counts <- tidy_books %>%
  count(word, sort = TRUE)

print(word_counts)

# Plot the most common words.
p <- word_counts %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

print(p)

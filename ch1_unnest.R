library(dplyr)
library(tidytext)

text_orig <- c(
  "Because I could not stop for Death -",
  "He kindly stopped for me -",
  "The Carriage held but just Ourselves -",
  "And Immortality."
)
text_df <- tibble(text = text_orig, line = 1:4)

# Split the text into individual words, dropping punctuation.
un_nested <- text_df %>%
  unnest_tokens(word, text)

print(un_nested)

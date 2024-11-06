library(janeaustenr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
      ignore_case = TRUE
    )))
  ) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

joy_count <- tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

print(joy_count)

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, sentiment, index = linenumber %/% 80) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

print(jane_austen_sentiment)

p <- ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

print(p)

ggsave("sentiment_inner_join.svg", plot = p, device = "svg")

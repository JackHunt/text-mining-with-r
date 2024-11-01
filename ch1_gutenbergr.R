library(dplyr)
library(ggplot2)
library(gutenbergr) # i.e. project gutenberg, the online book repo
library(janeaustenr)
library(scales)
library(stringr)
library(tidyr)
library(tidytext)

data(stop_words)

# Load books from tidytext.
jane_austen_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
      ignore_case = TRUE
    )))
  ) %>%
  ungroup()

# Download the text of three books by H.G. Wells and 5 by the Bronte sisters.
hgwells <- gutenberg_download(c(35, 36, 5230))
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

# Split the text into individual words and remove stop words.
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_austen <- jane_austen_books %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Compute word counts.
hgwells_word_counts <- tidy_hgwells %>%
  count(word, sort = TRUE)

bronte_word_counts <- tidy_bronte %>%
  count(word, sort = TRUE)

print(hgwells_word_counts)
print(bronte_word_counts)

# Compute frequencies.
frequency <- bind_rows(
  mutate(tidy_bronte, author = "Brontë Sisters"),
  mutate(tidy_hgwells, author = "H.G. Wells"),
  mutate(tidy_austen, author = "Jane Austen")
) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, "Brontë Sisters":"H.G. Wells")

print(frequency)

# Plot
p <- ggplot(frequency, aes(
  x = proportion,
  y = `Jane Austen`,
  color = abs(`Jane Austen` - proportion)
)) +
  geom_abline(color = "gray", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position = "none") +
  labs(y = "Jane Austen", x = NULL)

ggsave("austen.svg", plot = p, device = "svg")

# Compute the correlations vs austen.
corr_austen_bronte <- cor.test(
  data = frequency[frequency$author == "Brontë Sisters", ],
  ~ proportion + `Jane Austen`,
)
print(corr_austen_bronte)

corr_austen_hgwells <- cor.test(
  data = frequency[frequency$author == "H.G. Wells", ],
  ~ proportion + `Jane Austen`,
)
print(corr_austen_hgwells)

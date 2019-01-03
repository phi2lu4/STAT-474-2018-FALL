# Loading the necessary packages
library(tidyverse)
library(tidytext)
library(janeaustenr)

# Tidy the text in Jane Austen's novels
tidied_books <- austen_books() %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, "[a-z']+"))

#### Explore the term frequency in Jane Austen's novels ####

# Compute the term frequency
book_words <- tidied_books %>%
  count(book, word, sort = TRUE) %>%
  group_by(book) %>%
  mutate(total = sum(n),
         proportion = n/total)

# Visualize the term frequency
ggplot(book_words) +
  geom_histogram(aes(x = proportion, fill = book), show.legend = FALSE) +
  xlim(0, 0.0009) +
  facet_wrap(~ book, ncol = 2, scales = "free_y")

# Checkpoint: One of the variant of term frequency is the raw count on the logarithmical scale. Create the histogram based on this variant.

#### Verify the Zipf's law ####

# Adding the rank
freq_by_rank <- book_words %>%
  group_by(book) %>%
  arrange(desc(n)) %>%
  mutate(rank = row_number())

freq_by_rank

# The product of rank and frequency
freq_by_rank %>% mutate(constant = rank * n) %>%
  arrange(book, desc(n)) 

# Visualize the rank and the term frequency
freq_by_rank %>% 
  ggplot(aes(rank, proportion, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

# Adding the reference line
freq_by_rank %>% filter(rank < 500, rank > 10) %>%
  lm(log10(proportion) ~ log10(rank), data = .) %>%
  summary()
  
freq_by_rank %>% 
  ggplot(aes(rank, proportion, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  geom_abline(intercept = -0.62, slope = -1.113, linetype = 2) +
  scale_x_log10() +
  scale_y_log10()

# Fit is not great. 
# Checkpoint: Any insights?

#### TF-IDF ####

# Function bind_tf_idf() is very useful
book_words <- book_words %>% bind_tf_idf(word, book, n)
book_words

# Task: Can we re-do what the function bind_tf_idf has done?

# Explore the TF-IDF
book_words %>% arrange(desc(tf_idf))

# Visualization
book_words %>% group_by(book) %>%
  arrange(desc(tf_idf)) %>%
  top_n(15) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf)) +
  geom_col(aes(fill = book), show.legend = FALSE) + 
  labs(x = NULL, y ="tf_idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

# Task: Let's look at some classic physics texts from Project Gutenberg abd see what terms are important in these works, as measured by tf-idf. Let’s download Discourse on Floating Bodies by Galileo Galilei, Treatise on Light by Christiaan Huygens, Experiments with Alternate Currents of High Potential and High Frequency by Nikola Tesla, and Relativity: The Special and General Theory by Albert Einstein.

library(gutenbergr)
physics <- gutenberg_download(c(37729, 14725, 13476, 5001), 
                              meta_fields = "author")

#### n-gram ####

# Create the document vector with bi-grams

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

# Obtaining the count as the usual way
austen_bigrams %>% count(bigram, sort = TRUE)

# The most common bigrams include stop words!!!
# Let remove bigram with stop words
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  anti_join(stop_words, by = c("word1" = "word")) %>%
  anti_join(stop_words, by = c("word2" = "word"))

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united 
bigrams_united %>% count(bigram, sort = TRUE)

# Task: What are the most influence bigram among Jane Austen's novel, measured by tf-idf?
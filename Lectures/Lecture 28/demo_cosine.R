# Loading the necessary packages
library(tidyverse)
library(tidytext)
library(janeaustenr)

# Tidy the text in Jane Austen's novels
tidied_books <- austen_books() %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  filter(!is.na(word))

# Add tf-idf
book_words <- tidied_books %>% 
  count(book, word, sort = TRUE) %>%
  bind_tf_idf(word, book, n)

book_words

# Convert the collection into term-document model format
# Work well with other tools in the tidytext package
book_dtm <- book_words %>% cast_dtm(book, word, tf_idf)
book_dtm

# Extract info from document term model format
book_dtm %>% Terms() %>% head()
book_dtm %>% Docs()

# Convert the collection into term-document matrix format
# Here, we use the sparse matrix format instead of the regular
book_matrix <- book_words %>% cast_sparse(book, word, tf_idf)
book_matrix[, 1:10]

# Checkpoint: All of the entries in the first 10 columns are zero. Is it right?

# Compute the cosine similarity score
SandS <- book_matrix[1,]
PandP <- book_matrix[2,]

# Cosine similarity between Sense & Sensibility and Pride & Prejudice
sum(SandS * PandP)/sqrt(sum(SandS*SandS)*sum(PandP*PandP))


# Checkpoint: Is Emma novel more similar to Sense & Sensibility than Pride & Prejudice?

# Working with package "widyr"
# install.package("widyr")
library(widyr)

# Compute pairwise cosine similarity measure
book_words %>% 
  pairwise_similarity(book, word, tf_idf, sort = TRUE, upper = FALSE)

# The package widyr also provides tools to deal with other pairwise computation
# Example: Counting the pair of words that tends to come together in a small segment. Here, we only consider the "Pride & Prejudice" novel with each segment consisting of 10 lines

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  filter(!is.na(word)) %>%
  anti_join(stop_words)

austen_section_words

# Co-occurence of words
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

# We can also perform pairwise correlation. 
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors
# Tidy text
# This example lesson is available here: https://www.tidytextmining.com/tidytext.html#the-unnest_tokens-function

# First, install the packages you'll need for the lesson

install.packages("dplyr")
install.packages("tibble")
install.packages("tidytext")
install.packages("janeaustenr")
install.packages("stringr")
install.packages("ggplot2")


# Then, load the packages 

library(dplyr)
library(tibble)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)

# The unnest tokens function

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

# To view the data, run the variable below

text

# This is a typical character vector that we might want to analyze. 
# In order to turn it into a tidy text dataset, we first need to put it into a data frame.

library(dplyr)
text_df <- tibble(line = 1:4, text = text)

# To view the data, run the variable below

text_df

# Now we'll break the text into individual tokens

text_df %>%
  unnest_tokens(word, text)

# Tidying the works of Jane Austen

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()

# View the data by calling the variable 

original_books

# Now let's make original_books data viewable as one-token-per-row

tidy_books <- original_books %>%
  unnest_tokens(word, text)

# The tidytext dataset includes a list of stopwords. We can use dplyr to "anti-join" these stop words, removing them from the dataset

data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

# Now that we've removed the stopwords, let's see the word count

tidy_books %>%
  count(word, sort = TRUE) 

# Now we'll use the ggplot package to visualize this data

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

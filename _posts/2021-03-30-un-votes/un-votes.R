## Setup and data preparation

# Loading libraries
library(forcats)
library(tidytext)
library(tidyverse)
library(tidytuesdayR)

# Loading data
tt <- tt_load("2021-03-23")

# Extracting US votes on each UN resolution
US_votes <- tt$unvotes %>%
  filter(country_code == "US") %>%
  select(rcid, vote)
colnames(US_votes) <- c("rcid", "US_vote")

# Combining US votes and resolution issues with the "roll_calls" data set for
# important votes
important_votes <- tt$roll_calls %>%
  filter(rcid %in% US_votes$rcid &
           importantvote == 1)
important_votes <- left_join(important_votes, tt$unvotes,
                                   by = "rcid")
important_votes <- left_join(important_votes, US_votes,
                                   by = "rcid")
important_votes <- important_votes %>%
  filter(country != "United States") %>%
  mutate(agree_with_US = vote == US_vote)
important_votes$agree_with_US <- as.factor(important_votes$agree_with_US)
levels(important_votes$agree_with_US)
levels(important_votes$agree_with_US) <- c("Votes in disagreement with US",
                                           "Votes in agreement with US")
important_votes <- left_join(important_votes,
                                   tt$issues, by = "rcid")
important_votes

# Counting the number of votes for each country that coincide with the US vote
# on each resolution
agree_with_US_count <- important_votes %>%
  group_by(agree_with_US) %>%
  select(country, agree_with_US) %>%
  count(agree_with_US, country, sort = TRUE)
agree_with_US_count$country <- as.factor(agree_with_US_count$country)

# Counting the number of votes that are the same/different as the US vote on
# UN resolutions in different categories
votes_by_issue <- important_votes %>%
  filter(!is.na(issue)) %>%
  select(date, agree_with_US, issue) %>%
  group_by(date, issue) %>%
  count(agree_with_US)

# Calculating term frequency-inverse document frequency (tf-idf) for the
# descriptions of UN resolutions across different issues
issue_descr_words <- important_votes %>% 
  filter(!is.na(issue)) %>% 
  select(issue, descr) %>% 
  unnest_tokens(word, descr) %>%
  count(issue, word, sort = TRUE)
issue_descr_words$issue <- as.factor(issue_descr_words$issue)
# Removing "stop words" from resolution descriptions (e.g. "the", "and", "of")
issue_descr_words <- issue_descr_words %>%
  anti_join(stop_words, by = "word")
total_words <- issue_descr_words %>%
  group_by(issue) %>%
  summarise(total = sum(n))
issue_descr_words <- left_join(issue_descr_words, total_words, by = "issue")
issue_descr_words <- issue_descr_words %>%
  bind_tf_idf(word, issue, n)

## Plotting the countries that have agreed or disagreed with the most US votes

# Plotting countries that have agreed/disagreed with the most US votes
agree_with_US_count %>%
  slice_head(n = 15) %>%
  ggplot(aes(n, fct_reorder(country, n), fill = agree_with_US)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~agree_with_US, ncol = 2, scales = "free") +
  theme_classic() + 
  labs(x = "Votes on important UN resolutions", y = "Countries",
       title = "Votes on important United Nations (UN) resolutions",
       subtitle = "Resolution importance classified by the United States (US) State Department")

## Plotting votes on UN resolutions on different issues

# Plotting votes on different categories of UN resolutions over time
votes_by_issue %>%
  ggplot(aes(x = date, y = n, group = agree_with_US, colour = agree_with_US)) +
  geom_line(size = 0.8) +
  facet_wrap(~issue, scales = "free") +
  theme_classic() +
  theme(legend.position = "top") +
  theme(legend.text = element_text(size = 12)) +
  labs(x = "Time", y = "UN resolution votes",
       title = "Votes on UN resolutions over time by issue",
       subtitle = "Agreement/disagreement with US vote used to gauge consensus on each issue",
       colour = NULL)

## Plotting keywords in UN resolution descriptions

# Plotting keywords in UN resolution descriptions
issue_descr_words %>%
  group_by("issue") %>%
  slice_max(tf_idf, n = 35) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = issue)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~issue, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  theme_classic() +
  labs(x = "Term frequency-inverse document freqeuncy (tf-idf)",
       title = "Keywords in UN resolution descriptions across different categories",
       subtitle = "tf-idf applied to resolution descriptions on different issues")

## Plotting the percentage of UN votes on each issue in data set

# Plotting percentage of votes in each UN resolution category
important_votes %>% 
  filter(!is.na(issue)) %>% 
  select(issue) %>% 
  group_by(issue) %>% 
  count(issue) %>%  
  ungroup() %>%
  mutate(perc = round(n/sum(n) * 100)) %>%
  ggplot(aes(x = "", y = perc, fill = issue)) + 
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(label = paste(perc, "%", sep = "")), position = position_stack(vjust=0.5)) +
  labs(title = "Percentage of UN resolution votes on different issues",
       subtitle = "Data set covers 6,202 UN resolutions from 1946 to 2019",
       fill = "Issue")

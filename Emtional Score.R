install.packages("tidyverse")
install.packages("syuzhet")
install.packages("openxlsx")
install.packages("ggplot2")


library(tidyverse)
library(syuzhet)
library(openxlsx)
library(ggplot2)

data_df <- read.xlsx("shit-lockdown.xlsx", sheet = 1)
words_df <- data_df %>%
  select(word = `WORDS`, freq = FREQ) %>%
  filter(!is.na(word) & word != "RE-USE WORDS" & word != "TOTAL" & !is.na(freq)) %>%
  mutate(word = as.character(word))

words_df$sentiment_score <- get_sentiment(words_df$word, method = "syuzhet")

words_df <- words_df %>%
  mutate(
    sentiment_category = case_when(
      sentiment_score > 0.1 ~ "positive",
      sentiment_score < -0.1 ~ "negative",
      TRUE ~ "neutral"
    )
  )

p1 <- ggplot(words_df, aes(x = "", fill = sentiment_category)) +
  geom_bar(width = 1) +
  geom_text(
    aes(label = paste0(round(..count.. / sum(..count..) * 100, 1), "%")),
    stat = "count",
    position = position_stack(vjust = 0.5),
    size = 5,
    color = "white",
    fontface = "bold"
  ) +
  coord_polar("y") +
  labs(title = "LOCKDOWN Adjective emotion distribution", fill = "sentiment classification") +
  scale_fill_manual(values = c("positive" = "#e4a46b", "negative" = "#1f73c3", "neutral" = "#81a5aa")) +
  theme_void()

top_20 <- words_df %>% arrange(desc(freq)) %>% head(20)
p2 <- ggplot(top_20, aes(x = reorder(word, sentiment_score), y = sentiment_score, fill = sentiment_category)) +
  geom_col() +
  coord_flip() +
  labs(title = "Emotional scores of the top 20 high-frequency words", x = "words", y = "Emotional score") +
  scale_fill_manual(values = c("positive" = "#e4a46b", "negative" = "#1f73c3", "neutral" = "#81a5aa")) +
  theme_minimal()

p3 <- ggplot(words_df, aes(x = sentiment_score, fill = sentiment_category)) +
  geom_histogram(binwidth = 0.5, alpha = 0.8) +
  labs(title = "Distribution of emotional scores", x = "Emotional score", y = "Vocabulary quantity") +
  scale_fill_manual(values = c("positive" = "#e4a46b", "negative" = "#1f73c3", "neutral" = "#81a5aa")) +
  theme_minimal()

print(p1)
print(p2)
print(p3)

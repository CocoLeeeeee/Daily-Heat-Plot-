install.packages("tidyverse")
install.packages("lubridate")
install.packages("viridis")

library(tidyverse)
library(lubridate)
library(viridis)

word_usage_data <- data.frame(
  date = seq(as.Date("2020-03-01"), as.Date("2022-12-01"), by = "month"),
  count = c(27, 384, 574, 579, 363, 324, 168, 124, 131, 86, 106, 72, 
            73, 91, 80, 75, 78, 70, 60, 94, 69, 43, 40, 19, 20, 33, 
            44, 49, 33, 47, 9, 16, 9, 15),
  countries = c(7, 16, 18, 17, 18, 18, 16, 14, 15, 16, 14, 11, 13, 
                12, 9, 11, 9, 10, 11, 8, 10, 8, 8, 8, 8, 9, 11, 11, 
                10, 9, 5, 7, 4, 4)
)

word_usage_data <- word_usage_data %>%
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE),
    month_num = month(date)
  )

cat("数据摘要:\n")
summary(word_usage_data)

cat("\n数据前6行:\n")
head(word_usage_data)

write.csv(word_usage_data, "word_usage_complete_data.csv", row.names = FALSE)

cat("\n数据已保存为 'word_usage_complete_data.csv'\n")


ggplot(word_usage_data, aes(x = month, y = factor(year), fill = count)) +
  geom_tile(color = "white", size = 0.8) +
  scale_fill_viridis_c(name = "frequency", option = "F", direction = -1) +
  labs(
    title = "Word Usage frequency Heat Map",
    subtitle = "2020.03 - 2022.12",
    x = "Month",
    y = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

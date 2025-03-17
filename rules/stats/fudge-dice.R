library(tidyverse)

no_outcomes <- 81
fudge_outcome <- tribble(
  ~outcome, ~probability,
  -4, 1/no_outcomes,
  -3, 4/no_outcomes,
  -2, 10/no_outcomes,
  -1, 16/no_outcomes,
  0, 19/no_outcomes,
  +1, 16/no_outcomes,
  +2, 10/no_outcomes,
  +3, 4/no_outcomes,
  +4, 1/no_outcomes,
)

fudge_outcome <- fudge_outcome %>%
  arrange(outcome) %>%
  mutate(cumulative_prob = cumsum(probability)) %>%
  mutate(cumulative_complement = 1 - cumulative_prob + probability)  # P(X > x)

plt <- ggplot(fudge_outcome, aes(x = outcome, y = probability)) +
  geom_bar(stat = "identity", fill = "#DBE4E4", color = "black") +
  scale_x_continuous(breaks = seq(-4, 4, by = 1)) +  # Only integer ticks on x-axis
  scale_y_continuous(breaks = NULL) +  # Removes y-axis values
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Removes all grid lines
    axis.title = element_blank(),  # Removes axis labels
    axis.text.y = element_blank()   # Removes y-axis numbers
  )

ggsave("4dF.pdf", plot = plt, width = 7, height = 4, units = "cm")

plt <- ggplot(fudge_outcome, aes(x = outcome, y = cumulative_complement)) +
  geom_step(color = "#58180D", size = 1.2) +  # Step function for CCDF
  scale_x_continuous(breaks = seq(-4, 4, by = 1)) +
  labs(title = "Cumulative Probability (P(X > x))",
       x = "Threshold (x)",
       y = "P(X > x)") +
  theme_minimal()

plt


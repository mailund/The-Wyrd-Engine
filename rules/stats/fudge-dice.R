library(tidyverse)

no_outcomes <- 81
fudge_outcome <- tribble(
  ~outcome, ~probability,
  -5, 0,
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
  mutate(cumulative_complement = 1 - cumulative_prob)  # P(X > x)

fudge_outcome <- fudge_outcome %>%
  mutate(success_percent = round(cumulative_complement * 100, 0))  # Convert to percentage

plt <- ggplot(fudge_outcome[fudge_outcome$outcome > -5,], aes(x = outcome, y = probability)) +
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
  geom_bar(stat = "identity", fill = "#DBE4E4", color = "black") +  # Histogram bars
  scale_x_continuous(breaks = seq(-5, 4, by = 1)) +  # Keep integer ticks
  theme_minimal() + 
  coord_flip() +  # Flip x and y axes
  theme(
    panel.grid = element_blank(),  # Removes all grid lines
    axis.title = element_blank()   # Removes axis labels
  ) 

ggsave("4dF-success.pdf", plot = plt, width = 7, height = 5, units = "cm")
plt
fudge_outcome

difficulty_levels <- data.frame(
  outcome = seq(-2, 4, by = 1),
  label = c("Trivial", "Simple", "Basic", "Challenging", "Difficult", "Formidable", "Arduous")
)



# Create the plot with difficulty levels at the top and success percentages on bars
plt <- ggplot(fudge_outcome, aes(x = outcome, y = cumulative_complement)) +
  geom_bar(stat = "identity", fill = "#DBE4E4", color = "black") +  # Histogram bars
  coord_flip() +
  scale_x_continuous(breaks = seq(-5, 4, by = 1)) +  # Keep integer ticks
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Removes all grid lines
    axis.title = element_blank()   # Removes axis labels
  ) +
  # Add difficulty labels at the top
  geom_text(data = difficulty_levels, aes(x = outcome, y = max(fudge_outcome$cumulative_complement) * 1.1, label = label),
            color = "#58180D", fontface = "bold", size = 2, vjust = 0, hjust = 0.8) +
  # Add success percentages inside bars
  geom_text(aes(label = paste0(success_percent, "%")), 
            color = "#C9AD6A", fontface = "bold", size = 2, hjust = -0.5)  # Slightly above the bars

# Display the plot
plt

ggsave("4dF-DR.pdf", plot = plt, width = 7.5, height = 5, units = "cm")



# Define difficulty labels and corresponding difficulty thresholds
difficulty_levels <- data.frame(
  difficulty = seq(-2, 4, by = 1),
  label = c("Trivial", "Simple", "Basic", "Challenging", "Difficult", "Formidable", "Arduous")
)

# Define skill levels
skill_levels <- data.frame(
  skill_level = c("Untrained\n(Skill 0)", "Novice\n(Skill +1)", "Skilled\n(Skill +2)", "Expert\n(Skill +3)"),
  shift = c(0, 1, 2, 3)  # Shift difficulty thresholds for each skill level
)

# Expand data: Calculate success rate for each difficulty at each skill level
difficulty_shifts <- expand.grid(skill_level = skill_levels$skill_level, difficulty = difficulty_levels$difficulty) %>%
  left_join(skill_levels, by = "skill_level") %>%
  mutate(adjusted_difficulty = difficulty - shift) %>%
  left_join(fudge_outcome, by = c("adjusted_difficulty" = "outcome")) %>%
  mutate(success_percent = round(cumulative_complement * 100, 0))

difficulty_shifts <- difficulty_shifts %>%
  mutate(success_percent = ifelse(is.na(success_percent), 0, success_percent))

plt <- ggplot(difficulty_shifts, aes(x = skill_level, y = success_percent, group = as.factor(difficulty), color = as.factor(difficulty))) +
  geom_line(size = .5) +  # Line plot for difficulty success rate
  geom_point(size = 2) +  # Add points at each skill level
  scale_color_manual(values = c("#58180D", "#C9AD6A", "#58180D", "#C9AD6A", "#58180D", "#C9AD6A", "#58180D")) +  # Custom colors
  # Add difficulty labels at the right margin
  geom_text(data = difficulty_shifts %>% filter(skill_level == "Expert"), 
            aes(label = difficulty, x = "Expert", y = success_percent), 
            hjust = -0.2, color = "#58180D", fontface = "bold", size = 2, inherit.aes = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),  # Remove y-axis text (but keep x-axis labels)
    legend.position = "none",  # Remove legend
    axis.text.x = element_text(size = 5, face = "bold")  # Ensure skill level labels appear
  ) +
  scale_x_discrete(limits = skill_levels$skill_level, labels = skill_levels$skill_level) +  # Force skill level labels
  scale_y_continuous(breaks = NULL)  # Remove y-axis numbers


plt
ggsave("4dF-DR-per-skill-level.pdf", plot = plt, width = 8, height = 4, units = "cm")

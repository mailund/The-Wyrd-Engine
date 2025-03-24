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
  +5, 0,
  +6, 0,
  +7, 0,
)

fudge_outcome <- fudge_outcome %>%
  arrange(outcome) %>%
  mutate(cumulative_prob = cumsum(probability)) %>%
  mutate(cumulative_complement = 1 - cumulative_prob)  # P(X > x)

fudge_outcome <- fudge_outcome %>%
  arrange(outcome) %>%
  mutate(cumulative_prob = cumsum(probability)) %>%
  mutate(cumulative_complement = 1 - lag(cumulative_prob, default = 0))  # P(X ≥ x)

fudge_outcome <- fudge_outcome %>%
  mutate(success_percent = round(cumulative_complement * 100, 1))  # Convert to percentage

plt <- ggplot(fudge_outcome %>% filter(outcome > -5) %>% filter(outcome < 5), aes(x = outcome, y = probability)) +
  geom_bar(stat = "identity", fill = "#DBE4E4", color = "black") +
  scale_x_continuous(breaks = seq(-4, 4, by = 1)) +  # Only integer ticks on x-axis
  scale_y_continuous(breaks = NULL) +  # Removes y-axis values
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Removes all grid lines
    axis.title = element_blank(),  # Removes axis labels
    axis.text.y = element_blank()   # Removes y-axis numbers
  )
plt
ggsave("4dF.pdf", plot = plt, width = 7, height = 4, units = "cm")

(plt <- ggplot(fudge_outcome %>% filter(outcome > -5) %>% filter(outcome < 5), aes(x = outcome, y = cumulative_complement)) +
    geom_bar(stat = "identity", fill = "#DBE4E4", color = "black") +  # Histogram bars
    scale_x_continuous(breaks = 1+seq(-5, 4, by = 1)) +  # Keep integer ticks
    theme_minimal() +
    theme(
      panel.grid = element_blank(),  # Removes all grid lines
      axis.title = element_blank(),   # Removes axis labels
      axis.text.y = element_blank()   # Removes y-axis numbers
    ) +
    # Add success percentages inside bars
    geom_text(aes(label = paste0(success_percent, "%")), 
              color = "#58180D", fontface = "bold", size = 2, vjust = -0.3)  # Slightly above the bars
)
ggsave("4dF-success.pdf", plot = plt, width = 8, height = 5, units = "cm")
plt
fudge_outcome

difficulty_levels <- data.frame(
  outcome = seq(-4, 5, by = 1),
  label = c("Trivial", "Simple", "Easy", "Basic", "Challenging", "Difficult", "Formidable", "Arduous", "Extreme", "Impossible")
)



# Define the updated difficulty labels with numerical values
difficulty_labels <- c(
  "Trivial (-3)",
  "Simple (-2)",
  "Easy (-1)",
  "Basic (0)",
  "Challenging (+1)",
  "Difficult (+2)",
  "Formidable (+3)",
  "Arduous (+4)",
  "Extreme (+5)",
  "Legendary (+6)",
  "Impossible (+7)"
)

# Assign these labels to the corresponding x-axis positions
difficulty_positions <- seq(-3, 7, by = 1)

# Update the plot with labeled x-axis and angled labels
plt <- ggplot(fudge_outcome, aes(x = outcome, y = cumulative_complement)) +
  geom_bar(stat = "identity", fill = "#DBE4E4", color = "black") +  # Histogram bars
  scale_x_continuous(breaks = difficulty_positions, labels = difficulty_labels) +  # Replace numerical ticks with difficulty names
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Removes all grid lines
    axis.title = element_blank(),   # Removes axis labels
    axis.text.y = element_blank(),  # Removes y-axis numbers
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, face = "bold")  # Angled x-axis labels
  ) +
  # Add success percentages inside bars
  geom_text(aes(label = paste0(success_percent, "%")), 
            color = "#58180D", fontface = "bold", size = 2, vjust = -0.3)  # Slightly above the bars

plt <- ggplot(fudge_outcome %>% filter(outcome >= -3) %>% filter(outcome <= 5), aes(x = outcome, y = cumulative_complement)) +
  geom_bar(stat = "identity", fill = "#DBE4E4", color = "black") +  # Histogram bars
  scale_x_continuous(breaks = difficulty_positions, labels = difficulty_labels) +  # Replace numerical ticks with difficulty names
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Removes all grid lines
    axis.title = element_blank(),   # Removes axis labels
    axis.text.y = element_blank(),  # Removes y-axis numbers
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5, face = "bold", color = "#58180D")  # Difficulty labels in 58180D
  ) +
  geom_text(aes(label = paste0(success_percent, "%")), 
            color = "#58180D", fontface = "bold", size = 2, vjust = -0.3) 

# Display the plot
plt

ggsave("4dF-DR.pdf", plot = plt, width = 8, height = 5, units = "cm")

# Define skill levels in the correct order
skill_levels <- data.frame(
  skill_level = factor(c("Expert (+3)", "Skilled (+2)", "Novice (+1)", "Untrained (0)"), 
                       levels = c("Expert (+3)", "Skilled (+2)", "Novice (+1)", "Untrained (0)")),  # Force correct order
  shift = c(3, 2, 1, 0)  # Shift represents how difficulty moves up
)

# Expand data: Generate all combinations of skill levels and outcomes
fudge_outcome_shifted <- expand.grid(skill_level = skill_levels$skill_level, outcome = fudge_outcome$outcome) %>%
  left_join(skill_levels, by = "skill_level") %>%
  mutate(reference_outcome = outcome - shift) %>%  # Shift outcome reference
  left_join(fudge_outcome, by = c("reference_outcome" = "outcome")) %>%  # Match cumulative prob from lower difficulty
  mutate(success_percent = round(cumulative_complement * 100, 0))  # Convert probability to percentage

# Fix missing values: Only set to 100% if it was already at max probability
max_probability <- max(fudge_outcome$cumulative_complement, na.rm = TRUE)
fudge_outcome_shifted <- fudge_outcome_shifted %>%
  mutate(success_percent = ifelse(is.na(success_percent) & reference_outcome < min(fudge_outcome$outcome),
                                  100, success_percent))  # Only fill 100% if no valid reference exists

# Define difficulty labels and their x-axis positions
difficulty_positions <- seq(-3, 7, by = 1)

# Create the faceted plot with shifted difficulties
plt <- ggplot(fudge_outcome_shifted %>% filter(outcome >= -3), aes(x = outcome, y = success_percent)) +
  geom_bar(stat = "identity", fill = "#DBE4E4", color = "black") +  # Histogram bars
  scale_x_continuous(breaks = difficulty_positions, labels = difficulty_labels) +  # Adjust x-axis
  facet_grid(skill_level ~ ., scales = "free_y", switch = "y") +  # Facet labels on left
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Removes all grid lines
    axis.title = element_blank(),   # Removes axis labels
    axis.text.y = element_blank(),  # Removes y-axis numbers
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5, face = "bold", color = "#58180D"),  # Difficulty labels
    strip.text.y.left = element_text(size = 7, face = "bold", color = "#58180D")  # Left-aligned facet labels in 58180D
  ) +
  # Add success percentages inside bars
  geom_text(aes(label = ifelse(success_percent >= 10, paste0(success_percent, "%"), "")), 
            color = "#9C2B1B", fontface = "bold", size = 2, vjust = 1.5) +
  geom_text(aes(label = ifelse(success_percent < 10, paste0(success_percent, "%"), "")), 
            color = "#9C2B1B", fontface = "bold", size = 2, vjust = -1.5) 

# Display the plot
plt

# Save the plot
ggsave("shifted-DR.pdf", plot = plt, width = 8, height = 10, units = "cm")


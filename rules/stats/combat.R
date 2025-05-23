library(tidyverse)

# Base Fudge outcome distribution
no_outcomes <- 81
base_fudge <- tribble(
  ~outcome, ~probability,
  -4, 1/no_outcomes,
  -3, 4/no_outcomes,
  -2, 10/no_outcomes,
  -1, 16/no_outcomes,
  0, 19/no_outcomes,
  1, 16/no_outcomes,
  2, 10/no_outcomes,
  3, 4/no_outcomes,
  4, 1/no_outcomes
)

# Range of attacker skill levels relative to defender
skill_diffs <- -4:6

# Function to compute damage distribution
compute_damage_distribution <- function(skill_diff) {
  attacker_dist <- base_fudge %>%
    mutate(outcome = outcome + skill_diff) %>%
    rename(attacker = outcome, p_attacker = probability)
  
  defender_dist <- base_fudge %>%
    rename(defender = outcome, p_defender = probability)
  
  attacker_dist %>%
    crossing(defender_dist) %>%
    mutate(
      damage = pmax(0, attacker - defender),
      prob = p_attacker * p_defender
    ) %>%
    group_by(skill_diff = skill_diff, damage) %>%
    summarise(probability = sum(prob), .groups = "drop")
}

# Compute all distributions
all_distributions <- map_dfr(skill_diffs, compute_damage_distribution)

# Collapse damage ≥ 6 into "6+" and convert to percentage
collapsed_distributions <- all_distributions %>%
  mutate(
    damage = if_else(damage >= 7, "7+", as.character(damage)),
    probability = round(probability * 100, 1)
  ) %>%
  group_by(skill_diff, damage) %>%
  summarise(probability = sum(probability), .groups = "drop")

# Pivot to wide format
wide_table <- collapsed_distributions %>%
  pivot_wider(
    names_from = damage,
    values_from = probability,
    values_fill = 0
  ) %>%
  arrange(skill_diff)

# Print nicely
print(wide_table, n = Inf)



# Create DnD-style LaTeX table from your wide_table
format_dnd_table <- function(data) {
  # Ensure correct column order
  damage_columns <- c("0", "1", "2", "3", "4", "5", "6", "7+")
  data <- data %>%
    select(skill_diff, all_of(damage_columns))
  
  # Format header
  header <- paste0("    \\textbf{Attacker - Defender} & ", paste0("\\textbf{", damage_columns, "}", collapse = " & "), " \\\\")
  
  # Format rows
  body <- apply(data, 1, function(row) {
    skill_label <- sprintf("%+d", as.integer(row["skill_diff"]))
    values <- row[-1]
    formatted <- sapply(values, function(p) {
      if (p == 0) {
        "  -"
      } else {
        sprintf("%5.1f\\%%", as.numeric(p))
      }
    })
    paste0("    ", skill_label, " & ", paste(formatted, collapse = " & "), " \\\\")
  })
  
  # Combine into full LaTeX table
  table_code <- c(
    "\\begin{DndTable}[header=Damage probability by relative skill level]{lrrrrrrr}",
    header,
    "    \\hline",
    body,
    "\\end{DndTable}"
  )
  
  cat(paste(table_code, collapse = "\n"))
}

format_dnd_table(wide_table)




collapsed_distributions_grouped <- collapsed_distributions %>%
  mutate(
    damage_group = case_when(
      damage %in% c("1", "2") ~ "1–2",
      damage %in% c("3", "4") ~ "3–4",
      damage %in% c("5", "6") ~ "5–6",
      TRUE ~ as.character(damage)
    )
  ) %>%
  group_by(skill_diff, damage_group) %>%
  summarise(probability = sum(probability), .groups = "drop") %>%
  mutate(
    damage_group = factor(
      damage_group,
      levels = c("0", "1–2", "3–4", "5–6", "7+")
    ),
    skill_diff = factor(skill_diff, levels = sort(unique(as.numeric(as.character(skill_diff)))))
  )
collapsed_distributions_grouped <- collapsed_distributions_grouped %>%
  mutate(
    skill_diff_label = if_else(as.numeric(as.character(skill_diff)) >= 0,
                               paste0("+", as.character(skill_diff)),
                               as.character(skill_diff))
  )

# Get unique skill diffs and generate proper labels
skill_order <- sort(unique(as.numeric(as.character(collapsed_distributions_grouped$skill_diff))))
skill_labels <- ifelse(skill_order >= 0, paste0("+", skill_order), as.character(skill_order))

# Reassign labels with correct ordering
collapsed_distributions_grouped <- collapsed_distributions_grouped %>%
  mutate(
    skill_diff_label = factor(
      if_else(as.numeric(as.character(skill_diff)) >= 0,
              paste0("+", skill_diff),
              as.character(skill_diff)),
      levels = skill_labels
    )
  )


signed_labeller <- function(variable, value) {
  ifelse(value > 0, paste0("+", value), as.character(value))
}

ggplot(collapsed_distributions_grouped, aes(x = damage_group, y = probability, fill = I("#58180D"))) +
  geom_col(width = 0.7, show.legend = FALSE) +
  facet_grid(skill_diff_label ~ .) +
  labs(
    title = "Damage Distribution by Attack - Defense Difference",
    x = "Damage Dealt",
    y = "Probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

ggsave("damage_distribution.png", width = 6, height = 10)


prob_any_damage <- collapsed_distributions_grouped %>%
  group_by(skill_diff) %>%
  summarise(prob_damage = sum(probability[damage_group != "0"]))
prob_any_damage

damage_values <- c("0" = 0, "1–2" = 1.5, "3–4" = 3.5, "5–6" = 5.5, "7+" = 7)

expected_damage <- collapsed_distributions_grouped %>%
  mutate(damage_value = damage_values[as.character(damage_group)]) %>%
  group_by(skill_diff) %>%
  summarise(expected_damage = sum(probability * damage_value / 100))
expected_damage




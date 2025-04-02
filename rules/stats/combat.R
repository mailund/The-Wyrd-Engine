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
skill_diffs <- -3:3

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
    damage = if_else(damage >= 6, "6+", as.character(damage)),
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
  damage_columns <- c("0", "1", "2", "3", "4", "5", "6+")
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
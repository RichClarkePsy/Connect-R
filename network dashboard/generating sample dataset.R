library(tidyverse)

set.seed(20251228)

# ---- Project catalogue ----
projects_catalogue <- tribble(
  ~project_id, ~project_name, ~project_description,
  1, "project 1", "Mapping health research expertise and collaboration to strengthen interdisciplinary health systems",
  2, "project 2", "Building reproducible data pipelines for analysing health and wellbeing survey data",
  3, "project 3", "Examining how trust, risk perception, and information environments shape health decisions",
  4, "project 4", "Replicating influential health psychology findings using open and transparent research practices",
  5, "project 5", "Developing and validating psychometric tools to assess health, wellbeing, and psychological distress"
)

# ---- Helpers ----
make_keywords <- function(n_terms = sample(2:5, 1)) {
  pool <- c(
    "psychological stress", "health behaviour", "statistics",
    "public health", "environmental health", "replication",
    "survey methods", "data science", "wellbeing", "risk perception"
  )
  paste(sample(pool, n_terms, replace = FALSE), collapse = "; ")
}

make_projects_string <- function(catalogue,
                                 n_projects = sample(1:1, 1)) {
  
  chosen <- catalogue %>% slice_sample(n = n_projects)
  
  roles <- rep("member", n_projects)
  roles[sample(seq_len(n_projects), 1)] <- "creator"
  
  paste(
    paste0(
      chosen$project_id, " | ",
      chosen$project_name, " | ",
      chosen$project_description, " | role=", roles
    ),
    collapse = " ;; "
  )
}

# ---- Generate 50 users ----
n_users <- 50

users <- tibble(
  user_id      = 1:n_users,
  username     = paste0("user", user_id),
  display_name = paste("Person", user_id),
  organisation = sample(c("UoG", "NHS"), n_users, TRUE),
  role_title   = sample(c("Lecturer", "Researcher", "Analyst", "Clinician", "PhD Student"), n_users, TRUE),
  region       = sample(c("glos", "south west", "midlands", "london", "wales"), n_users, TRUE),
  is_admin     = rbinom(n_users, 1, 0.1),
  keywords     = map_chr(seq_len(n_users), ~ make_keywords()),
  projects     = map_chr(seq_len(n_users), ~ make_projects_string(projects_catalogue))
)


# ---- Force user2 to match your example exactly ----
users <- users %>%
  mutate(
    display_name  = if_else(user_id == 2, "Richard Clarke", display_name),
    organisation  = if_else(user_id == 2, "UoG", organisation),
    role_title    = if_else(user_id == 2, "Lecturer", role_title),
    region        = if_else(user_id == 2, "glos", region),
    is_admin      = if_else(user_id == 2, 1L, is_admin),
    keywords      = if_else(user_id == 2, "psychological stress response; recurrence; statistics", keywords),
    projects      = if_else(
      user_id == 2,
      "5 | project 5 | Create a lightweight psychometrics toolkit for scale scoring, reliability checks, and reporting | role=member",
      projects
    )
  )

# Optional: ensure everyone has >=1 project (if you want to practice joins/unnesting without empties)
# users <- users %>%
#   mutate(projects = if_else(projects == "", map_chr(user_id, \(.) make_projects_string(projects_catalogue, allow_none = FALSE)), projects))

# ---- Inspect + export ----
users %>% print(n = 10)

write_csv(users, "sample_users.csv")



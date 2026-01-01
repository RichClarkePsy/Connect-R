# app.R ---------------------------------------------------------------
# Collaboration network from users table
# Edges = users sharing at least one project

library(shiny)
library(tidyverse)
library(visNetwork)
library(DT)

# --------------------------------------------------------------------
# 0) Expect: users data already exists
# --------------------------------------------------------------------
users <- read_csv("sample_users.csv")

stopifnot(exists("users"))
stopifnot(all(c(
  "user_id","username","display_name","organisation","role_title",
  "region","is_admin","keywords","projects"
) %in% names(users)))

# --------------------------------------------------------------------
# 1) WRANGLE: Nodes + edges from project overlap
# --------------------------------------------------------------------

# ---- Nodes (people) -------------------------------------------------
nodes <- users %>%
  mutate(
    name          = if_else(str_squish(display_name) == "", username, display_name),
    institution   = organisation,
    dept          = role_title,
    keywords      = coalesce(keywords, ""),
    label         = name,
    group         = if_else(str_squish(institution) == "", "Unknown", institution),
    keywords_text = tolower(keywords)
  ) %>%
  transmute(
    id = user_id,
    name,
    institution = group,
    dept,
    region,
    is_admin,
    keywords,
    label,
    group = institution,
    keywords_text
  )

# ---- Project memberships (long) -------------------------------------
# projects string format:
# "2 | project 2 | description ... | role=creator ;; 3 | project 3 | ... | role=member"
memberships <- users %>%
  select(user_id, projects) %>%
  mutate(projects = coalesce(projects, "")) %>%
  filter(str_squish(projects) != "") %>%
  separate_rows(projects, sep = "\\s*;;\\s*") %>%
  mutate(projects = str_squish(projects)) %>%
  separate(
    col = projects,
    into = c("project_id", "project_name", "project_description", "role_part"),
    sep = "\\s*\\|\\s*",
    fill = "right",
    extra = "merge"
  ) %>%
  mutate(
    project_id = suppressWarnings(as.integer(str_squish(project_id))),
    project_name = str_squish(project_name),
    project_description = str_squish(project_description),
    role = str_remove(str_squish(role_part), "^role=")
  ) %>%
  select(user_id, project_id, project_name, project_description, role) %>%
  filter(!is.na(project_id))

# ---- Edges: user-user if share a project ----------------------------
# Build all pairs within each project, then collapse to unique edges with weights
edges <- memberships %>%
  mutate(
    project_name = str_squish(project_name),
    project_description = str_squish(coalesce(project_description, ""))
  ) %>%
  distinct(user_id, project_id, project_name, project_description) %>%
  inner_join(
    memberships %>%
      distinct(user_id, project_id),
    by = "project_id",
    suffix = c("_a", "_b")
  ) %>%
  filter(user_id_a < user_id_b) %>%
  group_by(user_id_a, user_id_b) %>%
  summarise(
    weight = n_distinct(project_id),
    shared_details = paste0(
      sort(unique(paste0(project_name, " — ", project_description))),
      collapse = "<br>"
    ),
    .groups = "drop"
  ) %>%
  transmute(
    from  = user_id_a,
    to    = user_id_b,
    value = weight,
    width = pmin(1 + weight, 6),
    color = "rgba(120,120,120,0.25)",   # keep grey if you like
    title = paste0(
      "<div style='max-width:260px; white-space:normal;'>",
      "<b>Shared projects</b><br>",
      shared_details,
      "</div>"
    )
  )



# (Optional) If you want to show isolates too, keep nodes as-is.
# visNetwork will happily show nodes with no edges.

# --------------------------------------------------------------------
# 2) Shiny UI
# --------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Collaboration Network (Shared Projects)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "institution",
        "Organisation filter:",
        choices = c("All", sort(unique(nodes$institution))),
        selected = "All"
      ),
      textInput(
        "keyword",
        "Keyword filter (e.g. 'wellbeing', 'vaccination'):",
        value = ""
      ),
      checkboxInput(
        "hide_isolates",
        "Hide people with no shared projects (in current filter)",
        value = FALSE
      ),
      helpText(
        "Edges are created when two users share at least one project. ",
        "Edge thickness reflects the number of shared projects."
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Network",
          visNetworkOutput("network", height = "550px"),
          br(),
          h4("Selected person"),
          uiOutput("profile")
        ),
        tabPanel(
          "Table",
          DTOutput("people_table")
        )
      )
    )
  )
)

# --------------------------------------------------------------------
# 3) Shiny server
# --------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Filter nodes based on organisation + keyword
  filtered_nodes <- reactive({
    dat <- nodes
    
    if (input$institution != "All") {
      dat <- dat %>% filter(institution == input$institution)
    }
    
    kw <- tolower(str_squish(input$keyword))
    if (nzchar(kw)) {
      dat <- dat %>% filter(str_detect(keywords_text, fixed(kw)))
    }
    
    dat
  })
  
  # Filter edges to only include links between visible nodes
  filtered_edges <- reactive({
    ids <- filtered_nodes()$id
    edges %>% filter(from %in% ids, to %in% ids)
  })
  
  # Optionally hide isolates within the current filter
  filtered_nodes_no_isolates <- reactive({
    dat <- filtered_nodes()
    if (!isTRUE(input$hide_isolates)) return(dat)
    
    ed <- filtered_edges()
    keep_ids <- sort(unique(c(ed$from, ed$to)))
    dat %>% filter(id %in% keep_ids)
  })
  
  filtered_edges_no_isolates <- reactive({
    ids <- filtered_nodes_no_isolates()$id
    edges %>% filter(from %in% ids, to %in% ids)
  })
  
  output$network <- renderVisNetwork({
    req(nrow(filtered_nodes_no_isolates()) > 0)
    
    nodes_vis <- filtered_nodes_no_isolates() %>%
      transmute(
        id,
        label,
        group,
        title = paste0(
          "<b>", name, "</b><br>",
          "Organisation: ", institution, "<br>",
          "Role: ", dept, "<br>",
          "Region: ", region, "<br>",
          "Admin: ", is_admin, "<br>",
          "Keywords: ", keywords
        )
      )
    
    edges_vis <- filtered_edges_no_isolates()
    
    visNetwork(nodes_vis, edges_vis) %>%
      visGroups(groupname = "UoG", color = "#279952") %>%
      visGroups(groupname = "Hospital", color = "#1764a2") %>%
      visEdges(smooth = FALSE) %>%
      visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = TRUE
      ) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -150
        )
      )
  })
  
  output$people_table <- renderDT({
    filtered_nodes_no_isolates() %>%
      select(name, institution, dept, region, is_admin, keywords)
  })
  
  observeEvent(input$network_selectedNodes, {
    selected_id <- input$network_selectedNodes[1]
    
    person <- nodes %>% filter(id == selected_id)
    if (nrow(person) == 0) {
      output$profile <- renderUI(NULL)
      return()
    }
    
    # List this person's projects nicely
    person_projects <- memberships %>%
      filter(user_id == selected_id) %>%
      distinct(project_id, project_name, role) %>%
      arrange(project_id)
    
    proj_ui <- if (nrow(person_projects) == 0) {
      em("No projects listed.")
    } else {
      tags$ul(
        lapply(seq_len(nrow(person_projects)), function(i) {
          tags$li(paste0(
            person_projects$project_id[i], " — ",
            person_projects$project_name[i],
            " (", person_projects$role[i], ")"
          ))
        })
      )
    }
    
    output$profile <- renderUI({
      tagList(
        strong(person$name),
        br(),
        paste("Organisation:", person$institution),
        br(),
        paste("Role:", person$dept),
        br(),
        paste("Region:", person$region),
        br(),
        paste("Admin:", person$is_admin),
        br(),
        paste("Keywords:", person$keywords),
        br(), br(),
        strong("Projects"),
        proj_ui
      )
    })
  }, ignoreNULL = TRUE)
}

shinyApp(ui, server)


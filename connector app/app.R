library(shiny)
library(bslib)
library(sodium)
library(visNetwork)
library(DT)
library(dplyr)
library(stringr)
library(tidyr)

source("R/db.R")

# Safer than relying on %||% being available everywhere
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

organisation_choices <- c("NHS", "UoG", "Both NHS & UoG", "Other")

login_card_ui <- function() {
  card(
    class = "auth-card",
    card_header("Sign in"),
    textInput("login_username", "Username"),
    passwordInput("login_password", "Password"),
    div(
      style = "display:flex; gap:.5rem;",
      actionButton("login_submit", "Login", class = "btn-primary"),
      actionButton("login_clear", "Clear", class = "btn-outline-secondary")
    ),
    tags$hr(),
    uiOutput("login_feedback")
  )
}

register_card_ui <- function() {
  card(
    class = "auth-card",
    card_header("Create an account"),
    p("This creates a new user record and stores a hashed password."),
    textInput("reg_username", "Username", placeholder = "name"),
    passwordInput("reg_password", "Password"),
    passwordInput("reg_password2", "Confirm password"),
    div(
      style = "display:flex; gap:.5rem;",
      actionButton("reg_submit", "Create account", class = "btn-primary"),
      actionButton("reg_clear", "Clear", class = "btn-outline-secondary")
    ),
    tags$hr(),
    uiOutput("reg_feedback")
  )
}

ui <- page_navbar(
  title = "RCCS-Connect (Step 5)",
  theme = bs_theme(
    version = 5,
    primary = "#2e7d32",
    secondary = "#1b5e20",
    success = "#43a047",
    info = "#4caf50"
  ),
  
  nav_panel(
    "Home",
    tags$head(
      tags$style(HTML("
        body {
          background: #f4faf4;
        }
        .navbar, .card-header {
          background: linear-gradient(90deg, #1b5e20, #2e7d32);
          color: #f1fff1;
        }
        .navbar-nav .nav-link, .navbar-brand, .navbar-text {
          color: #f1fff1 !important;
        }
        .home-layout .sidebar {
          min-width: 520px;
        }
        @media (max-width: 991px) {
          .home-layout .sidebar {
            min-width: 100%;
          }
        }
        .home-layout { gap: 1rem; }
        .home-sidebar {
          display: flex;
          flex-direction: column;
          gap: 1rem;
          max-height: none;
          overflow: visible;
          padding-right: .5rem;
        }
        .home-sidebar .auth-card {
          min-height: 360px;
          width: 100%;
        }
        .home-main {
          display: flex;
          flex-direction: column;
          gap: 1rem;
          min-height: calc(100vh - 180px);
          overflow-y: auto;
        }
        .home-network-card { min-height: 540px; }
        .projects-carousel-controls {
          display: flex;
          justify-content: center;
          gap: .75rem;
          margin-top: .5rem;
        }
        .projects-carousel-controls .btn {
          min-width: 140px;
        }
        .network-explorer-card {
          min-height: 900px;
        }
        #network_view {
          min-height: 700px !important;
        }
      "))
    ),
    layout_sidebar(
      sidebar = div(
        class = "home-sidebar",
        login_card_ui(),
        register_card_ui()
      ),
      fillable = TRUE,
      class = "home-layout",
      div(
        class = "home-main",
        card(
          card_header("Welcome"),
          p("RCCS-Connect helps researchers discover collaborators, organise project teams, and see where expertise is concentrated across the network."),
          p("Create a profile, register or sign in, then join or launch projects so the collaboration map stays up to date. Use the explorer to filter by organisation or keyword and find people working in similar spaces.")
        ),
        uiOutput("projects_carousel_ui"),
        uiOutput("network_home_ui")
      )
    )
  ),
  nav_panel("Network Explorer", uiOutput("network_explorer_ui")),
  
  nav_panel("My Profile", uiOutput("profile_ui")),
  nav_panel("My Projects", uiOutput("projects_ui")),
  nav_panel("Admin", uiOutput("admin_ui")),
  
  nav_spacer(),
  nav_item(uiOutput("auth_controls"))
)

build_network_data <- function(users_df) {
  if (is.null(users_df) || nrow(users_df) == 0) {
    empty <- tibble::tibble()
    return(list(nodes = empty, edges = empty, memberships = empty))
  }
  
  normalize_organisation <- function(value) {
    value <- str_squish(value %||% "")
    if (!nzchar(value)) return("Other")
    if (value %in% organisation_choices) return(value)
    "Other"
  }
  
  nodes <- users_df %>%
    mutate(
      name          = if_else(str_squish(display_name %||% "") == "", username, display_name %||% ""),
      institution   = vapply(organisation, normalize_organisation, character(1)),
      dept          = role_title,
      keywords      = coalesce(keywords, ""),
      label         = name,
      group         = institution,
      keywords_text = tolower(str_squish(str_replace_all(keywords, "\\s*;;\\s*", ", ")))
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
      group,
      keywords_text
    )
  
  memberships <- users_df %>%
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
  
  edges <- tibble::tibble()
  if (nrow(memberships) > 0) {
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
        color = "rgba(120,120,120,0.25)",
        title = paste0(
          "<div style='max-width:260px; white-space:normal;'>",
          "<b>Shared projects</b><br>",
          shared_details,
          "</div>"
        )
      )
  }
  
  list(nodes = nodes, edges = edges, memberships = memberships)
}

server <- function(input, output, session) {
  
  # ---- DB ----
  con <- db_connect()
  onStop(function() db_disconnect(con))
  db_init(con)
  
  keyword_choices <- read.csv(file.path("data", "keywords.csv"), stringsAsFactors = FALSE)$keyword
  keyword_choices <- sort(unique(trimws(keyword_choices)))
  keyword_choices <- keyword_choices[nzchar(keyword_choices)]
  
  # ---- Helper: locked page ----
  locked_page <- function(title = "Locked", body = "Please log in to view this page.") {
    layout_column_wrap(width = 1, card(card_header(title), p(body)))
  }
  
  # ---- Session user state (REAL) ----
  user <- reactiveValues(
    is_logged_in = FALSE,
    user_id = NULL,
    username = NULL,
    is_admin = FALSE
  )
  
  projects_refresh <- reactiveVal(0)
  
  all_projects_data <- reactive({
    projects_refresh()
    db_list_all_projects(con)
  })
  
  network_export_data <- reactive({
    projects_refresh()
    db_export_users_projects_profiles(con)
  })
  
  network_data <- reactive({
    build_network_data(network_export_data())
  })
  
  parse_keyword_tokens <- function(raw_input) {
    if (is.null(raw_input)) return(character(0))
    
    if (is.character(raw_input) && length(raw_input) > 1) {
      tokens <- raw_input
    } else {
      raw_text <- tolower(raw_input %||% "")
      tokens <- str_split(raw_text, "\\s*[;,]\\s*")[[1]]
    }
    
    tokens <- tolower(tokens)
    tokens <- unique(str_trim(tokens))
    tokens[nzchar(tokens)]
  }

  extract_keyword_options <- function(keyword_values) {
    if (is.null(keyword_values) || length(keyword_values) == 0) return(character(0))
    raw <- tolower(keyword_values)
    tokens <- unlist(str_split(raw, "\\s*(?:;;|,|;)\\s*"))
    tokens <- unique(str_trim(tokens))
    tokens[nzchar(tokens)]
  }
  
  # Navbar controls
  output$auth_controls <- renderUI({
    if (!user$is_logged_in) {
      tags$span("Not signed in")
    } else {
      div(
        style = "display:flex; gap: .5rem; align-items:center;",
        tags$span(sprintf("Signed in as: %s", user$username)),
        actionButton("logout_btn", "Logout", class = "btn-outline-secondary")
      )
    }
  })
  
  observeEvent(input$logout_btn, {
    user$is_logged_in <- FALSE
    user$user_id <- NULL
    user$username <- NULL
    user$is_admin <- FALSE
    
    updateTextInput(session, "profile_display_name", value = "")
    updateSelectInput(session, "profile_organisation", selected = "Other")
    updateTextInput(session, "profile_role_title", value = "")
    updateTextInput(session, "profile_region", value = "")
  })
  
  # =========================
  # Step 3: LOGIN
  # =========================
  login_state <- reactiveValues(type = NULL, msg = NULL)
  
  output$login_feedback <- renderUI({
    if (is.null(login_state$type)) return(NULL)
    if (identical(login_state$type, "success")) {
      div(class = "alert alert-success", login_state$msg)
    } else {
      div(class = "alert alert-danger", login_state$msg)
    }
  })
  
  observeEvent(input$login_clear, {
    updateTextInput(session, "login_username", value = "")
    updateTextInput(session, "login_password", value = "")
    login_state$type <- NULL
    login_state$msg <- NULL
  })
  
  observeEvent(input$login_submit, {
    username <- trimws(input$login_username %||% "")
    password <- input$login_password %||% ""
    
    if (username == "") {
      login_state$type <- "error"
      login_state$msg <- "Please enter your username."
      return()
    }
    
    row <- db_get_user_by_username(con, username)
    if (nrow(row) == 0) {
      login_state$type <- "error"
      login_state$msg <- "Username not found."
      return()
    }
    
    ok <- FALSE
    try({
      ok <- sodium::password_verify(row$password_hash[[1]], password)
    }, silent = TRUE)
    
    if (!isTRUE(ok)) {
      login_state$type <- "error"
      login_state$msg <- "Incorrect password."
      return()
    }
    
    user$is_logged_in <- TRUE
    user$user_id <- row$user_id[[1]]
    user$username <- row$username[[1]]
    user$is_admin <- as.integer(row$is_admin[[1]]) == 1
    
    login_state$type <- "success"
    login_state$msg <- "Logged in successfully."
    
    updateTextInput(session, "login_password", value = "")
  })
  
  # =========================
  # Step 2: REGISTRATION
  # =========================
  reg_state <- reactiveValues(type = NULL, msg = NULL)
  
  validate_registration <- function(username, pw1, pw2) {
    username <- trimws(username %||% "")
    if (username == "") return("Please enter a username.")
    if (!identical(pw1, pw2)) return("Passwords do not match.")
    if (db_user_exists(con, username)) return("That username is already taken.")
    NULL
  }
  
  output$reg_feedback <- renderUI({
    if (is.null(reg_state$type)) return(NULL)
    if (identical(reg_state$type, "success")) {
      div(class = "alert alert-success", reg_state$msg)
    } else {
      div(class = "alert alert-danger", reg_state$msg)
    }
  })
  
  observeEvent(input$reg_clear, {
    updateTextInput(session, "reg_username", value = "")
    updateTextInput(session, "reg_password", value = "")
    updateTextInput(session, "reg_password2", value = "")
    reg_state$type <- NULL
    reg_state$msg <- NULL
  })
  
  observeEvent(input$reg_submit, {
    username <- trimws(input$reg_username %||% "")
    pw1 <- input$reg_password %||% ""
    pw2 <- input$reg_password2 %||% ""
    
    err <- validate_registration(username, pw1, pw2)
    if (!is.null(err)) {
      reg_state$type <- "error"
      reg_state$msg <- err
      return()
    }
    
    hash <- sodium::password_store(pw1)
    
    tryCatch({
      new_id <- db_create_user(con, username = username, password_hash = hash, is_admin = 0)
      reg_state$type <- "success"
      reg_state$msg <- paste0("Account created successfully. user_id = ", new_id)
      
      updateTextInput(session, "reg_password", value = "")
      updateTextInput(session, "reg_password2", value = "")
      projects_refresh(projects_refresh() + 1)
    }, error = function(e) {
      reg_state$type <- "error"
      reg_state$msg <- paste("Registration failed:", conditionMessage(e))
    })
  })
  
  # ---- profile ----
  output$profile_ui <- renderUI({
    if (!user$is_logged_in) {
      locked_page("My Profile (Locked)", "Please log in to view your profile.")
    } else {
      
      session$onFlushed(function() {
        uid <- isolate(user$user_id)
        if (!is.null(uid)) load_profile_into_inputs(uid)
      }, once = TRUE)
      
      layout_column_wrap(
        width = 1/2,
        
        card(
          card_header("My Profile"),
          textInput("profile_display_name", "Display name"),
          selectInput("profile_organisation", "Organisation", choices = organisation_choices, selected = "Other"),
          textInput("profile_role_title", "Role / job title"),
          textInput("profile_region", "Region"),
          
          div(
            style = "display:flex; gap:.5rem; flex-wrap:wrap;",
            actionButton("profile_save", "Save profile", class = "btn-primary")
          ),
          tags$hr(),
          uiOutput("profile_feedback")
        ),
        
        card(
          card_header("Keywords"),
          p(class = "text-muted", "Select as many as you like from the controlled list."),
          selectizeInput(
            "profile_keywords",
            "Keywords",
            choices = keyword_choices,
            selected = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "Start typing to search keywords...",
              maxOptions = 2000
            )
          ),
          tags$hr(),
          uiOutput("keywords_feedback")
        ),
        
        card(
          card_header("Account actions"),
          p("Delete your profile and login details (this also removes projects you created)."),
          actionButton("delete_account_btn", "Delete my account", class = "btn-danger"),
          tags$hr(),
          uiOutput("account_delete_feedback")
        )
      )
    }
  })
  
  profile_state <- reactiveValues(type = NULL, msg = NULL)
  output$keywords_feedback <- renderUI(NULL)
  
  output$profile_feedback <- renderUI({
    if (is.null(profile_state$type)) return(NULL)
    
    if (identical(profile_state$type, "success")) {
      div(class = "alert alert-success", profile_state$msg)
    } else {
      div(class = "alert alert-danger", profile_state$msg)
    }
  })
  
  load_profile_into_inputs <- function(user_id) {
    row <- db_get_user_profile(con, user_id)
    
    if (nrow(row) == 0) {
      profile_state$type <- "error"
      profile_state$msg <- "Could not find your user record in the database."
      return()
    }
    
    updateTextInput(session, "profile_display_name", value = row$display_name[[1]] %||% "")
    org_value <- row$organisation[[1]] %||% ""
    org_value <- if (org_value %in% organisation_choices) org_value else "Other"
    updateSelectInput(session, "profile_organisation", selected = org_value)
    updateTextInput(session, "profile_role_title", value = row$role_title[[1]] %||% "")
    updateTextInput(session, "profile_region", value = row$region[[1]] %||% "")
    
    profile_state$type <- NULL
    profile_state$msg <- NULL
    
    selected_kws <- db_get_user_keywords(con, user_id)
    
    updateSelectizeInput(
      session,
      "profile_keywords",
      choices = keyword_choices,
      selected = selected_kws,
      server = TRUE
    )
  }
  
  observeEvent(input$profile_save, {
    req(user$is_logged_in)
    
    display_name <- trimws(input$profile_display_name %||% "")
    organisation <- trimws(input$profile_organisation %||% "")
    role_title   <- trimws(input$profile_role_title %||% "")
    region       <- trimws(input$profile_region %||% "")
    keywords     <- input$profile_keywords %||% character(0)
    
    db_set_user_keywords(con, user_id = user$user_id, keywords = keywords)
    
    tryCatch({
      db_update_user_profile(
        con,
        user_id = user$user_id,
        display_name = display_name,
        organisation = organisation,
        role_title = role_title,
        region = region
      )
      
      profile_state$type <- "success"
      profile_state$msg <- "Profile saved."
      projects_refresh(projects_refresh() + 1)
    }, error = function(e) {
      profile_state$type <- "error"
      profile_state$msg <- paste("Save failed:", conditionMessage(e))
    })
  })
  
  account_delete_state <- reactiveValues(type = NULL, msg = NULL)
  
  output$account_delete_feedback <- renderUI({
    if (is.null(account_delete_state$type)) return(NULL)
    class_name <- if (identical(account_delete_state$type, "success")) "alert alert-success" else "alert alert-danger"
    div(class = class_name, account_delete_state$msg)
  })
  
  observeEvent(input$delete_account_btn, {
    req(user$is_logged_in)
    account_delete_state$type <- NULL
    account_delete_state$msg <- NULL
    
    showModal(modalDialog(
      title = "Delete account",
      p("This will delete your profile, keywords, login details, and any projects you created."),
      p("Type your username to confirm."),
      textInput("delete_confirm_username", "Username", value = ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("delete_account_confirm", "Delete account", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$delete_account_confirm, {
    req(user$is_logged_in)
    username_check <- trimws(input$delete_confirm_username %||% "")
    
    if (!identical(tolower(username_check), tolower(user$username))) {
      account_delete_state$type <- "error"
      account_delete_state$msg <- "Username does not match."
      return()
    }
    
    tryCatch({
      db_delete_user(con, user$user_id)
      account_delete_state$type <- "success"
      account_delete_state$msg <- "Account deleted."
      removeModal()
      
      user$is_logged_in <- FALSE
      user$user_id <- NULL
      user$username <- NULL
      user$is_admin <- FALSE
      
      updateTextInput(session, "login_username", value = "")
      updateTextInput(session, "login_password", value = "")
      updateTextInput(session, "profile_display_name", value = "")
      updateSelectInput(session, "profile_organisation", selected = "Other")
      updateTextInput(session, "profile_role_title", value = "")
      updateTextInput(session, "profile_region", value = "")
      updateSelectizeInput(session, "profile_keywords", selected = character(0))
      projects_refresh(projects_refresh() + 1)
    }, error = function(e) {
      account_delete_state$type <- "error"
      account_delete_state$msg <- paste("Delete failed:", conditionMessage(e))
    })
  })
  
  output$projects_ui <- renderUI({
    if (!user$is_logged_in) {
      locked_page("My Projects (Locked)", "Please log in to manage your projects.")
    } else {
      
      projects_refresh()
      
      all_projects <- all_projects_data()
      user_projects <- db_list_projects_for_user(con, user$user_id)
      
      layout_column_wrap(
        width = 1/2,
        
        card(
          card_header("Create a new project (adds it to the global list)"),
          textInput("proj_new_title", "Project title"),
          textAreaInput("proj_new_desc", "Project description", rows = 3),
          div(
            style = "display:flex; gap:.5rem; flex-wrap:wrap;",
            uiOutput("proj_create_or_save_btn")
          ),
          tags$hr(),
          uiOutput("proj_create_feedback")
        ),
        
        card(
          card_header("Your joined projects"),
          uiOutput("user_projects_cards")
        ),
        
        card(
          card_header("Join an existing project"),
          selectizeInput(
            "proj_join_picker",
            "Join an existing project",
            choices = c(`(choose a project)` = "", setNames(all_projects$project_id, all_projects$title)),
            options = list(
              placeholder = "Search or select a project...",
              maxOptions = 1000
            )
          ),
          div(
            style = "display:flex; gap:.5rem; flex-wrap:wrap;",
            actionButton("proj_join_btn", "Join project", class = "btn-primary")
          ),
          tags$hr(),
          uiOutput("proj_join_feedback")
        )
      )
    }
  })
  
  proj_create_state <- reactiveValues(type = NULL, msg = NULL)
  proj_join_state   <- reactiveValues(type = NULL, msg = NULL)
  proj_leave_state  <- reactiveValues(type = NULL, msg = NULL)
  
  output$proj_create_feedback <- renderUI({
    if (is.null(proj_create_state$type)) return(NULL)
    div(class = if (proj_create_state$type == "success") "alert alert-success" else "alert alert-danger",
        proj_create_state$msg)
  })
  
  output$proj_join_feedback <- renderUI({
    if (is.null(proj_join_state$type)) return(NULL)
    div(class = if (proj_join_state$type == "success") "alert alert-success" else "alert alert-danger",
        proj_join_state$msg)
  })
  
  output$user_projects_cards <- renderUI({
    req(user$is_logged_in)
    projects_refresh()
    
    df <- db_list_projects_for_user(con, user$user_id)
    
    if (nrow(df) == 0) {
      return(div(class = "text-muted", "You haven't joined any projects yet."))
    }
    
    tagList(
      lapply(seq_len(nrow(df)), function(i) {
        p <- df[i, ]
        
        card(
          card_header(p$title),
          if (!is.null(p$description[[1]]) && nzchar(p$description[[1]])) {
            p(p$description[[1]])
          },
          p(tags$strong("Role: "), p$role_in_project),
          p(tags$strong("Joined: "), p$created_at),
          
          div(
            style = "display:flex; gap:.5rem; flex-wrap:wrap;",
            
            if (as.integer(p$created_by_user_id) == as.integer(user$user_id)) {
              actionButton(
                paste0("edit_", p$project_id),
                "Edit",
                class = "btn-outline-secondary btn-sm"
              )
            },
            
            actionButton(
              paste0("leave_", p$project_id),
              "Leave project",
              class = "btn-outline-danger btn-sm"
            )
          )
        )
      })
    )
  })
  
  editing_project_id <- reactiveVal(NULL)
  
  observe({
    req(user$is_logged_in)
    projects_refresh()
    
    df <- db_list_projects_for_user(con, user$user_id)
    if (nrow(df) == 0) return()
    
    for (i in seq_len(nrow(df))) {
      local({
        row <- df[i, , drop = FALSE]
        pid_local <- row$project_id[[1]]
        is_creator <- as.integer(row$created_by_user_id[[1]]) == as.integer(user$user_id)
        
        if (!is_creator) return()
        
        btn_id <- paste0("edit_", pid_local)
        
        observeEvent(input[[btn_id]], {
          editing_project_id(pid_local)
          
          updateTextInput(session, "proj_new_title", value = row$title[[1]] %||% "")
          updateTextAreaInput(session, "proj_new_desc", value = row$description[[1]] %||% "")
          
          proj_create_state$type <- "success"
          proj_create_state$msg <- "Editing mode: make changes above, then click “Save changes”."
        }, ignoreInit = TRUE)
      })
    }
  })
  
  observe({
    req(user$is_logged_in)
    projects_refresh()
    
    df <- db_list_projects_for_user(con, user$user_id)
    if (nrow(df) == 0) return()
    
    for (pid in df$project_id) {
      local({
        pid_local <- pid
        btn_id <- paste0("leave_", pid_local)
        
        observeEvent(input[[btn_id]], {
          tryCatch({
            db_leave_project(con, user_id = user$user_id, project_id = pid_local)
            proj_leave_state$type <- "success"
            proj_leave_state$msg <- "Left project."
            projects_refresh(projects_refresh() + 1)
          }, error = function(e) {
            proj_leave_state$type <- "error"
            proj_leave_state$msg <- paste("Leave failed:", conditionMessage(e))
          })
        }, ignoreInit = TRUE)
      })
    }
  })
  
  output$proj_create_or_save_btn <- renderUI({
    req(user$is_logged_in)
    
    if (is.null(editing_project_id())) {
      actionButton("proj_create_btn", "Create project", class = "btn-primary")
    } else {
      div(
        style = "display:flex; gap:.5rem; flex-wrap:wrap;",
        actionButton("proj_save_btn", "Save changes", class = "btn-primary"),
        actionButton("proj_cancel_edit", "Cancel", class = "btn-outline-secondary")
      )
    }
  })
  
  observeEvent(input$proj_cancel_edit, {
    editing_project_id(NULL)
    updateTextInput(session, "proj_new_title", value = "")
    updateTextAreaInput(session, "proj_new_desc", value = "")
    proj_create_state$type <- NULL
    proj_create_state$msg <- NULL
  })
  
  observeEvent(input$proj_save_btn, {
    req(user$is_logged_in)
    pid <- editing_project_id()
    req(!is.null(pid))
    
    title <- trimws(input$proj_new_title %||% "")
    desc  <- input$proj_new_desc %||% ""
    
    if (title == "") {
      proj_create_state$type <- "error"
      proj_create_state$msg <- "Title cannot be empty."
      return()
    }
    
    tryCatch({
      affected <- db_update_project_if_creator(
        con,
        project_id = as.integer(pid),
        user_id = as.integer(user$user_id),
        title = title,
        description = desc
      )
      
      if (affected == 0) {
        proj_create_state$type <- "error"
        proj_create_state$msg <- "Not saved (you can only edit projects you created)."
        return()
      }
      
      editing_project_id(NULL)
      
      updateTextInput(session, "proj_new_title", value = "")
      updateTextAreaInput(session, "proj_new_desc", value = "")
      
      proj_create_state$type <- "success"
      proj_create_state$msg <- "Project updated."
      
      projects_refresh(projects_refresh() + 1)
      
    }, error = function(e) {
      proj_create_state$type <- "error"
      proj_create_state$msg <- paste("Save failed:", conditionMessage(e))
    })
  })
  
  observeEvent(input$proj_join_btn, {
    req(user$is_logged_in)
    
    pid <- input$proj_join_picker %||% ""
    if (pid == "") {
      proj_join_state$type <- "error"
      proj_join_state$msg <- "Please select a project to join."
      return()
    }
    
    pid <- as.integer(pid)
    
    tryCatch({
      if (db_user_is_member_of_project(con, user$user_id, pid)) {
        proj_join_state$type <- "error"
        proj_join_state$msg <- "You are already a member of that project."
        return()
      }
      
      db_join_project(con, user_id = user$user_id, project_id = pid, role_in_project = "member")
      proj_join_state$type <- "success"
      proj_join_state$msg <- "Joined project."
      
      projects_refresh(projects_refresh() + 1)
      
    }, error = function(e) {
      proj_join_state$type <- "error"
      proj_join_state$msg <- paste("Join failed:", conditionMessage(e))
    })
  })
  
  observeEvent(input$proj_create_btn, {
    req(user$is_logged_in)
    
    title <- trimws(input$proj_new_title %||% "")
    desc  <- input$proj_new_desc %||% ""
    
    if (title == "") {
      proj_create_state$type <- "error"
      proj_create_state$msg <- "Please enter a project title."
      return()
    }
    
    tryCatch({
      new_pid <- db_create_project_global(
        con,
        created_by_user_id = as.integer(user$user_id),
        title = title,
        description = desc
      )
      
      db_join_project(
        con,
        user_id = as.integer(user$user_id),
        project_id = as.integer(new_pid),
        role_in_project = "creator"
      )
      
      proj_create_state$type <- "success"
      proj_create_state$msg <- paste0("Project created (ID ", new_pid, "). You are the creator.")
      
      updateTextInput(session, "proj_new_title", value = "")
      updateTextAreaInput(session, "proj_new_desc", value = "")
      
      projects_refresh(projects_refresh() + 1)
      
    }, error = function(e) {
      proj_create_state$type <- "error"
      proj_create_state$msg <- paste("Create failed:", conditionMessage(e))
    })
  })
  
  output$admin_export_csv <- downloadHandler(
    filename = function() {
      paste0("rccs_connect_export_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".csv")
    },
    content = function(file) {
      req(user$is_logged_in, isTRUE(user$is_admin))
      
      df <- db_export_users_projects_profiles(con)
      
      utils::write.csv(df, file, row.names = FALSE, na = "")
    }
  )
  
  output$admin_ui <- renderUI({
    if (!user$is_logged_in) {
      locked_page("Admin (Locked)", "Please log in first.")
    } else if (!isTRUE(user$is_admin)) {
      locked_page("Admin (Forbidden)", "You are logged in, but not an admin.")
    } else {
      layout_column_wrap(
        width = 1,
        card(
          card_header("Admin export"),
          p("Download a CSV with user profiles + keywords + project memberships (one row per user)."),
          downloadButton("admin_export_csv", "Download CSV", class = "btn-primary")
        )
      )
    }
  })
  
  output$projects_carousel_ui <- renderUI({
    projects_refresh()
    projects <- all_projects_data()
    
    card(
      card_header("Projects spotlight"),
      if (nrow(projects) == 0) {
        div(class = "text-muted", "No projects have been added yet. Create one to see it here.")
      } else {
        carousel_id <- "projectsCarousel"
        items <- lapply(seq_len(nrow(projects)), function(i) {
          proj <- projects[i, ]
          tags$div(
            class = paste("carousel-item", if (i == 1) "active" else ""),
            tags$div(
              class = "d-flex justify-content-center p-4",
              tags$div(
                style = "max-width:600px;",
                tags$h4(proj$title),
                tags$p(if (!is.null(proj$description[[1]]) && nzchar(proj$description[[1]])) proj$description[[1]] else "No description provided."),
                tags$small(
                  class = "text-muted",
                  sprintf("Created by user %s", proj$created_by_user_id)
                )
              )
            )
          )
        })
        
        controls <- NULL
        control_buttons <- NULL
        if (nrow(projects) > 1) {
          controls <- tagList(
            tags$button(
              class = "carousel-control-prev",
              type = "button",
              `data-bs-target` = paste0("#", carousel_id),
              `data-bs-slide` = "prev",
              tags$span(class = "carousel-control-prev-icon", `aria-hidden` = "true"),
              tags$span(class = "visually-hidden", "Previous")
            ),
            tags$button(
              class = "carousel-control-next",
              type = "button",
              `data-bs-target` = paste0("#", carousel_id),
              `data-bs-slide` = "next",
              tags$span(class = "carousel-control-next-icon", `aria-hidden` = "true"),
              tags$span(class = "visually-hidden", "Next")
            )
          )
          
          control_buttons <- div(
            class = "projects-carousel-controls",
            tags$button(
              class = "btn btn-outline-success",
              type = "button",
              `data-bs-target` = paste0("#", carousel_id),
              `data-bs-slide` = "prev",
              "Previous project"
            ),
            tags$button(
              class = "btn btn-success",
              type = "button",
              `data-bs-target` = paste0("#", carousel_id),
              `data-bs-slide` = "next",
              "Next project"
            )
          )
        }
        
        tagList(
          div(
            id = carousel_id,
            class = "carousel slide",
            `data-bs-ride` = "carousel",
            `data-bs-interval` = "7000",
            tags$div(class = "carousel-inner", items),
            controls
          ),
          control_buttons,
          tags$hr(),
          p(class = "text-muted", "Cycle through projects or use the navigation to manage them.")
        )
      }
    )
  })
  
  output$network_home_ui <- renderUI({
    nd <- network_data()$nodes
    
    card(
      class = "home-network-card",
      card_header("Collaboration network"),
      if (!user$is_logged_in) {
        div(class = "text-muted", "Log in to explore the live collaboration map.")
      } else if (nrow(nd) == 0) {
        div(class = "text-muted", "No people or projects to show yet.")
      } else {
        visNetworkOutput("network_home_view", height = "540px")
      }
    )
  })
  
  network_keyword_options <- reactive({
    nodes <- network_data()$nodes
    profile_keywords <- extract_keyword_options(nodes$keywords %||% character(0))
    sort(unique(c(keyword_choices, profile_keywords)))
  })
  
  output$network_explorer_ui <- renderUI({
    nd <- network_data()$nodes
    
    card(
      class = "network-explorer-card",
      card_header("Collaboration network"),
      if (!user$is_logged_in) {
        div(class = "text-muted", "Log in to explore the live collaboration map.")
      } else if (nrow(nd) == 0) {
        div(class = "text-muted", "No people or projects to show yet.")
      } else {
        org_choices <- c("All", organisation_choices)
        
        tagList(
          layout_column_wrap(
            width = 1/3,
            selectInput("network_org_filter", "Organisation filter:", choices = org_choices, selected = "All"),
            selectizeInput(
              "network_keyword_filter",
              "Keyword filter:",
              choices = network_keyword_options(),
              selected = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Start typing to search keywords...",
                maxOptions = 2000
              )
            ),
            checkboxInput("network_hide_isolates", "Hide people with no shared projects (current filter)", value = FALSE)
          ),
          visNetworkOutput("network_view", height = "700px"),
          tags$hr(),
          h5("People in view"),
          DTOutput("network_table")
        )
      }
    )
  })
  
  output$network_home_view <- renderVisNetwork({
    req(user$is_logged_in)
    nodes_vis <- network_data()$nodes
    edges_vis <- network_data()$edges
    req(nrow(nodes_vis) > 0)
    
    visNetwork(nodes_vis %>% transmute(
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
    ), edges_vis) %>%
      visGroups(groupname = "UoG", color = "#279952") %>%
      visGroups(groupname = "NHS", color = "#1764a2") %>%
      visGroups(groupname = "Both NHS & UoG", color = "#1aa6a6") %>%
      visGroups(groupname = "Other", color = "#9e9e9e") %>%
      visEdges(smooth = FALSE) %>%
      visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = FALSE
      ) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -150
        )
      )
  })
  
  network_nodes_filtered <- reactive({
    req(user$is_logged_in)
    nodes <- network_data()$nodes
    if (nrow(nodes) == 0) return(nodes)
    
    org <- input$network_org_filter %||% "All"
    if (org != "All") {
      nodes <- nodes %>% filter(institution == org)
    }
    
    kw_tokens <- parse_keyword_tokens(input$network_keyword_filter)
    if (length(kw_tokens) > 0) {
      pattern <- paste(str_replace_all(kw_tokens, "([\\W])", "\\\\\\1"), collapse = "|")
      nodes <- nodes %>% filter(str_detect(keywords_text, regex(pattern, ignore_case = TRUE)))
    }
    nodes
  })
  
  network_edges_filtered <- reactive({
    req(user$is_logged_in)
    edges <- network_data()$edges
    ids <- network_nodes_filtered()$id
    edges %>% filter(from %in% ids, to %in% ids)
  })
  
  network_nodes_no_isolates <- reactive({
    req(user$is_logged_in)
    nodes <- network_nodes_filtered()
    if (!isTRUE(input$network_hide_isolates)) return(nodes)
    
    edges <- network_edges_filtered()
    keep_ids <- sort(unique(c(edges$from, edges$to)))
    nodes %>% filter(id %in% keep_ids)
  })
  
  output$network_view <- renderVisNetwork({
    req(user$is_logged_in)
    nodes_vis <- network_nodes_no_isolates()
    edges_vis <- network_edges_filtered()
    req(nrow(nodes_vis) > 0)
    
    visNetwork(nodes_vis %>% transmute(
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
    ), edges_vis) %>%
      visGroups(groupname = "UoG", color = "#279952") %>%
      visGroups(groupname = "NHS", color = "#1764a2") %>%
      visGroups(groupname = "Both NHS & UoG", color = "#1aa6a6") %>%
      visGroups(groupname = "Other", color = "#9e9e9e") %>%
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
  
  output$network_table <- renderDT({
    req(user$is_logged_in)
    dat <- network_nodes_no_isolates() %>%
      select(name, institution, dept, region, is_admin, keywords)
    datatable(dat, options = list(pageLength = 10))
  })
}

shinyApp(ui, server)

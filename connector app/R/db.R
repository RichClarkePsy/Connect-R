# R/db.R
library(DBI)
library(RSQLite)
library(here)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---- Paths ----
db_dir <- function() {
  dir <- here::here("data")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}

db_path <- function() {
  file.path(db_dir(), "rccs_connect.sqlite")
}


# ---- Connection handling ----
db_connect <- function() {
  DBI::dbConnect(RSQLite::SQLite(), dbname = db_path())
}

db_disconnect <- function(con) {
  try(DBI::dbDisconnect(con), silent = TRUE)
}

# ---- Schema creation (idempotent) ----
db_init <- function(con) {
  # Turn on foreign keys in SQLite
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  
  DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS users (
    user_id       INTEGER PRIMARY KEY AUTOINCREMENT,
    username      TEXT NOT NULL UNIQUE,
    password_hash TEXT NOT NULL,
    display_name  TEXT,
    organisation  TEXT,
    role_title    TEXT,
    region        TEXT,
    is_admin      INTEGER NOT NULL DEFAULT 0,
    created_at    TEXT NOT NULL DEFAULT (datetime('now')),
    updated_at    TEXT NOT NULL DEFAULT (datetime('now'))
  );
")
  
  DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS projects (
    project_id          INTEGER PRIMARY KEY AUTOINCREMENT,
    title               TEXT NOT NULL,
    description         TEXT,
    created_by_user_id  INTEGER NOT NULL,
    created_at          TEXT NOT NULL DEFAULT (datetime('now')),
    updated_at          TEXT NOT NULL DEFAULT (datetime('now')),
    FOREIGN KEY (created_by_user_id) REFERENCES users(user_id) ON DELETE RESTRICT
  );
")
  
  
  DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS user_projects (
    user_id     INTEGER NOT NULL,
    project_id  INTEGER NOT NULL,
    role_in_project TEXT,
    created_at  TEXT NOT NULL DEFAULT (datetime('now')),
    PRIMARY KEY (user_id, project_id),
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE,
    FOREIGN KEY (project_id) REFERENCES projects(project_id) ON DELETE CASCADE
  );
")
  
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS audit_log (
      audit_id    INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id     INTEGER,
      event_type  TEXT NOT NULL,
      event_detail TEXT,
      created_at  TEXT NOT NULL DEFAULT (datetime('now'))
    );
  ")
  DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS user_keywords (
    user_id    INTEGER NOT NULL,
    keyword    TEXT NOT NULL,
    created_at TEXT NOT NULL DEFAULT (datetime('now')),
    PRIMARY KEY (user_id, keyword),
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
  );
")
  
  invisible(TRUE)
}

# ---- Simple utilities ----
db_table_names <- function(con) {
  DBI::dbListTables(con)
}

db_required_tables <- function() {
  c("users", "projects", "user_projects", "user_keywords", "audit_log")
}

db_health_check <- function(con) {
  existing <- db_table_names(con)
  required <- db_required_tables()
  
  list(
    db_file = db_path(),
    db_exists = file.exists(db_path()),
    tables_existing = sort(existing),
    tables_required = required,
    missing_tables = setdiff(required, existing),
    ok = all(required %in% existing)
  )
}

# ---- Users: helpers ----

db_user_exists <- function(con, username) {
  stopifnot(is.character(username), length(username) == 1)
  res <- DBI::dbGetQuery(
    con,
    "SELECT 1 AS exists_flag FROM users WHERE lower(username) = lower(?) LIMIT 1;",
    params = list(username)
  )
  nrow(res) > 0
}

db_create_user <- function(con, username, password_hash, is_admin = 0) {
  stopifnot(is.character(username), length(username) == 1)
  stopifnot(is.character(password_hash), length(password_hash) == 1)
  
  DBI::dbExecute(
    con,
    "
    INSERT INTO users (username, password_hash, is_admin)
    VALUES (?, ?, ?);
    ",
    params = list(username, password_hash, as.integer(is_admin))
  )
  
  DBI::dbGetQuery(
    con,
    "SELECT user_id FROM users WHERE lower(username) = lower(?) LIMIT 1;",
    params = list(username)
  )$user_id[[1]]
}

db_get_user_by_username <- function(con, username) {
  stopifnot(is.character(username), length(username) == 1)
  DBI::dbGetQuery(
    con,
    "
    SELECT user_id, username, password_hash, is_admin,
           display_name, organisation, role_title, region
    FROM users
    WHERE lower(username) = lower(?)
    LIMIT 1;
    ",
    params = list(username)
  )
}

# ---- Profile helpers ----

db_get_user_profile <- function(con, user_id) {
  stopifnot(length(user_id) == 1)
  
  DBI::dbGetQuery(
    con,
    "
    SELECT user_id, username, is_admin,
           display_name, organisation, role_title, region
    FROM users
    WHERE user_id = ?
    LIMIT 1;
    ",
    params = list(user_id)
  )
}

db_update_user_profile <- function(con, user_id, display_name, organisation, role_title, region) {
  stopifnot(length(user_id) == 1)
  
  DBI::dbExecute(
    con,
    "
    UPDATE users
    SET display_name = ?,
        organisation = ?,
        role_title = ?,
        region = ?,
        updated_at = datetime('now')
    WHERE user_id = ?;
    ",
    params = list(display_name, organisation, role_title, region, user_id)
  )
}

# ---- Projects (global catalogue) ----

db_list_all_projects <- function(con) {
  DBI::dbGetQuery(
    con,
    "
    SELECT project_id, title, description, created_by_user_id, updated_at
    FROM projects
    ORDER BY lower(title) ASC;
    "
  )
}

db_create_project_global <- function(con, created_by_user_id, title, description) {
  stopifnot(length(created_by_user_id) == 1)
  title <- trimws(title %||% "")
  description <- description %||% ""
  
  DBI::dbExecute(
    con,
    "
    INSERT INTO projects (title, description, created_by_user_id)
    VALUES (?, ?, ?);
    ",
    params = list(title, description, created_by_user_id)
  )
  
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id;")$id[[1]]
}

# ---- Membership (user <-> projects) ----

db_list_projects_for_user <- function(con, user_id) {
  stopifnot(length(user_id) == 1)
  
  DBI::dbGetQuery(
    con,
    "
    SELECT p.project_id,
           p.title,
           p.description,
           p.created_by_user_id,
           up.role_in_project,
           up.created_at
    FROM user_projects up
    JOIN projects p ON p.project_id = up.project_id
    WHERE up.user_id = ?
    ORDER BY lower(p.title) ASC;
    ",
    params = list(user_id)
  )
}

db_update_project_if_creator <- function(con, project_id, user_id, title, description) {
  stopifnot(length(project_id) == 1, length(user_id) == 1)
  
  DBI::dbExecute(
    con,
    "
    UPDATE projects
    SET title = ?,
        description = ?,
        updated_at = datetime('now')
    WHERE project_id = ?
      AND created_by_user_id = ?;
    ",
    params = list(title, description, project_id, user_id)
  )
}


db_user_is_member_of_project <- function(con, user_id, project_id) {
  stopifnot(length(user_id) == 1, length(project_id) == 1)
  
  res <- DBI::dbGetQuery(
    con,
    "
    SELECT 1 AS flag
    FROM user_projects
    WHERE user_id = ? AND project_id = ?
    LIMIT 1;
    ",
    params = list(user_id, project_id)
  )
  nrow(res) > 0
}

db_join_project <- function(con, user_id, project_id, role_in_project = NULL) {
  stopifnot(length(user_id) == 1, length(project_id) == 1)
  
  DBI::dbExecute(
    con,
    "
    INSERT OR IGNORE INTO user_projects (user_id, project_id, role_in_project)
    VALUES (?, ?, ?);
    ",
    params = list(user_id, project_id, role_in_project)
  )
}

db_leave_project <- function(con, user_id, project_id) {
  stopifnot(length(user_id) == 1, length(project_id) == 1)
  
  DBI::dbExecute(
    con,
    "
    DELETE FROM user_projects
    WHERE user_id = ? AND project_id = ?;
    ",
    params = list(user_id, project_id)
  )
}

db_set_user_keywords <- function(con, user_id, keywords) {
  keywords <- unique(trimws(keywords))
  keywords <- keywords[nzchar(keywords)]
  
  DBI::dbExecute(con, "DELETE FROM user_keywords WHERE user_id = ?;", params = list(user_id))
  
  if (length(keywords) == 0) return(invisible(TRUE))
  
  for (kw in keywords) {
    DBI::dbExecute(
      con,
      "INSERT OR IGNORE INTO user_keywords (user_id, keyword) VALUES (?, ?);",
      params = list(user_id, kw)
    )
  }
  
  invisible(TRUE)
}

db_get_user_keywords <- function(con, user_id) {
  stopifnot(length(user_id) == 1)
  
  res <- DBI::dbGetQuery(
    con,
    "
    SELECT keyword
    FROM user_keywords
    WHERE user_id = ?
    ORDER BY lower(keyword);
    ",
    params = list(user_id)
  )
  
  res$keyword
}

db_export_users_projects_profiles <- function(con) {
  # Pull base user info
  users <- DBI::dbGetQuery(con, "
    SELECT user_id, username, display_name, organisation, role_title, region,
           is_admin, created_at, updated_at
    FROM users
    ORDER BY user_id;
  ")
  
  # Keywords (long)
  user_keywords <- DBI::dbGetQuery(con, "
    SELECT user_id, keyword
    FROM user_keywords
    ORDER BY user_id, lower(keyword);
  ")
  
  # Projects (long)
  user_projects <- DBI::dbGetQuery(con, "
    SELECT up.user_id,
           p.project_id,
           p.title AS project_title,
           p.description AS project_description,
           p.created_by_user_id,
           up.role_in_project,
           up.created_at AS joined_at
    FROM user_projects up
    JOIN projects p ON p.project_id = up.project_id
    ORDER BY up.user_id, lower(p.title);
  ")
  
  # Collapse keywords -> 1 cell per user
  kw_collapsed <- aggregate(
    keyword ~ user_id,
    data = user_keywords,
    FUN = function(x) paste(unique(x), collapse = "; ")
  )
  
  # Collapse projects -> 1 cell per user
  # (include role and project_id to keep it unambiguous)
  # Clean description for CSV (avoid line breaks / delimiter collisions)
  clean_desc <- function(x) {
    x <- x %||% ""
    x <- gsub("\\|", "/", x)
    x <- gsub("[\r\n]+", " ", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }
  
  user_projects$proj_blob <- with(
    user_projects,
    paste0(
      project_id, " | ",
      project_title, " | ",
      clean_desc(project_description), " | ",
      "role=", ifelse(is.na(role_in_project) | role_in_project == "", "member", role_in_project)
    )
  )
  
  
  proj_collapsed <- aggregate(
    proj_blob ~ user_id,
    data = user_projects,
    FUN = function(x) paste(unique(x), collapse = " ;; ")
  )
  names(proj_collapsed)[2] <- "projects"
  
  # Merge into one flat dataset
  out <- merge(users, kw_collapsed, by = "user_id", all.x = TRUE)
  out <- merge(out, proj_collapsed, by = "user_id", all.x = TRUE)
  
  # Tidy names / fill blanks
  out$keyword <- ifelse(is.na(out$keyword), "", out$keyword)
  out$projects <- ifelse(is.na(out$projects), "", out$projects)
  
  # Rename keyword column to something clearer
  names(out)[names(out) == "keyword"] <- "keywords"
  
  out
}

db_delete_user <- function(con, user_id) {
  stopifnot(length(user_id) == 1)
  
  DBI::dbWithTransaction(con, function(conn) {
    created_projects <- DBI::dbGetQuery(
      conn,
      "
      SELECT project_id
      FROM projects
      WHERE created_by_user_id = ?;
      ",
      params = list(user_id)
    )$project_id
    
    if (length(created_projects) > 0) {
      placeholders <- paste(rep("?", length(created_projects)), collapse = ",")
      
      DBI::dbExecute(
        conn,
        sprintf("DELETE FROM user_projects WHERE project_id IN (%s);", placeholders),
        params = as.list(created_projects)
      )
      
      DBI::dbExecute(
        conn,
        sprintf("DELETE FROM projects WHERE project_id IN (%s);", placeholders),
        params = as.list(created_projects)
      )
    }
    
    DBI::dbExecute(conn, "DELETE FROM user_projects WHERE user_id = ?;", params = list(user_id))
    DBI::dbExecute(conn, "DELETE FROM user_keywords WHERE user_id = ?;", params = list(user_id))
    DBI::dbExecute(conn, "DELETE FROM audit_log WHERE user_id = ?;", params = list(user_id))
    DBI::dbExecute(conn, "DELETE FROM users WHERE user_id = ?;", params = list(user_id))
  })
  
  invisible(TRUE)
}



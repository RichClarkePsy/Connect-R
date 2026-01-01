dbfile <- "C:/Users/s2118966/OneDrive - University of Gloucestershire/Research Projects/Connect-R/connector app/data/rccs_connect.sqlite"

con <- dbConnect(SQLite(), dbfile)

dbGetQuery(con, "SELECT user_id, username, is_admin FROM users;")  # sanity check

dbExecute(
  con,
  "UPDATE users SET is_admin = 1 WHERE lower(username) = lower(?);",
  params = list("user2")
)

dbGetQuery(con, "SELECT user_id, username, is_admin FROM users WHERE lower(username)=lower('user2');")

dbDisconnect(con)


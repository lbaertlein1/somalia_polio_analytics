# =============================================================================
# add_user.R -- add or update a login for this app (its own accounts, in the
# vaccine_buddy database; not the health area adjuster's).
#
# Run from the vaccine_buddy/ folder, after editing the four values below:
#   source('scripts/add_user.R')
# Rerunning with an existing username resets that user's password, name
# and role.
# =============================================================================

username     <- 'luke'
display_name <- 'Luke'
role         <- 'admin'          # 'admin' or 'user'
password     <- readline('Password for this user: ')

library(DBI)
if (file.exists('.env')) dotenv::load_dot_env('.env')
con <- dbConnect(RPostgres::Postgres(),
                 host = Sys.getenv('VB_DB_HOST'), port = as.integer(Sys.getenv('VB_DB_PORT', '5432')),
                 dbname = Sys.getenv('VB_DB_NAME', 'vaccine_buddy'), user = Sys.getenv('VB_DB_USER'),
                 password = Sys.getenv('VB_DB_PASSWORD'), sslmode = Sys.getenv('VB_DB_SSL', 'require'))
stopifnot(nchar(password) >= 8, role %in% c('admin', 'user'))
dbExecute(con, "
  INSERT INTO users (username, password, display_name, role) VALUES ($1, $2, $3, $4)
  ON CONFLICT (username) DO UPDATE SET password = EXCLUDED.password, display_name = EXCLUDED.display_name,
                                       role = EXCLUDED.role, updated_at = NOW()",
  list(username, bcrypt::hashpw(password), display_name, role))
print(dbGetQuery(con, 'SELECT username, display_name, role, updated_at FROM users ORDER BY username'))
dbDisconnect(con)
rm(password)

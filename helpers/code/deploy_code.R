# # One-time (or whenever your token expires): authenticate to Connect Cloud
# rsconnect::connectCloudUser()

rsconnect::deployApp(
  appName = "ucmr_explorer",          # stable identity so re-deploys update the same app
  appTitle = "UCMR Exploration",        # optional, the human-readable name
  appFiles = c(
    "app.R",
    "helpers/code/db_tbl_prep.R",
    "helpers/code/make_chloro_map.R",
    "helpers/code/make_jitter_plot.R",
    "renv.lock"
  ),
  envVars = c(
    "R2_ACCOUNT_ID",
    "R2_ACCESS_KEY_ID_RR",
    "R2_SECRET_ACCESS_KEY_rr",
    "R2_BUCKET"
  ),
  server = "connect.posit.cloud"      # only needed if you have other accounts (e.g. shinyapps.io) configured
)

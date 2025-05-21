# Update timestamp
APP_TIMESTAMP <- format(Sys.time(), "%a, %b %d, %Y at %I:%M %p ET")
saveRDS(APP_TIMESTAMP, file = "dat/APP_TIMESTAMP.rds")

# Deploy the application
rsconnect::deployApp(appName = "atlas", logLevel = "verbose")

cat("Deployment completed with timestamp:", APP_TIMESTAMP, "\n")
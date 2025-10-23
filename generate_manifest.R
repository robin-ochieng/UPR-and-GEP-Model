# Generate manifest.json for Posit Connect deployment
# This script creates a manifest file with exact package versions from your system

library(rsconnect)

# Set the working directory to the app location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Generate the manifest.json file
# This will capture all dependencies and their exact versions
rsconnect::writeManifest(
  appDir = ".",
  appFiles = c(
    "app.R",
    "modules/dataOverviewModule.R",
    "modules/gepResultsModule.R",
    "modules/helperFunctions.R",
    "modules/uprSummaries.R",
    "www/css/custom_styles.css"
  ),
  appPrimaryDoc = "app.R",
  contentCategory = "shiny"
)

cat("\n✓ manifest.json has been generated successfully!\n")
cat("  Location: ", file.path(getwd(), "manifest.json"), "\n\n")
cat("You can now commit this file to your repository for Git-based deployment.\n")

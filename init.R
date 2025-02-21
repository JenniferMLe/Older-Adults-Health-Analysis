# List of required packages
my_packages <- c("dplyr", "ggplot2", "readr", "tidyr", "shiny", "shinythemes", "DT", "plotly", "maps")

# Function to install missing packages
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org/")
  }
}

# Install all required packages
invisible(sapply(my_packages, install_if_missing))

# Print success message
message("All required packages installed successfully.")

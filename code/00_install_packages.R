required_packages <- c(
  "readxl",
  "openxlsx",
  "dplyr",
  "tidyr",
  "tibble",
  "plm",
  "lmtest",
  "sandwich",
  "tseries",
  "moments",
  "ggplot2",
  "patchwork",
  "ragg",
  "scales",
  "panelvar",
  "corrplot",
  "zoo"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) == 0) {
  message("All required packages are already installed.")
} else {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

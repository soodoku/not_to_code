# Load necessary libraries
library(lintr)
library(dplyr)

# Function to run lintr on a single file and get summary
run_lintr <- function(file_path) {
  code <- readLines(file_path)
  
  # Wrap the linting process in a tryCatch block
  tryCatch(
    {
      lint_result <- lintr::lint(text = code)
      summary_result <- summary(lint_result)
      return(summary_result)
    },
    error = function(e) {
      # In case of error, return NA values for the summary
      message(paste("Error while linting", file_path, ":", e$message))
      return(data.frame(file = file_path, line = NA, column = NA, msg = NA, level = NA))
    }
  )
}

run_lintr_on_directory <- function(directory) {
  file_paths <- list.files(path = directory, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  lint_summary_list <- vector("list", length(file_paths))
  
  # Initialize the progress bar
  pb <- progress_bar$new(total = length(file_paths), format = "[:bar] :percent :eta", clear = FALSE)

  for (i in seq_along(file_paths)) {
    file_path <- file_paths[i]
    tryCatch({
      lint_summary <- suppressWarnings(run_lintr(file_path))
      lint_summary_list[[i]] <- lint_summary
    }, error = function(e) {
      # Handle any errors that occur during linting
      cat(paste("Error linting file", file_path, ":", conditionMessage(e), "\n"))
    })
    
    pb$tick()  # Update the progress bar
  }
  
  return(bind_rows(lint_summary_list))
}


# Provide the path to the root directory containing R files
root_directory <- "data/scripts/ajps_datasets_files/"

# Run lintr on all R files in the directory and its subdirectories
lint_results <- run_lintr_on_directory(root_directory)

mean(lint_results$style, na.rm = T)
mean(lint_results$error, na.rm = T)
mean(lint_results$warning, na.rm = T)


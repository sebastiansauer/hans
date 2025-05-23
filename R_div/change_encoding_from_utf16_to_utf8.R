# --- Configuration ---
main_folder_path <- '/Users/sebastiansaueruser/Downloads/data-raw Kopie/WiSe2223' # Make sure this is correct!


output_suffix <- "_utf8" # Suffix for the new UTF-8 files (e.g., "original.csv" -> "original_utf8.csv")
overwrite_original <- TRUE # Set to TRUE to overwrite original files (USE WITH CAUTION!)

# Set VROOM_CONNECTION_SIZE to 10 MB (10485760 bytes) to handle very long lines
Sys.setenv("VROOM_CONNECTION_SIZE" = 10485760)
message(paste0("VROOM_CONNECTION_SIZE set to: ", Sys.getenv("VROOM_CONNECTION_SIZE"), " bytes (10 MB)"))

# Install readr if you haven't already
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr)

# --- Get a list of all CSV files, including those in subfolders ---
# `recursive = TRUE` ensures that files in subdirectories are also found.
csv_files <- list.files(
  path = main_folder_path,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

if (length(csv_files) == 0) {
  message("No CSV files found in the specified directory or its subfolders. Please check 'main_folder_path'.")
} else {
  message(paste("Found", length(csv_files), "CSV files to process (including subfolders)."))
  
  # --- Loop through each CSV file ---
  for (file_path in csv_files) {
    message(paste0("\nProcessing file: ", basename(file_path), " (Full path: ", file_path, ")"))
    
    # 1. Guess the current encoding of the file
    guessed_encodings <- readr::guess_encoding(file_path)
    
    # Get the top guess for encoding
    current_encoding <- NULL
    if (nrow(guessed_encodings) > 0) {
      current_encoding <- guessed_encodings$encoding[1]
      confidence <- guessed_encodings$confidence[1]
      message(paste0("  Guessed encoding: '", current_encoding, "' (confidence: ", round(confidence, 2), ")"))
    } else {
      message("  Could not reliably guess encoding for this file.")
    }
    
    # --- Conditional Conversion: Only proceed if the file is detected as UTF-16 ---
    # This checks for common UTF-16 variants returned by guess_encoding().
    if (!is.null(current_encoding) && (current_encoding == "UTF-16LE" || current_encoding == "UTF-16BE" || current_encoding == "UTF-16")) {
      
      message("  File detected as UTF-16. Proceeding with conversion to UTF-8.")
      
      # 2. Read the CSV file using the guessed UTF-16 encoding
      data <- NULL
      tryCatch({
        data <- readr::read_csv(file_path, locale = locale(encoding = current_encoding))
        message("  File read successfully with specified encoding.")
      }, error = function(e) {
        message(paste0("  ERROR reading file '", basename(file_path), "' with encoding '", current_encoding, "': ", e$message))
        message("  Skipping this file due to read error.")
      })
      
      # If data was successfully read, proceed to write
      if (!is.null(data)) {
        # 3. Define the output file path
        if (overwrite_original) {
          output_file_path <- file_path # Overwrite the original file
          message("  Overwriting original file.")
        } else {
          # Create a new file name in the same directory as the original
          file_directory <- dirname(file_path)
          file_name_parts <- tools::file_path_sans_ext(basename(file_path))
          file_extension <- tools::file_ext(basename(file_path))
          output_file_path <- file.path(file_directory, paste0(file_name_parts, output_suffix, ".", file_extension))
          message(paste0("  Saving re-encoded file to: ", basename(output_file_path), " (in ", file_directory, ")"))
        }
        
        # 4. Write the CSV file to UTF-8 encoding
        # readr::write_csv automatically writes in UTF-8.
        tryCatch({
          readr::write_csv(data, output_file_path, na = "") # `na=""` writes empty strings for NA values
          message(paste0("  Successfully re-encoded '", basename(file_path), "' to UTF-8."))
        }, error = function(e) {
          message(paste0("  ERROR writing UTF-8 file for '", basename(file_path), "': ", e$message))
        })
      }
      
    } else {
      # This block executes if the file is NOT detected as UTF-16
      if (!is.null(current_encoding)) {
        message(paste0("  File is NOT detected as UTF-16 (detected as '", current_encoding, "'). Skipping conversion."))
      } else {
        message("  Encoding not reliably guessed as UTF-16. Skipping conversion.")
      }
    }
  }
}

message("\nAll CSV files processed.")
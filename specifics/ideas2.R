
# Pfad zum Skript:
script_path <- "60-llm.qmd"
# Skript einlesen
script_lines <- readLines(script_path)

# Alle tar_load()-Aufrufe finden
loads <- stringr::str_extract_all(lines, "tar_load\\([^)]*\\)") |> unlist()

# Extrahiere die Namen der Targets
targets_loaded <- stringr::str_match(loads, "tar_load\\(([^,)]+)")[, 2] |>
  stringr::str_trim() |>
  unique()

for (target in targets_loaded) {
  # Find lines containing the target name
  matches <- grep(target, script_lines, value = TRUE)

  # Exclude lines with tar_load
  used_lines <- matches[!grepl("tar_load", matches)]

  if (length(used_lines) > 0) {
    cat("\n", target, "is used in:\n")
    print(used_lines)
  }
}

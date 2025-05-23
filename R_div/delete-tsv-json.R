# pre-preparation:
# delete all redundant data files, i.,e all ".tsv" and ".json" files


target_dir <- "/Users/sebastiansaueruser/github-repos/hans/data/data-raw"

files_to_delete <- list.files(
  path = target_dir,
  pattern = "\\.(tsv|json)$",
  recursive = TRUE,
  full.names = TRUE
)

unlink(files_to_delete)

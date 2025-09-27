compute_glotzdauer <- function(d) {
  d |>
    # discard dublicates:
    distinct(.keep_all = TRUE) |> 
    group_by(nr, type, idvisit) |> 
    slice(1) |>
    ungroup() |> 
  glotzdauer_playpause()
}
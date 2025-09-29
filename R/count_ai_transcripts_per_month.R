count_ai_transcripts_per_month <- function(d) {
  d |>
    mutate(clicks_transcript = str_detect(value, "click_transcript_word")) |>
    group_by(idvisit) |>
    mutate(clicks_transcript_any = any(clicks_transcript == TRUE)) |>
    filter(type == "timestamp") |>
    add_dates() |>
    filter(date_time == min(date_time)) |>
    ungroup() |>
    select(-c(clicks_transcript))
}

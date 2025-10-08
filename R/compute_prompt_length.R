compute_prompt_length <- function(data, no_prompt_text = TRUE) {
  #' Compute the length of a prompt in characters
  #' This function computes the length of a given prompt in terms of number of characters.
  #' It takes a single argument 'prompt', which is expected to be a string.

  # if (!is.character(data$value)) {
    # stop("Input must be a character string.")
  # }

  llm_interactions <-
    data |> 
    select(type, value, idvisit) |> 
    filter(type == "subtitle") |> 
    mutate(value = as.character(value)) |>
    filter(str_detect(value, "message_to_llm"))

  prompts <-
    llm_interactions |>
    mutate(prompt = str_extract(value, '(?<=Action: \\"\").*?(?=\\"\\")')) |>
    mutate(token_length = lengths(tokenize_words(prompt))) 
  

  if (no_prompt_text) {
    prompts <- prompts |> select(-prompt)
  }

  return(prompts)
}

# str_match(txt, '""([^"]+)""')
# txt

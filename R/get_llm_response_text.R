get_llm_response_text <- function(d = n_action_type) {
  
  d |> 
    filter(str_detect(value, "llm_response")) |> 
    select(value) |> 
    mutate(lang = str_extract(value, "llm_response_([\\w]+)", group = 1),
           tokens_n = lengths(tokenize_words(value)))
}


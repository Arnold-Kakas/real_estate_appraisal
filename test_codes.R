library(tidyverse)

characteristics <- c("Vlastníctvo", 
                     "Počet izieb/miestností", 
                     "Orientácia", 
                     "Rok výstavby", 
                     "Vonkajšie parkovacie miesto", # combine under Verejné parkovanie
                     "Garáž")

text_df <- data.frame(text = c("Vlastníctvo: Osobné Počet nadzemných podlaží: 9 Podlažie: 4 Počet izieb/miestností: 2 Výťah: Áno Počet balkónov: 1", 
                               "Energetický certifikát: nemá Vlastníctvo: Osobné Počet nadzemných podlaží: 8 Podlažie: 3 Počet izieb/miestností: 4 Umiestnenie: Vyvýšené prízemie"))


extract_value <- function(characteristic, text) {
  chars <- characteristics %>% select(-characteristic)
  pattern <- paste0("((?<=^|\\s)", characteristic, ":\\s*)([\\w\\s/-]+)(?=(\\s(", paste0(chars, collapse = "|"), "):|\\s*$))")
  # modified regex pattern to capture everything after the keyword until the last word is another keyword from the list
  # this pattern looks for the keyword followed by a colon and optional whitespace, then captures any word characters, spaces, slashes, or hyphens that appear after that, up until the last word before the next characteristic or the end of the line
  match <- str_extract(text, pattern)
  ifelse(is.na(match), NA, match)
}

result <- lapply(characteristics, function(x) extract_value(x, text_df$text))

names(result) <- characteristics
result
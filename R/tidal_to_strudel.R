library(tidyverse)

filename <- "tidal-drum-patterns/Sound/Tidal/Drum/Trap1a.hs"
extract_instr <- function(filename) {
  pattern_strings <- read_lines(filename) |>
    str_subset('".*"')
  instruments <- pattern_strings |>
    str_remove(" = .*")
  patterns <- pattern_strings |>
    str_extract('(?<=").*(?=")')
  set_names(patterns, instruments)
}


# extract_patterns(filename)

l <- fs::dir_ls("tidal-drum-patterns/Sound/Tidal/Drum/") %>%
  .[str_detect(., "All.hs", negate = T)] |>
  map(extract_instr)

names(l) <- str_remove(names(l), ".*/")
df_name_repair <- tibble::tribble(
  ~orig, ~strudel_name,
  "cow",          "cb",
  "ch",          "hh",
  "rm",         "rim",
  "ac",            NA,
  "sn",          "sd",
  "cy",            NA,
  "cl",          "cp",
  "sh",            NA,
  "bd",          "bd",
  "lt",          "lt",
  "mt",          "mt",
  "ht",          "ht",
  "oh",          "oh",
  "cp",          "cp",
  "cb",          "cb"
)

lookuptable <- df_name_repair |>
  mutate(strudel_name = coalesce(strudel_name, orig))

rename_list_elements <- function(lst, lookuptable) {
  names(lst) = lookuptable$strudel_name[match(names(lst), lookuptable$orig)]
  lst
}


l_strudel <- l |>
  map(~rename_list_elements(.x, lookuptable))
names(l_strudel) <- paste0("strudel_beats/", str_replace(names(l), "hs$", "txt"))



pattern_to_chr_vec <- function(pattern) {
  pattern |>
    str_replace_all("t", names(pattern)) %>%
    paste0('"', ., '",')
}




write_strudel_file <- function(x, filename) {
  strudel_vec <- c(
    "// made by using the patterns from https://github.com/lvm/tidal-drum-patterns",
    "// (licensed under GPL-3.0 license)",
    "// using the R script from this repository: https://github.com/urswilke/read_beats",
    "stack(",
    paste0("  ", x),
    ").s().slow(2)"
  )
  write_lines(strudel_vec, filename)

}
l_strudel |>
  map(pattern_to_chr_vec) |>
  iwalk(~write_strudel_file(.x, .y))

library(tidyverse)

filename <- "tidal-drum-patterns/Sound/Tidal/Drum/Trap1a.hs"
extract_patterns <- function(filename) {
  pattern_strings <- read_lines(filename) |>
    str_subset('".*"')
  instruments <- pattern_strings |>
    str_remove(" = .*")
  pattern_strings |>
    str_extract('(?<=").*(?=")') |>
    str_replace_all("t", instruments) %>%
    paste0('"', ., '",')
}

# extract_patterns(filename)

l <- fs::dir_ls("tidal-drum-patterns/Sound/Tidal/Drum/") |>
  map(extract_patterns)

names(l) <- str_remove(names(l), ".*/")


l_strudel <- l
names(l_strudel) <- paste0("strudel_beats/", str_replace(names(l), "hs$", "txt"))
write_strudel_file <- function(x, filename) {
  strudel_vec <- c(
    "// made by using the patterns from https://github.com/lvm/tidal-drum-patterns",
    "stack(",
    paste0("  ", x),
    ").s().slow(2)"
  )
  write_lines(strudel_vec, filename)

}
l_strudel |> iwalk(~write_strudel_file(.x, .y))

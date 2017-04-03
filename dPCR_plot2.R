library(dplyr)
library(ggplot2)


all_lines <- readLines("./data/digital_PCR.txt")
all_abstr <- all_lines[grep("^[0-9]+\\. [A-Z]", all_lines)]
potential_abstr <- grep("^[0-9]+\\. [A-Z]", all_lines)
abstr_id <- as.numeric(sapply(strsplit(all_lines[potential_abstr], ". ", fixed = TRUE), first))

abstr_status <- c(TRUE, sapply(2L:length(abstr_id - 1), function(i)
  abstr_id[i] - abstr_id[i - 1] == 1 | abstr_id[i + 1] - abstr_id[i] == 1))
ith_id <- 1
abstr_lines <- paste0(all_lines[good_abstr[ith_id]:(good_abstr[ith_id + 1] - 1)], collapse = "")
grepl("digital PCR", abstr_lines) | grepl("dPCR", abstr_lines)
abstr_status <- c(TRUE, sapply(2L:length(abstr_id - 1), function(i)
  abstr_id[i] - abstr_id[i - 1] == 1 | abstr_id[i + 1] - abstr_id[i] == 1))

good_abstr <- c(potential_abstr[abstr_status], length(all_lines))
real <- sapply(1L:(length(good_abstr) -1), function(ith_id) {
  abstr_lines <- paste0(all_lines[good_abstr[ith_id]:(good_abstr[ith_id + 1] - 1)], collapse = "")
  grepl("digital PCR", abstr_lines) | grepl("dPCR", abstr_lines) |
    grepl("digital", abstr_lines) & grepl("nucleic", abstr_lines)
})

lapply(strsplit(all_lines[good_abstr[-length(good_abstr)][real]], " "), function(i) {
  potential_years <- as.numeric(i[-1])
  potential_years[!is.na(potential_years)][1]
}) 

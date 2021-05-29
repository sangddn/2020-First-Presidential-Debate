get_debate <- function() {
  d <- read.csv("../processed_data/pres_debate.csv", header = T, stringsAsFactors = F)
  return(d)
}

get_word_freq <- function() {
  d <- read.csv("../processed_data/word_frequency_processed.csv", header = T, stringsAsFactors = F)
  return(d)
}

get_verbs <- function() {
  d <- read.csv("../processed_data/english_verbs_processed.csv", header = T, stringsAsFactors = F)
  return(d)
}
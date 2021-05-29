#Transcipt to Dataset
transcript <- file("../processed_data/Presidential_Debate_Transcript_processed.txt", "r")
lines <- readLines(transcript)

d <- data.frame(linenumber = "2", speaker = "WALLACE", statement = lines[2], time = "1min20sec", stringsAsFactors = F)
spkrNames <- c("WALLACE", "TRUMP", "BIDEN")

for (i in seq(5, length(lines), 3)) {
  spkrDetect <- str_detect(lines[i-1], c("Wallace: ", "Trump: ", "Biden: "))
  thisSpeaker <- spkrNames[ which(spkrDetect %in% TRUE) ]
  
  timestamp <- str_sub(lines[i-1], str_locate(c(lines[i-1]), "\\(")[1]+1, str_locate(lines[i-1], "\\)")[1])
  str_replace(timestamp, ":", "XX") %>% str_replace("\\)","sec") -> timestamp
  if (all(str_detect(timestamp, c("XX",":")))) {
    str_replace(timestamp, "XX", "hour") %>% str_replace(":", "min") -> timestamp
  } else str_replace(timestamp, "XX", "min") -> timestamp
  
  d[nrow(d) + 1,] <- c(paste(i), thisSpeaker, lines[i], timestamp)
}

write.csv(d, "../processed_data/pres_debate.csv", row.names = F)

#Word Frequency
word_freq <- read.csv("../raw_data/word_frequency.csv", header = T, stringsAsFactors = F)

select(word_freq, -Rank, -Part.of.speech, -Dispersion) -> word_freq
names(word_freq) <- c("word", "count")
dplyr::mutate_all(word_freq, funs(toupper)) -> word_freq
word_freq$count <- as.integer(word_freq$count)
str_remove_all(word_freq$word, "[[:space:]]") -> word_freq$word

write.csv(word_freq, "../processed_data/word_frequency_processed.csv", row.names = F)

#English Verbs
verbs <- read.csv("../raw_data/english_verbs.csv", header = F, stringsAsFactors = F, fileEncoding="latin1")

c("v1", "v2", "v3", "v4", "v5") -> names(verbs)
dplyr::mutate_all(verbs, funs(toupper)) -> verbs

write.csv(verbs, "../processed_data/english_verbs_processed.csv", row.names = F)
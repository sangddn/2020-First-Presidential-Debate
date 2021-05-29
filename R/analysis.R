get_word_counts <- function(d, speaker) {
  #Get Split Sentences
  splitSen <- get_split_sen(d, speaker)
  
  #Word Counts
  wordCounts <- as.data.frame(table(splitSen), stringsAsFactors = F)
  c("word", "count") -> names(wordCounts)
  return(wordCounts[3:nrow(wordCounts),]) #Filter out NULL and "$" values
}

total_word_counts <- function(d, speakers) {
  splitSen <- get_split_sen(d, speakers[1])
  for (i in 2:length(speakers))
    splitSen <- c(splitSen, get_split_sen(d, speakers[i]))
  
  #Total Word Counts
  wordCounts <- as.data.frame(table(splitSen))
  c("word", "count") -> names(wordCounts)
  return(wordCounts[3:nrow(wordCounts),]) #Filter out NULL and $ values
}

prepare_word_cloud <- function(d, speaker, trump = T, biden = T, wallace = T, common = T, weightCommon = 1) {
  speakr <- speaker
  dplyr::filter(word_freq_data(d, trump, biden, wallace, common, weightCommon), speaker == toupper(speakr)) %>%
    dplyr::select(-speaker, -tf, -idf, -count) -> out
  c("word", "weight") -> names(out)
  return(out)
}

get_split_sen <- function(d, speaker) {
  speakr <- speaker
  speakerSen <- dplyr::filter(d, speaker == toupper(speakr))$statement #Extract statements only
  splitSen <- unlist(strsplit(speakerSen, split=" ")) #Store all words in a vector
  
  return(clean_words(splitSen))
}

clean_words <- function(words) {
  get_verbs() -> verbs
  toupper(words) %>% 
    str_remove_all("[[:punct:]&&[^’]]|\\”|\\“|\\…|CROSSTALK|[[:digit:]]|[[:space:]]
                   |N’T|’S|’M|’RE|’LL|’VE|’D|BIDEN|CHRIS|JOE") %>%
      sort() -> words
  
  #Convert verbs to original forms
  for(i in 2:5) {
    which(verbs[[i]] %in% words) -> thisTense
    for(j in 1:length(thisTense)) {
      which(words %in% verbs[[i]][thisTense[j]]) -> thesePositions
      words[thesePositions] <- verbs[[1]][thisTense[j]]
    }
  }
  return(words)
}

word_freq_data <- function(d, trump = T, biden = T, wallace = T, common = T, weightCommon = 1) {
  trumpWords <- ifelse(trump, get_words(d, "trump"))
  bidenWords <- ifelse(biden, get_words(d, "biden"))
  wallaceWords <- ifelse(wallace, get_words(d, "wallace"))
  commonWords <- ifelse(common, data.frame(speaker = "COMMON", change_weight(get_word_freq(), weightCommon), stringsAsFactors = F))
    
  #Calculate inverse speaker frequency
  allWords <- rbind(ifelse(trump, trumpWords), ifelse(biden, bidenWords), 
                    ifelse(wallace, wallaceWords), ifelse(common, commonWords)) %>%
    bind_tf_idf(word, speaker, count) %>%
      arrange(desc(tf_idf))
  return(allWords)
}

get_words <- function(d, spkr) {
  return(data.frame(speaker = toupper(spkr), get_word_counts(d, spkr), stringsAsFactors = F))
}

change_weight <- function(d, newWeight = 1) {
  d$count <- d$count * newWeight
  return(d)
}

ifelse <- function(yesno, yay) {
  if(yesno) 
    return(yay)
  else
    return(NA)
}
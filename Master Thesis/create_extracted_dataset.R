# Master Thesis Kaz Roomer
# Creating final dataset from Duolingo Spaced Repetition

library(stringr)
library(dplyr)
library(zoo)

#function to read inital dataset and sort on english learning language + spanish interface learners
#Duolingo spaced repetition (1.21 GB)
#https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/N8XJME
#B. Settles and B. Meeder. 2016. A Trainable Spaced Repetition Model for Language Learning. 
#In Proceedings of the Association for Computational Linguistics (ACL), pages 1848-1858.
read_data <- function(input){
  in_d <- read.csv(input, header = TRUE, sep = ',')
  
  #EN learning + ES interface + omit gen and apos
  ex_d <- in_d[which(in_d$learning_language == 'en' & in_d$ui_language == 'es'  & 
                       in_d$lexeme_id != 'd3619a3e50c3b522703d66166855d3bf' & in_d$lexeme_id != '78b9b97a0aaf3ce9102280144c693fd5'),]
  #history incorrect variable and dummy variables for nouns and verbs
  ex_d$history_incorrect <- ex_d$history_seen - ex_d$history_correct
  ex_d$dummy_noun <- ifelse(grepl('<n>|<np>', ex_d$lexeme_string), 1, 0)
  ex_d$dummy_verb <- ifelse(grepl('<vaux>|<vbdo>|<vbhaver>|<vblex>|<vbser>', ex_d$lexeme_string), 1, 0)
  #extract words from lexeme
  ex_d$words <- ifelse(str_extract(ex_d$lexeme_string, "[^/]+") == '<*sf>',
                       str_extract(ex_d$lexeme_string, "(?<=\\/)\\w+"),
                       str_extract(ex_d$lexeme_string, "[^/]+"))
  rownames(ex_d) <- NULL
  return(ex_d)
}

#function to read SUBTLEX-US data 
#(manually created on http://www.lexique.org/shiny/openlexicon/)
read_subtlex <- function(input){
  in_d <- read.csv(input, header = TRUE, sep = ';')
  return(in_d)
}

#function to read emotion dictionary data 
#Geoff Hollis and Chris Westbury based on Warriner, Kuperman and Brysbaert (2007)
#link: http://crr.ugent.be/programs-data
#direct link: https://sites.ualberta.ca/~hollis/files/emotion_dict.csv
read_emo <- function(input){
  input <- read.csv(input, header = TRUE, sep = ',')
  emo_d <- input[c("Word","Valence","Arousal","Concreteness")]
  return(emo_d)
}

#function to rescale variables (done later in Python, but kept for data analysis)
rescale <- function(input){
  (input-min(input))/(max(input)-min(input))
}

#function to create .txt file for SUBTLEX-US query on http://www.lexique.org/shiny/openlexicon/
create_txt <- function(input){
  write.table(unique(input),"unique words.txt",row.names=FALSE,sep="\t", quote = FALSE)
}

#function to get missing words
sort_missing <- function(input){
  input$missing_words <- input$words
  input$missing_words <- ifelse(input$words %in% setdiff(input$words, subtlex_dataset$ï..Word),
                                str_to_title(input$missing_words),
                                input$missing_words)
  return(input$missing_words)
}

#function to merge and create final dataset
merge_final <- function(input1, input2, input3){
  
  #to remain order
  input1$id  <- 1:nrow(input1)
  
  #Create target label/variabel
  input1$p_recall <- ifelse(input1$p_recall == 1,1,0)
  
  #drop unwanted col
  input1 <- input1[c("p_recall","timestamp", "delta", "history_seen", "history_correct", "history_incorrect", 
                     "dummy_noun", "dummy_verb", "words", "missing_words", "id")]
  
  #merge by words
  final_d <- merge(x = input1, y = input2, by.x = "missing_words", by.y = "ï..Word")
  final_d <- merge(x = final_d, y = input3, by.x = "words", by.y = "Word", all.x = TRUE)
  
  #mean sub all NA's in columns emotion_dict
  final_d$Valence <- na.aggregate(final_d$Valence)
  final_d$Arousal <- na.aggregate(final_d$Arousal)
  final_d$Concreteness <- na.aggregate(final_d$Concreteness)
  
  #original order
  final_d <- final_d[order(final_d$id), ]
  final_d <- final_d[c("p_recall","timestamp", "delta", "history_seen", "history_correct", "history_incorrect", 
                       "dummy_noun", "dummy_verb", "words","FREQcount","CDcount","Valence","Arousal","Concreteness")]
  rownames(final_d) <- NULL
  return(final_d)
}

#Set work dir/path
work_dir <- "~/aaMaster DSS/data"
setwd(work_dir)

#load and create Spaced Repetition Duolingo dataset
dataset_name <- "learning_traces.13m.csv"
extracted_dataset <- read_data(dataset_name)

# SKIP THIS LINE IF 'Lexique-query final.csv' ALREADY CREATED
#export for SUBTLEX-US query first time
create_txt(extracted_dataset$words)

#load SUBTLEX-US query data created as csv 
#for word frequency and contextual diversity
dataset_name <- "Lexique-query.csv"
subtlex_dataset <- read_subtlex(dataset_name)

#78 words missing, because of capitals in SUBTLEX-US database
setdiff(extracted_dataset$words, subtlex_dataset$ï..Word)

#create new variable to correct capitals for SUBTLEX-US query
extracted_dataset$missing_words <- sort_missing(extracted_dataset)

#1804 total word for final SUBTLEX-US query
create_txt(extracted_dataset$missing_words)

#load final SUBTLEX-US query data created as csv 
#for word frequency and contextual diversity
dataset_name <- "Lexique-query final.csv"
subtlex_dataset <- read_subtlex(dataset_name)
#check no missing values with:
setdiff(extracted_dataset$missing_words, subtlex_dataset$ï..Word)

#load and create emotion dictionary dataset
dataset_name <- "emotion_dict.csv"
emotion_dict <- read_emo(dataset_name)

#join datasets together for final dataset
final_dataset <- merge_final(extracted_dataset, subtlex_dataset, emotion_dict)

#export final dataset to work_dir (323 MB)
write.csv(final_dataset,"final_dataset.csv", row.names = FALSE)

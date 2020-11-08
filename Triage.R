#Part I: Reading in the data and basic preprocessing 
We read in relevant VZ .txt files and perform basic cleaning of the data while dumping it into a dataframe that can be used later for feature engineering and subsequent analysis
```{r, eval=FALSE}

library(tm)
library(stringr)
library(lubridate)

# Read text files and perform basic text cleaning
unlink(".RData")
rm(list = ls())

# specify root directory where transcript files are stored
dirName <- paste0(getwd(), "/data/Verizon/")
filelist_fin <- list.files(dirName)

# Loading data into dataframe
# create empty data frame to store file name, speaker, and text
transcript_df <-
  data.frame(
    fileName = character(),
    speaker = character(),
    text = character(),
    pos = integer()
  )

# cycle through all documents in directory
for (i in 1:length(filelist_fin)) {
  # concatenate directory and filename; read contents of text file
  path <- paste0(dirName , filelist_fin[i])
  fullText <- readLines(path)
  
  fullText <-
    iconv(fullText,
          from = "latin1",
          to = "ASCII",
          sub = "")
  fullText <- stripWhitespace(fullText)                                               # remove extra white spaces
  fullText <-
    trimws(fullText, which = "both")                                                  # cleaning white spaces at the beginning and end
  
  fullText <-
    fullText[-1]                                                                      # remove first row (file header with no meaning)
  
  # remove various periods commonly used for abbreviations
  fullText <- gsub("[Ii]nc\\.", "Inc", fullText, fixed = FALSE)
  fullText <- gsub("[Ll]td\\.", "Ltd", fullText, fixed = FALSE)
  fullText <- gsub("[Cc]o\\.", "Co", fullText, fixed = FALSE)
  fullText <- gsub("[Cc]orp\\.", "Corp", fullText, fixed = FALSE)
  fullText <- gsub("[Cc]omm\\.", "Comm", fullText, fixed = FALSE)
  fullText <- gsub("[Oo]ps\\.", "Ops", fullText, fixed = FALSE)
  fullText <-
    gsub("[Aa]nalyst\\.", "Analyst", fullText, fixed = FALSE)
  fullText <- gsub("[Dd]ev\\.", "Dev", fullText, fixed = FALSE)
  fullText <- gsub("[Ss]ec\\.", "Sec", fullText, fixed = FALSE)
  fullText <- gsub("V\\.P\\.", "VP", fullText, fixed = FALSE)
  fullText <- gsub("[Mm]gr\\.", "Mgr", fullText, fixed = FALSE)
  fullText <-
    gsub("Platforms Group\\.", "Platforms Group", fullText, fixed = FALSE)
  fullText <- gsub("\\.\\ \\-", "\\ \\-", fullText, fixed = FALSE)
  fullText <- gsub("\\.\\,", "\\,", fullText, fixed = FALSE)
  fullText <- gsub("I\\.R\\.", "IR", fullText, fixed = FALSE)
  fullText <- gsub("J\\.P\\.", "JP", fullText, fixed = FALSE)
  
  trans_date <-
    mdy(str_sub(
      fullText,
      start = regexpr("\\(Thomson StreetEvents\\)", fullText) - 13,
      end = regexpr("\\(Thomson StreetEvents\\)", fullText) - 1
    ))
  # adding line to extract doc title
  doctitle <-
    str_sub(
      fullText,
      start = regexpr('"1"' , fullText) + 4 ,
      end = regexpr("\\(Thomson StreetEvents\\)", fullText) - 1
    )
  
  # determine which file format the transcript is in
  # if speakers are separated by ----------- characters, use the following to parse text
  if (grepl("-{5}", fullText, fixed = F)) {
    # fill empty space between delimiters with "NULL" so no empty text is returned while parsing
    fullText <-
      gsub("-{5,}\\s+-{5,}", "------ NULL ------", fullText)
    script <-
      strsplit(fullText, '-{5}')[[1]]                                                  # split text into list at each occurrence of more than 5 hyphens
    
    temp <- NULL
    temp <-
      which(nchar(script) == 0)                                                        # marking the lines with 0 character length
    if (length(temp) > 0)
      script <- script[-temp]                                                          # removing empty lines, line breaks
    
    script <- script[-1]                                                               # remove header from list
    script <-
      gsub("--", "", script, fixed = TRUE)                                             # remove all double hyphens
    
    temp <- NULL
    temp <- grep("^ $", script)                                                        # mark lines with one single space
    if (length(temp) > 0)
      script <-
      script[-temp]                                                                    # removing lines containing single empty space
    
    # create data frame populated with file name, transcript date, and empty values for other cols # added doc title
    script_df <-
      data.frame(
        fileName = filelist_fin[i],
        doctitle = doctitle ,
        transDate = trans_date,
        speaker = rep(NA, length(script) / 2),
        text = rep(NA, length(script) / 2),
        pos = rep(NA, length(script) / 2)
      )
    
    # cycle through list and store into data frame
    j <- 1
    while (j <= length(script)) {
      script_df$speaker[(j + 1) / 2] <- script[j]
      script_df$text[(j + 1) / 2] <- script[j + 1]
      script_df$pos[(j + 1) / 2] <- (j + 1) / 2
      j <- j + 2
    }
    
    #append the data frame from this iteration of loop to master dataframe containing all transcripts
    transcript_df <- rbind(transcript_df, script_df)
  } else
    # transcript not formatted with --------- delimiters
  {
    # check to verify if Q&A section to transcript - if not, skip to end of loop and try next file
    if (grepl("QUESTIONS AND ANSWERS", fullText)) {
      # parse out all text coming after Q&A
      fullText <-
        str_sub(fullText, start = str_locate(fullText, "QUESTIONS AND ANSWERS")[2] +
                  2)
      
      #split text into list at each occurence of number inside brackets
      script <- strsplit(fullText, '[[0-9]+]')[[1]]
      script <-
        gsub("--", ".", script)                                                        # replace double hyphen with period
      
      #create empty dataframe for this file
      script_df <-
        data.frame(
          fileName = filelist_fin[i],
          doctitle = doctitle,
          transDate = trans_date,
          speaker = rep(NA, length(script) - 1),
          text = rep(NA, length(script) - 1),
          pos = rep(NA, length(script) - 1)
        )
      
      for (j in 1:length(script) - 1) {
        # extract speaker and text information by looking for last period or question mark in text
        script_df$speaker[j] <-
          str_sub(script[j], start = regexpr("[.?)] [^.?]*$", script[j]) + 1)
        script_df$text[j] <-
          str_sub(script[j + 1],
                  start = 1L,
                  end = regexpr("[.?)] [^.?]*$", script[j + 1]))
        script_df$pos[j] <- j
        
      } #close for loop for this doc
      
      transcript_df <-
        rbind(transcript_df, script_df)                                            # append current data frame to master data frame
      
    }                                                                              # close if for Q&A
  }                                                                                # close if for file format check
  
}                                                                                  # close for loop iterating through all docs


#Part II: Data Processing and Feature Engineering


# "doctitle" column
transcript_df$doctitle = str_sub(transcript_df$doctitle,
                                 1,
                                 str_length(transcript_df$doctitle) - 13)        # remove date at the end
transcript_df$doctitle <-
  gsub("[',`]", "", transcript_df$doctitle, fixed = F)                             # remove puncts
transcript_df$doctitle <-
  gsub('"', "", transcript_df$doctitle, fixed = F)                                 # remove puncts
transcript_df$doctitle <-
  tolower(transcript_df$doctitle)                                               # convert to lower case
citylist = list(
  "new york city",
  "new york",
  "boston",
  "london",
  "las vegas",
  # removing cities from title
  "barcelona",
  "palm beach",
  "beverly hills",
  "los angeles",
  "orlando",
  "miami",
  "marina del rey",
  "newport beach",
  "phoenix",
  "bellevue",
  "stamford",
  "newbury",
  "san francisco"
)
find.string <- paste(unlist(citylist), collapse = "|")
transcript_df$doctitle  <-
  gsub(find.string, replacement = "", x = transcript_df$doctitle)

# "type" (doctype) column
# extracting earnings from doctitle to populate document type - earnings vs other
transcript_df$type <-
  ifelse(
    grepl("earning", transcript_df$doctitle, ignore.case = T),
    "VZ Earnings",
    "Other/Announcement"
  )
transcript_df$type <-
  ifelse(
    grepl("rural.*earning", transcript_df$doctitle, ignore.case = T),
    "RCC Earnings",
    transcript_df$type
  )
transcript_df$type <-
  ifelse((transcript_df$type == "Other/Announcement") &
           (
             grepl("verizon.*\\bat\\b", transcript_df$doctitle, ignore.case = T)
           ),
         "VZ Conference",
         transcript_df$type
  )

# "speaker" column
# drop rows corresponding to "operator" as its irrelevant
transcript_df <-
  transcript_df[!grepl("(Operator|operator)", transcript_df$speaker), ]
transcript_df$speaker <-
  ifelse(
    grepl(
      "(unidentified|audience|member|participant)",
      transcript_df$speaker,
      ignore.case = T
    ),
    "anonymous participant",
    transcript_df$speaker
  )
transcript_df$speaker <-
  gsub("\\[\\d{0,3}\\]", "", transcript_df$speaker, fixed = FALSE)
transcript_df$speaker <- tolower(transcript_df$speaker)

# extract speaker type
transcript_df$speaker_type <-
  str_extract(transcript_df$speaker, "\\-\\s.*")
transcript_df$speaker_type <-
  gsub("\\-", "", transcript_df$speaker_type, fixed = FALSE)
transcript_df$speaker <-
  gsub("\\s\\-\\s.*", " ", transcript_df$speaker, fixed = FALSE)

# for sentiment analysis, we'd be mainly interested in sentiment of CEO/CFO/analyst.. lets refine these only
# populating wherever type is null...this is quite specific to VZ company
transcript_df$speaker_type <-
  ifelse((is.na(transcript_df$speaker_type)) &
           (
             grepl(
               "(verizon|company representative)",
               transcript_df$speaker,
               ignore.case = T
             )
           ), "verizon rep", transcript_df$speaker_type)
transcript_df$speaker_type <-
  ifelse((is.na(transcript_df$speaker_type)) &
           (
             grepl(
               "(unidentified|speaker|audience|member|participant)",
               transcript_df$speaker,
               ignore.case = T
             )
           ),
         "anonymous participant",
         transcript_df$speaker_type)
transcript_df$speaker_type <-
  ifelse((is.na(transcript_df$speaker_type)) &
           (grepl(
             "moderator", transcript_df$speaker, ignore.case = T
           )), "moderator", transcript_df$speaker_type)
transcript_df$speaker_type <-
  ifelse((is.na(transcript_df$speaker_type)) &
           (grepl(
             "\\beditor\\b", transcript_df$speaker, ignore.case = T
           )), "editor", transcript_df$speaker_type)
transcript_df$speaker_type <-
  ifelse((is.na(transcript_df$speaker_type)) &
           (grepl(
             "rural", transcript_df$speaker, ignore.case = T
           )), "rcc rep", transcript_df$speaker_type)
transcript_df$speaker_type <-
  ifelse((is.na(transcript_df$speaker_type)) &
           (
             grepl("richard.*eckst.*", transcript_df$speaker, ignore.case = T)
           ), "ceo", transcript_df$speaker_type)
transcript_df$speaker_type <-
  ifelse((is.na(transcript_df$speaker_type)) &
           (grepl(
             "john diercksen", transcript_df$speaker, ignore.case = T
           )), "svp", transcript_df$speaker_type)
transcript_df$speaker_type <-
  ifelse((is.na(transcript_df$speaker_type)) &
           (grepl(
             "fred salerno", transcript_df$speaker, ignore.case = T
           )), "verizon rep", transcript_df$speaker_type)
transcript_df$speaker_type <-
  ifelse((is.na(transcript_df$speaker_type)) &
           (grepl(
             "chris borass", transcript_df$speaker, ignore.case = T
           )), "rcc rep", transcript_df$speaker_type)
transcript_df$speaker_type <-
  ifelse((is.na(transcript_df$speaker_type)), "analyst", transcript_df$speaker_type)

# extract company name from speaker column - populate VZ vs Other
transcript_df$speaker_comp <-
  str_extract(transcript_df$speaker, ",(.*)")
transcript_df$speaker_comp <-
  gsub(",", "", transcript_df$speaker_comp, fixed = T)
transcript_df$speaker_comp <-
  gsub("\\[", "", transcript_df$speaker_comp, fixed = F)
transcript_df$speaker_comp <-
  gsub("\\]", "", transcript_df$speaker_comp, fixed = F)
transcript_df$speaker_comp <-
  ifelse(
    grepl(
      "(verizon|president|cfo)",
      transcript_df$speaker_comp,
      ignore.case = T
    ),
    "verizon",
    "Other"
  ) # replace with verizon or other
transcript_df$speaker_comp <-
  ifelse(
    grepl("aol", transcript_df$speaker_comp, ignore.case = T),
    "Other",
    transcript_df$speaker_comp
  ) # exception
transcript_df$speaker_comp <-
  stripWhitespace(transcript_df$speaker_comp)
transcript_df$speaker_comp <-
  trimws(transcript_df$speaker_comp, which = "both")

# extract speaker_name
transcript_df$speaker_name <-
  str_extract(transcript_df$speaker, "(.*),")
transcript_df$speaker_name <-
  gsub(",", "", transcript_df$speaker_name, fixed = T)
transcript_df$speaker_name <-
  gsub(" verizon comm.*", "", transcript_df$speaker_name, fixed = F)
transcript_df$speaker_name <-
  gsub("\\-", "", transcript_df$speaker_name, fixed = F)
transcript_df$speaker_name <-
  ifelse(
    is.na(transcript_df$speaker_name),
    "anonymous participant",
    transcript_df$speaker_name
  )
transcript_df$speaker_name <-
  stripWhitespace(transcript_df$speaker_name)
transcript_df$speaker_name <-
  trimws(transcript_df$speaker_name, which = "both")

# speaker type is inconsistent, lets re-populate for CEO/CFO
transcript_df$speaker_type <-
  ifelse((transcript_df$speaker_comp == "verizon") &
           (grepl(
             "ceo", transcript_df$speaker_type, ignore.case = T
           )),
         "ceo",
         transcript_df$speaker_type
  )
transcript_df$speaker_type <-
  ifelse((transcript_df$speaker_comp == "verizon") &
           (grepl(
             "cfo", transcript_df$speaker_type, ignore.case = T
           )),
         "cfo",
         transcript_df$speaker_type
  )

#reorder columns for readability
transcript_df <- transcript_df[, c(1, 2, 7, 3, 4, 10, 9, 8, 5, 6)]

# Note - speaker_type is only accurate for ceo/cfo/analyst positions - in leiu of sentiment analysis

# cleaning "text" column
transcript_df$text <-
  tolower(transcript_df$text) # convert to lowercase
transcript_df$text <-
  gsub("\\(.*\\)",
       " ",
       transcript_df$text,
       fixed = FALSE,
       ignore.case = TRUE) #remove all text in ()
transcript_df$text <-
  gsub("\\[.*\\]",
       " ",
       transcript_df$text,
       fixed = FALSE,
       ignore.case = TRUE) #remove all text in []
transcript_df$text <-
  gsub(
    "inaudible question",
    " ",
    transcript_df$text,
    fixed = FALSE,
    ignore.case = TRUE
  )  #remove remarks -> inaudible..
transcript_df$text <-
  gsub(
    "inaudible.*member",
    " ",
    transcript_df$text,
    fixed = FALSE,
    ignore.case = TRUE
  )   #remove remarks -> inaudible..
transcript_df$text <-
  gsub("\\d{2,}", " ", transcript_df$text, fixed = FALSE) # remove digits as 2 or more
transcript_df$text <-
  gsub("\\d\\.\\d{1,2}", " ", transcript_df$text, fixed = FALSE) # remove digits as 3.255 or 3.2
transcript_df$text <-
  gsub("$", " dollarsign ", transcript_df$text, fixed = T)
transcript_df$text <-
  gsub("%", " percent ", transcript_df$text, fixed = T)
transcript_df$text <-
  trimws(stripWhitespace(transcript_df$text), which = "both")  # remove extra whitespace
transcript_df <-
  subset(transcript_df, nchar(as.character(text)) > 1) # remove lines with a single character
transcript_df <-
  transcript_df[!apply(transcript_df["text"] == "", 1, all), ] # remove empty lines

# replace certain words -- observed after frequency analysis
transcript_df$text <-
  gsub("in line", "inline", transcript_df$text, fixed = T)    # in gets removed with stopwrds hence
transcript_df$text <-
  gsub("top line", "topline", transcript_df$text, fixed = T)
transcript_df$text <-
  gsub("bottom line", "bottomline", transcript_df$text, fixed = T)
transcript_df$text <-
  gsub("wi fi", "wifi", transcript_df$text, fixed = T)
transcript_df$text <-
  gsub("verizon wireless",
       "verizonwireless",
       transcript_df$text,
       fixed = T)                                                       # different subsidiary #1
transcript_df$text <-
  gsub("fios", "verizonfios", transcript_df$text, fixed = T)            # different subsidiary #2
transcript_df$text <-
  gsub("1g", "onegnetwork", transcript_df$text, fixed = T)
transcript_df$text <-
  gsub("2g", "twognetwork", transcript_df$text, fixed = T)
transcript_df$text <-
  gsub("3g", "threegnetwork", transcript_df$text, fixed = T)
transcript_df$text <-
  gsub("4g", "fourgnetwork", transcript_df$text, fixed = T)
transcript_df$text <-
  gsub("5g", "fivegnetwork", transcript_df$text, fixed = T)


# Part III: Preliminary Data Analysis 

library(zoo)
library(ggplot2)

# Plot 1 - Plotting number of docs released over time
myvars <- c("fileName", "transDate")
df1 <- transcript_df[myvars]
df1 = df1[!duplicated(df1$fileName),]
df1$qtr <- as.yearqtr(df1$transDate, format = "%Y-%m-%d")
df1$qtr = factor(df1$qtr)
df2 <- data.frame(table(df1$qtr))

ggplot(df2, aes(x = Var1, y = Freq, group = 1)) +
  coord_cartesian(ylim = c(0, 12)) +
  geom_line(color = "#69b3a2",
            size = 1,
            alpha = 0.8) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Number of Documents Released Over Time") +
  ylab("Number of Documents") +
  xlab("Quarter") +
  scale_y_continuous(breaks = seq(0, 12, 3)) +
  theme(axis.text = element_text(color = "grey20", size = 6),
        axis.title = element_text(color = "black", size = 12),
        title = element_text(size = 14))


# Plot 2 - Plotting number of documents for each type
df1 <- transcript_df[, c("fileName", "type")] 
df1 = unique(df1)
df2 = data.frame(table(df1$type))

ggplot(df1, aes(x=type, fill = type)) +
  geom_bar() +
  theme_gray() +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  ggtitle("Volume of document types released from 2002-2017") +
  ylab("Number of Documents") +
  xlab("Document Type") +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(color = "grey20", size = 10),
        axis.title = element_text(color = "black", size = 12),
        title = element_text(size = 12))



#Part IV: Frequency Analysis 

library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(textstem)
library(tm) 
library(wordcloud)
library(tidyr)

# Verizon Earnings - Unigram
vz_earn_df <- transcript_df[transcript_df$type == "VZ Earnings", ]

# Tokenize by word and store into df
vz_words <- vz_earn_df %>%
  unnest_tokens(word, text)

# Removing neutral frequent words
stopwords_neutral <-
  data.frame(
    word = c(
      "verizon",
      "percent",
      "dollarsign",
      "continue",
      "quarter",
      "revenue",
      "business",
      "margin",
      "question",
      "service",
      "million",
      "billion",
      "vice",
      "ii",
      "americas",
      "bye",
      "securities",
      "exchange",
      "business",
      "access",
      "ll",
      "talk",
      "ebitda"
    )
  )
stopwords_names <-
  data.frame(
    word = c(
      "matt",
      "jack",
      "doreen",
      "jackie",
      "brian",
      "stacey",
      "horan",
      "diercksen",
      "kathy",
      "rollins",
      "andy",
      "sandy",
      "victor",
      "larson",
      "kris",
      "cathy",
      "richard",
      "tom",
      "larry",
      "denny",
      "fred",
      "morgan",
      "laura",
      "ivan",
      "tim"
    )
  )

# lemmatize, remove stopwords and custom stop words, count, then bind tf_idf to ca_words_1
vz_words <- vz_words %>%
  mutate(word = lemmatize_words(vz_words$word)) %>%
  anti_join(stop_words) %>%
  anti_join(stopwords_neutral) %>%
  anti_join(stopwords_names) %>%
  mutate(word = gsub("(?!\\.)[[:punct:]]", "", word, perl = TRUE)) %>%
  count(fileName, word, sort = TRUE) %>%
  bind_tf_idf(word, fileName, n) %>%
  arrange(desc(tf_idf))

# wordcloud based on tfidf values instead of counts
pal2 <- brewer.pal(8, "Dark2")
wordcloud(
  vz_words$word,
  vz_words$tf_idf,
  scale = c(1.5, .5),
  max.words = 300,
  random.order = FALSE,
  rot.per = .15,
  colors = pal2
)

# Verizon Earnings - Bigram

# tokenize original "docDF" dataframe into bigrams and count frequencies of the bigrams
vz_bigram <- vz_earn_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(fileName, bigram, sort = TRUE)

vz_bigram_split <- vz_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Evaluate the split dataset and keep only those rows where neither word appears in the stop word list
vz_bigram_filtered <- vz_bigram_split %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# shouldn't use this list for LDA..removing neutral frequent words..
mystopwords <-
  data.frame(
    word = c(
      "verizon",
      "percent",
      "dollarsign",
      "continue",
      "question",
      "million",
      "billion",
      "vice",
      "ii",
      "americas",
      "bye",
      "matt",
      "jack",
      "doreen",
      "jackie",
      "brian",
      "stacey",
      "horan",
      "diercksen",
      "kathy",
      "rollins",
      "andy",
      "victor",
      "larson",
      "kris",
      "cathy",
      "richard",
      "tom",
      "larry",
      "denny",
      "fred",
      "morgan",
      "laura",
      "securities",
      "exchange",
      "business",
      "access",
      "ll",
      "talk",
      "ivan",
      "tim",
      "paper",
      "ebitda"
    )
  )

# Evaluate the filtered dataset and keep only those rows where neither word appears in our custom stop word list
vz_bigram_filtered <- vz_bigram_filtered %>%
  filter(!word1 %in% mystopwords$word) %>%
  filter(!word2 %in% mystopwords$word)

# copy the filtered dataframe
vz_bigram <- vz_bigram_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# Compute tf-idf for the bigram data set
vz_bigram <- vz_bigram %>%
  mutate(bigram = gsub("(?!\\.)[[:punct:]]", "", bigram, perl = TRUE)) %>%
  bind_tf_idf(bigram, fileName, n) %>%
  arrange(desc(tf_idf))

# wordcloud based on tfidf values instead of counts
pal2 <- brewer.pal(8, "Dark2")
wordcloud(
  vz_bigram$bigram,
  vz_bigram$tf_idf,
  scale = c(1.5, .1),
  max.words = 200,
  random.order = FALSE,
  rot.per = .30,
  color = pal2,
  fixed.asp = T
)


# Verizon at Conferences - Unigram

vz_conf_df <- transcript_df[transcript_df$type == "VZ Conference", ]

# Tokenize by word and store into df
vz_words <- vz_conf_df %>%
  unnest_tokens(word, text)

# lemmatize, remove stopwords and custom stop words, count, then bind tf_idf
vz_words <- vz_words %>%
  mutate(word = lemmatize_words(vz_words$word)) %>%
  anti_join(stop_words) %>%
  anti_join(stopwords_neutral) %>%
  anti_join(stopwords_names) %>%
  mutate(word = gsub("(?!\\.)[[:punct:]]", "", word, perl = TRUE)) %>%
  count(fileName, word, sort = TRUE) %>%
  bind_tf_idf(word, fileName, n) %>%
  arrange(desc(tf_idf))

# wordcloud based on tfidf values instead of counts
pal2 <- brewer.pal(8, "Dark2")
wordcloud(
  vz_words$word,
  vz_words$tf_idf,
  scale = c(1.5, .5),
  max.words = 300,
  random.order = FALSE,
  rot.per = .15,
  colors = pal2
)

# Verizon at Conferences - Bigram

# tokenize original "docDF" dataframe into bigrams and count frequencies of the bigrams
vz_bigram <- vz_conf_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(fileName, bigram, sort = TRUE)

vz_bigram_split <- vz_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Evaluate the split dataset and keep only those rows where neither word appears in the stop word list
vz_bigram_filtered <- vz_bigram_split %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Evaluate the filtered dataset and keep only those rows where neither word appears in our custom stop word list
vz_bigram_filtered <- vz_bigram_filtered %>%
  filter(!word1 %in% mystopwords$word) %>%
  filter(!word2 %in% mystopwords$word)

# copy the filtered dataframe
vz_bigram <- vz_bigram_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# Compute tf-idf for the bigram data set
vz_bigram <- vz_bigram %>%
  mutate(bigram = gsub("(?!\\.)[[:punct:]]", "", bigram, perl = TRUE)) %>%
  bind_tf_idf(bigram, fileName, n) %>%
  arrange(desc(tf_idf))

# wordcloud based on tfidf values instead of counts
wordcloud(
  vz_bigram$bigram,
  vz_bigram$tf_idf,
  scale = c(1.5, .1),
  max.words = 200,
  random.order = FALSE,
  rot.per = .30,
  color = pal2,
  fixed.asp = T
)

#Part V: LDA Topic Modeling 


library(dplyr)
library(tidytext)
library(textstem)
library(ggplot2)
library(broom)
library(topicmodels)
library(tidyr)

# VZ EARNINGS
vz_earn_df <- transcript_df[transcript_df$type == "VZ Earnings", ]

# Stopwords
custom_stopwords <-
  data.frame(
    word = c(
      "verizon",
      "percent",
      "dollarsign",
      "continue",
      "question",
      "million",
      "billion",
      "vice",
      "ii",
      "americas",
      "bye",
      "matt",
      "jack",
      "doreen",
      "jackie",
      "brian",
      "stacey",
      "horan",
      "diercksen",
      "kathy",
      "rollins",
      "andy",
      "victor",
      "larson",
      "kris",
      "cathy",
      "richard",
      "tom",
      "larry",
      "denny",
      "fred",
      "morgan",
      "laura",
      "securities",
      "exchange",
      "talk",
      "ivan",
      "tim",
      "paper",
      "fran",
      "prior",
      "sequential",
      "sequentially",
      "penetration",
      "lowell",
      "view",
      "home",
      "impact",
      "john",
      "shammo",
      "half",
      "quarter",
      "sort",
      "provide",
      "feel",
      "focus"
    )
  )
# tokenize into words
vz_words <- vz_earn_df %>%
  unnest_tokens(word, text)

# Remove stop words,punctuations,etc and find document-word counts
word_count <- vz_words %>%
  mutate(word = lemmatize_words(vz_words$word)) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stopwords) %>%
  mutate(word = gsub("(?!\\.)[[:punct:]]", "", word, perl = TRUE)) %>%
  count(fileName, doctitle, word, sort = TRUE)

# compute tf-idf for each token
word_count <- word_count %>%
  bind_tf_idf(word, fileName, n)

# drop all tokens in the bottom quartile of tf-idf score
word_count <- word_count %>%
  filter(tf_idf > quantile(tf_idf, probs = 0.30))

# create document-term matrix
dtm_vz <- word_count %>%
  cast_dtm(doctitle, word, n)

# run LDA specifying k
adv_vz_lda <-
  LDA(
    dtm_vz,
    k = 10,
    method = "Gibbs",
    control = list(iter = 2000, seed = 1234)
  )

# construct data frame of word-topic probabilities (beta)
adv_vz_topics <- tidy(adv_vz_lda, matrix = "beta")

# select top five terms in each topic
adv_top_terms <- adv_vz_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic,-beta)

# visualize the top terms 
adv_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8,
           stat = "identity",
           show.legend = FALSE) +
  facet_wrap( ~ topic, scales = "free", ncol = 3) +
  ggtitle("VZ Earnings - Top terms for topics") +
  coord_flip()

# Lets look at the number of documents that contain the topics
# create data frame showing probabilities that an article discusses a topic (gamma)
articles_gamma <- tidy(adv_vz_lda, matrix = "gamma")

# document labels/titles for each topic number
k <- 10
labels <-
  c(
    "Network Delivery",
    "VZ Fios broadband",
    "4g EDGE on Fios",
    "Issue with DSL expense reduction",
    "Operational assets",
    "Wirelines - Alltel",
    "Pension Fund Contract",
    "Curating Video Content", 
    "Access to DSL - local services", 
    "LTE 4G network"
  )


# replace numeric topic labels with text labels specified above
for (i in 2:9) {
  articles_gamma$topic <-
    gsub(i, labels[i], articles_gamma$topic, fixed = T)
}

articles_gamma$topic <-
  gsub("10", labels[10], articles_gamma$topic, fixed = T)

articles_gamma$topic <-
  gsub("1", labels[1], articles_gamma$topic, fixed = T)


# View the distribution of the gamma values to roughly decide a threshold value
ggplot(articles_gamma, aes(x = gamma)) +
  geom_density()

# allow articles to fall into multiple topics, based on a pre-defined threshold value (in this case above 0.15)
article_classification_multi <- articles_gamma %>%
  filter(gamma > .15)

# Display num articles from overlapping classification
article_classification_multi %>%
  group_by(topic) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = factor(topic), y = n)) + geom_col() +
  xlab("Topic") + ylab("Number of Documents") +
  ggtitle("VZ Earnings - Number of Documents Per Topic (Overlapping)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# VZ CONFERENCE
vz_conf_df <- transcript_df[transcript_df$type == "VZ Conference", ]

# tokenize into words
vz_words <- vz_conf_df %>%
  unnest_tokens(word, text)

# Remove stop words,punctuations,etc and find document-word counts
word_count <- vz_words %>%
  mutate(word = lemmatize_words(vz_words$word)) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stopwords) %>%
  mutate(word = gsub("(?!\\.)[[:punct:]]", "", word, perl = TRUE)) %>%
  count(fileName, doctitle, word, sort = TRUE)

# compute tf-idf for each token
word_count <- word_count %>%
  bind_tf_idf(word, fileName, n)

# drop all tokens in the bottom quartile of tf-idf score
word_count <- word_count %>%
  filter(tf_idf > quantile(tf_idf, probs = 0.30))

# create document-term matrix
dtm_vz <- word_count %>%
  cast_dtm(doctitle, word, n)

# run LDA using the "best fitting" model
adv_vz_lda <-
  LDA(
    dtm_vz,
    k = 10,
    method = "Gibbs",
    control = list(iter = 2000, seed = 1234)
  )
adv_vz_lda

# construct data frame of word-topic probabilities (beta)
adv_vz_topics <- tidy(adv_vz_lda, matrix = "beta")

# select top five terms in each topic
adv_top_terms <- adv_vz_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic,-beta)

# visualize the same
adv_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8,
           stat = "identity",
           show.legend = FALSE) +
  facet_wrap( ~ topic, scales = "free", ncol = 3) +
  ggtitle("VZ Conference - Top terms for topics") +
  coord_flip()

# Lets look at the number of documents that contain the topics
# create data frame showing probabilities that an article discusses a topic (gamma)
articles_gamma <- tidy(adv_vz_lda, matrix = "gamma")

# document labels/titles for each topic number
k <- 10
labels <-
  c("Spectrum transaction","Strategy - Mobile Content Curator","VZ Fios - Revenue Increase",
    "VZ Fios - cable issues","Video on device thrugh 4G LTE","LTE Spectrum 3g",
    "Broadband technology - VZ Fibre","Growth in wireline enterprise","Fios LTE - price margin impact",
    "Cloud")

# replace numeric topic labels with text labels specified above
for (i in 2:9) {
  articles_gamma$topic <-
    gsub(i, labels[i], articles_gamma$topic, fixed = T)
}

articles_gamma$topic <-
  gsub("10", labels[10], articles_gamma$topic, fixed = T)

articles_gamma$topic <-
  gsub("1", labels[1], articles_gamma$topic, fixed = T)


# View the distribution of the gamma values to roughly decide a threshold value
ggplot(articles_gamma, aes(x = gamma)) +
  geom_density()

# allow articles to fall into multiple topics, based on a pre-defined threshold value (in this case above 0.15)
article_classification_multi <- articles_gamma %>%
  filter(gamma > .15)

# Display num articles from overlapping classification
article_classification_multi %>%
  group_by(topic) %>%
  tally %>%
  ggplot(aes(x = factor(topic), y = n)) + geom_col() +
  xlab("Topic") + ylab("Number of Documents") +
  ggtitle("VZ Conference - Number of Documents Per Topic (Overlapping)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 

#Part VI: Topic Sentiment Analysis
"""
In this analysis, we make use of the topics derived from the LDA TM analysis and  take a look at the trend and the evolution of the sentiment of topics relevant to VZ and the Telecom Industry. We further examine the changing sentiment of these topics over the years and try to tie it back to important events that could have driven the trend of the sentiment or have been caused as a result of it. We look at the following topics: 
  1.Verizon Products or Subsidiaries -  Verizon Fios, Quad Play, Verizon Wireless, LTE Network
2.Industry wide metrics reported by VZ - User Retention, User Churn, Cable, 3.Industry related news - Spectrum Auction, Pension
4.Acquisitions - Genuity, MCI, Alltel, Yahoo, AOL
"""

library(sentimentr)
library(lexicon)
library(ggplot2)

unnested_transcript_df <- transcript_df %>%
  unnest_tokens(sentence, text, token = "regex", pattern = "[.]") # unnest for sentences

# Topic 1 : VERIZON FIOS
# Create df with text containing topic
df_topic_sent <- unnested_transcript_df
df_topic_sent <-
  df_topic_sent[grep("(verizonfios|fios|triple play)", df_topic_sent$sentence), ]
df_topic_sent$text <-
  gsub("[[:punct:]]", "", df_topic_sent$sentence, fixed = F)

# Compute sentiment
df_topic_sent$sentiment <-
  sentiment(
    df_topic_sent$text,
    polarity_dt = hash_sentiment_loughran_mcdonald,
    valence_shifters_dt = lexicon::hash_valence_shifters
  )$sentiment

# Drop unnecessary columns
keeps <- c("transDate", "type", "sentiment")
df_topic_sent <- df_topic_sent[, (names(df_topic_sent) %in% keeps)]

# Compute daily average sentiment
df_topic_sent <- df_topic_sent %>%
  group_by(transDate, type) %>%
  summarize(sentiment = average_downweighted_zero(sentiment))

# Visualise sentiment
colour <- c("#5F9EA0", "#f21a5e", "#FFC300")
# 5F9EA0 : blue (other) , #FFC300 : yellow (earnings) , #f21a5e : pink (conf)

df_topic_sent %>%
  filter (!type == "RCC Earnings") %>%
  ggplot(aes(x = transDate, y = sentiment, color = type)) +
  stat_smooth (n = 10,
               show.legend = TRUE,
               se = FALSE) +
  labs(title = "Verizon FIOS  - Sentiment Over Time", x = "Year", y = "Sentiment") +
  scale_x_date(date_breaks = "years" , date_labels = "%Y") +
  theme_gray() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(values = colour) +
  geom_hline(yintercept = 0, linetype = 2)


# Topic 2 : QUAD PLAY
# Create df with text containing topic
df_topic_sent <- unnested_transcript_df
df_topic_sent <-
  df_topic_sent[grep("(quad play|bundle)", df_topic_sent$sentence), ]
df_topic_sent$text <-
  gsub("[[:punct:]]", "", df_topic_sent$sentence, fixed = F)

# Compute sentiment
df_topic_sent$sentiment <-
  sentiment(
    df_topic_sent$text,
    polarity_dt = hash_sentiment_loughran_mcdonald,
    valence_shifters_dt = lexicon::hash_valence_shifters
  )$sentiment

# Drop unnecessary columns
keeps <- c("transDate", "type", "sentiment")
df_topic_sent <- df_topic_sent[, (names(df_topic_sent) %in% keeps)]

# Compute daily average sentiment
df_topic_sent <- df_topic_sent %>%
  group_by(transDate, type) %>%
  summarize(sentiment = average_downweighted_zero(sentiment))

# Visualise sentiment
colour <- c("#5F9EA0", "#f21a5e", "#FFC300")

df_topic_sent %>%
  filter (!type == "RCC Earnings") %>%
  ggplot(aes(x = transDate, y = sentiment, color = type)) +
  stat_smooth (n = 10,
               show.legend = TRUE,
               se = FALSE) +
  labs(title = "Quad Play (Bundles) - Sentiment Over Time", x = "Year", y = "Sentiment") +
  scale_x_date(date_breaks = "years" , date_labels = "%Y") +
  theme_gray() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(values = colour) +
  geom_hline(yintercept = 0, linetype = 2)


# Topic 3 : VERIZON WIRELESS
# Create df with text containing topic
df_topic_sent <- unnested_transcript_df
df_topic_sent <-
  df_topic_sent[grep("(verizonwireless|wireless|broadband|data)",
                     df_topic_sent$sentence), ]
df_topic_sent$text <-
  gsub("[[:punct:]]", "", df_topic_sent$sentence, fixed = F)

# Compute sentiment
df_topic_sent$sentiment <-
  sentiment(
    df_topic_sent$text,
    polarity_dt = hash_sentiment_loughran_mcdonald,
    valence_shifters_dt = lexicon::hash_valence_shifters
  )$sentiment

# Drop unnecessary columns
keeps <- c("transDate", "type", "sentiment")
df_topic_sent <- df_topic_sent[, (names(df_topic_sent) %in% keeps)]

# Compute daily average sentiment
df_topic_sent <- df_topic_sent %>%
  group_by(transDate, type) %>%
  summarize(sentiment = average_downweighted_zero(sentiment))

# Visualise sentiment
colour <- c("#5F9EA0", "#f21a5e", "#FFC300")

df_topic_sent %>%
  filter (!type == "RCC Earnings") %>%
  ggplot(aes(x = transDate, y = sentiment, color = type)) +
  stat_smooth (n = 10,
               show.legend = TRUE,
               se = FALSE) +
  labs(title = "Verizon Wireless - Sentiment Over Time", x = "Year", y = "Sentiment") +
  scale_x_date(date_breaks = "years" , date_labels = "%Y") +
  theme_gray() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(values = colour) +
  geom_hline(yintercept = 0, linetype = 2)


# Topic 4 : LTE NETWORK
# Create df with text containing topic
df_topic_sent <- unnested_transcript_df
df_topic_sent <-
  df_topic_sent[grep("(\\blte\\b|fourgnetwork|4g)", df_topic_sent$sentence), ]
df_topic_sent$text <-
  gsub("[[:punct:]]", "", df_topic_sent$sentence, fixed = F)

# Compute sentiment
df_topic_sent$sentiment <-
  sentiment(
    df_topic_sent$text,
    polarity_dt = hash_sentiment_loughran_mcdonald,
    valence_shifters_dt = lexicon::hash_valence_shifters
  )$sentiment

# Drop unnecessary columns
keeps <- c("transDate", "type", "sentiment")
df_topic_sent <- df_topic_sent[, (names(df_topic_sent) %in% keeps)]

# Compute daily average sentiment
df_topic_sent <- df_topic_sent %>%
  group_by(transDate, type) %>%
  summarize(sentiment = average_downweighted_zero(sentiment))

# Visualise sentiment
colour <- c("#5F9EA0", "#f21a5e", "#FFC300")

df_topic_sent %>%
  filter (!type == "RCC Earnings") %>%
  ggplot(aes(x = transDate, y = sentiment, color = type)) +
  stat_smooth (n = 10,
               show.legend = TRUE,
               se = FALSE) +
  labs(title = "VZ LTE Network  - Sentiment Over Time", x = "Year", y = "Sentiment") +
  scale_x_date(date_breaks = "years" , date_labels = "%Y") +
  theme_gray() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(values = colour) +
  geom_hline(yintercept = 0, linetype = 2)


# Topic 5 : USER RETENTION
# Create df with text containing topic
df_topic_sent <- unnested_transcript_df
df_topic_sent <-
  df_topic_sent[grep("retention", df_topic_sent$sentence), ]
df_topic_sent$text <-
  gsub("[[:punct:]]", "", df_topic_sent$sentence, fixed = F)

# Compute sentiment
df_topic_sent$sentiment <-
  sentiment(
    df_topic_sent$text,
    polarity_dt = hash_sentiment_loughran_mcdonald,
    valence_shifters_dt = lexicon::hash_valence_shifters
  )$sentiment

# Drop unnecessary columns
keeps <- c("transDate", "type", "sentiment")
df_topic_sent <- df_topic_sent[, (names(df_topic_sent) %in% keeps)]

# Compute daily average sentiment
df_topic_sent <- df_topic_sent %>%
  group_by(transDate, type) %>%
  summarize(sentiment = average_downweighted_zero(sentiment))

# Visualise sentiment
colour <- c("#f21a5e", "#FFC300")

df_topic_sent %>%
  filter (!(type == "RCC Earnings" |
              type == "Other/Announcement")) %>%
  ggplot(aes(x = transDate, y = sentiment, color = type)) +
  stat_smooth (n = 10,
               show.legend = TRUE,
               se = FALSE) +
  labs(title = "User Retention - Sentiment Over Time", x = "Year", y = "Sentiment") +
  scale_x_date(date_breaks = "years" , date_labels = "%Y") +
  theme_gray() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(values = colour) +
  geom_hline(yintercept = 0, linetype = 2)


# Topic 6 : USER CHUR
# Create df with text containing topic
df_topic_sent <- unnested_transcript_df
df_topic_sent <-
  df_topic_sent[grep("churn", df_topic_sent$sentence), ]
df_topic_sent$text <-
  gsub("[[:punct:]]", "", df_topic_sent$sentence, fixed = F)

# Compute sentiment
df_topic_sent$sentiment <-
  sentiment(
    df_topic_sent$text,
    polarity_dt = hash_sentiment_loughran_mcdonald,
    valence_shifters_dt = lexicon::hash_valence_shifters
  )$sentiment

# Drop unnecessary columns
keeps <- c("transDate", "type", "sentiment")
df_topic_sent <- df_topic_sent[, (names(df_topic_sent) %in% keeps)]

# Compute daily average sentiment
df_topic_sent <- df_topic_sent %>%
  group_by(transDate, type) %>%
  summarize(sentiment = average_downweighted_zero(sentiment))

# Visualise sentiment
colour <- c("#f21a5e", "#FFC300")

df_topic_sent %>%
  filter (!(type == "RCC Earnings" |
              type == "Other/Announcement")) %>%
  ggplot(aes(x = transDate, y = sentiment, color = type)) +
  stat_smooth (n = 10,
               show.legend = TRUE,
               se = FALSE) +
  labs(title = "User Churn - Sentiment Over Time", x = "Year", y = "Sentiment") +
  scale_x_date(date_breaks = "years" , date_labels = "%Y") +
  theme_gray() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(values = colour) +
  geom_hline(yintercept = 0, linetype = 2)


# Topic 7 : CABLE
# Create df with text containing topic
df_topic_sent <- unnested_transcript_df
df_topic_sent <-
  df_topic_sent[grep("cable", df_topic_sent$sentence), ]
df_topic_sent$text <-
  gsub("[[:punct:]]", "", df_topic_sent$sentence, fixed = F)

# Compute sentiment
df_topic_sent$sentiment <-
  sentiment(
    df_topic_sent$text,
    polarity_dt = hash_sentiment_loughran_mcdonald,
    valence_shifters_dt = lexicon::hash_valence_shifters
  )$sentiment

# Drop unnecessary columns
keeps <- c("transDate", "type", "sentiment")
df_topic_sent <- df_topic_sent[, (names(df_topic_sent) %in% keeps)]

# Compute daily average sentiment
df_topic_sent <- df_topic_sent %>%
  group_by(transDate, type) %>%
  summarize(sentiment = average_downweighted_zero(sentiment))

# Visualise sentiment
colour <- c("#5F9EA0", "#f21a5e", "#FFC300")

df_topic_sent %>%
  filter (!type == "RCC Earnings") %>%
  ggplot(aes(x = transDate, y = sentiment, color = type)) +
  stat_smooth (n = 10,
               show.legend = TRUE,
               se = FALSE) +
  labs(title = "Cable - Sentiment Over Time", x = "Year", y = "Sentiment") +
  scale_x_date(date_breaks = "years" , date_labels = "%Y") +
  theme_gray() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(values = colour) +
  geom_hline(yintercept = 0, linetype = 2)


# Topic 8 : SPECTRUM AUCTION
# Create df with text containing topic
df_topic_sent <- unnested_transcript_df
df_topic_sent <-
  df_topic_sent[grep("(spectrum|auction)", df_topic_sent$sentence), ]
df_topic_sent$text <-
  gsub("[[:punct:]]", "", df_topic_sent$sentence, fixed = F)

# Compute sentiment
df_topic_sent$sentiment <-
  sentiment(
    df_topic_sent$text,
    polarity_dt = hash_sentiment_loughran_mcdonald,
    valence_shifters_dt = lexicon::hash_valence_shifters
  )$sentiment

# Drop unnecessary columns
keeps <- c("transDate", "type", "sentiment")
df_topic_sent <- df_topic_sent[, (names(df_topic_sent) %in% keeps)]

# Compute daily average sentiment
df_topic_sent <- df_topic_sent %>%
  group_by(transDate, type) %>%
  summarize(sentiment = average_downweighted_zero(sentiment))

# Visualise sentiment
colour <- c("#5F9EA0", "#f21a5e", "#FFC300")

df_topic_sent %>%
  filter (!type == "RCC Earnings") %>%
  ggplot(aes(x = transDate, y = sentiment, color = type)) +
  stat_smooth (n = 10,
               show.legend = TRUE,
               se = FALSE) +
  labs(title = "Spectrum Auction - Sentiment Over Time", x = "Year", y = "Sentiment") +
  scale_x_date(date_breaks = "years" , date_labels = "%Y") +
  theme_gray() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(values = colour) +
  geom_hline(yintercept = 0, linetype = 2)


# Topic 9 : GENUITY
# Create df with text containing topic
df_topic_sent <- unnested_transcript_df
df_topic_sent <-
  df_topic_sent[grep("genuity", df_topic_sent$sentence), ]
df_topic_sent$text <-
  gsub("[[:punct:]]", "", df_topic_sent$sentence, fixed = F)

# Compute sentiment
df_topic_sent$sentiment <-
  sentiment(
    df_topic_sent$text,
    polarity_dt = hash_sentiment_loughran_mcdonald,
    valence_shifters_dt = lexicon::hash_valence_shifters
  )$sentiment

# Drop unnecessary columns
keeps <- c("transDate", "type", "sentiment")
df_topic_sent <- df_topic_sent[, (names(df_topic_sent) %in% keeps)]

# Compute daily average sentiment
df_topic_sent <- df_topic_sent %>%
  group_by(transDate, type) %>%
  summarize(sentiment = average_downweighted_zero(sentiment))

# Visualise sentiment
colour <- c("#5F9EA0", "#f21a5e", "#FFC300")

df_topic_sent %>%
  filter (!type == "RCC Earnings") %>%
  filter (transDate < '2007-01-01') %>%
  ggplot(aes(x = transDate, y = sentiment, color = type)) +
  stat_smooth (n = 20,
               show.legend = TRUE,
               se = FALSE) +
  labs(title = "Spectrum Auction - Sentiment Over Time", x = "Year", y = "Sentiment") +
  scale_x_date(date_breaks = "months" , date_labels = "%m-%Y") +
  theme_gray() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(values = colour) +
  geom_hline(yintercept = 0, linetype = 2)


# Topic 10 : MCI
# Create df with text containing topic
df_topic_sent <- unnested_transcript_df
df_topic_sent <- df_topic_sent[grep("mci", df_topic_sent$sentence), ]
df_topic_sent$text <-
  gsub("[[:punct:]]", "", df_topic_sent$sentence, fixed = F)

# Compute sentiment
df_topic_sent$sentiment <-
  sentiment(
    df_topic_sent$text,
    polarity_dt = hash_sentiment_loughran_mcdonald,
    valence_shifters_dt = lexicon::hash_valence_shifters
  )$sentiment

# Drop unnecessary columns
keeps <- c("transDate", "type", "sentiment")
df_topic_sent <- df_topic_sent[, (names(df_topic_sent) %in% keeps)]

# Compute daily average sentiment
df_topic_sent <- df_topic_sent %>%
  group_by(transDate, type) %>%
  summarize(sentiment = average_downweighted_zero(sentiment))

# Visualise sentiment
colour <- c("#5F9EA0", "#f21a5e", "#FFC300")

df_topic_sent %>%
  filter (!type == "RCC Earnings") %>%
  ggplot(aes(x = transDate, y = sentiment, color = type)) +
  stat_smooth (n = 10,
               show.legend = TRUE,
               se = FALSE) +
  labs(title = "MCI - Sentiment Over Time", x = "Year", y = "Sentiment") +
  scale_x_date(date_breaks = "years" , date_labels = "%Y") +
  theme_gray() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(values = colour) +
  geom_hline(yintercept = 0, linetype = 2)


# Topic 11 : ALLTEL
# Create df with text containing topic
df_topic_sent <- unnested_transcript_df
df_topic_sent <-
  df_topic_sent[grep("alltel", df_topic_sent$sentence), ]
df_topic_sent$text <-
  gsub("[[:punct:]]", "", df_topic_sent$sentence, fixed = F)

# Compute sentiment
df_topic_sent$sentiment <-
  sentiment(
    df_topic_sent$text,
    polarity_dt = hash_sentiment_loughran_mcdonald,
    valence_shifters_dt = lexicon::hash_valence_shifters
  )$sentiment

# Drop unnecessary columns
keeps <- c("transDate", "type", "sentiment")
df_topic_sent <- df_topic_sent[, (names(df_topic_sent) %in% keeps)]

# Compute daily average sentiment
df_topic_sent <- df_topic_sent %>%
  group_by(transDate, type) %>%
  summarize(sentiment = average_downweighted_zero(sentiment))

# Visualise sentiment
colour <- c("#5F9EA0", "#f21a5e", "#FFC300")

df_topic_sent %>%
  filter (!type == "RCC Earnings") %>%
  ggplot(aes(x = transDate, y = sentiment, color = type)) +
  stat_smooth (n = 10,
               show.legend = TRUE,
               se = FALSE) +
  labs(title = "Alltel - Sentiment Over Time", x = "Year", y = "Sentiment") +
  scale_x_date(date_breaks = "years" , date_labels = "%Y") +
  theme_gray() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(values = colour) +
  geom_hline(yintercept = 0, linetype = 2)


# Topic 12 : YAHOO
# Create df with text containing topic
df_topic_sent <- unnested_transcript_df
df_topic_sent <-
  df_topic_sent[grep("yahoo", df_topic_sent$sentence), ]
df_topic_sent$text <-
  gsub("[[:punct:]]", "", df_topic_sent$sentence, fixed = F)

# Compute sentiment
df_topic_sent$sentiment <-
  sentiment(
    df_topic_sent$text,
    polarity_dt = hash_sentiment_loughran_mcdonald,
    valence_shifters_dt = lexicon::hash_valence_shifters
  )$sentiment

# Drop unnecessary columns
keeps <- c("transDate", "type", "sentiment")
df_topic_sent <- df_topic_sent[, (names(df_topic_sent) %in% keeps)]

# Compute daily average sentiment
df_topic_sent <- df_topic_sent %>%
  group_by(transDate, type) %>%
  summarize(sentiment = average_downweighted_zero(sentiment))

# Visualise sentiment
colour <- c("#5F9EA0", "#f21a5e", "#FFC300")

df_topic_sent %>%
  filter (!(type == "RCC Earnings")) %>%
  filter (transDate > "2015-01-01") %>%
  ggplot(aes(x = transDate, y = sentiment, color = type)) +
  stat_smooth (n = 10,
               show.legend = TRUE,
               se = FALSE) +
  labs(title = "Yahoo - Sentiment Over Time", x = "Year", y = "Sentiment") +
  scale_x_date(date_breaks = "months" , date_labels = "%m-%Y") +
  theme_gray() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(values = colour) +
  geom_hline(yintercept = 0, linetype = 2)


# Topic 13 : AOL
# Create df with text containing topic
df_topic_sent <- unnested_transcript_df
df_topic_sent <- df_topic_sent[grep("aol", df_topic_sent$sentence), ]
df_topic_sent$text <-
  gsub("[[:punct:]]", "", df_topic_sent$sentence, fixed = F)

# Compute sentiment
df_topic_sent$sentiment <-
  sentiment(
    df_topic_sent$text,
    polarity_dt = hash_sentiment_loughran_mcdonald,
    valence_shifters_dt = lexicon::hash_valence_shifters
  )$sentiment

# Drop unnecessary columns
keeps <- c("transDate", "type", "sentiment")
df_topic_sent <- df_topic_sent[, (names(df_topic_sent) %in% keeps)]

# Compute daily average sentiment
df_topic_sent <- df_topic_sent %>%
  group_by(transDate, type) %>%
  summarize(sentiment = average_downweighted_zero(sentiment))

# Visualise sentiment
colour <- c("#f21a5e", "#FFC300")

df_topic_sent %>%
  filter (!(type == "RCC Earnings" |
              type == "Other/Announcement")) %>%
  filter (transDate > "2015-01-01") %>%
  ggplot(aes(x = transDate, y = sentiment, color = type)) +
  stat_smooth (n = 10,
               show.legend = TRUE,
               se = FALSE) +
  labs(title = "AOL - Sentiment Over Time", x = "Year", y = "Sentiment") +
  scale_x_date(date_breaks = "months" , date_labels = "%m-%Y") +
  theme_gray() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(values = colour) +
  geom_hline(yintercept = 0, linetype = 2)


# Topic 14 : PENSION
# Create df with text containing topic
df_topic_sent <- unnested_transcript_df
df_topic_sent <-
  df_topic_sent[grep("pension", df_topic_sent$sentence), ]
df_topic_sent$text <-
  gsub("[[:punct:]]", "", df_topic_sent$sentence, fixed = F)

# Compute sentiment
df_topic_sent$sentiment <-
  sentiment(
    df_topic_sent$text,
    polarity_dt = hash_sentiment_loughran_mcdonald,
    valence_shifters_dt = lexicon::hash_valence_shifters
  )$sentiment

# Drop unnecessary columns
keeps <- c("transDate", "type", "sentiment")
df_topic_sent <- df_topic_sent[, (names(df_topic_sent) %in% keeps)]

# Compute daily average sentiment
df_topic_sent <- df_topic_sent %>%
  group_by(transDate, type) %>%
  summarize(sentiment = average_downweighted_zero(sentiment))

# Visualise sentiment
colour <- c("#f21a5e", "#FFC300")

df_topic_sent %>%
  filter (!(type == "RCC Earnings" |
              type == "Other/Announcement")) %>%
  filter (transDate > "2015-01-01") %>%
  ggplot(aes(x = transDate, y = sentiment, color = type)) +
  stat_smooth (n = 10,
               show.legend = TRUE,
               se = FALSE) +
  labs(title = "Pension Contracts/Expenses - Sentiment Over Time", x = "Year", y = "Sentiment") +
  scale_x_date(date_breaks = "months" , date_labels = "%m-%Y") +
  theme_gray() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(values = colour) +
  geom_hline(yintercept = 0, linetype = 2)

```



#Part VII: Speaker (CEO/CFO) Sentiment Analysis 

library(tidyverse)
library(stringr)
library(dplyr)
library(sentimentr)
library(lexicon)
library(ggplot2)
library(reshape2)
library(tm)
library(lubridate)
library(scales)
library(tidyquant)

# Calculate sentiment using Loughran McDonald lexicon meant for financial texts
lm_sentiment <-
sentiment(
get_sentences(transcript_df),
polarity_dt = hash_sentiment_loughran_mcdonald,
valence_shifters_dt = lexicon::hash_valence_shifters
)

# Filter results for VZ speakers only
verizon_sentiment <- lm_sentiment %>%
filter(speaker_comp == "verizon")

# Sentiment for CEO/CFO at VZ Earnings
verizon_sentiment %>%
filter(verizon_sentiment$type == "VZ Earnings") %>%
filter(speaker_type == "cfo" | speaker_type == "ceo") %>%
group_by(transDate) %>%
summarize(sentiment = average_downweighted_zero(sentiment)) %>%
ggplot(aes(x = transDate, y = sentiment)) +
geom_line() +
labs(title = "VZ Earnings - CEO & CFO Sentiment", x = "Year", y = "Sentiment") +
theme(
legend.position = "bottom",
legend.direction = "horizontal",
legend.title = element_blank()
) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
axis.text = element_text(color = "grey20", size = 12),
axis.title = element_text(color = "black", size = 12),
title = element_text(size = 14)) +
scale_x_date(date_breaks = "years" , date_labels = "%Y") 

# Sentiment for CEO/CFO at VZ Conferences
verizon_sentiment %>%
filter(verizon_sentiment$type == "VZ Conference") %>%
filter(speaker_type == "cfo" | speaker_type == "ceo") %>%
group_by(transDate) %>%
summarize(sentiment = average_downweighted_zero(sentiment)) %>%
ggplot(aes(x = transDate, y = sentiment)) +
geom_line() +
stat_smooth(
show.legend = TRUE,
se = FALSE,
span = 0.2,
color = "darkred",
size = 0.75
) +
labs(title = "VZ Conferences - CEO & CFO Sentiment", x = "Year", y = "Sentiment") +
theme(
legend.position = "bottom",
legend.direction = "horizontal",
legend.title = element_blank()
) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
axis.text = element_text(color = "grey20", size = 12),
axis.title = element_text(color = "black", size = 12),
title = element_text(size = 14)) +
scale_x_date(date_breaks = "years" , date_labels = "%Y")

# Sentiment for CEO/CFO under Misc Announcements
verizon_sentiment %>%
filter(verizon_sentiment$type == "Other/Announcement") %>%
filter(speaker_type == "cfo" | speaker_type == "ceo") %>%
group_by(transDate) %>%
summarize(sentiment = average_downweighted_zero(sentiment)) %>%
ggplot(aes(x = transDate, y = sentiment)) +
geom_line() +
labs(title = "Announcements and Other Documents - CEO & CFO Sentiment", x = "Year", y = "Sentiment")  +
theme(
legend.position = "bottom",
legend.direction = "horizontal",
legend.title = element_blank()
) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
axis.text = element_text(color = "grey20", size = 12),
axis.title = element_text(color = "black", size = 12),
title = element_text(size = 10)) +
scale_x_date(date_breaks = "years" , date_labels = "%Y")

# Plotting event - The Verizon wokers strike in August of 2011 on previous plot
verizon_sentiment %>%
filter(verizon_sentiment$type == "Other/Announcement") %>%
filter(speaker_type == "cfo" | speaker_type == "ceo") %>%
group_by(transDate) %>%
summarize(sentiment = average_downweighted_zero(sentiment)) %>%
ggplot(aes(x = transDate, y = sentiment)) +
geom_line(size = 0.5) +
geom_vline(
xintercept = as.Date("2011-08-07"),
size = 1,
color = "blue",
linetype = 3
) +
labs(title = "Announcements and Other Documents - CEO and CFO Sentiment", x = "Year", y = "Sentiment") +
theme(
legend.position = "bottom",
legend.direction = "horizontal",
legend.title = element_blank()
) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
scale_x_date(date_breaks = "years" , date_labels = "%Y") +
annotate("text", x = as.Date("2011-08-07"), y= 0.3, label = "August 2011 Verizon Workers Strike Begins") +
theme(axis.text = element_text(color = "grey20", size = 12),
axis.title = element_text(color = "black", size = 12),
title = element_text(size = 10))

# Plotting Event - Sentiment for CEO/CFO for all documents combined
verizon_sentiment %>%
filter(speaker_type == "cfo" | speaker_type == "ceo") %>%
group_by(transDate) %>%
summarize(sentiment = average_downweighted_zero(sentiment)) %>%
ggplot(aes(x = transDate, y = sentiment)) +
geom_line(size = 0.5) +
stat_smooth(
show.legend = TRUE,
se = FALSE,
span = 0.15,
color = "darkred",
size = 1
) +
geom_vline(
xintercept = as.Date("2011-08-07"),
size = 1,
color = "blue",
linetype = 3
) +
labs(title = "CEO and CFO Sentiment", x = "Year", y = "Sentiment") +
theme(axis.text = element_text(color = "grey20", size = 12),
axis.title = element_text(color = "black", size = 12),
title = element_text(size = 14))


# Below, we load in the VZ stock mkt data sourced from Yahoo Finance and correlate trends to CEO/CFO sentiment computed above

# Load in stock mkt data and process
vz_stock <-
read.csv(file="/VZ_stockdata.csv", header=TRUE, sep=",")

vz_stock$qtr <- as.yearqtr(vz_stock$Date, format = "%Y-%m-%d")
drops <- c("Open", "Adj.Close", "Volume")
vz_stock <- vz_stock[,!(names(vz_stock) %in% drops)]

# reshape dataframe
vz_stock_long <- melt(vz_stock, id = c("Date", "qtr"))

# Plot 1 - plot stock price - high,low,closing
vz_stock_long$Date <- as.Date(vz_stock_long$Date)

ggplot(vz_stock_long, aes(x = Date, y = value, colour = variable)) +
geom_line() +
theme_gray() +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
ggtitle("Verizon (NYSE: VZ) - Stock Price Movement 2002 - 2017") +
scale_x_date(date_breaks = "12 month", labels = date_format("%Y")) +
theme(
legend.position = "bottom",
legend.direction = "horizontal",
legend.title = element_blank()
) +
ylab("Price") +
xlab("Date")

# Plot 2 - Plot with only closing price
vz_stock_long_subset <-
vz_stock_long[vz_stock_long$variable == 'Close', ]
ggplot(vz_stock_long_subset, aes(x = Date, y = value, colour = variable)) +
geom_line() +
theme_gray() +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
ggtitle("Verizon (NYSE: VZ) - Daily Closing Stock Price 2002 - 2017") +
scale_x_date(date_breaks = "12 month", labels = date_format("%Y")) +
theme(
legend.position = "bottom",
legend.direction = "horizontal",
legend.title = element_blank()
) +
ylab("Price") +
xlab("Date")


# Joining stock data with VZ transcript data
verizon_sentiment$transDate <- ymd(verizon_sentiment$transDate)
vz_stock_long_subset$transDate <- ymd(vz_stock_long_subset$Date)
sentiment_with_stock <-
left_join(vz_stock_long_subset, verizon_sentiment, by = "transDate")


# Visualizing relationship between CEO sentiment (VZ Earnings) and VZ closing price
sentiment_with_stock %>%
filter(sentiment_with_stock$type == "VZ Earnings") %>%
filter(speaker_type == "ceo") %>%
select(transDate, sentiment, value, speaker_type) %>%
group_by(transDate, value, speaker_type) %>%
summarize(sentiment = sd(sentiment)) %>%
ggplot() +
geom_line(aes(x = transDate, y = sentiment, color = "red")) +
geom_line(aes(x = transDate, y = value / 350, color = "black")) +
scale_y_continuous(sec.axis = sec_axis( ~ . * 350, name = "Stock Value at Close")) +
labs(title = "CEO Sentiment (VZ Earnings) Compared to VZ Closing Stock Price", x = "Year", y = "Sentiment") +
scale_color_manual(
name = "",
labels = c("CEO Sentiment", "Stock Value at Close"),
values = c("red", "black")
) +
theme(
legend.position = "bottom",
legend.direction = "horizontal",
legend.title = element_blank()
) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
scale_x_date(date_breaks = "years" , date_labels = "%Y")

# Visualizing relationship between CEO sentiment (VZ Conference) and VZ closing price

sentiment_with_stock %>%
filter(sentiment_with_stock$type == "VZ Conference") %>%
filter(speaker_type == "ceo") %>%
select(transDate, sentiment, value, speaker_type) %>%
group_by(transDate, value, speaker_type) %>%
summarize(sentiment = sd(sentiment)) %>%
ggplot(show.legend = TRUE) +
geom_line(aes(x = transDate, y = sentiment, color = "red")) +
geom_line(aes(x = transDate, y = value / 350, color = "black")) +
scale_y_continuous(sec.axis = sec_axis( ~ . * 350, name = "Stock Value at Close")) +
labs(title = "CEO Sentiment (VZ Conferences) Compared to VZ Closing Stock Price", x = "Year", y = "Sentiment") +
scale_color_manual(
name = "",
labels = c("CEO Sentiment", "Stock Value at Close"),
values = c("red", "black")
) +
theme(
legend.position = "bottom",
legend.direction = "horizontal",
legend.title = element_blank()
) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
scale_x_date(date_breaks = "years" , date_labels = "%Y")

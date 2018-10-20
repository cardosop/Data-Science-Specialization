# Capstone Project
#####################################################################
# Libraries
require(doParallel) # memory efficient processing
require(dplyr) # user function creation for cleaning
require(stringr) # regex cleanup
require(tau) # pattern counting on text documents
require(caret) # to create data partition and prediction model
require(data.table) # to perform memory efficient table operations
require(NLP) # for natural language processing ;)
require(openNLP)
registerDoParallel(makeCluster(4))
#####################################################################
# Functions
## Func 1 data cleanning
CleanString <- function(file){
  file <- tolower(file)
  file <- str_replace_all(file, "([iu]n)-([a-z])", "\\1\\2")
  file <- str_replace_all(file, " \\'|\\' ", " ")
  file <- str_replace_all(file, "([abiep])\\.([cdegm])\\.", "\\1\\2")
  file <- str_replace_all(file, "[^a-z.' ]", " ")
  file <- str_replace_all(file, "([a-z])\\.([a-z])", "\\1 \\2")
  file <- str_replace_all(file, "([0-9])(st|nd|rd|th)", "\\1")
  file <- str_replace_all(file, "( [a-z])\\. ", "\\1 ")
  file <- str_replace_all(file, " (m[rs]|mrs)\\.", " \\1 ")
  file <- str_replace_all(file, " (dr|st|rd|av|ave|blvd|ct)\\.", " \\1 ")
  file <- str_replace_all(file, "\\.$", "")
  file <- str_replace_all(file, "^ +| +$|", "")
  file <- str_replace_all(file, " *\\. *","\\.")
  file <- str_replace_all(file, " {2,}", " ")
  file <- str_replace_all(file, "www [a-z]+ [a-z]+", "")
  file <- str_replace_all(file, " ([a-z])\\1+ |^([a-z])\\1+ | ([a-z])\\1+$|^([a-z])\\1+$", " ")
  file <- str_replace_all(file, "([a-z])\\1{2,}", "\\1\\1")
  file <- str_replace_all(file, "\\'+([a-z]+)\\'+", "\\1")
  file <- str_replace_all(file, "\\'+ \\'+", " ")
  file <- str_replace_all(file, "(\\'+ )+|( \\'+)+|^\\'+|\\'+$", " ")
  file <- str_replace_all(file, "^[a-z]+$", "")
  file <- str_replace_all(file, "( [^ai])+ |^([^ai] )+|( [^ai])+$", " ")
  file <- str_replace_all(file, " +$|^ +", "")
  return(file)
}
## Func 2 Split string
StringSplitter <- function(file){
  file <- str_split(file, "\\.")
  file <- unlist(file)
  file <- file[file != ""]
}
## Text Parser
TextParser <- function(text) {
  tmp <- unlist(str_split(text, " "))
  tmp <- tmp[tmp != ""]
  return(tmp)
}
## Cheking the presence of bad words and replacing by ***
FilterBadWord <- function(text) {
  tmp <- text
  if (length(tmp) > 0) {
    words <- parse_text(tmp)
    word.count <- length(words)
    if (word.count > 0) {
      for (i in 1:word.count) {
        if (words[i] %in% badWords) words[i] <- paste(substring(words[i], 1, 1), "***", sep = "")
      }
      tmp_w <- paste(words[1]) 
      if (word.count > 1) {
        for (i in 2:word.count) tmp_w <- paste(tmp_w, words[i])
      }
      return(tmp_w)
    }
  }
  return(tmp)
}
## Shiny Functions
st_an <- Maxent_Sent_Token_Annotator()
wt_an <- Maxent_Word_Token_Annotator()
pt_an <- Maxent_POS_Tag_Annotator()

GetDefault <- function(text) {
  if (length(text) > 0) {
    a2 <- annotate(as.String(text), list(st_an, wt_an))
    a3 <- annotate(as.String(text), pt_an, a2)
    a3w <- subset(a3, type == "word")
    tags <- sapply(a3w$features, `[[`, "POS")
    if (tags %like% "NN") {
      return("in")
    } else if (tags %like% "VB") {
      return("a")
    } else if (tags %like% "JJ") {
      return("time")
    } else if (tags %like% "PRP") {
      return("first")
    } else if (tags %like% "CC") {
      return("i")
    } else if (text == "the") {
      return("first")
    }
  }
  return("the")
}

GetWord <- function(text) {
  if (text != " ") { 
    words <- parse_text(tolower(text))
    word.count <- length(words)
    if (word.count > 0) {
      filter <- paste("^", words[word.count], sep = "")
      tmp_dt <- fUni[word0 %like% filter]
      pred_word <- dim(tmp_dt)[1]
      if (pred_word > 0) {
        tmp_dt <- tmp_dt[order(rank(-appear.percent))]
        pred <- tmp_dt[1]$word0
        if (word.count > 2) {
          tmp_w <- paste(words[1])
          for (i in 2:(word.count - 1)) tmp_w <- paste(tmp_w, words[i])
          return(paste(tmp_w, badWords(pred)))
        } else if (word.count > 1) {
          tmp_w <- paste(words[1])
          return(paste(tmp_w, badWords(pred)))
        }
      }
    }
  }
  return(text)
}

GetPred <- function(text) {
  if (text != " ") { 
    input_words <- TextParser(CleanR(text))
    len <- length(input_words)
    
    if (len > 1) {
      w1 <- input_words[len]
      w2 <- input_words[len - 1]
    } else if (len > 0) {
      w1 <- input_words[len]
      w2 <- "NA"
    } else return("the")
    
    l1 <- .95
    l2 <- .04
    l3 <- .01
    
    len3 <- length(fTri[fTri[word2 == w2 & word1 == w1]]$appear.percent)
    len2 <- length(fBi[fBi[word1 == w1]]$appear.percent)
    matches <- matrix(nrow = len3 + len2, ncol = 2)
    matches[,1] <- ""
    matches[,2] <- 0
    
    if (len3 > 0) {
      for (i in 1:len3) {
        matches[i, 1] <- fTri[fTri[word2 == w2 & word1 == w1]]$word0[i]
        cnt2 <- length(fBi[fBi[word1 == w1 & word0 == matches[i, 1]]]$appear.percent)
        cnt1 <- length(fUni[fUni[word0 == matches[i, 1]]]$appear.percent)
        if (cnt2 > 0) freq2 <- fBi[fBi[word1 == w1 & word0 == matches[i, 1]]]$appear.percent 
        else freq2 <- 0
        if (cnt1 > 0) freq1 <- fUni[fUni[word0 == matches[i, 1]]]$appear.percent
        else freq1 <- 0
        matches[i, 2] <- fTri[fTri[word2 == w2 & word1 == w1]]$appear.percent[i] * l1 + freq2 * l2 + freq1 * l3     
      }
    }
    if (len2 > 0) {
      for (i in sum(len3, 1):sum(len3, len2)) {
        matches[i, 1] <- freq.tbl.bi[freq.tbl.bi[word1 == w1]]$word0[i - len3]
        cnt1 <- length(fUni[fUni[word0 == matches[i, 1]]]$appear.percent)
        if (cnt1 > 0) freq1 <- fUni[fUni[word0 == matches[i, 1]]]$appear.percent else freq1 <- 0
        matches[i, 2] <- fBi[fBi[word1 == w1]]$appear.percent[i - len3] * l2 + freq1 * l3   
      }
    }
    match_len <- length(matches[which.max(matches[,2])])
    if (match_len > 0) return(matches[which.max(matches[,2])])
    return(get_default(w1))
  }
  return(" ")
}

#####################################################################
# Reading Data
bRaw <- readLines("en_US.news.txt", encoding= "UTF-8", warn = F)
nRaw <- readLines("en_US.news.txt", encoding= "UTF-8", warn = F)
tRaw <- readLines("en_US.twitter.txt", encoding= "UTF-8", warn = F)
badWords <- readLines("http://badwordslist.googlecode.com/files/badwords.txt", warn = F)
#####################################################################
# Converting to character vector
bRaw <- iconv(bRaw, from="UTF-8", to="latin1", sub=" ")
nRaw <- iconv(nRaw, from="UTF-8", to="latin1", sub=" ")
tRaw <- iconv(tRaw, from="UTF-8", to="latin1", sub=" ")
#####################################################################
# Fixing bad words
badWords <- tolower(badWords)
badWords <- str_replace_all(badWords, "\\(", "\\\\(")
saveRDS(badWords, file = "badwords.rds")
#####################################################################
# Data cleaning
bClean <- CleanString(bRaw)
nClean <- CleanString(nRaw)
tClean <- CleanString(tRaw)
bClean <- StringSplitter(bClean)
nClean <- StringSplitter(nClean)
tClean <- StringSplitter(tClean)
# Putting all data together
cleanData <- c(bClean, nClean, tClean)
save(cleanData, file = "cleanData.RData")
#####################################################################
# Creating train subset
set.seed(123)
p <- createDataPartition(y = 1:length(cleanData), p = 0.20, list = F)
train <- cleanData[p]
train <- CleanString(train)
save(train, file = "train.RData")
#####################################################################
# Creatin Ngrams lists
# Unigram
train1 <- textcnt(train, method = "string", split = "[[:space:]]", n = 1L, decreasing = T)
# Bigram
train2 <- textcnt(train, method = "string", split = "[[:space:]]", n = 2L, decreasing = T)
# Trigram
train3 <- textcnt(train, method = "string", split = "[[:space:]]", n = 3L, decreasing = T)
#####################################################################
# Creating frequency tables
# Unigram
fUni <- data.table(text = names(train1), as.matrix(train1))
setnames(fUni, "V1", "count")
setnames(fUni, "text", "word0")
n <- sum(fUni$count)
fUni <- mutate(fUni, appear.percent = round(count/n, 7))
fUni$count <- NULL
setkeyv(fUni, c("word0", "appear.percent"))
saveRDS(fUni, file = "fUni.rds")
# Bigram
fBi <- data.table(text = names(train2), as.matrix(train2))
setnames(fBi, "V1", "count")
fBi[, c("word1", "word0")  := do.call(Map, c(f = c, strsplit(text, " ")))]
fBi <- mutate(fBi, appear.percent = round(count/train1[word1][[1]], 7))
fBi$text <- NULL
fBi$count <- NULL
setkey(fBi, word1)
fBi <- fBi[,lapply(.SD, function(x) head(x, 5)), by = key(fBi)]
setkeyv(fBi, c("word1", "appear.percent", "word0"))
saveRDS(fBi, file = "fBi.rds")
fTri <- data.table(text = names(train3), as.matrix(train3))
# Trigram
fTri <- data.table(text = names(train3), as.matrix(train3))
setnames(fTri, "V1", "count")
fTri <- subset(fTri, count > 1)
fTri[, c("word2", "word1", "word0")  := do.call(Map, c(f = c, strsplit(text, " ")))]
fTri <- mutate(fTri, appear.percent = round(count/train2[paste(word2, word1)][[1]], 7))
fTri$text <- NULL
fTri$count <- NULL
setkeyv(fTri, c("word2", "word1"))
fTri <- fTri[,lapply(.SD, function(x) head(x, 5)),by = key(fTri)]
setkeyv(fTri, c("word2", "word1", "appear.percent", "word0"))
saveRDS(fTri, file = "fTri.rds")
#####################################################################








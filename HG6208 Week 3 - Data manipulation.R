# creates /data folder if it is not already there
if (!dir.exists("data")) {
   dir.create("data")
}

# downloads the data file from the course's GitHub if it is not already there
if (!file.exists("data/Week 2 - NSC 3003.txt")) {
   download.file("https://github.com/ljunwen/HG6208/raw/main/data/Week%202%20-%20NSC%203003.txt", paste0("data/Week 2 - NSC 3003.txt"), method = "libcurl")
}

# time to write your own comments!
conv <- read.delim(file = paste0(ifelse (exists("path"), path, path <- "data/"), "Week 2 - NSC 3003.txt"), header=TRUE, skipNul = TRUE, encoding="UTF-8", stringsAsFactors=FALSE)

# alternatively:
conv <- read.csv(file = paste0(ifelse (exists("path"), path, path <- "data/"), "Week 2 - NSC 3003.txt"), header=TRUE, sep="\t", skipNul = TRUE, encoding="UTF-8", stringsAsFactors=FALSE)


class(conv)
class(conv$SCD)
class(conv$speaker)
class(conv$tmin)
class(conv$text)

conv$SCD <- as.factor(conv$SCD)
conv$speaker <- as.factor(conv$speaker)

class(conv$SCD)
class(conv$speaker)

levels(conv$SCD)
levels(conv$speaker)

conv$speaker[1] <- "C:"
conv$speaker[1] <- "A:"

head(conv)

# alternatively:
conv[c(1:6),]

conv[c(1:6), c(3,5)]

# alternatively:
head(conv[, c(3,5)])

conv[3, 5]

#alternatively:
conv$text[3]

conv$duration <- 

conv_bak <- conv

conv <- conv[, c(1,3:4,6,2,5)]

conv <- conv[, -1]

if(!require(stringr)){
  install.packages("stringr")   # installs the 'stringr' package (for 'str_count') if it isn't installed
  library(stringr)   # loads the package on first install
}

# counts the number of 'lah's in each utterance
conv$count <- str_count(conv$text, r"{\[lah\]}")

# removes silence and noise markers
conv <- subset(conv, text != "<S>" & text != "<Z>")

# splits the conversation by speaker
conv_a <- subset(conv, speaker == "A:")
conv_b <- subset(conv, speaker == "B:")

# total 'lah's used by each speaker
sum(conv_a$count)
sum(conv_b$count)

# calculates the number of words in each utterance
conv_a$wordcount <- str_count(conv_a$text, " ") + 1
conv_b$wordcount <- str_count(conv_b$text, " ") + 1

# total wordcount for each speaker
sum(conv_a$wordcount)
sum(conv_b$wordcount)
# Install Packages
#install.packages("tm")
#install.packages("wordcloud")

#Library Calls
library(tm)
library(wordcloud)

#Read in text of speech
sba = readLines("./data/Bush_911.txt")
head(sba, 3)

#Pre-processing
words = VectorSource(sba)
words
wordsCorpus = Corpus(words)
wordsCorpus = tm_map(wordsCorpus, content_transformer(tolower))
wordsCorpus = tm_map(wordsCorpus, removePunctuation)
wordsCorpus = tm_map(wordsCorpus, removeNumbers)
wordsCorpus = tm_map(wordsCorpus, removeWords, stopwords("english"))
wordsCorpus
tdm = TermDocumentMatrix(wordsCorpus)
tdm

#WordCloud
m = as.matrix(tdm)
wordCounts = rowSums(m)
wordCounts = sort(wordCounts, decreasing = TRUE)
head(wordCounts)
cloudFrame = data.frame(word=names(wordCounts),freq=wordCounts)

#Save WordCloud to a jpeg
jpeg("./images/Bush_911_WordClould.jpg")
wordcloud(cloudFrame$word,cloudFrame$freq)
wordcloud(names(wordCounts), wordCounts, min.freq=2, max.words=50, rot.per=0.35,colors=brewer.pal(8,"Dark2"))
dev.off()

#Sentiment Analysis
#define file names
pos = "./data/positive-words.txt"
neg = "./data/negative-words.txt"

#read the files and separate each word
p = scan(pos, character(0), sep = "\n")
n = scan(neg, character(0), sep = "\n")

head(p, 10)

#get rid of junk lines at beginning of files
p = p[-1:-34]
n = n[-1:-34]

head(p,10)
head(n, 10)

#Calc total number of words
totalWords = sum(wordCounts)
totalWords

#create vector with just words
words = names(wordCounts)

#match words to pos list
matched = match(words, p, nomatch=0)
head(matched,10)

#the 10th word matched to the 766th word in the pos list
p[766]
words[10]

#Counts of positive words that matched
pCounts = wordCounts[which(matched !=0)]
length(pCounts)
pWords = names(pCounts)
nPos = sum(pCounts)
nPos
#68 unique words, 123 total words

#Counts of negative words that matched
matched = match(words, n, nomatch = 0)
nCounts = wordCounts[which(matched !=0)]
length(nCounts)
nWords = names(nCounts)
nNeg =  sum(nCounts)
nNeg
#66 unique words, 109 total words

#calc % of pos or neg words
totalWords = length(words)
ratioPos = nPos/totalWords
ratioPos
ratioNeg = nNeg/totalWords
ratioNeg

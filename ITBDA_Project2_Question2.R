# Question 2.1: Load and preprocess the news articles

if (!requireNamespace("tm", quietly = TRUE)) {
  install.packages("tm")
}

library(tm)

# Find all text files in the extracted sa_news folder
news_folder <- "sa_news"

txt_files <- list.files(
  path = news_folder,
  pattern = "\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(txt_files) == 0) {
  stop("No .txt files found. Check that the sa_news folder is in the project.")
}

# Prepare English stopwords
stop_words <- unique(c(
  setdiff(tm::stopwords("en"), c("no", "not", "nor")),
  "said", "according", "also", "including", "will",
  "monday", "tuesday", "wednesday", "thursday",
  "friday", "saturday", "sunday", "sanewsgovza"
))

# Clean the article text
clean_news_text <- function(text) {
  text <- tolower(text)
  text <- gsub("http\\S+|www\\.\\S+", " ", text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, stop_words)
  text <- stripWhitespace(text)
  trimws(text)
}

# Read each article and create the requested data frame
news_df <- data.frame(
  file_name = basename(txt_files),
  content = vapply(
    txt_files,
    function(path) {
      article <- paste(
        readLines(path, warn = FALSE, encoding = "UTF-8"),
        collapse = " "
      )
      clean_news_text(article)
    },
    character(1)
  ),
  stringsAsFactors = FALSE
)

# Check the result
cat("Articles loaded:", nrow(news_df), "\n")
cat("Data frame dimensions:", dim(news_df), "\n")
cat("Empty cleaned articles:", sum(news_df$content == ""), "\n")

head(news_df, 2)

cat("Articles loaded:", nrow(news_df), "\n")
cat("Columns:", paste(names(news_df), collapse = ", "), "\n")
cat("Dimensions:", nrow(news_df), "rows x", ncol(news_df), "columns\n")
cat("Empty cleaned articles:", sum(news_df$content == ""), "\n\n")

cat("Article filenames:\n")
print(news_df$file_name)

cat("\nSample of cleaned text:\n")
cat(substr(news_df$content[1], 1, 250), "...\n")


# Question 2.2: Fit an LDA topic model

if (!requireNamespace("topicmodels", quietly = TRUE)) {
  install.packages("topicmodels")
}

library(topicmodels)

# Convert each cleaned article into a document
news_corpus <- VCorpus(VectorSource(news_df$content))

# Create a document-term matrix of word counts
news_dtm <- DocumentTermMatrix(
  news_corpus,
  control = list(wordLengths = c(3, Inf))
)

# Remove any documents with no words remaining
dtm_counts <- as.matrix(news_dtm)
news_dtm <- news_dtm[rowSums(dtm_counts) > 0, ]

cat("Articles used for LDA:", nrow(news_dtm), "\n")
cat("Unique terms used for LDA:", ncol(news_dtm), "\n")

# Set the number of topics and make the model reproducible
n_topics <- 4
top_k <- 10
set.seed(1234)

lda_model <- LDA(
  news_dtm,
  k = n_topics,
  method = "Gibbs",
  control = list(
    seed = 1234,
    burnin = 1000,
    iter = 2000,
    thin = 100
  )
)

# Get each word's probability within each topic
word_probabilities <- posterior(lda_model)$terms

# Select the 10 highest-probability words for each topic
top_words <- do.call(
  rbind,
  lapply(seq_len(n_topics), function(topic_number) {
    probabilities <- word_probabilities[topic_number, ]
    selected <- order(probabilities, decreasing = TRUE)[
      seq_len(min(top_k, length(probabilities)))
    ]
    
    data.frame(
      topic = paste("Topic", topic_number),
      rank = seq_along(selected),
      word = names(probabilities)[selected],
      probability = round(as.numeric(probabilities[selected]), 4),
      percentage = round(as.numeric(probabilities[selected]) * 100, 2)
    )
  })
)

print(top_words, row.names = FALSE)

#Question 2.3

#Method 1 - Bar Graph
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

ggplot(top_words, aes(x = reorder(word, percentage), y = percentage)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  facet_wrap(~ topic, scales = "free_y") +
  labs(
    title = "Top 10 Words in Each LDA Topic",
    x = "Word",
    y = "Word probability (%)"
  ) +
  theme_minimal()

#Method 2 - Word Cloud

if (!requireNamespace("wordcloud", quietly = TRUE)) {
  install.packages("wordcloud")
}
library(wordcloud)

set.seed(123)
par(mfrow = c(2, 2), mar = c(1, 1, 3, 1))

for (topic_number in seq_len(n_topics)) {
  probabilities <- word_probabilities[topic_number, ]
  selected <- order(probabilities, decreasing = TRUE)[
    seq_len(min(40, length(probabilities)))
  ]
  
  words <- names(probabilities)[selected]
  word_probabilities_for_cloud <- as.numeric(probabilities[selected])
  topic_label <- paste(words[1:3], collapse = " / ")
  
  wordcloud(
    words = words,
    freq = word_probabilities_for_cloud,
    max.words = 40,
    random.order = FALSE,
    scale = c(2.5, 0.7),
    colors = c("gray40", "darkorange", "steelblue", "mediumpurple", "deeppink3", "olivedrab")
  )
  
  title(main = paste("Topic", topic_number, ":", topic_label), cex.main = 0.9)
}

par(mfrow = c(1, 1))
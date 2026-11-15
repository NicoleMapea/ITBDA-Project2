#Question 3.1

if (!requireNamespace("rvest", quietly = TRUE)) {
  install.packages("rvest")
}
library(rvest)

url <- "https://science.nasa.gov/earth/earth-atmosphere/the-atmosphere-move-as-ising-a-weather-eye-on-earths-climate-instabilities/"

page <- read_html(url)

paragraphs <- page |>
  html_elements("main p") |>
  html_text2()

paragraphs <- paragraphs[nchar(paragraphs) > 40]

web_df <- data.frame(
  title = "The Atmosphere: Keeping a Weather Eye on Earth's Climate Instabilities",
  source = "NASA Science",
  url = url,
  content = paste(paragraphs, collapse = " "),
  stringsAsFactors = FALSE
)

cat("Paragraphs collected:", length(paragraphs), "\n")
cat("Characters collected:", nchar(web_df$content), "\n")
cat("Text preview:", substr(web_df$content, 1, 500), "\n")

#Question 3.2

if (!requireNamespace("tm", quietly = TRUE)) {
  install.packages("tm")
}
library(tm)

clean_text <- function(text) {
  text <- tolower(text)
  text <- gsub("https?://\\S+|www\\.\\S+", " ", text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, stopwords("en"))
  text <- stripWhitespace(text)
  trimws(text)
}

web_df$clean_content <- clean_text(web_df$content)

cat("Original characters:", nchar(web_df$content), "\n")
cat("Cleaned characters:", nchar(web_df$clean_content), "\n")
cat("Cleaned text preview:", substr(web_df$clean_content, 1, 500), "\n")

#Question 3.3

# Remove the page heading and link at the end
article_text <- sub(
  "^.*?Series\\s*",
  "",
  web_df$content,
  perl = TRUE
)

article_text <- sub(
  "For more on AIRS, visit.*$",
  "",
  article_text,
  ignore.case = TRUE
)

# Split the article into sentences
sentences <- unlist(
  strsplit(article_text, "(?<=[.!?])\\s+", perl = TRUE)
)
sentences <- trimws(sentences)

# Keep reasonably complete sentences
sentence_word_count <- lengths(
  strsplit(gsub("[^A-Za-z]+", " ", sentences), "\\s+")
)
sentences <- sentences[
  sentence_word_count >= 10 & sentence_word_count <= 45
]

# Technique 1: Lead summary using the first three sentences
lead_summary <- paste(head(sentences, 3), collapse = " ")

# Prepare word frequencies for Technique 2
summary_stopwords <- unique(c(
  tm::stopwords("en"),
  "said", "says", "also", "nasa", "airs", "fetzer",
  "visit", "series", "humanity"
))

words <- unlist(strsplit(clean_text(article_text), "\\s+"))
words <- words[
  nzchar(words) &
    grepl("^[a-z]+$", words) &
    !words %in% summary_stopwords
]
word_frequency <- table(words)

# Score sentences by the frequency of their useful words
score_sentence <- function(sentence) {
  sentence_words <- unlist(strsplit(clean_text(sentence), "\\s+"))
  sentence_words <- sentence_words[
    nzchar(sentence_words) &
      !sentence_words %in% summary_stopwords
  ]
  
  if (length(sentence_words) < 8) return(0)
  
  sum(as.numeric(word_frequency[sentence_words]), na.rm = TRUE) /
    sqrt(length(sentence_words))
}

sentence_scores <- vapply(sentences, score_sentence, numeric(1))
selected <- order(sentence_scores, decreasing = TRUE)[
  seq_len(min(3, length(sentence_scores)))
]
frequency_summary <- paste(sentences[sort(selected)], collapse = " ")

cat("Lead summary:\n", lead_summary, "\n\n")
cat("Word-frequency summary:\n", frequency_summary, "\n")

# Prepare the most frequent words for both visualisations
ranked_words <- sort(word_frequency, decreasing = TRUE)
number_to_plot <- min(15, length(ranked_words))

top_terms <- data.frame(
  word = names(ranked_words)[seq_len(number_to_plot)],
  frequency = as.integer(ranked_words[seq_len(number_to_plot)])
)

# Visualisation 1: Bar chart
bar_plot <- ggplot2::ggplot(
  top_terms,
  ggplot2::aes(x = reorder(word, frequency), y = frequency)
) +
  ggplot2::geom_col(fill = "steelblue") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Most Frequent Words in the NASA Article",
    x = "Word",
    y = "Frequency"
  ) +
  ggplot2::theme_minimal()

print(bar_plot)

# Visualisation 2: Word cloud
if (!requireNamespace("wordcloud", quietly = TRUE)) {
  install.packages("wordcloud")
}

number_for_cloud <- min(40, length(ranked_words))

wordcloud::wordcloud(
  words = names(ranked_words)[seq_len(number_for_cloud)],
  freq = as.numeric(ranked_words[seq_len(number_for_cloud)]),
  max.words = number_for_cloud,
  random.order = FALSE,
  colors = c("steelblue", "darkorange", "darkgreen", "purple", "deeppink3")
)

title("Frequent Words in the NASA Article")
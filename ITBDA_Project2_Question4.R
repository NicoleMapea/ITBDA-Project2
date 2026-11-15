#Question 4.1

if (!requireNamespace("rvest", quietly = TRUE)) {
  install.packages("rvest")
}
library(rvest)

books_url <- "https://books.toscrape.com/"
books_page <- read_html(books_url)
book_cards <- html_elements(books_page, "article.product_pod")

title_visible <- vapply(book_cards, function(card) {
  html_text2(html_element(card, "h3 a"))
}, character(1))

title_full <- vapply(book_cards, function(card) {
  html_attr(html_element(card, "h3 a"), "title")
}, character(1))

price_raw <- vapply(book_cards, function(card) {
  html_text2(html_element(card, ".price_color"))
}, character(1))

availability_raw <- vapply(book_cards, function(card) {
  html_text2(html_element(card, ".availability"))
}, character(1))

rating_raw <- vapply(book_cards, function(card) {
  html_attr(html_element(card, ".star-rating"), "class")
}, character(1))

# Use regex to extract a numeric price while retaining the raw price
price_gbp <- as.numeric(gsub("[^0-9.]", "", price_raw))

books_raw <- data.frame(
  title_visible = title_visible,
  title_full = title_full,
  price_raw = price_raw,
  price_gbp = price_gbp,
  availability_raw = availability_raw,
  rating_raw = rating_raw,
  stringsAsFactors = FALSE
)

cat("Records collected:", nrow(books_raw), "\n")
cat("Data frame dimensions:", dim(books_raw), "\n")
head(books_raw)

#Question 4.2

# Use the full title to replace shortened visible titles
clean_title <- ifelse(
  !is.na(books_raw$title_full) & books_raw$title_full != "",
  books_raw$title_full,
  books_raw$title_visible)
clean_title <- trimws(clean_title)

# Clean the price text and convert it to a number
clean_price <- as.numeric(gsub("[^0-9.]", "", books_raw$price_raw))

# Convert the written star rating to a number
rating_word <- sub("^star-rating\\s+", "", books_raw$rating_raw)
clean_rating <- match(rating_word, c("One", "Two", "Three", "Four", "Five"))

# Standardise availability text
clean_availability <- trimws(tolower(books_raw$availability_raw))
clean_availability <- ifelse(
  grepl("in stock", clean_availability), "In stock",
  ifelse(grepl("out of stock", clean_availability), "Out of stock", NA))

books_clean <- data.frame(
  book_title = clean_title,
  price_gbp = clean_price,
  rating = clean_rating,
  availability = clean_availability,
  stringsAsFactors = FALSE)

# Impute missing numeric values with the median, if any exist
if (anyNA(books_clean$price_gbp) && any(!is.na(books_clean$price_gbp))) {
  books_clean$price_gbp[is.na(books_clean$price_gbp)] <-
    median(books_clean$price_gbp, na.rm = TRUE)
}

if (anyNA(books_clean$rating) && any(!is.na(books_clean$rating))) {
  books_clean$rating[is.na(books_clean$rating)] <-
    median(books_clean$rating, na.rm = TRUE)}

# Remove duplicate books and check the cleaned data
books_clean <- books_clean[!duplicated(books_clean$book_title), ]

cat("Cleaned data dimensions:", dim(books_clean), "\n")
cat("Missing values by column:\n")
print(colSums(is.na(books_clean)))

# Export the cleaned dataset
write.csv(books_clean, "books_clean.csv", row.names = FALSE)

head(books_clean)

library(ggplot2)
library(tidyr)
library(dplyr)
library(tm)
library(wordcloud2)
library(Matrix)

data<-read.csv("/Users/jianingcai/Downloads/netflix_titles.csv")


# ----- Plot 1: Top 10 Countries with Most Movies and Shows ---------
# Assuming 'data' is your dataset
data_filtered_country <- data[!is.na(data$country) & data$country != "", ]

# Split the comma-separated countries into separate rows
data_separated_country <- separate_rows(data_filtered_country, country, sep = ", ")

# Find the top 10 countries with the most content
top_countries <- head(sort(table(data_separated_country$country), decreasing = TRUE), 10)

data_separated_country_top10 <- data_separated_country[data_separated_country$country 
                                                       %in% names(top_countries), ]

# Calculate the total count for each country
country_total_count <- data_separated_country_top10 %>%
  group_by(country, type) %>%
  summarise(count = n()) %>%
  group_by(country) %>%
  summarise(total_count = sum(count))

# Order the countries based on total count
ordered_countries <- country_total_count %>%
  arrange(desc(total_count)) %>%
  pull(country)

# Reorder the factor levels of 'country' based on total count
data_separated_country_top10$country <- factor(data_separated_country_top10$country, levels = ordered_countries)

# Determine the maximum count to set the y-axis limit
max_count <- max(as.numeric(top_countries))

# Round up to the nearest multiple of 500 for the y-axis breaks
y_breaks <- seq(0, ceiling(max_count / 500) * 500, by = 500)

# Create the bar plot with dodged bars and color based on 'type'
ggplot(data_separated_country_top10, aes(x = country, fill = type)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("Movie" = "#ee7043", "TV Show" = "#757cff")) +
  scale_y_continuous(breaks = y_breaks, labels = y_breaks) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 10 Countries with Most Content", x = "Country", y = "Count")

# ----- Plot 2: Genre Count Over the Years ------
genres_count <- data %>%
  separate_rows(listed_in, sep = ", ") %>%
  filter(listed_in != "") %>%  # Remove empty entries
  group_by(listed_in) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
genres_count<-head(genres_count,10)

data_filtered_genre <- data[!is.na(data$listed_in) & data$listed_in != "", ]
# Split the comma-separated countries into separate rows
data_separated_genre <- separate_rows(data_filtered_genre, listed_in, sep = ", ")


data_separated_genre_above90 <- data_separated_genre[data_separated_genre$listed_in 
                                                       %in% genres_count$listed_in, ]
# Create the line plot with ordered genres
ggplot(data_separated_genre_above90, aes(x = release_year, color = listed_in)) +
  geom_line(stat="count", size=1) +
  geom_vline(xintercept = 1997, linetype = "dashed", color = "red") + 
  geom_text(aes(x = 1997, y = 230, 
                label = "Netflix is founded at\nAugust 29, 1997"),
            hjust = -0.2, color = "red", size = 3) + 
  scale_color_manual(values = c("#2b1153", "#582f9a", "#c95286", "#c952c5", 
                                "#fe7fa4", "#757cff", "#ee7043", "#6fc9b2", 
                                "#ffcc29", "#47e5bc")) +  # Set custom colors
  labs(title = "Top 10 Genre Count Over the Years",
       x = "Release Year", y = "Count", color = "Genre") +
  xlim(1990, max(data_separated_genre_above90$release_year))

# ----- Plot 3: Word Cloud on US Movie Descriptions Since 2000 ------
# Assuming 'data' is your dataset
data_filtered_description <- data[!is.na(data$description) & data$description != "", ]
data_filtered_description_separated_country <- separate_rows(data_filtered_description, country, sep = ", ")


data_since_2000 <- data_filtered_description_separated_country[data_filtered_description_separated_country$release_year >= 2000 
                                             & data_filtered_description_separated_country$type == "Movie" 
                                             & grepl("United States", data_filtered_description_separated_country$country), ]

# Create a Corpus
corpus <- Corpus(VectorSource(data_since_2000$description))

# Preprocess the text
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Create a Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)

# Get the word frequencies
word_freq <- colSums(as.matrix(dtm))

# Convert to a data frame
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)

# Filter words with frequency greater than a threshold (e.g., 5)
word_freq_df <- word_freq_df[word_freq_df$freq > 5, ]

# Generate a word cloud using wordcloud2
wordcloud2(word_freq_df)

# ----- Plot 4: Distribution of Movie Duration (Conclusion: Use Median)------
data_movies <- data[data$type == "Movie", ]

# Extract numeric value from the "duration" column
data_movies$duration_numeric <- as.numeric(gsub(" min", "", data_movies$duration))

# Create a histogram for movie durations
ggplot(data_movies, aes(x = duration_numeric)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Movie Durations",
       x = "Duration (minutes)",
       y = "Count")

# ----- Plot 5: Distribution of TV Shows Duration (Conclusion: Use Median)------
data_tv_shows<- data[data$type == "TV Show", ]

# Extract numeric value from the "duration" column
data_tv_shows$duration_numeric <- as.numeric(gsub(" Season[s]*", "", data_tv_shows$duration))

# Create a histogram for TV show durations
ggplot(data_tv_shows, aes(x = duration_numeric)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of TV Show Durations (Seasons)",
       x = "Duration (seasons)",
       y = "Count")


# ----- Plot 6: Median Movie Duration Over the Years with Quartiles------
# Assuming 'data' is your dataset
data_movies <- data[data$type == "Movie", ]

# Extract numeric value from the "duration" column
data_movies$duration_numeric <- as.numeric(gsub(" min", "", data_movies$duration))

# Calculate the median for each year
medians <- data_movies %>%
  group_by(release_year) %>%
  summarize(median_duration = median(duration_numeric))

summary_stats <- data_movies %>%
  group_by(release_year) %>%
  summarize(median_duration = median(duration_numeric),
            q25 = quantile(duration_numeric, 0.25, na.rm = TRUE),
            q75 = quantile(duration_numeric, 0.75, na.rm = TRUE))

# Create a line graph with median and quartiles
# Create the line plot with ordered genres
pplot<-ggplot(summary_stats, aes(x = release_year, y = median_duration)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#582f9a", alpha = 0.3) +
  geom_line(color = "#582f9a", size = 1) +
  geom_point(color = "#582f9a", size = 2) +
  theme_minimal()  +
  scale_x_continuous(breaks = seq(min(summary_stats$release_year), 2022, by = 10)) +
  labs(title = "Median Movie Duration Over the Years with Quartiles",
       x = "Release Year",
       y = "Median Duration (minutes)")+
  geom_vline(xintercept = 1997, linetype = "dashed", color = "red") + 
  geom_text(aes(x = 1997, y = 55, 
                label = "Netflix is founded at\nAugust 29, 1997"),
            hjust = 1.1, color = "red", size = 3) 

pplot+annotate("text", x = 2002, y = 200, label = "Shaded area")



# ----- Plot 7: Median TV Show Duration Over the Years with Quartiles------
# Assuming 'data' is your dataset
data_tv_shows <- data[data$type == "TV Show", ]

# Extract numeric value from the "duration" column
data_tv_shows$duration_numeric <- as.numeric(gsub(" Season[s]*", "", data_tv_shows$duration))

# Calculate the median for each year
medians <- data_tv_shows %>%
  group_by(release_year) %>%
  summarize(median_duration = median(duration_numeric))

summary_stats <- data_tv_shows %>%
  group_by(release_year) %>%
  summarize(median_duration = median(duration_numeric),
           q25 = quantile(duration_numeric, 0.25, na.rm = TRUE),
           q75 = quantile(duration_numeric, 0.75, na.rm = TRUE))

# Create a line graph with median and quartiles
ggplot(summary_stats, aes(x = release_year, y = median_duration)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "blue", alpha = 0.3) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(summary_stats$release_year), 
                                  max(summary_stats$release_year), by = 10)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  labs(title = "Median TV Show Duration Over the Years with Quartiles",
       x = "Release Year",
       y = "Median Duration (seasons)")
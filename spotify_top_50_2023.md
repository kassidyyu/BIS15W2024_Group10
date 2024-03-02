---
title: "Top 50 Tracks Spotify"
output: 
  html_document: 
    keep_md: true
date: "2024-02-29"
---




```r
library(tidyverse)
library(janitor)
library(skimr)
library(naniar)
library(visdat)
library(here)
library(palmerpenguins)
library(RColorBrewer)
library(paletteer)
```


```r
spotify2023 <- read_csv("top_50_2023.csv")
```

```
## Rows: 50 Columns: 19
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr   (3): artist_name, track_name, genres
## dbl  (14): danceability, valence, energy, loudness, acousticness, instrument...
## lgl   (1): is_explicit
## date  (1): album_release_date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


```r
genre_list <- strsplit(spotify2023$genres, ", ")
unique_genres <- unique(unlist(genre_list))
genre_matrix <- matrix(0, nrow = nrow(spotify2023), ncol = length(unique_genres), dimnames = list(NULL, unique_genres))
for (i in 1:length(genre_list)) {
  genre_matrix[i, genre_list[[i]]] <- 1
}
genre_df <- as.data.frame(genre_matrix)
spotify2023 <- cbind(spotify2023, genre_df)
```

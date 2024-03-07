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
library(naniar)
library(tidyr)
```

## Load the data

```r
spotify2023 <- read_csv("top_50_2023.csv") %>% clean_names()
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
split_spotify2023 <- spotify2023 %>% 
  separate(genres, into = c("genre1", "genre2", "genre3", "genre4", "genre5", "genre6"), sep=",") %>% 
  pivot_longer(cols = starts_with("genre"),
               names_to = "type",
               values_to = "class") %>% 
  filter(class != "NA") %>% 
  select(-type)
```

```
## Warning: Expected 6 pieces. Missing pieces filled with `NA` in 48 rows [1, 2, 3, 4, 5,
## 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, ...].
```

## Re-use select to pull out the punctuation

```r
split_spotify2023 <- split_spotify2023 %>% 
  separate(class, into = c("punctuation", "class2"), sep = "'") %>% 
  select(-punctuation)
```

```
## Warning: Expected 2 pieces. Additional pieces discarded in 126 rows [1, 2, 3, 4, 5, 6,
## 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].
```

## pivot_wider 

```r
split_spotify2023 <- split_spotify2023 %>%
    pivot_wider(
        names_from = class2,
        values_from = class2,
        values_fn = length) %>% 
  mutate(across(everything(), ~replace(.x, is.na(.x), 0)))
```

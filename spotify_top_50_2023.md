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
spotify2023
```

```
## # A tibble: 50 × 19
##    artist_name    track_name  is_explicit album_release_date genres danceability
##    <chr>          <chr>       <lgl>       <date>             <chr>         <dbl>
##  1 Miley Cyrus    Flowers     FALSE       2023-08-18         ['pop…        0.706
##  2 SZA            Kill Bill   FALSE       2022-12-08         ['pop…        0.644
##  3 Harry Styles   As It Was   FALSE       2022-05-20         ['pop…        0.52 
##  4 Jung Kook      Seven (fea… TRUE        2023-11-03         ['k-p…        0.79 
##  5 Eslabon Armado Ella Baila… FALSE       2023-04-28         ['cor…        0.668
##  6 Taylor Swift   Cruel Summ… FALSE       2019-08-23         ['pop…        0.552
##  7 Metro Boomin   Creepin' (… TRUE        2022-12-02         ['rap…        0.715
##  8 Rema           Calm Down … FALSE       2023-04-27         ['afr…        0.799
##  9 Bizarrap       Shakira: B… FALSE       2023-01-11         ['arg…        0.778
## 10 Taylor Swift   Anti-Hero   FALSE       2022-10-21         ['pop…        0.637
## # ℹ 40 more rows
## # ℹ 13 more variables: valence <dbl>, energy <dbl>, loudness <dbl>,
## #   acousticness <dbl>, instrumentalness <dbl>, liveness <dbl>,
## #   speechiness <dbl>, key <dbl>, tempo <dbl>, mode <dbl>, duration_ms <dbl>,
## #   time_signature <dbl>, popularity <dbl>
```




```r
spotify2023 %>% 
  pivot_wider(names_from = "genres", #the observations under key will become new columns
              values_from = "is_explicit")
```

```
## # A tibble: 50 × 52
##    artist_name    track_name      album_release_date danceability valence energy
##    <chr>          <chr>           <date>                    <dbl>   <dbl>  <dbl>
##  1 Miley Cyrus    Flowers         2023-08-18                0.706   0.632  0.691
##  2 SZA            Kill Bill       2022-12-08                0.644   0.418  0.735
##  3 Harry Styles   As It Was       2022-05-20                0.52    0.662  0.731
##  4 Jung Kook      Seven (feat. L… 2023-11-03                0.79    0.872  0.831
##  5 Eslabon Armado Ella Baila Sola 2023-04-28                0.668   0.834  0.758
##  6 Taylor Swift   Cruel Summer    2019-08-23                0.552   0.564  0.702
##  7 Metro Boomin   Creepin' (with… 2022-12-02                0.715   0.172  0.62 
##  8 Rema           Calm Down (wit… 2023-04-27                0.799   0.811  0.802
##  9 Bizarrap       Shakira: Bzrp … 2023-01-11                0.778   0.498  0.632
## 10 Taylor Swift   Anti-Hero       2022-10-21                0.637   0.533  0.643
## # ℹ 40 more rows
## # ℹ 46 more variables: loudness <dbl>, acousticness <dbl>,
## #   instrumentalness <dbl>, liveness <dbl>, speechiness <dbl>, key <dbl>,
## #   tempo <dbl>, mode <dbl>, duration_ms <dbl>, time_signature <dbl>,
## #   popularity <dbl>, `['pop']` <lgl>, `['pop', 'r&b', 'rap']` <lgl>,
## #   `['k-pop']` <lgl>,
## #   `['corrido', 'corridos tumbados', 'sad sierreno', 'sierreno']` <lgl>, …
```


```r
spotify2023$genres <-
  gsub("\\[|\\]", "", spotify2023$genres)
```
  separate(patient, into= c("patient", "sex"), sep = "_") #seperate patient from sex with the _


```r
strsplit(spotify2023$genres, ",")
```

```
## [[1]]
## [1] "'pop'"
## 
## [[2]]
## [1] "'pop'"  " 'r&b'" " 'rap'"
## 
## [[3]]
## [1] "'pop'"
## 
## [[4]]
## [1] "'k-pop'"
## 
## [[5]]
## [1] "'corrido'"            " 'corridos tumbados'" " 'sad sierreno'"     
## [4] " 'sierreno'"         
## 
## [[6]]
## [1] "'pop'"
## 
## [[7]]
## [1] "'rap'"
## 
## [[8]]
## [1] "'afrobeats'"     " 'nigerian pop'"
## 
## [[9]]
## [1] "'argentine hip hop'" " 'pop venezolano'"   " 'trap argentino'"  
## [4] " 'trap latino'"      " 'urbano latino'"   
## 
## [[10]]
## [1] "'pop'"
## 
## [[11]]
## [1] "'garage rock'"      " 'modern rock'"     " 'permanent wave'" 
## [4] " 'rock'"            " 'sheffield indie'"
## 
## [[12]]
## [1] "'reggaeton'"             " 'reggaeton colombiano'"
## [3] " 'trap latino'"          " 'urbano latino'"       
## 
## [[13]]
## [1] "'reggaeton'"        " 'urbano mexicano'"
## 
## [[14]]
## [1] "'big room'"   " 'dance pop'" " 'edm'"       " 'pop'"       " 'pop dance'"
## 
## [[15]]
## [1] "'canadian contemporary r&b'" " 'canadian pop'"            
## [3] " 'pop'"                     
## 
## [[16]]
## [1] "'musica chihuahuense'"
## 
## [[17]]
## [1] "'pop'"     " 'uk pop'"
## 
## [[18]]
## [1] "'colombian pop'"         " 'latin pop'"           
## [3] " 'reggaeton'"            " 'reggaeton colombiano'"
## [5] " 'trap latino'"          " 'urbano latino'"       
## 
## [[19]]
## [1] "'bedroom pop'"
## 
## [[20]]
## [1] "'gen z singer-songwriter'" " 'singer-songwriter pop'" 
## 
## [[21]]
## [1] "'chill pop'"
## 
## [[22]]
## [1] "'canadian contemporary r&b'" " 'canadian pop'"            
## [3] " 'pop'"                     
## 
## [[23]]
## [1] "'latin hip hop'"  " 'reggaeton'"     " 'trap latino'"   " 'urbano latino'"
## 
## [[24]]
## [1] "'k-pop girl group'"
## 
## [[25]]
## [1] "'canadian contemporary r&b'" " 'canadian pop'"            
## [3] " 'pop'"                     
## 
## [[26]]
## [1] "'piano rock'" " 'pop'"      
## 
## [[27]]
## [1] "'bedroom pop'"
## 
## [[28]]
## [1] "'contemporary country'"
## 
## [[29]]
## [1] "'modern indie pop'"       " 'pov: indie'"           
## [3] " 'singer-songwriter pop'"
## 
## [[30]]
## [1] "'gen z singer-songwriter'"
## 
## [[31]]
## [1] "'reggaeton'"      " 'trap latino'"   " 'urbano latino'"
## 
## [[32]]
## [1] "'argentine hip hop'" " 'pop venezolano'"   " 'trap argentino'"  
## [4] " 'trap latino'"      " 'urbano latino'"   
## 
## [[33]]
## [1] "'puerto rican pop'" " 'reggaeton'"       " 'trap latino'"    
## [4] " 'urbano latino'"  
## 
## [[34]]
## [1] "'colombian pop'"         " 'pop reggaeton'"       
## [3] " 'reggaeton'"            " 'reggaeton colombiano'"
## [5] " 'trap latino'"          " 'urbano latino'"       
## 
## [[35]]
## [1] "'dance pop'" " 'pop'"     
## 
## [[36]]
## [1] "'modern alternative rock'" " 'modern rock'"           
## [3] " 'pop'"                   
## 
## [[37]]
## [1] "'pop'"  " 'r&b'" " 'rap'"
## 
## [[38]]
## [1] "'pop'"
## 
## [[39]]
## [1] "'canadian contemporary r&b'" " 'canadian pop'"            
## [3] " 'pop'"                     
## 
## [[40]]
## [1] "'reggaeton'"      " 'trap latino'"   " 'urbano latino'"
## 
## [[41]]
## [1] "'k-pop'"
## 
## [[42]]
## [1] "'detroit hip hop'" " 'hip hop'"        " 'rap'"           
## 
## [[43]]
## [1] "'uk contemporary r&b'" " 'uk pop'"            
## 
## [[44]]
## [1] "'pop'"
## 
## [[45]]
## [1] "'corridos tumbados'" " 'sad sierreno'"    
## 
## [[46]]
## [1] "'hip hop'" " 'rap'"   
## 
## [[47]]
## [1] "'reggaeton'"      " 'trap latino'"   " 'urbano latino'"
## 
## [[48]]
## [1] "'k-pop'"             " 'k-pop girl group'"
## 
## [[49]]
## [1] "'corrido'"            " 'corridos tumbados'" " 'musica mexicana'"  
## [4] " 'sad sierreno'"      " 'sierreno'"         
## 
## [[50]]
## [1] "'bedroom pop'"
```


```r
genrevector <- spotify2023$genres
```


```r
genrevector
```

```
##  [1] "'pop'"                                                                                                
##  [2] "'pop', 'r&b', 'rap'"                                                                                  
##  [3] "'pop'"                                                                                                
##  [4] "'k-pop'"                                                                                              
##  [5] "'corrido', 'corridos tumbados', 'sad sierreno', 'sierreno'"                                           
##  [6] "'pop'"                                                                                                
##  [7] "'rap'"                                                                                                
##  [8] "'afrobeats', 'nigerian pop'"                                                                          
##  [9] "'argentine hip hop', 'pop venezolano', 'trap argentino', 'trap latino', 'urbano latino'"              
## [10] "'pop'"                                                                                                
## [11] "'garage rock', 'modern rock', 'permanent wave', 'rock', 'sheffield indie'"                            
## [12] "'reggaeton', 'reggaeton colombiano', 'trap latino', 'urbano latino'"                                  
## [13] "'reggaeton', 'urbano mexicano'"                                                                       
## [14] "'big room', 'dance pop', 'edm', 'pop', 'pop dance'"                                                   
## [15] "'canadian contemporary r&b', 'canadian pop', 'pop'"                                                   
## [16] "'musica chihuahuense'"                                                                                
## [17] "'pop', 'uk pop'"                                                                                      
## [18] "'colombian pop', 'latin pop', 'reggaeton', 'reggaeton colombiano', 'trap latino', 'urbano latino'"    
## [19] "'bedroom pop'"                                                                                        
## [20] "'gen z singer-songwriter', 'singer-songwriter pop'"                                                   
## [21] "'chill pop'"                                                                                          
## [22] "'canadian contemporary r&b', 'canadian pop', 'pop'"                                                   
## [23] "'latin hip hop', 'reggaeton', 'trap latino', 'urbano latino'"                                         
## [24] "'k-pop girl group'"                                                                                   
## [25] "'canadian contemporary r&b', 'canadian pop', 'pop'"                                                   
## [26] "'piano rock', 'pop'"                                                                                  
## [27] "'bedroom pop'"                                                                                        
## [28] "'contemporary country'"                                                                               
## [29] "'modern indie pop', 'pov: indie', 'singer-songwriter pop'"                                            
## [30] "'gen z singer-songwriter'"                                                                            
## [31] "'reggaeton', 'trap latino', 'urbano latino'"                                                          
## [32] "'argentine hip hop', 'pop venezolano', 'trap argentino', 'trap latino', 'urbano latino'"              
## [33] "'puerto rican pop', 'reggaeton', 'trap latino', 'urbano latino'"                                      
## [34] "'colombian pop', 'pop reggaeton', 'reggaeton', 'reggaeton colombiano', 'trap latino', 'urbano latino'"
## [35] "'dance pop', 'pop'"                                                                                   
## [36] "'modern alternative rock', 'modern rock', 'pop'"                                                      
## [37] "'pop', 'r&b', 'rap'"                                                                                  
## [38] "'pop'"                                                                                                
## [39] "'canadian contemporary r&b', 'canadian pop', 'pop'"                                                   
## [40] "'reggaeton', 'trap latino', 'urbano latino'"                                                          
## [41] "'k-pop'"                                                                                              
## [42] "'detroit hip hop', 'hip hop', 'rap'"                                                                  
## [43] "'uk contemporary r&b', 'uk pop'"                                                                      
## [44] "'pop'"                                                                                                
## [45] "'corridos tumbados', 'sad sierreno'"                                                                  
## [46] "'hip hop', 'rap'"                                                                                     
## [47] "'reggaeton', 'trap latino', 'urbano latino'"                                                          
## [48] "'k-pop', 'k-pop girl group'"                                                                          
## [49] "'corrido', 'corridos tumbados', 'musica mexicana', 'sad sierreno', 'sierreno'"                        
## [50] "'bedroom pop'"
```

```r
pop <- which(genrevector %in% c("'pop'"))
```



```r
spotify2023 %>%
  mutate(pop = ifelse("'pop'" %in% genres, TRUE, FALSE))
```

```
## # A tibble: 50 × 20
##    artist_name    track_name  is_explicit album_release_date genres danceability
##    <chr>          <chr>       <lgl>       <date>             <chr>         <dbl>
##  1 Miley Cyrus    Flowers     FALSE       2023-08-18         'pop'         0.706
##  2 SZA            Kill Bill   FALSE       2022-12-08         'pop'…        0.644
##  3 Harry Styles   As It Was   FALSE       2022-05-20         'pop'         0.52 
##  4 Jung Kook      Seven (fea… TRUE        2023-11-03         'k-po…        0.79 
##  5 Eslabon Armado Ella Baila… FALSE       2023-04-28         'corr…        0.668
##  6 Taylor Swift   Cruel Summ… FALSE       2019-08-23         'pop'         0.552
##  7 Metro Boomin   Creepin' (… TRUE        2022-12-02         'rap'         0.715
##  8 Rema           Calm Down … FALSE       2023-04-27         'afro…        0.799
##  9 Bizarrap       Shakira: B… FALSE       2023-01-11         'arge…        0.778
## 10 Taylor Swift   Anti-Hero   FALSE       2022-10-21         'pop'         0.637
## # ℹ 40 more rows
## # ℹ 14 more variables: valence <dbl>, energy <dbl>, loudness <dbl>,
## #   acousticness <dbl>, instrumentalness <dbl>, liveness <dbl>,
## #   speechiness <dbl>, key <dbl>, tempo <dbl>, mode <dbl>, duration_ms <dbl>,
## #   time_signature <dbl>, popularity <dbl>, pop <lgl>
```



```r
df <- data.frame(matrix(unlist(strsplit(spotify2023$genres, ", ")), nrow=1, byrow=TRUE))
```


```r
# Sample data frame with genres in a single column
df <- data.frame(genres = "['pop']")

# Split the genres column into separate columns
split_genres <- strsplit(as.character(df$genres), ", ")[[1]]

# Transpose the result to create columns instead of rows
split_genres <- t(matrix(split_genres, nrow=1))

# Create a data frame with the split genres as columns
df_split <- data.frame(split_genres)

# Rename the columns if needed
colnames(df_split) <- paste0("genre_", 1:ncol(df_split))

# Print the resulting data frame
print(df_split)
```

```
##   genre_1
## 1 ['pop']
```

```r
#genre_list <- strsplit(spotify2023$genres, ", ")
#unique_genres <- unique(unlist(genre_list))
#b <- cbind(spotify2023,genre_matrix)

#genre_matrix <- matrix(0, nrow = nrow(spotify2023), ncol = length(unique_genres), dimnames = list(NULL, unique_genres))
#for (i in 1:length(genre_list)) {
  #genre_matrix[i, genre_list[[i]]] <- 1}
#genre_df <- as.data.frame(genre_matrix)
#df <- cbind(df, genre_df)
```

















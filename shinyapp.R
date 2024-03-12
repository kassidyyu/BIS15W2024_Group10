library(tidyverse)
library(janitor)
library(naniar)
library(tidyr)
library(shiny)
library(shinydashboard)

spotify2023 <- read_csv("top_50_2023.csv") %>% clean_names()
split_spotify2023 <- spotify2023 %>% 
  separate(genres, into = c("genre1", "genre2", "genre3", "genre4", "genre5", "genre6"), sep=",") %>% 
  pivot_longer(cols = starts_with("genre"),
               names_to = "type",
               values_to = "class") %>% 
  filter(class != "NA") %>% 
  select(-type)
split_spotify2023 <- split_spotify2023 %>% 
  separate(class, into = c("punctuation", "class2"), sep = "'") %>% 
  select(-punctuation)
split_spotify2023 <- split_spotify2023 %>%
  pivot_wider(
    names_from = class2,
    values_from = class2,
    values_fn = length) %>% 
  mutate(across(everything(), ~replace(.x, is.na(.x), 0)))
spotify <- split_spotify2023 %>% 
  separate(album_release_date, into = c("year", "month", "day"), sep = "-") %>% 
  clean_names()

ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Spotify Data Analysis", titleWidth = 350),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Songs in Each Genre", tabName = "genre"),
                        menuItem("Pop Songs Table", tabName = "pop"),
                        menuItem("Top Artists", tabName = "top5"),
                        menuItem("Pop Genres", tabName = "pop_genre"),
                        menuItem("Canadian Pop Table", tabName = "canadian_pop"),
                        menuItem("K-Pop and K-Pop Girl Group Table", tabName = "kpop"),
                        menuItem("Average Duration", tabName = "duration"),
                        menuItem("Explicit vs. Non-Explicit Songs", tabName = "loudness"),
                        menuItem("Danceability and Energy", tabName = "dance_energy"),
                        menuItem("Danceability and Tempo", tabName = "dance_tempo"),
                        menuItem("Energy and Tempo", tabName = "energy_tempo"),
                        menuItem("Acoustics and Liveness", tabName = "acoustic_liveness"),
                        menuItem("Danceability and Valance", tabName = "dance_valance"),
                        menuItem("Energy and Valance", tabName = "energy_valance")
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "genre",
                                fluidRow(
                                  box(plotOutput("genre"), width = 12)
                                )
                        ),
                        
                        tabItem(tabName = "pop",
                                fluidRow(
                                  box(tableOutput("pop"), width = 12)
                                )
                        ),
                        tabItem(tabName = "top5",
                                fluidRow(
                                  box(plotOutput("top5"), width = 12)
                                )
                        ),
                        tabItem(tabName = "pop_genre",
                                fluidRow(
                                  box(plotOutput("pop_genre"), width = 12)
                                )
                        ),
                        tabItem(tabName = "canadian_pop",
                                fluidRow(
                                  box(tableOutput("canadian_pop"), width = 12)
                                )
                        ),
                        tabItem(tabName = "kpop",
                                fluidRow(
                                  box(tableOutput("kpop"), width = 12)
                                )
                        ),
                        tabItem(tabName = "duration",
                                fluidRow(
                                  box(plotOutput("duration"), width = 12)
                                )
                        ),
                        tabItem(tabName = "loudness",
                                fluidRow(
                                  box(plotOutput("loudness"), width = 12)
                                )
                        ),
                        tabItem(tabName = "dance_energy",
                                fluidRow(
                                  box(plotOutput("dance_energy"), width = 12)
                                )
                        ),
                        tabItem(tabName = "dance_tempo",
                                fluidRow(
                                  box(plotOutput("dance_tempo"), width = 12)
                                )
                        ),
                        tabItem(tabName = "energy_tempo",
                                fluidRow(
                                  box(plotOutput("energy_tempo"), width = 12)
                                )
                        ),
                        tabItem(tabName = "acoustic_liveness",
                                fluidRow(
                                  box(plotOutput("acoustic_liveness"), width = 12)
                                )
                        ),
                        tabItem(tabName = "dance_valance",
                                fluidRow(
                                  box(plotOutput("dance_valance"), width = 12)
                                )
                        ),
                        tabItem(tabName = "energy_valance",
                                fluidRow(
                                  box(plotOutput("energy_valance"), width = 12)
                                )
                        )
                      )
                    )
)

server <- function(input, output) {
  output$genre <- renderPlot({
    spotify %>% 
      select(pop:musica_chihuahuense) %>% 
      gather(key = "genre", value = "count") %>% 
      ggplot(aes(x = reorder(genre, count), y = count)) +
      geom_bar(stat = "identity", fill = "springgreen3", color="black") +
      labs(title = "Number of Songs in Each Genre",
           x = "Genre",
           y = "Number of Songs")+
      coord_flip()+
      theme_minimal()+
      theme(text = element_text(size = 15))+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$top5 <- renderPlot({
    spotify %>%
      count(artist_name) %>%
      top_n(6) %>%
      ggplot(aes(x = reorder(artist_name, -n), y = n)) +
      geom_bar(stat = "identity", fill = "springgreen3", color="black") +
      labs(title = "Top Artists with the Most Songs",
           x = NULL,
           y = "Number of Songs")+
      theme_minimal()+
      theme(text = element_text(size = 20))+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$pop <- renderTable({
    spotify %>% 
      filter(pop == 1) %>% 
      select(track_name, artist_name)
  })
  
  
  output$pop_genre <- renderPlot({
    spotify %>% 
      select(contains("pop")) %>% 
      select(-popularity) %>% 
      gather(key = "pop", value = "count") %>% 
      ggplot(aes(x = reorder(pop, count), y = count)) +
      geom_bar(stat = "identity", fill = "springgreen3", color="black") +
      labs(title = "All Pop Songs",
           x = NULL,
           y = "Number of Songs")+
      coord_flip()+
      theme_minimal()+
      theme(text = element_text(size = 20))+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$canadian_pop <- renderTable({
    spotify %>% 
      filter(canadian_pop == 1) %>% 
      select(track_name, artist_name)
  })
  
  output$kpop <- renderTable({
    spotify %>%
      filter(k_pop > 0 | k_pop_girl_group >0) %>%
      select(artist_name, track_name)
  })
  
  output$duration <- renderPlot({
    spotify %>% 
      mutate(duration_ms = duration_ms/60000) %>%
      ggplot(aes(x = duration_ms)) +
      geom_density(fill = "springgreen3") +
      labs(title = " Average Duration of All Songs",
           x = "Duration (min)",
           y = "Density")+
      theme_minimal()+
      theme(text = element_text(size = 20))+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$loudness <- renderPlot({
    spotify %>%
      mutate(is_explicit = ifelse(is_explicit == 1, "Explicit", "Not Explicit")) %>%
      ggplot(aes(x = is_explicit, y = loudness, fill = is_explicit)) +
      geom_boxplot(fill="springgreen3") +
      labs(title = "Loudness of Explicit and Non-Explicit Songs",
           x = NULL,
           y = "Loudness")+
      theme_minimal()+
      theme(text = element_text(size = 20))+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$dance_energy <- renderPlot({
    spotify %>% 
      ggplot(aes(x = danceability, y = energy)) +
      geom_point(size = 2.5) +
      geom_smooth(method = "lm", color="springgreen3") +
      labs(title = "Analysis of the Danceability and Energy of All Songs",
           x = "Danceability",
           y = "Energy")+
      theme_minimal()+
      theme(text = element_text(size = 20))+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$dance_tempo <- renderPlot({
    spotify %>%
      ggplot(aes(x = danceability, y = tempo)) +
      geom_point(size=2.5) +
      labs(title = "Analysis of the Danceability and Tempo of All Songs",
           x = "Danceability",
           y = "Tempo")+
      theme_minimal()+
      theme(text = element_text(size = 20))+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$energy_tempo <- renderPlot({
    spotify %>%
      ggplot(aes(x = energy, y = tempo)) +
      geom_point(size=2.5) +
      labs(title = "Analysis of the Energy and Tempo of All Songs",
           x = "Energy",
           y = "Tempo")+
      theme_minimal()+
      theme(text = element_text(size = 20))+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$acoustic_liveness <- renderPlot({
    spotify %>%
      ggplot(aes(x = acousticness, y = liveness)) +
      geom_point(size=2.5) +
      labs(title = "Analysis of the Acoustics and Liveness of All Songs",
           x = "Acousticness",
           y = "Liveness")+
      theme_minimal()+
      theme(text = element_text(size = 20))+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$dance_valance <- renderPlot({
    spotify %>%
      ggplot(aes(x = danceability, y = valence)) +
      geom_point(size=2.5) +
      geom_smooth(method = "lm", color="springgreen3") +
      labs(title = "Analysis of the Dancability and Valance of All Songs",
           x = "Danceability",
           y = "Valence")+
      theme_minimal()+
      theme(text = element_text(size = 20))+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$energy_valance <- renderPlot({
    spotify %>%
      ggplot(aes(x = energy, y = valence)) +
      geom_point(size=2.5) +
      geom_smooth(method = "lm", color="springgreen3") +
      labs(title = "Analysis of the Energy and Valance of All Songs",
           x = "Energy",
           y = "Valence")+
      theme_minimal()+
      theme(text = element_text(size = 20))+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
}

shinyApp(ui, server)


rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(RColorBrewer)
library(viridisLite)
library(viridis)
library(plotly)


############
##### 

# Load necessary library
library(tidyverse)

# Load the data
taylor_all_songs <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_all_songs.csv")


# Aggregate mean tempo for each combination of key_mode, mode_name, and year
taylor_all_songs %>%
  mutate(year = year(as.Date(album_release, format = "%Y-%m-%d"))) %>%
  group_by(key_mode, mode_name, year) %>%
  summarise(mean_tempo = mean(tempo, na.rm = TRUE),
            mean_danceability = mean(danceability,
                                     na.rm = TRUE),
            mean_energy = mean(energy,na.rm = TRUE),
            mean_loudness = mean(loudness,
                                 na.rm = TRUE),
            mean_speechiness = mean(speechiness,
                                    na.rm = TRUE),
            mean_acousticness = mean(acousticness,
                                     na.rm = TRUE),
            mean_instrumentalness = mean(instrumentalness,
                                         na.rm = TRUE),
            mean_liveness = mean(liveness,
                                 na.rm = TRUE),
            mean_valence = mean(valence,
                                na.rm = TRUE),
            mean_duration_ms = mean(duration_ms,
                                    na.rm = TRUE)) %>%
  ungroup() %>% # Ungroup the data
  mutate(key_mode = factor(key_mode)) %>%
  mutate(key_mode = fct_rev(fct_infreq(key_mode))) %>%  # Order key_mode by frequency
  drop_na() %>%
  ggplot(aes(x = key_mode,  # Now key_mode is ordered by frequency
             y = as.factor(year),
             fill = mean_tempo)) +
  geom_tile() +
  scale_fill_viridis(direction = -1) +
  coord_flip() +
  labs(title = "Mean Tempo of Taylor Swift's Songs by Key Mode and Year",
       x = "Key Mode",
       y = "Year",
       fill = "Mean Tempo") +
  theme_minimal() 

plot

library(spotifyr)
library(tidyverse)
library()

Sys.setenv(SPOTIFY_CLIENT_ID = '53ba575163c44c7fafb6d92fce5b6ccf')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '811671ac311940deb8c02445f07fd3cb')

access_token <- get_spotify_access_token()

viz <- get_playlist_tracks(playlist_id = "1Z5RqB2SspsCGnvgVEhViT")

viz2 <- get_track_audio_features(viz$track.id) |> 
  left_join(viz |> select(track.id, track.name, track.artists), 
            by = c("id"="track.id")) |> 
  unnest(track.artists, names_sep="_")

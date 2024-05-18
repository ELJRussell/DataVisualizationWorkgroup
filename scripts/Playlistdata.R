library(spotifyr)
library(tidyverse)
library(here)
options(scipen=999)

access_token <- get_spotify_access_token()

viz <- get_playlist_tracks(playlist_id = "1Z5RqB2SspsCGnvgVEhViT")
RollingStones <- get_playlist_tracks(playlist_id="55DolH2rhrKh7IkM8y6KXG")
RollingStones100 <- get_playlist_tracks(playlist_id="55DolH2rhrKh7IkM8y6KXG", offset=100)
RollingStones200 <- get_playlist_tracks(playlist_id="55DolH2rhrKh7IkM8y6KXG", offset=200)
RollingStones300 <- get_playlist_tracks(playlist_id="55DolH2rhrKh7IkM8y6KXG", offset=300)
RollingStones400 <- get_playlist_tracks(playlist_id="55DolH2rhrKh7IkM8y6KXG", offset=400)

viz2 <- get_track_audio_features(viz$track.id) |> 
  left_join(viz |> select(track.id, track.name, track.artists), 
            by = c("id"="track.id")) |> 
  unnest(track.artists, names_sep="_") |> 
  mutate(mode=case_when(
           mode==1 ~ "Major",
           mode==0 ~ "Minor"
         ),
         key=case_when(
           key==0 ~ "C",
           key==1 ~ "C#",
           key==2 ~ "D",
           key==3 ~ "D#",
           key==4 ~ "E",
           key==5 ~ "F",
           key==6 ~ "F#",
           key==7 ~ "G",
           key==8 ~ "G#",
           key==9 ~ "A",
           key==10 ~ "A#",
           key==11 ~ "B"
         ),
         key_mode=paste(key, mode, sep=" ")) |> 
  relocate(track.name,track.artists_name,key,mode,key_mode) |> 
  select(1:14) |> 
  rename(song=track.name,
         artist=track.artists_name)


Rollingstonesconvert <- function(song_id,off_set){
  
RollingStones2 <- get_track_audio_features(song_id$track.id) |> 
  left_join(song_id |> select(track.id, track.name, track.artists), 
            by = c("id"="track.id")) |> 
  unnest(track.artists, names_sep="_") |> 
  distinct(id, .keep_all = TRUE) |> 
  mutate(top_100_number=row_number()+off_set,
         mode=case_when(
           mode==1 ~ "Major",
           mode==0 ~ "Minor"
         ),
         key=case_when(
           key==0 ~ "C",
           key==1 ~ "C#",
           key==2 ~ "D",
           key==3 ~ "D#",
           key==4 ~ "E",
           key==5 ~ "F",
           key==6 ~ "F#",
           key==7 ~ "G",
           key==8 ~ "G#",
           key==9 ~ "A",
           key==10 ~ "A#",
           key==11 ~ "B"
         ),
         key_mode=paste(key, mode, sep=" ")) |> 
  relocate(top_100_number,track.name,track.artists_name,key,mode,key_mode) |>
  select(1:15) |> 
  rename(song=track.name,
         artist=track.artists_name)
}

Rollingstones1 <- Rollingstonesconvert(RollingStones,0)
Rollingstones2 <- Rollingstonesconvert(RollingStones100,100)
Rollingstones3 <- Rollingstonesconvert(RollingStones200,200)
Rollingstones4 <- Rollingstonesconvert(RollingStones300,300)
Rollingstones5 <- Rollingstonesconvert(RollingStones400,400)

Rollingstones <- bind_rows(Rollingstones1,Rollingstones2,Rollingstones3,Rollingstones4,Rollingstones5)

ggplot(Rollingstones, aes(x=energy, y=acousticness)) +
  geom_point(aes(color=mode)) +
  labs(title="Rolling Stones Playlist") +
  geom_smooth(method="gam") +
  theme_minimal()

write.csv(Rollingstones, here("data","RollingStones.csv"), row.names=FALSE)
write.csv(viz2, here("data","WorkingGroupSpotify.csv"), row.names=FALSE)

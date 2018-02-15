################
#    INPUTS    #
#################################################################################################################
user_id = c("1253287932", "22g4tscsn2sd3nqor5du5i5gq", "1219714259") # all user ids of the people you want to make the playlist for
length_of_playlist = 12 # hours
playlist_name = "Partitioning Around Medoids"
#################################################################################################################


library(devtools)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(dbscan)
library(cluster)
library(jsonlite)
library(httr)
# devtools::install_github("brooke-watson/BRRR")
library(BRRR)


########################
#     AUTHORIZATION    #
#################################################################################################################
client_id = "#"
client_secret = "#"
token_id = "#"
o = httr::oauth_endpoint(authorize = "https://accounts.spotify.com/authorize", access = "https://accounts.spotify.com/api/token")
a = httr::oauth_app("app_id", client_id, client_secret)
keys = httr::oauth2.0_token(o, a, scope = "playlist-modify-public")
remove(o,a)
#################################################################################################################

#########################
#    DATA COLLECTION    #
#################################################################################################################
# Creates a dataframe called 'user_features' with all available song features from the users' playlists
# 34 seconds to run
#################################################################################################################

# get all song features when provided a string of tracks
getPlaylistFeatures = function(playlist_tracks, token){
  req2 = httr::GET(playlist_tracks, httr::config(token = keys))
  json2.1 = httr::content(req2)
  json2.2 = jsonlite::fromJSON(jsonlite::toJSON(json2.1))$items
  # create string of ids for first 100 songs in playlist (API limit)
  for(i in 1:length(json2.2$track$id)){
    if(i == 1){
      tracks_string = json2.2$track$id[[i]]  
    } else {
      tracks_string = paste0(tracks_string, ",", json2.2$track$id[[i]])
    }
  }
  req3 = httr::GET(paste0("https://api.spotify.com/v1/audio-features/?ids=",tracks_string), httr::config(token = token)) 
  json3.1 = httr::content(req3)
  json3.2 = jsonlite::fromJSON(jsonlite::toJSON(json3.1))$audio_features 
  dados3 = data.frame(id=json3.2["id"],
                     danceability=json3.2["danceability"],
                     energy=json3.2["energy"],
                     key=json3.2["key"],
                     loudness=json3.2["loudness"],
                     mode=json3.2["mode"],
                     speechiness=json3.2["speechiness"],
                     acousticness=json3.2["acousticness"],
                     instrumentalness=json3.2["instrumentalness"],
                     liveness=json3.2["liveness"],
                     valence=json3.2["valence"],
                     tempo=json3.2["tempo"],
                     duration_ms=json3.2["duration_ms"],
                     time_signature=json3.2["time_signature"],
                     uri=json3.2["uri"],
                     analysis_url=json3.2["analysis_url"],stringsAsFactors = F)
  return(dados3)
}

for(i in 1:length(user_id)){
  if(i == 1){
    # Get the Top 20 playlists from the user
    req1 = httr::GET(paste0("https://api.spotify.com/v1/users/",user_id[1],"/playlists"), httr::config(token = keys))
    json1 = httr::content(req1)
    json2 = jsonlite::fromJSON(jsonlite::toJSON(json1))$items
    
    # Get the song features from each song in all of the playlists
    for(i in 1:20){
      if (i == 1){
        user_features = getPlaylistFeatures(json2$tracks$href[[1]], keys)
      } else{
        user_features = rbind(user_features, getPlaylistFeatures(json2$tracks$href[[i]], keys))
      }
    }
  } else{
    
    # Get the Top 20 playlists from the user
    req1 = httr::GET(paste0("https://api.spotify.com/v1/users/",user_id[i],"/playlists"), httr::config(token = keys))
    json1 = httr::content(req1)
    json2 = jsonlite::fromJSON(jsonlite::toJSON(json1))$items
    
    # Get the song features from each song in all of the playlists
    for(i in 1:20){
      user_features = rbind(user_features, getPlaylistFeatures(json2$tracks$href[[i]], keys))
    }
  }
}
remove(i, req1, json1, json2, getPlaylistFeatures)

# 37 seconds

#######################
#    PRE-PROCESSING   #
#################################################################################################################
# Create a euclidean distance matrix of songs in 'user_features' using range standardized numeric variables
# Use PCA to reduce variables to n that explain 90% of variance in dataset
#################################################################################################################
df = cbind(unlist(user_features["danceability"]),
           unlist(user_features["energy"]),
           unlist(user_features["key"]),
           unlist(user_features["loudness"]),
           unlist(user_features["mode"]),
           unlist(user_features["speechiness"]),
           unlist(user_features["acousticness"]),
           unlist(user_features["instrumentalness"]),
           unlist(user_features["liveness"]),
           unlist(user_features["valence"]),
           unlist(user_features["tempo"]),
           unlist(user_features["duration_ms"]),
           unlist(user_features["time_signature"])); 
rownames(df) = unlist(user_features["id"]); df = data.frame(df)

df_std = data.frame(apply(df,2,function(x){center = min(x); spread = max(x) - min(x);list = (x - center)/spread;}))
pca = prcomp(df_std) 
pca_cutoff = function(pca_sdev, cutoff=.9){
  num_pc = 0;
  for(i in 1:length(pca_sdev)) {
    num_pc = num_pc+1;
    if((sum(pca_sdev[1:i]^2)/sum(pca_sdev^2)) > cutoff) return(num_pc);
  }
}
df_pca = data.frame(pca$x[,1:pca_cutoff(pca$sdev, cutoff=.9)]) # use scores of those principal components for clustering
dist = as.matrix(dist(df_pca,method = "euclidean")) 

remove(df, df_pca, df_std, pca, pca_cutoff)


#######################
#      CLUSTERING     #
#################################################################################################################
# Perform clustering using k-medoids
# skip swap phase for run-time efficiency
#################################################################################################################
k = round( 3600000*length_of_playlist / mean(unlist(user_features$duration_ms)) ) # ~ 186 songs for a 12 hour playlist
pam_fit = pam(dist, diss = TRUE, k = k)
playlist = unlist(user_features$uri)[as.numeric(pam_fit$medoids)]
remove(k, pam_fit)


#######################
#   OUTPUT PLAYLIST   #
#################################################################################################################
# Create a playlist with the provided name
# Have all specified users follow it
#################################################################################################################

createPlaylist = function(playlist, playlist_name, user_id, keys){
  # Make the playlist
  req1 = httr::POST(paste0("https://api.spotify.com/v1/users/",user_id,"/playlists"), httr::config(token = keys),  body=list(name=playlist_name), encode="json")
  
  # Add the songs to the playlist in chunks of 100
  for(i in 1:floor(length(playlist)/100)){
    req2 = httr::POST(paste0("https://api.spotify.com/v1/users/",user_id,"/playlists/", content(req1)$id,"/tracks"), httr::config(token = keys), body=list(uris=playlist[(i*100-99):(i*100)]), encode="json")
  }; req2 = httr::POST(paste0("https://api.spotify.com/v1/users/",user_id,"/playlists/", content(req1)$id,"/tracks"), httr::config(token = keys), body=list(uris=playlist[(100*floor(length(playlist)/100)+1):(length(playlist))]), encode="json")

  return(content(req1)$id)
}

playlist_id = createPlaylist(playlist, playlist_name, token_id, keys)

remove(dist, user_features, playlist, playlist, createPlaylist)


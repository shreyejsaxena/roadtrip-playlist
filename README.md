# Mashup your friends' Spotify playlists
Creates the perfect roadtrip playlist by clustering all of the songs in you and your friends' followed Spotify playlists and extracting the most representative songs from that universe of music. 

Edit the INPUTS to have the Spotify URIs of the users you want to to cluster on as well as the length and name of the playlist. Edit the AUTHORIZATIONS to have your applicable Spotify credentials. 

Clustering methodology uses partioning around medoids to identify representative observations. Will only take songs from each users first 20 followed playlists due to rate limiting. 

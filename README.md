# Billboard Top 100 - Dimensionality Reduction
In this project the goal is to preprocess the data and perform some feature engineering on the Billboard Top 100 songs data. We will be performing couple of Dimensionalty Reduction techniques such as Principal Component Analysis(PCA) and Partial Least Squares(PLS).

## Goals:
Our modelling goal is to use dimensionality reduction for features of Billboard Top 100 songs, connecting data about where the songs were in the rankings with the various audio features available to us through Spotify (present in the *audio_features.csv* file)


## Results:
### PCA - 
- The first component is mainly about the loudness, energy and negatively related to acousticness of the songs.
- The second component is about the valence, high valence means more positive, happy the mood of the song.

### PLS - 
- Here we saw that the *spotify_track_popularity* feature is a big contributor to the first and second components.
- The second component has negative relation to speechiness and loudness of the song.
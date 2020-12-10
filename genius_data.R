
# first i want to check if it is allowed to scrap data
library("robotstxt")
get_robotstxt(domain = "https://genius.com/")


library(tidyverse)


# First we want to extract the top 100 songs of each genre so we can perform easier classification
# later by predicting the genre (and already having the label)
library(rvest)

# Top 100 rap songs of the month
genius_rap.html <- read_html("Genius_top100_rap.html")
artist <- genius_rap.html %>% html_nodes("div.PageGridFull-idpot7-0.jeWXO  a  h4") %>% html_text()
song <- genius_rap.html %>% html_nodes("div.PageGridFull-idpot7-0.jeWXO  a  div.ChartSongdesktop__CoverAndTitle-sc-18658hh-0.jzapEV  h3  div.ChartSongdesktop__Title-sc-18658hh-3.fODYHn") %>% html_text()
toprap <- tibble(data.frame(artist=artist,song =song,genre="rap"))


# Top 100 pop songs of the month  

genius_pop.html <- read_html("Genius_top100_pop.html")
artist <- genius_pop.html %>% html_nodes("div.PageGridFull-idpot7-0.jeWXO  a  h4") %>% html_text()
song <- genius_pop.html %>% html_nodes("div.PageGridFull-idpot7-0.jeWXO  a  div.ChartSongdesktop__CoverAndTitle-sc-18658hh-0.jzapEV  h3  div.ChartSongdesktop__Title-sc-18658hh-3.fODYHn") %>% html_text()
toppop <- tibble(data.frame(artist=artist,song =song,genre="pop"))

# Top 100 rock songs of the month 
genius_rock.html <- read_html("Genius_top100_rock.html")
artist <- genius_rock.html %>% html_nodes("div.PageGridFull-idpot7-0.jeWXO  a  h4") %>% html_text()
song <- genius_rock.html %>% html_nodes("div.PageGridFull-idpot7-0.jeWXO  a  div.ChartSongdesktop__CoverAndTitle-sc-18658hh-0.jzapEV  h3  div.ChartSongdesktop__Title-sc-18658hh-3.fODYHn") %>% html_text()
toprock <- tibble(data.frame(artist=artist,song =song,genre= "rock"))

# Top 100 r&b songs of the month 
genius_rb.html <- read_html("Genius_top100_r&b.html")
artist <- genius_rb.html %>% html_nodes("div.PageGridFull-idpot7-0.jeWXO  a  h4") %>% html_text()
song <- genius_rb.html %>% html_nodes("div.PageGridFull-idpot7-0.jeWXO  a  div.ChartSongdesktop__CoverAndTitle-sc-18658hh-0.jzapEV  h3  div.ChartSongdesktop__Title-sc-18658hh-3.fODYHn") %>% html_text()
toprb <- tibble(data.frame(artist=artist,song =song,genre="r&b"))

#tibble with top100 of each genre
topall <- rbind(toprap,toppop,toprock,toprb)


#use the package genius to extract lyrics

install.packages("genius")
library(genius)

#the folowing code allows us to try the package
#learnr::run_tutorial("genius_tutorial", "genius")
all_lyricsrap <- toprap %>% add_genius(artist, song, type = "lyrics")
final_lyrics <- all_lyrics %>% group_by(artist,song,genre) %>% summarize(lyrics= toString(lyric)) %>% ungroup()
 
 fwrite(final_lyrics, "final_lyrics.csv")


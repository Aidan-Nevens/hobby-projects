# This file is in written in an r markdown format. It is updated regularly. 

# Streaming Service Analysis and Relevant Strategy

The following analysis seeks to compare different streaming services to examine what exactly their unique product offering might be, and in what way each streaming service can improve its strategy in attracting a US-based audience. We'll be examining a dataset compiled by (Insert here) to get a sense of the accessibility and diversity of the titles offered within each streaming service. Additionally we'll conduct some sentiment analysis on twitter to see what the public's opinion of each streaming service varies, and how we might be better able to connect its service offerings in with these opinions. 

## Data Cleaning Processes

```{r setup, include=FALSE}
library(tidyverse)
library(stringr)
library(twitteR)
library(tidytext)
library(ggpubr)
theme_set(theme_pubr())
df_streaming = read_csv("MoviesOnStreamingPlatforms_updated.csv")
```

```{r cleaning}
df_streaming = df_streaming[,c(-1,-2)]
```

```{r peak}
df_streaming
```

```{r peak}
df_streaming$Genres = sub(",.*", "", df_streaming$Genres) #Removes all additionally listed genres after the first listed genre.
```
Usually movies are rated as PG, PG-13, TV-14, R, or NR (not-rated). We'll modify the Age column to better represent this.

```{r}
df_streaming$Age <- sub("13+\\+", "PG-13", df_streaming$Age)
df_streaming$Age <- sub("18+\\+", "R", df_streaming$Age)
df_streaming$Age <- sub("7+\\+", "PG", df_streaming$Age)
df_streaming$Age <- sub("16+\\+", "TV-14", df_streaming$Age) #TV-14 is equivalent to 16+ 
df_streaming$Age %>% replace_na("Not Rated")
df_streaming$Age <- sub("all", "G", df_streaming$Age)
```

```{r peaks}
df_streaming
```
```{r}
library(dplyr)
library(forcats)
df_streaming$Genres = fct_lump(df_streaming$Genres, prop = 0.005) #Keeps genres were there is close to 100 or more observations in the dataset, lumps smaller genres into "Other"
table(df_streaming$Genres)
```

#### Segmenting the Language Variable

Recall, that the language variable designates what languages a movie is available in. There are many ways one could analyze the language variable alone. Creating a dummy variable out of each language is one option, where we assign a 1 or 0 based on if the language is English (or Spanish, Italian, Chinese, etc.) or not. We can also assign a simple count of the number of languages a movie is shown in. Recall that the purpose of this analysis is to analyze the library of movies available in the United States. Therefore, we'll be analyzing whether or not the movie is in Spanish or English. This is because within the US there are an estimates 254,000,000 native English speakers, and 43,200,000 native Spanish speakers (https://www.babbel.com/en/magazine/most-spoken-languages-in-the-us). With an estimated population of 331,449,281 as reported in 2021 by the Census Bureau this would mean 89.66% of viewers in the US are native Spanish or English speakers and may even prefer to watch a movie available in their native languages. 

Therefore, we'll simply create two Dummy variables, one which shows whether a movie is available in English, and one that shows whether a movie is available in Spanish.

```{r language}
#English Binary Variable
df_streaming$English = str_extract(string = df_streaming$Language, pattern = "English") 
#Creates column "English and places the text "English" if the movie is shown in English
df_streaming$English = ifelse(df_streaming$English == "English", 1, 0) # Converts "English" to binary. 1 if movie available in English, 0 if not.
df_streaming$English = df_streaming$English %>% replace_na(0) # Replaces any NAs in the English Column with the value 0.

#Spanish Binary Variable
df_streaming$Spanish = str_extract(string = df_streaming$Language, pattern = "Spanish") 
#Creates column "Spanish and places the text "Spanish" if the movie is shown in Spanish
df_streaming$Spanish = ifelse(df_streaming$Spanish == "Spanish", 1, 0) # Converts "Spanish" to binary. 1 if movie available in Spanish, 0 if not.
df_streaming$Spanish = df_streaming$Spanish %>% 
  replace_na(0) # Replaces any NAs in the English Column with the value 0.

#Remove Original Language Variable
df_streaming = df_streaming[,-14]

df_streaming
```

### Segmenting the Country Variable

Recall that the country variable indicates what countries the specified movie title is available in. It is important to note that certain streaming services such as Netflix do not allow subscribers to view titles not available in their region. Because of this, it's important that we create a new dummy variable which we'll use to subset our dataset. The dataset needs to be subset in this manner because for the purposes of this analysis we only wish to compare title offerings across different streaming services within the US. We'll call this temporary variable "USA", and we'll simply filter our observations to instances where the title is available in the United States. The process to doing this is similar to how we created our two dummy variables for languages, with an added step where we filter our observations.

```{r Country}
df_streaming$USA = str_extract(string = df_streaming$Country, pattern ="United States")
#Extracts observations where the title is available in the US
df_streaming = df_streaming %>%
  filter(USA == "United States") #Filters observations to only include titles available in the United States
df_streaming = df_streaming [,c(-13,-17)] # Removes temporary "USA" column and Country Column
df_streaming
```
### Removing the % in Rotten Tomatoes to Convert to Numeric

```{r}

df_streaming$`Rotten Tomatoes`<- gsub('.{1}$', '', df_streaming$`Rotten Tomatoes`) #Removes Percent sign
df_streaming$`Rotten Tomatoes` <- as.numeric(df_streaming$`Rotten Tomatoes`) #Converts to numeric
colnames(df_streaming)[5] = "rotten_tomatoes_pct" #name fix
```


## Understanding Streaming Service Sentiments and Expectations

https://utstat.toronto.edu/~nathan/teaching/sta4002/Class1/scrapingtwitterinR-NT.html

As a major component of this analysis we want to understand some of the sentiments people hold towards the streaming services. Do we see positive sentiments? Negative? Can we potentially tie some of these sentiments to the exploratory analysis we can conduct and flag these assumptions as needing further research? These are all questions that we may be able to inch closer to an evidence-based answer following sentiment analysis.

```{r twitter, include = FALSE}
library(twitteR)

consumer_key <- "WITHHELD FROM GIT COMMIT"
consumer_secret <- "WITHELD FROM GIT COMMIT"
access_token <- "WITHELD FROM GIT COMMIT"
access_secret <- "WITHELD FROM GIT COMMIT"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
```

```{r Netflix}
fn_twitter <- searchTwitter("Netflix",n=1000,lang="en")

fn_twitter_df <- twListToDF(fn_twitter) # Convert to data frame


tweet_words <- fn_twitter_df %>% 
  select(id, text) %>%
  unnest_tokens(word,text)
```

```{r}

my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("netflix", "https", "t.co", "rt", "and","a","of","you","now", "is", "on", "a", "the", "amp", "developing", "development", "wii", "tdw0jykdr2", "stream", "breaking", "xakn8mgoby", "2", "streaming", "joe", "henderson", "choose", "ign", "remember", "learned", "33days",  "it’s", "poster", "i32o2x6cdu", "giant", "chibre", "nochucuddles", "netflixfr", "youngroyaisbaby","edit", "whoisaddison", "august", "julesisjohnson", "he's", "piece", "movie", "royals", "ellemagazine", "netflixnordic", "lucifer", "kob", "27th", "rydingedvin", "simonshoodie", "kemiadetiba", "edvinspasta", "20", "jungkook", "netlfix", "watching", "refresh", "accounts", "trailer", "set", "season", "tonight", "officially", "watch", "movies", "news", "executive", "write", "nibellion", "landing", "reminders", "goldenthriils", "youtube", "source", "netflix's", "watched", "premium", "pokemon", "pokémon"))) 

tweet_words_interesting <- tweet_words %>%
  anti_join(my_stop_words)

tweet_words_interesting %>% 
  group_by(word) %>%
  tally(sort=TRUE) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  xlab("")
```

Using this code we've ranked 10 relevant keywords. These are keywords that don't tie to specific titles. Because twitter is constantly changing, its important to store our findings at the time of analysis. So the following code was used to create a polished set of visualizations, which we'll simply import into the markdown as an image. 

```{r}
tweet_words_interesting %>% 
  group_by(word) %>%
  tally(sort=TRUE) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 10, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 10, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 15, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"))+ #Axis lines customizer
  theme(axis.ticks =element_line(color="light grey"))+
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.key=element_blank())+ #gganimate below
labs(title = 'Netflix: Twitter Keyword Rankings 6/26/21', x = 'Keywords', y = '# of Tweets Containing Word') 
```
```{r}
ggsave("NetflixKeywords.png")
```

```{r Netflix_Variety}

tweet_words_interesting %>% 
  group_by(word) %>%
  tally(sort=TRUE) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n, fill=factor(ifelse(word=="variety","Highlighted","Normal"))))+ 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("steelblue", "grey75"))+
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 10, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 10, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 15, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"))+ #Axis lines customizer
  theme(axis.ticks =element_line(color="light grey"))+
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.key=element_blank())+
   theme(legend.position = "none")+
labs(title = 'Netflix: Twitter Keyword Rankings 6/26/21', x = 'Keywords', y = '# of Tweets Containing Word')+
  annotate("text", x = 6.9, y = 680, label = "Tweeters often used the word 'variety' when discussing Netflix.", color = "steelblue", size = 4, face = "bold")
ggsave("NetflixTwitterVariety.png")
```

```{r Netflix_Content_Pref}
tweet_words_interesting %>% 
  group_by(word) %>%
  tally(sort=TRUE) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n, fill=factor(ifelse(word=="series","Highlighted",ifelse(word == "live", "Highlighted", ifelse(word=="action", "Highlighted", ifelse(word=="original", "Highlighted", "Normal")))))))+ 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("steelblue", "grey75"))+
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 10, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 10, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 15, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"))+ #Axis lines customizer
  theme(axis.ticks =element_line(color="light grey"))+
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.key=element_blank())+
   theme(legend.position = "none")+
labs(title = 'Netflix: Twitter Keyword Rankings 6/26/21', x = 'Keywords', y = '# of Tweets Containing Word')+
  annotate("text", x = 6.9, y = 900, label = "Tweeters often used the words 'series', 'live',", color = "steelblue", size = 4, face = "bold")+
   annotate("text", x = 7.3, y = 830, label = "'action', & 'original' when discussing Netflix content.", color = "steelblue", size = 4, face = "bold")+
ggsave("NetflixTwitterContent.png")
```
We can use the bing lexicon to derive sentiments from the words as well to see how Netflix stands with tweeters. We'll c

```{r}
bing_lex <- get_sentiments("bing")

fn_sentiment <- tweet_words_interesting %>% 
  left_join(bing_lex)

fn_sentiment %>% 
  filter(!is.na(sentiment)) %>%
  group_by(sentiment) %>% 
  summarise(n=n())
```

### Hulu

```{r Hulu}
fn_twitter <- searchTwitter("hulu",n=1000,lang="en")

fn_twitter_df <- twListToDF(fn_twitter) # Convert to data frame


tweet_words <- fn_twitter_df %>% 
  select(id, text) %>%
  unnest_tokens(word,text)
```

```{r hulustop}
my_stop_words <- stop_words %>% 
  select(-lexicon) %>% 
  bind_rows(data.frame(word = c("hulu", "https", "t.co", "rt", "and","a","of","you","now", "is", "on", "a", "the", "amp", "developing",  "it’s", "novak", "bjnovak", "b.j", "announce", "netflix", "building", "murders", "nbc’s", "platt", "ellis", "jon", "lucas", "tracee", "dae", "daniel", "kim", "bernthal", "ross", "ben", "dae", "daniel", "fir", "edebiri", "hedges", "star", "dever", "ayo", "kaitlyn", "discussingfilm"))) 

tweet_words_interesting <- tweet_words %>%
  anti_join(my_stop_words)

tweet_words_interesting %>% 
  group_by(word) %>%
  tally(sort=TRUE) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 10, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 10, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 15, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"))+ #Axis lines customizer
  theme(axis.ticks =element_line(color="light grey"))+
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.key=element_blank())+ #gganimate below
labs(title = 'Hulu: Twitter Keyword Rankings 6/26/21', x = 'Keywords', y = '# of Tweets Containing Word') 
ggsave("HuluTweetKeywords.png")
```
```{r HuluContentTweet}
tweet_words_interesting %>% 
  group_by(word) %>%
  tally(sort=TRUE) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n, fill=factor(ifelse(word=="excited","Highlighted",ifelse(word=="awaited", "Highlighted", "Normal")))))+ 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("steelblue", "grey75"))+
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 10, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 10, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 15, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"))+ #Axis lines customizer
  theme(axis.ticks =element_line(color="light grey"))+
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.key=element_blank())+
   theme(legend.position = "none")+
labs(title = 'Hulu: Twitter Keyword Rankings 6/26/21', x = 'Keywords', y = '# of Tweets Containing Word')+
  annotate("text", x = 5, y = 240, label = "Tweeters expressed anticipation towards recent Hulu Projects.", color = "steelblue", size = 4, type = "bold")
ggsave("HuluTwitterExcitement.png")
```

```{r}
tweet_words_interesting %>% 
  group_by(word) %>%
  tally(sort=TRUE) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n, fill=factor(ifelse(word=="series","Highlighted",ifelse(word=="anthology", "Highlighted", ifelse(word =="comedy", "Highlighted", ifelse(word == "originals", "Highlighted", "Normal")))))))+ 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("steelblue", "grey75"))+
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 10, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 10, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 15, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"))+ #Axis lines customizer
  theme(axis.ticks =element_line(color="light grey"))+
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.key=element_blank())+
   theme(legend.position = "none")+
labs(title = 'Hulu: Twitter Keyword Rankings 6/26/21', x = 'Keywords', y = '# of Tweets Containing Word')+
  annotate("text", x = 6.5, y = 230, label = "Tweeters often used terms such as 'series', 'anthology', 'comedy',", color = "steelblue", size = 4, type = "bold")+
   annotate("text", x = 5.5, y = 210, label = "& 'originals' when describing content on Hulu.", color = "steelblue", size = 4, type = "bold")
ggsave("HuluTwitterContent.png")

```

```{r SportingEvents}
tweet_words_interesting %>% 
  group_by(word) %>%
  tally(sort=TRUE) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n, fill=factor(ifelse(word=="olympics","Highlighted",ifelse(word=="coverage", "Highlighted", "Normal")))))+ 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("steelblue", "grey75"))+
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 10, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 10, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 15, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"))+ #Axis lines customizer
  theme(axis.ticks =element_line(color="light grey"))+
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.key=element_blank())+
   theme(legend.position = "none")+
labs(title = 'Hulu: Twitter Keyword Rankings 6/26/21', x = 'Keywords', y = '# of Tweets Containing Word')+
  annotate("text", x = 6, y = 230, label = "Tweeters were engaged with Hulu Live Tv's Olympics Streams.", color = "steelblue", size = 4, type = "bold")+
ggsave("HuluLiveOlympics.png")
```  
  
```{r}
bing_lex <- get_sentiments("bing")

fn_sentiment <- tweet_words_interesting %>% 
  left_join(bing_lex)

fn_sentiment %>% 
  filter(!is.na(sentiment)) %>%
  group_by(sentiment) %>% 
  summarise(n=n())

#Positive 368 Negative 182
```

### Prime Video

```{r Prime}
fn_twitter <- searchTwitter("Prime Video",n=1000,lang="en")

fn_twitter_df <- twListToDF(fn_twitter) # Convert to data frame


tweet_words <- fn_twitter_df %>% 
  select(id, text) %>%
  unnest_tokens(word,text)
```

```{r PrimeTweekRankings}
my_stop_words <- stop_words %>% 
  select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "prime", "video", "t.co", "rt", "and","a","of","you","now", "is", "on", "a", "the", "amp", "developing",  "it’s", "novak", "bjnovak", "b.j", "announce", "netflix", "building", "murders", "prime", "minister", "video", "petersefanovi2", "amazon", "million", "australia", "y'all", "changbin", "seorecord", "iraqi", "joe", "watch", "imrankhanpti", "youtube", "die", "fault", "vaccine", "spotify", "job", "makes" ,"katemossalike", "u.s", "vi", "peterstefanovi2", "viu", "bennet", "censor", "demands", "check", "facebook", "fgnujnbt7k", "israeli", "israel", "naftali", "party", "streaming", "disney", "mission", "premium", "ready", "baby", "al", "know_more_news"))) 

tweet_words_interesting <- tweet_words %>%
  anti_join(my_stop_words)

tweet_words_interesting %>% 
  group_by(word) %>%
  tally(sort=TRUE) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 10, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 10, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 15, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"))+ #Axis lines customizer
  theme(axis.ticks =element_line(color="light grey"))+
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.key=element_blank())+ #gganimate below
labs(title = 'Prime Video: Twitter Keyword Rankings 6/26/21', x = 'Keywords', y = '# of Tweets Containing Word') 
ggsave("PrimeVideoTweetKeywords.png")
```

```{r News}
tweet_words_interesting %>% 
  group_by(word) %>%
  tally(sort=TRUE) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n, fill=factor(ifelse(word=="news","Highlighted",ifelse(word=="informed", "Highlighted", ifelse(word=="informed", "Highlighted", ifelse(word=="biden", "Highlighted", ifelse(word=="president", "Highlighted", "Normal"))))))))+ 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("steelblue", "grey75"))+
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 10, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 10, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 15, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"))+ #Axis lines customizer
  theme(axis.ticks =element_line(color="light grey"))+
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.key=element_blank())+
   theme(legend.position = "none")+
labs(title = 'Prime Video: Twitter Keyword Rankings 6/26/21', x = 'Keywords', y = '# of Tweets Containing Word')+
  annotate("text", x = 7.5, y = 125, label = "Tweeters often used words relating to politics", color = "steelblue", size = 4, type = "bold")+
  annotate("text", x = 7.18, y = 115, label = "or news when discussing Prime Video.", color = "steelblue", size = 4, type = "bold")+
ggsave("PrimeNews.png")
```
```{r}
bing_lex <- get_sentiments("bing")

fn_sentiment <- tweet_words_interesting %>% 
  left_join(bing_lex)

fn_sentiment %>% 
  filter(!is.na(sentiment)) %>%
  group_by(sentiment) %>% 
  summarise(n=n())

#Positive 426 Negative 297
```

### Disney+ 

```{r Prime}
fn_twitter <- searchTwitter("disneyplus",n=1000,lang="en")

fn_twitter_df <- twListToDF(fn_twitter) # Convert to data frame


tweet_words <- fn_twitter_df %>% 
  select(id, text) %>%
  unnest_tokens(word,text)
```

```{r DisneyTweetRankings}
my_stop_words <- stop_words %>% 
  select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "prime", "video", "t.co", "rt", "and","a","of","you","now", "is", "on", "a", "the", "mayor", "app", "covid", "home", "walt", "world", "legal", "app", "county", "bulan", "disney", "amp", "dm", "spotify", "netflix", "youtube", "crisis", "sounds", "premium", "jual", "studios", "loki", "viu", "florida", "1", "service", "pass", "star", "gaspard", "starz", "midnight", "launching", "moonknight", "actor", "streaming", "brazil", "cast", "disneyplus", "summerofdisneyplus", "god", "bart", "thesimpsons", "disneyd23", "stay", "disneyplu", "goofy’s", "headed", "inside", "luca", "pals", "ᵖᵘⁿʸ", "vpgyo6b5jh", "fintastic", "thestreamer", "whatif", "captainamerica", "wednesday", "stream", "secrets", "marvelstudios", "reveal", "carter", "peggy", "steve", "whatifofficial", "rogers", "sam", "st", "captain", "ori"))) 

tweet_words_interesting <- tweet_words %>%
  anti_join(my_stop_words)

tweet_words_interesting %>% 
  group_by(word) %>%
  tally(sort=TRUE) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 10, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 10, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 15, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"))+ #Axis lines customizer
  theme(axis.ticks =element_line(color="light grey"))+
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.key=element_blank())+ #gganimate below
labs(title = 'Disney Plus: Twitter Keyword Rankings 6/26/21', x = 'Keywords', y = '# of Tweets Containing Word') 
ggsave("Disney+Keywords.png")
```

```{r DisneyTweets}
tweet_words_interesting %>% 
  group_by(word) %>%
  tally(sort=TRUE) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n, fill=factor(ifelse(word=="marvel","Highlighted",ifelse(word=="episode", "Highlighted", ifelse(word=="multiverse", "Highlighted", ifelse(word=="orig", "Highlighted", ifelse(word=="original", "Highlighted", ifelse(word=="series", "Highlighted", ifelse(word=="episodes", "Highlighted","Normal"))))))))))+ 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("steelblue", "grey75"))+
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 10, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 10, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 15, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"))+ #Axis lines customizer
  theme(axis.ticks =element_line(color="light grey"))+
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.key=element_blank())+
   theme(legend.position = "none")+
labs(title = 'Disney Plus: Twitter Keyword Rankings 6/26/21', x = 'Keywords', y = '# of Tweets Containing Word')+
  annotate("text", x = 6.85, y = 275, label = "Tweeters often used words relating to original", color = "steelblue", size = 5, type = "bold")+
  annotate("text", x = 7.18, y = 250, label = "content, including series, when discussing Disney Plus", color = "steelblue", size = 5, type = "bold")+
ggsave("DisneyOrigianl.png")
```
## Exploratory Analysis: Streaming Services

For this next section we'll be exploring the differences in content offerings across different streaming services. 

### Movie Ages 

We'll start by creating a new variable called "Movie_Yrs" to see how old movies tend to be across differ services. This can help us determine the distribution of title ages across different services. For example, we could answer if Netflix tends to have older titles compared to Hulu, or if one specific service has much older titles. We'll be looking at the median year of release for titles on each streaming service as well as the average age of each title in years. Keep in mind because we're working with arithmetic averages, so it's not necessarily an accurate conclusion about the age of titles on any particular streaming service. For example, if we were to get an average age of 20 years for Netflix but our Median release year is around 2016, that tells us that there are a decent amount of "older" titles on Netflix. 

Another important note before getting into the age histograms and summary statistics, is that the histogram limits have been altered to best reflect the shape of the data. That means extreme outliers (ex. Movies made in the 1940s have been omitted, as they rarely occur and distort the true shape of the data). 

Lastly, it's important to note why we're examining these "Title Age" related variables first. We want to eventually look into ratings of movies, and one of the hypotheses that may subtly play into our insights is how movie critics (professional and casual) view older movies vs newer movies? Could nostalgia potentially play a role? This is important **for later analysis fill in**

Onto some exploratory analysis!

```{r movieyrs}
df_streaming$Movie_Yrs = (2021 - df_streaming$Year)
```

```{r servicesplit}
df_netflix = df_streaming %>%
  filter(Netflix == 1)
df_hulu = df_streaming %>%
  filter(Hulu == 1)
df_prime = df_streaming %>%
  filter(`Prime Video` == 1)
df_disney = df_streaming %>%
  filter(`Disney+` == 1)
```

```{r histogram, warning = FALSE}

med_age_netflix = median(df_netflix$Year)
med_age_prime = median(df_prime$Year)
med_age_disney = median(df_disney$Year)
med_age_hulu = median(df_hulu$Year)

#Netflix Title Release Distribution Plot
n_age = ggplot(data=df_netflix, aes(Year)) + 
  geom_histogram(bins = 20, col = "indianred2", fill = "indianred2", alpha = .50 ) + 
  geom_vline(xintercept = 2016, linetype = "dotted", size =1) +
  scale_x_continuous(limits = c(1975, 2021)) + 
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 15, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 15, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 20, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"))+ #Axis lines customizer
  theme(axis.ticks =element_line(color="light grey"))+
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.key=element_blank())+
   theme(legend.position = "none") + 
  labs(title = 'Netflix: Title Release Year Distribution', x = 'Release Year', y = '# of Titles')+
  annotate("text", x = 2005, y = 500, label = ("50% of Titles released after"), color = "indianred2", size = 6.5, type = "bold", fontface = 2)+
  annotate("text", x = 2013.6, y = 500, label = (med_age_netflix), color = "indianred2", size = 6.5, fontface = 2)

#Prime Title Release Distribution Plot
p_age = ggplot(data=df_prime, aes(Year)) + 
  geom_histogram(bins = 20, col = "steelblue2", fill = "steelblue2", alpha = .50 ) + 
  geom_vline(xintercept = 2010, linetype = "dotted", size =1) +
  scale_x_continuous(limits = c(1960, 2021)) + 
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 15, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 15, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 20, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"))+ #Axis lines customizer
  theme(axis.ticks =element_line(color="light grey"))+
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.key=element_blank())+
   theme(legend.position = "none") + 
  labs(title = 'Prime Video: Title Release Year Distribution', x = 'Release Year', y = '# of Titles')+
  annotate("text", x = 1996, y = 1450, label = ("50% of Titles released after"), color = "steelblue2", size = 6.5, type = "bold", fontface = 2)+
  annotate("text", x = 2007.5, y = 1450, label = (med_age_prime), color = "steelblue2", size = 6.5, fontface = 2)

#Hulu Title Release Distribution Plot
h_age = ggplot(data=df_hulu, aes(Year)) + 
  geom_histogram(bins = 20, col = "palegreen3", fill = "palegreen3", alpha = .50 ) + 
   geom_vline(xintercept = 2015, linetype = "dotted", size =1) +
  scale_x_continuous(limits = c(1975, 2021)) + 
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 15, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 15, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 20, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"))+ #Axis lines customizer
  theme(axis.ticks =element_line(color="light grey"))+
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.key=element_blank())+
   theme(legend.position = "none") + 
  labs(title = 'Hulu: Title Release Year Distribution', x = 'Release Year', y = '# of Titles')+
  annotate("text", x = 2003, y = 190, label = ("50% of Titles released after"), color = "palegreen3", size = 6.5, type = "bold", fontface = 2)+
  annotate("text", x = 2011.5, y = 190, label = (med_age_hulu), color = "palegreen3", size = 6.5, fontface = 2)

#Disney Plus Release Distribution Plot
d_age = ggplot(data=df_disney, aes(Year)) + 
  geom_histogram(bins = 20, col = "royalblue4", fill = "royalblue4", alpha = .50 ) + 
  geom_vline(xintercept = 2003, linetype = "dotted", size =1) +
  scale_x_continuous(limits = c(1960, 2021)) + 
  scale_y_continuous(limits = c(0,75))+
  theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 15, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 15, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 20, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"))+ #Axis lines customizer
  theme(axis.ticks =element_line(color="light grey"))+
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.key=element_blank())+
   theme(legend.position = "none") + 
  labs(title = 'Disney Plus: Title Release Year Distribution', x = 'Release Year', y = '# of Titles')+
  annotate("text", x = 1989, y = 70, label = ("50% of Titles released after"), color = "royalblue4", size = 6.5, type = "bold", fontface = 2)+
  annotate("text", x = 2000.45, y = 70, label = (med_age_disney), color = "royalblue4", size = 6.5, fontface = 2)

```

### Netflix Title Ages

```{r h_age, warning = FALSE}
n_age
print(mean(df_netflix$Movie_Yrs))
```

The average age of titles on Netflix are 7.89 years old. 

### Prime Video Title Ages

```{r p_age, warning = FALSE}
p_age
print(mean(df_prime$Movie_Yrs))
```

On average titles on Prime Video are 22.29 years old. 

### Hulu Title Ages

```{r h_age, warning = FALSE}
h_age
print(mean(df_hulu$Movie_Yrs))
```
On average the titles on Disney are 10 Years old.

### Disney Plus Titles Ages

```{r d_age, warning = FALSE}
d_age
print(mean(df_disney$Movie_Yrs))
```

On average the titles on Disney are 23.33 Years old.

### Insights: Title Ages


## Title Ratings

Recall we said earlier we'd be viewing the trends of title ratings over time. One may wonder why we are doing this, after all, it does not *directly* answer our research question regard ________.

```{r }
yr_ratings1 <- aggregate(IMDb  ~ Year, data = df_streaming, mean) # Mean of IMDb ratings 
yr_ratings2 <- aggregate(rotten_tomatoes_pct  ~ Year, data = df_streaming, mean) # Mean of rotten tomatoes percentages

yr_ratings = yr_ratings1 %>%
  inner_join(yr_ratings2) #Notes there are rotten tomatoes reviews that are missing for certain years.

yr_ratings$rotten_tomatoes_pct =  (yr_ratings$rotten_tomatoes_pct/10)

df_streaming$rotten_tomatoes_rating =  (df_streaming$rotten_tomatoes_pct/10)


rm("yr_ratings1", "yr_ratings2")
```

### Have the Average Ratings of Titles Changed Over Time?

Do we view older movies more favorably than newer ones? Could there be some non-measurable factor such as "nostalgia" that may effect how users rate movies? This is an important research question to answer. When comparing streaming service title ages 

```{r}
ggplot(yr_ratings)+
  geom_line(aes(Year, IMDb, col = "IMDb"), size = 1.2)+
  geom_line(aes(Year, rotten_tomatoes_pct, col = "Rotten Tomatoes"), size = 1.2)+ 
    scale_colour_manual("", 
                      values = c("IMDb"="deepskyblue4", "Rotten Tomatoes"="coral")) +
  scale_x_continuous(breaks=seq(1910,2020,10)) +
  scale_y_continuous(breaks=seq(1,10,1)) +
   theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 15, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 15, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 20, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"), axis.text.y = element_text(color = "dimgray", size = 10))+ #Axis lines 
  theme(axis.line.x=element_line(color="light grey"))+ #Axis lines customizer
 theme(axis.ticks =element_line(color="light grey"), 
       axis.text.x = element_text(color = "dimgray", size = 10))+ #X ticks customizer
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.text = element_text(size = 12), legend.position = "right")+
  labs(title = 'Average Title Ratings by Year', x = 'Release Year', y = 'Average Rating (Out of 10)')

```

We see an interesting trend here. Average Rotten Tomatoes ratings had seen a steady decline from the period of 1910 to the to around the mid 90s with the lowest average rating being in 1994 with a average rating of 4.5. Meanwhile, we see that IMDb ratings are fairly consistent around the 5.5-6.5 range on average. 

One thing to note from this plot is that Rotten Tomatoes critics tend to be more extreme; either really liking movies or disliking movies. This behavior is reflected by a volatile average over time. The consistency of the IMDb ratings can be marked by its relatively steady averages across several decades. This is something to take into account when creating a model. 

Recall that we purposefully analyzed the distribution title release dates by streaming service prior to analyzing ratings. There is a reason we conducted the analysis in this order. Say we want to create a model where we predict a movie rating based on the service a title is available on as well as other variables. If 50% of Disney Plus titles were published before 2003 (as we previously established), we may find that including the streaming service as variable may potentially be significant in predicting the Rotten Tomatoes rating (where higher ratings on Rotten Tomatoes tended to be older titles.

Additionally, the downward trend in average ratings can be attributed to different reasons. It could be nostalgia, an actual distaste for newer titles (less likely), or that titles older than 40 years old that are on streaming services may just consist of "classics". 

Let's take one step back and analyze how movie ratings are distributed overtime, but this time without aggregating by year and including all titles in a scatterplot. We'll also view each rating site individually, as we don't want to overcrowd the plot with too many observations (over 20,000 if included).

```{r}
ggplot(df_streaming)+
  geom_point(aes(Year, IMDb), col = "deepskyblue4" , size = 1.2)+
  geom_smooth(aes(Year,IMDb), col = "coral", level = .9)+ 
  scale_x_continuous(breaks=seq(1910,2020,10)) +
  scale_y_continuous(breaks=seq(1,10,1)) +
   theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 15, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 15, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 20, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"), axis.text.y = element_text(color = "dimgray", size = 10))+ #Axis lines 
  theme(axis.line.x=element_line(color="light grey"))+ #Axis lines customizer
 theme(axis.ticks =element_line(color="light grey"), 
       axis.text.x = element_text(color = "dimgray", size = 10))+ #X ticks customizer
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.text = element_text(size = 12), legend.position = "right")+
  labs(title = 'Title Rating Distribution by Year: IMDB', x = 'Release Year', y = 'Average Rating (Out of 10)')
```


```{r, warning}
ggplot(df_streaming)+
  geom_point(aes(Year, rotten_tomatoes_rating), col = "coral" , size = 1.2)+
  geom_smooth(aes(Year,rotten_tomatoes_rating), col = "dimgrey", level = .9)+ 
  scale_x_continuous(breaks=seq(1910,2020,10)) +
  scale_y_continuous(breaks=seq(1,10,1)) +
   theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) +
  theme(axis.title.x = element_text(color="grey60", size = 15, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 15, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 20, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"), axis.text.y = element_text(color = "dimgray", size = 10))+ #Axis lines 
  theme(axis.line.x=element_line(color="light grey"))+ #Axis lines customizer
 theme(axis.ticks =element_line(color="light grey"), 
       axis.text.x = element_text(color = "dimgray", size = 10))+ #X ticks customizer
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.text = element_text(size = 12), legend.position = "right")+
  labs(title = 'Title Rating Distribution by Year: Rotten Tomatoes', x = 'Release Year', y = 'Average Rating (Out of 10)')
```

Note that our data has a wider spread among Rotten Tomatoes especially among, titles 50 years old and newer. Additionally, notice how there is a noticeable upward trend across rotten tomato ratings since the year 2000. We don't necessarily have answer as to why this is. But maybe there are a few leads we can look into before proceeding with our analysis to view the genre distributions by streaming service.

### Do Average Ratings Tend to Vary Across Different Genres?

Oftentimes, viewers may notice that genres like Comedies tend score lower than other genres. Is an assumption such as this one true across IMDB and Rotten Tomatoes ratings within our dataset? It's important that we look into these hypotheses as it helps us make a more informed and evidence backed recommendation to streaming services on how to strategically plan there title selections. 

```{r df_agg, warning = False}
df_rating_genre1 = sqldf('SELECT GENRES AS genres, rotten_tomatoes_rating AS avg_tomato_rating, IMDB AS avg_imdb_rating FROM df_streaming
                        WHERE GENRES IS NOT NULL AND GENRES = "Action" OR GENRES = "Adventure" OR GENRES = "Animation" OR GENRES = "Biography"
                      ')

df_rating_genre2 = sqldf('SELECT GENRES AS genres, rotten_tomatoes_rating AS avg_tomato_rating, IMDB AS avg_imdb_rating FROM df_streaming
                        WHERE GENRES IS NOT NULL AND GENRES = "Comedy" OR GENRES = "Crime" OR GENRES = "Documentary" OR GENRES = "Drama"
                      ')

df_rating_genre3 = sqldf('SELECT GENRES AS genres, rotten_tomatoes_rating AS avg_tomato_rating, IMDB AS avg_imdb_rating FROM df_streaming
                        WHERE GENRES IS NOT NULL AND GENRES = "Family" OR GENRES = "Horror" OR GENRES = "Mystery" OR GENRES = "Romance"
                      ')

df_rating_genre4 = sqldf('SELECT GENRES AS genres, rotten_tomatoes_rating AS avg_tomato_rating, IMDB AS avg_imdb_rating FROM df_streaming
                        WHERE GENRES IS NOT NULL AND GENRES = "Short" OR GENRES = "Thriller" OR GENRES = "Western" OR GENRES = "Other"
                      ')

library(ggridges)
library(RColorBrewer)
theme_set(theme_ridges())

#Function to quickly plot the genre densities

density_genre = function(df, site, title){
  ggplot(df, aes(x = site, y = genres)) +
  geom_density_ridges(aes(fill = genres), alpha = .8)+
  scale_fill_manual(values = brewer.pal(n=4, name = "Blues"))+
  scale_x_continuous(breaks=seq(0,10,1)) +
  theme(axis.title.x = element_text(color="grey60", size = 17, face="bold"))+
  theme(axis.title.y = element_text(color="grey60", size = 17, face="bold"))+
  theme(plot.title=element_text(color="dimgray", size = 20, face="bold"))+ #Title customizer
  theme(axis.line.y=element_line(color="light grey"), axis.text.y = element_text(color = "dimgray", size = 10))+ #Axis lines 
  theme(axis.line.x=element_line(color="light grey"))+ #Axis lines customizer
 theme(axis.ticks =element_line(color="light grey"), 
       axis.text.x = element_text(color = "grey60", size = 12), axis.text.y = element_text(color = "grey60", size = 12))+ #X and yticks customizer
    theme(strip.background = element_blank(), strip.text.x = element_text(color = "dimgray", size = 12, face = "bold"))+
  theme(panel.background = element_blank())+
  theme(legend.text = element_text(color = "dimgray", size = 12), legend.position = "right", legend.title = element_blank())+
  labs(title = title, x = 'Rating', y = 'Genres', fill = "Genres")
}
```

```{r, warning= FALSE}
density_genre(df_rating_genre1, df_rating_genre1$avg_tomato_rating, 'Title Rating Distribution by Genres: Rotten Tomatoes')
density_genre(df_rating_genre2, df_rating_genre2$avg_tomato_rating, 'Title Rating Distribution by Genres: Rotten Tomatoes')
density_genre(df_rating_genre3, df_rating_genre3$avg_tomato_rating, 'Title Rating Distribution by Genres: Rotten Tomatoes')
density_genre(df_rating_genre4, df_rating_genre4$avg_tomato_rating, 'Title Rating Distribution by Genres: Rotten Tomatoes')
```

```{r , warning= FALSE}
density_genre(df_rating_genre1, df_rating_genre1$avg_imdb_rating, 'Title Rating Distribution by Genres: IMDB')
density_genre(df_rating_genre2, df_rating_genre2$avg_imdb_rating, 'Title Rating Distribution by Genres: IMDB')
density_genre(df_rating_genre3, df_rating_genre3$avg_imdb_rating, 'Title Rating Distribution by Genres: IMDB')
density_genre(df_rating_genre4, df_rating_genre4$avg_imdb_rating, 'Title Rating Distribution by Genres: IMDB')

rm("df_rating_genre1", "df_rating_genre2", "df_rating_genre3", "df_rating_genre4")
```

Across IMDB ratings, we do see less variation among average ratings, however it is important to note that the some genres tend to have wider or narrower distributions with higher or lower average ratings than others. For example, take IMDB ratings for Action genre titles and compare it with IMDB ratings for Documentary Genre titles. To ease comparison an additional plot has been created below that isolates the two genres:

```{r, warning = False}

act_doc = sqldf('SELECT GENRES AS genres, rotten_tomatoes_rating AS avg_tomato_rating, IMDB AS avg_imdb_rating FROM df_streaming
                        WHERE GENRES IS NOT NULL AND GENRES = "Action" OR GENRES = "Documentary"
                      ')

calc_doc = act_doc %>%
  filter(genres == "Documentary")

calc_act = act_doc %>%
  filter(genres == "Action")

mean(calc_doc$avg_imdb_rating, na.rm = TRUE)
mean(calc_act$avg_imdb_rating, na.rm = TRUE)

density_genre(act_doc, act_doc$avg_imdb_rating, "Title Rating Distribution by Genre: IMDB")

rm = c("act_doc", "calc_doc", "calc_act")
```

Notice the markedly narrower IMDB rating distribution present in Documentary films compared to Action films. Also, it's relatively clear the that these two genres have noticeably different average ratings on IMDB (an average IMDB rating of 5.09 for Action titles and 6.93781 for documentaries). There may be less variation in ratings compared to ratings found on Rotten Tomatoes, but there is still a difference in the perceived quality of titles based on genre. We do not currently possess the data or evidence that would explain why, but we do know from the plot above that something is causing movie raters to rate Documentaries on average more favorably than Action movies. The rating distribution thus is affected by the genre, and may play a role in a future statistical model we create. 


There is a few more hypotheses to look into involving ratings before proceeding onto the next logical step of our analysis. One hypotheses is that we want to know if average ratings also tend to vary depending on the age rating of movies. Could it be possible that PG and G movies are rated more generously compared to titles reserved for primarily mature audiences? This could play a role into our model creation and title recommendations to the streaming services.

### Do Average Ratings Vary Across Different Age Ratings?


Before running some final exploratory analysis on the streaming services themselves, let's take a final look at how rating may vary across title runtimes. 

### How Do Ratings Vary Across Title Runtimes?


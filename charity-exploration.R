### Usual start code:
library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(scales)
library(igraph)
library(ggraph)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(widyr)
#####################
# Here, I am working with: Gouge, "Of Domesticall Duties"; Gataker, "Marriage Duties"; and Guy "Pieties Pillar" (funeral sermon for Gouge's wife)
#####################
### Putting the .txt file into tidy dataframe
gouge_domesticall_v <- scan("gouge_domesticall.txt", what = "character", sep = "\n")
gouge_domesticall_df <- tibble(line = 1:4493, content = gouge_domesticall_v)
### No chapter divisions in sermons, using line number to track position
gouge_domesticall_df <- gouge_domesticall_df %>% 
  mutate(linenumber = row_number(), chapter = cumsum(linenumber))%>%
  ungroup()
gouge_domesticall_tidy <- gouge_domesticall_df %>% unnest_tokens(word, content)
### Removing common stopwords
data("stop_words")
gouge_domesticall_tidy <- gouge_domesticall_tidy %>% 
  anti_join(stop_words)
### Create my own stop words
my_stopwords_df <-
  tibble(
    word = c("thee", "thy", "thou", "wee", "co", "est", "de", "hee", "bee", "vp",
             "till", "lib","al", "cap", "doth", "fro", "ad", "ca", "pa", "ibid","esse",
             "apoc", "vt", "ouer", "euen", "vnto", "haue", "hath","yea", "thereof",
             "euer", "euery", "vpon", "quod", "doe", "goe", "nay", "tho", "vers", "pet",
             "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12","13", "14", "15", "16", "17","18", "19", "20",
             "21", "22","23", "24", "25", "26", "27", "28", "29", "30", "31",
             "vid", "boz", "car", "vide", "anie", "ab", "po", "me", "cu", "gra"))
#now I remove the custom stopwords using "anti_join"
gouge_domesticall_tidy <- gouge_domesticall_tidy %>% 
  anti_join(my_stopwords_df)
gouge_domesticall_tidy <- gouge_domesticall_tidy %>% 
  anti_join(stop_words)
gouge_domesticall_tidy %>% count(word, sort = TRUE)
### Plot
gouge_domesticall_tidy %>% 
  count(word, sort = TRUE) %>%
  filter(n > 150)%>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
############# Preparing Gataker
gataker_marriage_duties_v <- scan("gataker_marriage_duties.txt", what = "character", sep = "\n")
gataker_marriage_duties_df <- tibble(line = 1: 151, content = gataker_marriage_duties_v)
gataker_marriage_duties_df <- gataker_marriage_duties_df %>% mutate(linenumber = row_number(), chapter = cumsum(linenumber))%>%
  ungroup()
gataker_marriage_duties_tidy <- gataker_marriage_duties_df %>% unnest_tokens(word, content)
### Stopwords and custom stopwords
data("stop_words")
gataker_marriage_duties_tidy <- gataker_marriage_duties_tidy %>% 
  anti_join(my_stopwords_df)
gataker_marriage_duties_tidy <- gataker_marriage_duties_tidy %>% anti_join(stop_words)
gataker_marriage_duties_tidy %>% count(word, sort = TRUE)
gataker_marriage_duties_tidy %>% 
  count(word, sort = TRUE) %>%
  filter(n > 20)%>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
###### Preparing Guy
guy_pieties_pillar_v <- scan("guy_pieties_pillar.txt", what = "character", sep = "\n")
guy_pieties_pillar_df <- tibble(line = 1: 39, content = guy_pieties_pillar_v)
guy_pieties_pillar_df <- guy_pieties_pillar_df %>% mutate(linenumber = row_number(), chapter = cumsum(linenumber))
guy_pieties_pillar_tidy <- guy_pieties_pillar_df %>% unnest_tokens(word, content)
### Stopwords and Custom stopwords
data("stop_words")
guy_pieties_pillar_tidy <- guy_pieties_pillar_tidy %>% anti_join(stop_words)
guy_pieties_pillar_tidy <- guy_pieties_pillar_tidy %>% 
  anti_join(my_stopwords_df)
guy_pieties_pillar_tidy %>% count(word, sort = TRUE)
guy_pieties_pillar_tidy %>% 
  count(word, sort = TRUE) %>%
  filter(n > 20)%>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
#### Setting up for dispersion plots of "charity," "loue," "almes", "duty", "dutie", and "duties"
#preparing Gouge
gouge_lower_v <-tolower(gouge_domesticall_v)
gouge_words_l <-strsplit(gouge_lower_v, "\\W")
gouge_words_v <-unlist(gouge_words_l)
not_blanks_gouge_v <- which(gouge_words_v!="")
gouge_words_v <- gouge_words_v[not_blanks_gouge_v]
#preparing Gataker
gataker_lower_v <-tolower(gataker_marriage_duties_v)
gataker_words_l <-strsplit(gataker_lower_v, "\\W")
gataker_words_v <-unlist(gataker_words_l)
not_blanks_gataker_v <- which(gataker_words_v!="")
gataker_words_v <- gataker_words_v[not_blanks_gataker_v]
#preparing Guy
guy_lower_v <-tolower(guy_pieties_pillar_v)
guy_words_l <-strsplit(guy_lower_v, "\\W")
guy_words_v <-unlist(guy_words_l)
not_blanks_guy_v <- which(guy_words_v!="")
guy_words_v <- guy_words_v[not_blanks_guy_v]
###occurrences of specific words and comparison with unique words in text
length(gouge_words_v[which(gouge_words_v=="charity")])
#answer = 17
length(gouge_words_v[which(gouge_words_v=="loue")])
#answer= 633
length(gouge_words_v[which(gouge_words_v=="almes")])
#answer= 14
length(gouge_words_v[which(gouge_words_v=="duty")])
length(gouge_words_v[which(gouge_words_v=="dutie")])
length(gouge_words_v[which(gouge_words_v=="duties")])
#answer total= 169 + 466 + 416 = 1051
length(unique(gouge_words_v))
#answer = 14544
#### Creating a table of "words types" and their frequencies
gouge_freqs_t <- table(gouge_words_v)
sorted_gouge_freqs_t <- sort(gouge_freqs_t, decreasing = TRUE)
# Now the dispersion plot steps (chapter 4 in Jockers) of "charity," "loue," "almes", "duty", "dutie", and "duties"
gouge_time_v <- seq(1:length(gouge_words_v))
charity_v <- which(gouge_words_v == "charity")
loue_v <- which(gouge_words_v == "loue")
almes_v <- which(gouge_words_v == "almes")
duty_v <- which(gouge_words_v == "duty")
dutie_v <- which(gouge_words_v == "dutie")
duties_v <- which(gouge_words_v == "duties")
#finding all the spots in the sermon where "charity" is present and then setting them to "1" to represent "TRUE"
charity_count_v <- rep(NA, length(gouge_time_v))
charity_count_v[charity_v] <- 1
#now I can plot
plot(charity_count_v, main = "Dispersion Plot of 'charity' in Gouge", xlab = "Sermon Time", ylab = "charity", type = "h", ylim =c(0,1), yaxt='n')
#finding all the spots in the sermon where "loue" is present and then setting them to "1" to represent "TRUE"
loue_count_v <- rep(NA, length(gouge_time_v))
loue_count_v[loue_v] <- 1
#now I can plot
plot(loue_count_v, main = "Dispersion Plot of 'loue' in Gouge", xlab = "Sermon Time", ylab = "loue", type = "h", ylim =c(0,1), yaxt='n')
#finding all the spots in the sermon where "almes" is present and then setting them to "1" to represent "TRUE"
almes_count_v <- rep(NA, length(gouge_time_v))
almes_count_v[almes_v] <- 1
#now I can plot
plot(almes_count_v, main = "Dispersion Plot of 'almes' in Gouge", xlab = "Sermon Time", ylab = "almes", type = "h", ylim =c(0,1), yaxt='n')
#finding all the spots in the sermon where "duty" is present and then setting them to "1" to represent "TRUE"
duty_count_v <- rep(NA, length(gouge_time_v))
duty_count_v[duty_v] <- 1
#now I can plot
plot(duty_count_v, main = "Dispersion Plot of 'duty' in Gouge", xlab = "Sermon Time", ylab = "duty", type = "h", ylim =c(0,1), yaxt='n')
#finding all the spots in the sermon where "dutie" is present and then setting them to "1" to represent "TRUE"
dutie_count_v <- rep(NA, length(gouge_time_v))
dutie_count_v[dutie_v] <- 1
#now I can plot
plot(dutie_count_v, main = "Dispersion Plot of 'dutie' in Gouge", xlab = "Sermon Time", ylab = "dutie", type = "h", ylim =c(0,1), yaxt='n')
#finding all the spots in the sermon where "duties" is present and then setting them to "1" to represent "TRUE"
duties_count_v <- rep(NA, length(gouge_time_v))
duties_count_v[duties_v] <- 1
#now I can plot
plot(duties_count_v, main = "Dispersion Plot of 'duties' in Gouge", xlab = "Sermon Time", ylab = "duties", type = "h", ylim =c(0,1), yaxt='n')
#####
#####
#let's see if I can combine the various duties
newduty_v <- c(duty_v, dutie_v, duties_v)
#attempting to find all the spots in the sermon where various duties are present and then setting them to "1" to represent "TRUE"
newduty_count_v <- rep(NA, length(gouge_time_v))
newduty_count_v[newduty_v] <- 1
#now I can plot
plot(newduty_count_v, main = "Dispersion Plot of forms of 'duty' in Gouge", xlab = "Sermon Time", ylab = "combined duty terms", type = "h", ylim =c(0,1), yaxt='n')

####### N-grams ##########
#### Starting with bigrams on Gouge 
gouge_bigrams <- gouge_domesticall_df %>%
  unnest_tokens(bigram, content, token = "ngrams", n =2)
####getting rid of stopwords by first separating out the bigrams. Note: I had removed the stopwords in the tidy version above, not the df
gouge_bigrams_separated <- gouge_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
gouge_bigrams_filtered <- gouge_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
gouge_bigrams_counts<- gouge_bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
############### Note: the above is helpful in figuring out the biblical passages in a sermon and which ones are more frequent
# Reuniting the bigrams
gouge_bigrams_united <- gouge_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
########### Setting up for trigrams in Gouge
gouge_trigrams <- gouge_domesticall_df %>%
  unnest_tokens(trigram, content, token = "ngrams", n = 3)
gouge_trigrams_separated <- gouge_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
gouge_trigrams_filtered <- gouge_trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)
gouge_trigrams_counts<- gouge_trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE)
# NB: I don't understand the "NA" entries
gouge_trigrams_united <- gouge_trigrams_filtered %>%
  unite(trigram, word1, word2, word3, sep = " ")
##### Analyzing around the terms: "charity," "loue," "almes", "duty", "dutie", and "duties"
# Starting with "charity"
gouge_bigrams_filtered %>%
  filter(word2 == "charity") %>%
  count(word1, sort = TRUE)
gouge_bigrams_filtered %>%
  filter(word1 == "charity") %>%
  count(word2, sort = TRUE)
# next "loue"
gouge_bigrams_filtered %>%
  filter(word2 == "loue") %>%
  count(word1, sort = TRUE)
gouge_bigrams_filtered %>%
  filter(word1 == "loue") %>%
  count(word2, sort = TRUE)

### Creating a new dataframe that only contains the bigrams containing "loue" (WARNING: I think I am missing two bigrams that have "loue" in both positions)
gouge_love_bigrams <- gouge_bigrams_filtered %>%
  filter(word1 == "loue" | word2 == "loue")
### Counting each type of bigram: 
gouge_love_count <- gouge_love_bigrams %>%
  count(word1, word2, sort = TRUE)


### Network of Bigrams
#filter for only relatively common combinations
gouge_bigrams_graph <- gouge_bigrams_counts %>%
  filter(n >20) %>%
  graph_from_data_frame()
gouge_bigrams_graph
## you want to use set.seed to obtain the same graph with each run, otherwise the layout is random
set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(gouge_bigrams_graph, layout = "fr")+
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches'))+
  geom_node_point(color = "blue", size =5)+
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  theme_void()

### Putting the .txt file into a dataframe that tracks divisions by sections
### in a way that makes it possible to do the kind of correlation comparison
### I see on pp.62ff of Silge and Robison, "Text Mining with R: a Tidy Approach"
### 2/8/2023: see the "read me" file

gouge_dom_sections_v <- scan("gouge_domesticall_sections.txt", what = "character", sep = "\n")
gouge_dom_sections_df <- tibble(line = 1:4493, content = gouge_dom_sections_v)
### No chapter division in Gouge, using a custom label I added at the beginning of each
### treatise: AAGTREATISE and number (I to VIII). Note: [\\divxlc] allows you to add up Roman numerals.
original_treatises <- gouge_dom_sections_df %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(content, regex("^AAGTREATISE [\\divxlc]", 
                                                    ignore_case = TRUE)))) %>%
  ungroup()
### gouge_treatises_words is going to function like "austen_section_words" and "original_treatises" like "austen_books" on p. 62
### I am also going to use the filter to remove "chapter 0" for now even if it gets rid of the letter.
### Create my own stop words to remove the added "aagtreatise"
### Create my own stop words to remove the added "aagtreatise" and Latin terms I noticed in the first visualization of correlations
### I am also removing: "obser", "obseru" (which I assume is Gouge telling people to observe)
my_stopw_df <-
  tibble(
    word = c("aagtreatise", "sunt", "quod", "qui", "quae", "loc", "cum", "Cum", "ad", "obser", "obseru", "hier", "cap", "cic", "citat",
             "vel", "quo", "quam", "Quam", "iam", "hoc", "Hoc", "num", "ibid", "Ibid"))
gouge_treatises_words <- original_treatises %>%
  filter(chapter > 0) %>%
  unnest_tokens(word, content) %>%
  filter(!word %in% my_stopw_df$word) %>%
  filter(!word %in% stop_words$word)
### count words co-occuring within sections
gouge_word_pairs <- gouge_treatises_words %>%
  pairwise_count(word, chapter, sort = TRUE)
gouge_word_pairs %>%
  filter(item2 == "charity")

### The results are meaningless because the sections are too big. Let's try to do it like the book by line count: every 20 lines

gouge_lines_sections <- original_treatises %>%
  mutate(section = row_number() %/% 20) %>%
  filter(section > 0) %>%
  unnest_tokens(word, content) %>%
  filter(!word %in% my_stopw_df$word) %>%
  filter(!word %in% stop_words$word)
### count co-occuring pairs of words within sections
gouge_lines_pairs <- gouge_lines_sections %>%
  pairwise_count(word, section, sort =TRUE)
### gouge_treatises_words is going to function like "austen_section_words" and "original_treatises" like "austen_books" on p. 62
### I am also going to use the filter to remove "chapter 0" for now even if it gets rid of the letter.
gouge_treatises_words <- original_treatises %>%
  filter(chapter > 0) %>%
  unnest_tokens(word, content) %>%
  filter(!word %in% my_stopw_df$word) %>%
  filter(!word %in% stop_words$word)

### Collapsing variants of apostles' names into a standardized version. Note: "original_treatises" still has capitals
### Having issues: I need the periods after "Matt." and "Matth." Missing something else in how to make this string work (the problem is "str_replace", 
### use: str_replace_all)
### mutate(across('word', str_replace_all, ' matth.', 'matthew ')) %>% USE "str_replace_all"

gouge_lines_apostles <- original_treatises %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  mutate(across('content', str_replace_all, ' Matth.', ' matthew ')) %>%
  mutate(across('content', str_replace_all, ' Matt.', ' matthew ')) %>%
  mutate(across('content', str_replace_all, ' Mat.', ' matthew ')) %>%
  mutate(across('content', str_replace_all, ' Pet.', ' peter ')) %>%
  mutate(across('content', str_replace_all, ' Luk.', ' luke ')) %>%
  mutate(across('content', str_replace_all, ' Ioh.', ' john ')) %>%
  mutate(across('content', str_replace_all, ' Iohn.', ' john ')) %>%
  mutate(across('content', str_replace_all, ' Iohn.', ' john ')) %>%
  mutate(across('content', str_replace_all, ' Mark.', ' mark ')) %>%
  unnest_tokens(word, content) %>%
  filter(!word %in% my_stopw_df$word) %>%
  filter(!word %in% stop_words$word)
### count co-occuring pairs of words within sections
gouge_apostles_pairs <- gouge_lines_apostles %>%
  pairwise_count(word, section, sort =TRUE)

### count words co-occuring within sections
#gouge_word_pairs <- gouge_treatises_words %>%
#  pairwise_count(word, chapter, sort = TRUE)
#### the goal is going to be to compute the "phi coefficient"
### filter for relatively common words first
word_cors <- gouge_lines_apostles %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, section, sort = TRUE)
### visualizing the correlations for selected references of the Bible: "luke", "matt", "matth", "peter", "pet", "paul", "iohn", "ioh", "mat", "mark"
### note: I have now replaced the 
word_cors %>%
  filter(item1 %in% c("luke", "matthew", "peter", "paul", "john")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity", fill="plum3", colour="black") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

### visualizing the correlations for selected references of the Bible as a network
#trying different layouts: "fr" is the one used by Silge
word_cors_net <- word_cors %>% 
  rename(
    from = item1,
    to = item2) %>%
  mutate(group = sample(word_cors[1:3], n(), TRUE))
set.seed(2016)
g <- graph.data.frame(word_cors, directed = FALSE)
par(mar = c(0,0,0,0))
plot(g, layout = layout.fruchterman.reingold, vertex.size = 3)

word_cors %>%
  filter(correlation > 0.35) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "orange", size = 5) +
  #geom_text_repel(box.padding = 0.5, max.overlaps = Inf) +
  geom_node_text(aes(label = name), max.overlaps = Inf, repel = TRUE) +
  theme_void()
#trying: "lgl"
word_cors %>%
  filter(correlation > 0.25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "lgl") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "mediumblue", size = 5) +
  #geom_text_repel(box.padding = 0.5, max.overlaps = Inf) +
  geom_node_text(aes(label = name), max.overlaps = Inf, repel = TRUE) +
  theme_void()
#trying: "mds"
word_cors %>%
  filter(correlation > 0.35) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "lgl") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "yellow4", size = 5) +
  #geom_text_repel(box.padding = 0.5, max.overlaps = Inf) +
  geom_node_text(aes(label = name), max.overlaps = Inf, repel = TRUE) +
  theme_void()
#trying: "graphopt"
word_cors %>%
  filter(correlation > 0.35) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "graphopt") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "mediumvioletred", size = 3) +
  #geom_text_repel(box.padding = 0.5, max.overlaps = Inf) +
  geom_node_text(aes(label = name), max.overlaps = Inf, repel = TRUE) +
  theme_void()
library(igraph)
library(qgraph)
require("png")
require("jpeg")
require("igraph") 
### Let's give different layouts a shot. Looking at this site:  https://www.r-bloggers.com/2019/09/1-giraffe-2-giraffe-go/ 
word_cors_net <- word_cors %>% 
  rename(
    from = item1,
    to = item2
    )
ggraph(word_cors_net, layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point()



#################################OTHER STUFF
### I am going to bring in some of the code from Jockers, starting p. 33, to help me with this
### None of the below with Jockers was needed
#treatise_position_v <- grep("AAGTREATISE", gouge_dom_sections_v)
#gouge_dom_sections_v[treatise_position_v]
### treatise_position_v holds the positions where the string "AAGTREATISE" was found (see Jockers, p. 34)
### Now I am going to collect the text in between these positions. I need to add and "END" marker first
### so I can let R know where to finish gathering the last section of text
#gouge_dom_sections_v <- c(gouge_dom_sections_v, "END")
#last_position_v <- length(gouge_dom_sections_v)
#treatise_position_v <- c(treatise_position_v, last_position_v)


#test_gouge <- paste(unlist(gouge_dom_sections_df), collapse =" ")


#gouge_dom_sections_df <- gouge_dom_sections_df %>% 
#  mutate(linenumber = row_number(), chapter = cumsum(linenumber))%>%
#  ungroup()
#gouge_domesticall_tidy <- gouge_domesticall_df %>% unnest_tokens(word, content)


############################################################################# Below we have the 
###################### code for comparison of frequencies
#In the next section, I am computing the occurrence of each word in each sermon
#and binding the data frames together
#occurrence <- bind_rows(mutate(gouge_domesticall_tidy, author = "Gouge Domesticall"),
#                       mutate(gataker_marriage_duties_tidy, author = "Gataker Marriage Duties"),
#                       mutate(guy_pieties_pillar_tidy, author = "Guy Pieties Pillar")) %>%
#  mutate(word =str_extract(word, "[a-z']+")) %>%
#  count(author, word) %>%
#  group_by(author) %>%
#  mutate(occurrence = n ) %>%
#  select(-n) %>%
#  spread(author, occurrence) %>%
#  gather(author, occurrence, `Gataker Marriage Duties` : `Guy Pieties Pillar`)
#Pulling out the N/A entries in occurrence data frame
#cleanoccurence <- occurrence[complete.cases(occurrence), ]
#Computing td-idf for cleanoccurence Naming it: cleanoccur
#cleanoccur <- cleanoccurence %>%
#  bind_tf_idf(word, author, occurrence)
#Let's select the high tf-idf words
#cleanoccur %>%
#  arrange(desc(tf-idf)) %>%
#  mutate(word = factor(word, levels = rev(unique(word)))) %>%
#  group_by(author) %>%
#  top_n(15) %>%
#  ungroup %>%
#  ggplot(aes(word, tf_idf, fill = author))+
#  geom_col(show.legend = FALSE) +
#  labs(x=NULL, y = "tf-idf")+
#  facet_wrap(~author, ncol =2, scales = "free")+
#  coord_flip()
#In the next line, I am computing the frequency of each word--instead of the occurrences--in each sermon
#and binding the data frames together.
#frequency_full <- bind_rows(mutate(gouge_domesticall_tidy, author = "Gouge Domesticall"),
#                            mutate(gataker_marriage_duties_tidy, author = "Gataker Marriage Duties"),
#                            mutate(guy_pieties_pillar_tidy, author = "Guy Pieties Pillar")) %>%
#  mutate(word =str_extract(word, "[a-z']+")) %>%
#  count(author, word) %>%
#  group_by(author) %>%
#  mutate(proportion = n / sum(n)) %>%
#  select(-n) %>%
#  spread(author, proportion) %>%
#  gather(author, proportion, `Gataker Marriage Duties` : `Guy Pieties Pillar`)
#### Trying this pair-wise
#frequency <- bind_rows(mutate(gouge_domesticall_tidy, author = "Gouge Domesticall"),
#                       mutate(gataker_marriage_duties_tidy, author = "Gataker Marriage Duties")) %>% 
#  mutate(word = str_extract(word, "[a-z']+")) %>%
#  count(author, word) %>%
#  group_by(author) %>%
  #the online suggestion was group_by(word) instead... and using "pivot_wider" below, but I can't get it to work
#  mutate(proportion = n / sum(n)) %>%
#  select(-n) %>%
#  spread(author, proportion) %>% 
#  gather(author, proportion, `Gataker Marriage Duties`)
## Now I plot:
#ggplot(frequency, aes(x = proportion, y =  `Gouge Domesticall`, 
#                      color = abs(`Gouge Domesticall` -  proportion))) +
#  geom_abline(color = "gray40", lty = 2) +
  #geom_point(shape = 2)
#  geom_jitter(alpha = 0.3, size = 2.5, width = 0.3, height = 0.3) +
#  geom_text_repel(aes(label = word), min.segment.length = Inf, verbose = TRUE, seed = 123, max.time = 320, max.iter = 100000, max.overlaps = Inf, size = 3) +
  #geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
#  scale_x_log10(labels = percent_format()) +
  #xlim(-0.4,0.4)+
#  scale_y_log10(labels = percent_format()) +
  #ylim(-0.4,0.4)+
#  scale_color_gradient(limits = c(0, 0.001), 
#                       low = "darkorchid1", high = "darkorchid3") +
  #facet_wrap(~author, ncol = 2)+
#  theme(legend.position="none") +
#  labs(y = "Gouge Domesticall", x = "Gataker Marriage Duties")

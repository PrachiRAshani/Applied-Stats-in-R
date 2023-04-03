library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(ggrepel)
library(MASS)
library(ggthemes)
library(wesanderson)
library(RColorBrewer)
library(scales)
library(viridis)
library(tvthemes)
library(PerformanceAnalytics)
library(corrplot)
library(psych)
import_cinzel() #try this later

netflix = read.csv("/Users/Aishwarya/Documents/AISHWARYA MALOO/SMU ACADEMIC/SEMESTER 1/ASAR/ASAR Project/WIP/movies_trim.csv")

View(netflix)

# Correlation
my_data <- netflix_trim[, c(18,55,70,84,156,157,159,161)]
chart.Correlation(my_data, histogram=TRUE, pch=19, method = 'pearson')

png(height=1200, width=1500, pointsize=15, file="overlap.png")

corrplot(cor(my_data), method = 'color', addCoef.col = 'grey', order = 'AOE')

corPlot(my_data, cex =0.8, upper = FALSE, xlas = 2)


# Linear Regression with IMDb Score
m1 <- lm(IMDb_Score~Awards_Received+log(Boxoffice)+Rotten_Tomatoes_Score+Language.count+Country.count+Genre.count+X1.2.hours+Over.2.hours, data = netflix)
summary(m1)
#60.34

step <- stepAIC(m1, direction ="both")
summary(step)

# Linear Regression with Rotten Tomatoes

m2 <- lm(Rotten_Tomatoes_Score~Awards_Received+log(Boxoffice)+IMDb_Score+Language.count+Country.count+Genre.count+X1.2.hours+Over.2.hours, data = netflix)
summary(m2)
#60.34

step <- stepAIC(m2, direction ="both")
summary(step)

# Linear Regression with BoxOffice

m3 <- lm(log(Boxoffice)~Awards_Received+IMDb_Score+Language.count+Country.count+Genre.count+X1.2.hours+Over.2.hours, data = netflix)
summary(m3)
#60.34

step <- stepAIC(m3, direction ="both")
summary(step)

########### Chi Square Analysis

#Association between Box Office and Awards
boxoff_awards = netflix_data[!is.na(netflix_data$boxoffice) & !is.na(netflix_data$awards_received),c("boxoffice","awards_received")]
quantile(boxoff_awards$boxoffice)
quantile(boxoff_awards$awards_received)
boxoff_awards %>%
  mutate(Box_group = case_when(boxoffice <= 1300682 ~ "b_p25%",
                               boxoffice <= 25881068 ~ "b_p50%",
                               boxoffice <= 77812000 ~ "b_p75%",
                               boxoffice > 77812000 ~ "b_p100%"
  ),
  awards_group = case_when(awards_received <= 2 ~ "a_p25%",
                           awards_received <= 4 ~ "a_p50%",
                           awards_received <= 12 ~ "a_p75%",
                           awards_received > 12 ~ "a_p100%"
  )
  )%>%
  count(Box_group,awards_group)

R1 = c(238,254,325,161)
R2 = c(126,96,126,117)
R3 = c(193,143,122,185)
R4 = c(132,195,115,225)
rows = 4

Matriz = matrix(c(R1,R2,R3,R4), nrow = rows, byrow = TRUE)
colnames(Matriz) = c("b_p25%","b_p50%","b_p75%","b_p100%")
rownames(Matriz) = c("a_p25%","a_p50%","a_p75%","a_p100%")
Matriz
chisq.test(Matriz,correct = FALSE)



#Association between Awards and IMDb

awards_imdb = netflix_data[!is.na(netflix_data$awards_received) & !is.na(netflix_data$imdb_score),c("awards_received","imdb_score")]
awards_imdb
cor(awards_imdb["awards_received"],awards_imdb["imdb_score"])

#Association between awards_received and IMDB
awards_imdb = netflix_data[!is.na(netflix_data$awards_received) & !is.na(netflix_data$imdb_score),c("awards_received","imdb_score")]
quantile(awards_imdb$awards_received)
quantile(awards_imdb$imdb_score)
awards_imdb %>%
  mutate(awards_group = case_when(awards_received <= 1 ~ "a_p25%",
                                  awards_received <= 3 ~ "a_p50%",
                                  awards_received <= 8 ~ "a_p75%",
                                  awards_received> 8 ~ "a_p100%"
  ),
  imdb_group = case_when(imdb_score <= 6.2 ~ "i_p25%",
                         imdb_score<= 6.8 ~ "i_p50%",
                         imdb_score<= 7.3 ~ "i_p75%",
                         imdb_score > 7.3 ~ "i_p100%"
  )
  )%>%
  count(awards_group,imdb_group)

R1 = c(620,418,244,68)
R2 = c(386,366,324,164)
R3 = c(214,250,297,319)
R4 = c(111,179,239,627)
rows = 4

Matriz = matrix(c(R1,R2,R3,R4), nrow = rows, byrow = TRUE)
colnames(Matriz) = c("a_p25%","a_p50%","a_p75%","a_p100%")
rownames(Matriz) = c("i_p25%","i_p50%","i_p75%","i_p100%")
Matriz
chisq.test(Matriz,correct = FALSE)

##################################DATA for EDA

netflix_data = read.csv("/Users/Aishwarya/Documents/AISHWARYA MALOO/SMU ACADEMIC/SEMESTER 1/ASAR/ASAR Project/WIP/netflix_PA clean_csv_updated_r_21.03.22_7.14pm.csv")
View(netflix_data)

#Convert headers to lowercase
names(netflix_data) = tolower(names(netflix_data))

###################Genre count per movies

genre_count_ = netflix_data[!is.na(netflix_data$title_director_actors) & !is.na(netflix_data$primary_g1) & !(netflix_data$primary_g1 %in% c('Talk-Show','Game-Show','Film-Noir')),c("primary_g1","title_director_actors")]

genre_count_ %>%
  count(primary_g1, sort = TRUE) %>%
  top_n(15,n) %>%
  ggplot(aes(reorder(primary_g1,n),n)) +
  geom_bar(stat="identity",fill = "#cc2900", color = '#999999')+
  geom_text(aes(label = n), vjust=0.2, hjust=-0.1, color="black",
            position = position_dodge(0.9), size=3.5)+
  coord_flip()+
  xlab("Genres") +
  ylab("Number of movies")+
  labs(title = 'Number of movies by genres')+
  theme_minimal()+
  theme(text = element_text(family = "Garamond", size = 25, face = "bold"))+
  theme(axis.text.x = element_text(size=14, angle=0, face = "bold"))+ 
  theme(axis.text.y = element_text(size=14, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))

###############GENRE, ACTORS, DIRECTORS, LANGUAGES

# Top 5 genres atleast 20 movies with mean box office
# Top 5 genres are Action, Adventure, Animation, Comedy, Drama, #E69F00
#C4961A", "#00AFBB","#D16103","#52854C","#293352"

# x = netflix_data %>% 
#   group_by(primary_g1) %>%
#   summarise(me_bo = mean(boxoffice, na.rm = TRUE)) %>%
#   arrange(-mean_bo) %>%
#   slice(1:5)

#####Boxoffice Values histogram

ggplot(data = netflix_data, aes(x = boxoffice)) + geom_histogram(bins = 100, fill = "#4d194d", color = "white") +
  scale_x_continuous(labels = comma, limits = c(70, 600000000))+
  scale_y_continuous(limits = c(0, 300))+
  xlab("Box Office Numbers") +
  ylab("Sum of Box Office Numbers")+
  theme_fivethirtyeight()+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", face = "bold", hjust =1, size=14))+
  theme(axis.text.y = element_text(colour = "black", face = "bold", size=14))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Box Office Numbers Distribution")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))


###Genres R-code with Boxoffice (Powerpoint)

netfilx1 = netflix_data[!is.na(netflix_data$boxoffice) & !is.na(netflix_data$primary_g1) & !(netflix_data$primary_g1 %in% c('Talk-Show','Game-Show','Film-Noir')),c("primary_g1","boxoffice")] 

#genres having more than 20 movies
genre_count = netfilx1 %>%
  count(primary_g1, sort = TRUE)
genre_count2 = genre_count[(genre_count$n >= 20),]

genre_compare1 = inner_join(netfilx1, genre_count2, by = c("primary_g1"="primary_g1"))
genre_compare1

genre_compare1 %>% 
  group_by(primary_g1) %>%
  summarise(mean_bo = mean(boxoffice, na.rm = TRUE)) %>%
  arrange(-mean_bo) %>% 
  slice(1:5)

genre_compare1 = genre_compare1[genre_compare1$primary_g1 %in% c('Action','Adventure', 'Animation', 'Horror', 'Comedy'),]

genre_compare1 = genre_compare1 %>%
  mutate(highlight01 = ifelse(genre_compare1$primary_g1 %in% c('Action','Adventure', 'Animation', 'Horror', 'Comedy'),"Yes","No"))

genre_compare1 %>%
  ggplot(aes(reorder(primary_g1,boxoffice),boxoffice,fill = highlight01))+
  geom_boxplot() +
  stat_summary(fun.y="mean", shape=7, size = 0.2, color = 'white')+
  xlab("Genres") +
  scale_y_continuous(name="Box Office Value", labels = comma)+
  scale_fill_manual(values = c('#1f3d7a'))+
  theme_fivethirtyeight()+
  guides(fill = FALSE)+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", face = "bold", angle=45, hjust =1, size=14))+
  theme(axis.text.y = element_text(colour = "black", face = "bold", size=14))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Genres by Mean Box Office Values")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# ANOVA Analysis

# Genre by Boxoffice - ANOVA
# Significant
# Difference in mean: Comedy – Action, Comedy-Adventure, Comedy-Animation 
genre_compare1 %>% 
  group_by(primary_g1) %>%
  summarise(mean_bo = mean(boxoffice, na.rm = TRUE)) %>%
  arrange(-mean_bo) %>% 
  slice(1:5)

boxoffice_genre_high.aov <- aov(boxoffice ~ primary_g1, data = genre_compare1[genre_compare1$primary_g1 %in% c('Action', 'Adventure', 'Animation', 'Horror', 'Comedy'),])
summary(boxoffice_genre_high.aov)
TukeyHSD(boxoffice_genre_high.aov)

###Actors R-code with Boxoffice (powerpoint)

Imdb_actors1 = netflix_data[!is.na(netflix_data$boxoffice) & !is.na(netflix_data$actors),c("actors","boxoffice")]

#actors having more than 10 movies
actors_count = Imdb_actors1 %>%
  count(actors, sort = TRUE)
actors_count2 = actors_count[(actors_count$n >= 10),]
actors_count2

actor_compare1 = inner_join(Imdb_actors1, actors_count2, by = c("actors"="actors"))
actor_compare1

actor_compare1 %>% 
  group_by(actors) %>%
  summarise(mean_bo = mean(boxoffice, na.rm = TRUE)) %>%
  arrange(-mean_bo) %>% 
  slice(1:5)

actor_compare1 = actor_compare1[actor_compare1$actors %in% c('Robert Downey Jr.','Liam Neeson', 'Jennifer Lawrence', 'Eddie Murphy', 'Jack Black'),]

actor_compare1 = actor_compare1 %>%
  mutate(highlight01 = ifelse(actor_compare1$actors %in% c('Robert Downey Jr.','Liam Neeson', 'Jennifer Lawrence', 'Eddie Murphy', 'Jack Black'),"Yes","No"))

actor_compare1 %>%
  ggplot(aes(reorder(actors,boxoffice), boxoffice,fill = highlight01))+
  geom_boxplot() +
  stat_summary(fun.y="mean", shape=17, size = 0.2, color = 'white', face = 'bold')+
  xlab("Actors") +
  scale_y_continuous(name="Box Office Value", labels = comma) +
  scale_fill_manual(values = "#004d4d")+
  theme_fivethirtyeight()+
  guides(fill = "none")+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
  theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Actors by Mean Box Office Values")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# Actors by Boxoffice - ANOVA
##### INSIGNIFICANT 

actor_compare1 %>% 
  group_by(actors) %>%
  summarise(mean_bo = mean(boxoffice, na.rm = TRUE)) %>%
  arrange(-mean_bo) %>% 
  slice(1:5)

boxoffice_actor_high.aov <- aov(boxoffice ~ actors, data = actor_compare1[actor_compare1$actors %in% c('Eddie Murphy', 'Robert Downey Jr.', 'Liam Neeson', 'Jennifer Lawrence', 'Jack Black'),]) 
summary(boxoffice_actor_high.aov)
TukeyHSD(boxoffice_actor_high.aov)

###Directors R-code with Boxoffice

Box_dir1 = netflix_data[!is.na(netflix_data$boxoffice) & !is.na(netflix_data$director),c("director","boxoffice")]

#actors having more than 10 movies
dir_count = Box_dir1 %>%
  count(director, sort = TRUE)
dir_count2 = dir_count[(dir_count$n >= 10),]
dir_count2

dir_compare1 = inner_join(Box_dir1, dir_count2, by = c("director"="director"))
dir_compare1

dir_compare1 %>% 
  group_by(director) %>%
  summarise(mean_bo = mean(boxoffice, na.rm = TRUE)) %>%
  arrange(-mean_bo) %>% 
  slice(1:5)

dir_compare1 = dir_compare1[dir_compare1$director %in% c('George Lucas','Michael Bay', 'Steven Spielberg', 'Jay Roach', 'Robert Zemeckis'),]

dir_compare1 = dir_compare1 %>%
  mutate(highlight01 = ifelse(dir_compare1$director %in% c('George Lucas','Michael Bay', 'Steven Spielberg', 'Jay Roach', 'Robert Zemeckis'),"Yes","No"))

dir_compare1 %>%
  ggplot(aes(reorder(director,boxoffice), boxoffice,fill = highlight01))+
  geom_boxplot() +
  stat_summary(fun.y="mean", shape=17, size = 0.2, color = 'white')+
  xlab("Directors") +
  scale_y_continuous(name="Box Office Value", labels = comma, limit = c(50000000, 400000000)) +
  scale_fill_manual(values = "#cc7a00")+
  theme_fivethirtyeight()+
  guides(fill = "none")+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
  theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Directors by Mean Box Office Values")+
  theme(plot.title = element_text(hjust = 0.2, size = 20, face = "bold"))

# Directors by Boxoffice - ANOVA
##### SIGNIFICANT 

dir_compare1 %>% 
  group_by(director) %>%
  summarise(mean_bo = mean(boxoffice, na.rm = TRUE)) %>%
  arrange(-mean_bo) %>% 
  slice(1:5)

boxoffice_dir_high.aov <- aov(boxoffice ~ director, data = dir_compare1[dir_compare1$director %in% c('George Lucas','Michael Bay', 'Steven Spielberg', 'Jay Roach', 'Robert Zemeckis'),]) 
summary(boxoffice_dir_high.aov)
TukeyHSD(boxoffice_dir_high.aov)

# There is a difference in means between some pair of directors - Robert Zemeckis-George Lucas, Steven Spielberg-George Lucas, Michael Bay-George Lucas, Jay Roach-George Lucas

###Languages R-code with Boxoffice

Box_lang1 = netflix_data[!is.na(netflix_data$boxoffice) & !is.na(netflix_data$languages),c("languages","boxoffice")]

#actors having more than 10 movies
lang_count = Box_lang1 %>%
  count(languages, sort = TRUE)
lang_count2 = lang_count[(lang_count$n >= 50),]
lang_count2

lang_compare1 = inner_join(Box_lang1, lang_count2, by = c("languages"="languages"))
lang_compare1

lang_compare1 %>% 
  group_by(languages) %>%
  summarise(mean_bo = mean(boxoffice, na.rm = TRUE)) %>%
  arrange(-mean_bo) %>% 
  slice(1:5)

lang_compare1 = lang_compare1[lang_compare1$languages %in% c('English','Spanish', 'Chinese', 'Japanese', 'Hindi'),]

lang_compare1 = lang_compare1 %>%
  mutate(highlight01 = ifelse(lang_compare1$languages %in% c('English','Spanish', 'Chinese', 'Japanese', 'Hindi'),"Yes","No"))

lang_compare1 %>%
  ggplot(aes(reorder(languages,boxoffice), boxoffice,fill = highlight01))+
  geom_boxplot() +
  stat_summary(fun.y="mean", shape=17, size = 0.6, color = 'black')+
  xlab("Languages") +
  scale_y_continuous(name="Box Office Value", labels = comma, limits = c(1600000, 200000000), breaks = seq(0, 200000000, by = 25000000)) +
  scale_fill_manual(values = "#993333")+
  theme_fivethirtyeight()+
  guides(fill = "none")+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
  theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Languages by Mean Box Office Values")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# Languages by Boxoffice - ANOVA
##### SIGNIFICANT 
# Difference in mean: English-Chinese, Hindi-English, Japanese-English, Spanish-English, Japanese-Hindi 

lang_compare1 %>% 
  group_by(languages) %>%
  summarise(mean_bo = mean(boxoffice, na.rm = TRUE)) %>%
  arrange(-mean_bo) %>% 
  slice(1:5)

boxoffice_lang_high.aov <- aov(boxoffice ~ languages, data = lang_compare1[lang_compare1$languages %in% c('English','Spanish', 'Chinese', 'Japanese', 'Hindi'),]) 
summary(boxoffice_lang_high.aov)
TukeyHSD(boxoffice_lang_high.aov)

# There is difference in means, English - Chinese, Hindi-English, Japanese-English, Spanish-English, Japanese-Hindi


################## IMDb Score with Genres, Actors, Directors and Languages

#####IMDb Score histogram
ggplot(data = netflix_data, aes(x = imdb_score)) + geom_histogram(bins = 40, fill = "#009999", color = 'black') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))+
  xlab("IMDb Score") +
  ylab("Count")+
  theme_fivethirtyeight()+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", face = "bold", hjust =1, size=14))+
  theme(axis.text.y = element_text(colour = "black", face = "bold", size=14))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "IMDb Score Distribution")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))


#####IMDB score - pie chart

imdb_9 = nrow(filter(netflix_data, imdb_score >= 9)[1])
imdb_8 = nrow(filter(netflix_data, imdb_score >= 8 & imdb_score < 9)[1])
imdb_7 = nrow(filter(netflix_data, imdb_score >= 7 & imdb_score < 8)[1])
imdb_6 = nrow(filter(netflix_data, imdb_score >= 6 & imdb_score < 7)[1])
imdb_5 = nrow(filter(netflix_data, imdb_score >= 5 & imdb_score < 6)[1])
imdb_4 = nrow(filter(netflix_data, imdb_score >= 4 & imdb_score < 5)[1])
imdb_3 = nrow(filter(netflix_data, imdb_score >= 0 & imdb_score < 4)[1])
pie_val = c(imdb_9, imdb_8, imdb_7, imdb_6, imdb_5, imdb_4, imdb_3)
pie_labels = c('>=9 score','8-9 score','7-8 score','6-7 score','5-6 score','4-5 score','0-4 score')
pie_percent = round(pie_val/sum(pie_val)*100, 1)
pie_labels = paste(pie_labels, pie_percent) # add percents to labels
pie_labels = paste(pie_labels,"%",sep="") # ad % to labels
pie(pie_val, labels = pie_labels, col=rainbow(length(pie_labels)), main = "Percentage of Titles by IMDb Score")
  

#####IMDB score - pie chart
imdb_9 = nrow(filter(netflix_data, imdb_score >= 9)[1])
imdb_8 = nrow(filter(netflix_data, imdb_score >= 8 & imdb_score < 9)[1])
imdb_7 = nrow(filter(netflix_data, imdb_score >= 7 & imdb_score < 8)[1])
imdb_6 = nrow(filter(netflix_data, imdb_score >= 6 & imdb_score < 7)[1])
imdb_5 = nrow(filter(netflix_data, imdb_score >= 5 & imdb_score < 6)[1])
imdb_4 = nrow(filter(netflix_data, imdb_score >= 4 & imdb_score < 5)[1])
imdb_3 = nrow(filter(netflix_data, imdb_score >= 0 & imdb_score < 4)[1])
pie_val = c(imdb_9, imdb_8, imdb_7, imdb_6, imdb_5, imdb_4, imdb_3)
pie_labels = c('>=9 score','8-9 score','7-8 score','6-7 score','5-6 score','4-5 score','0-4 score')
pie_percent = round(pie_val/sum(pie_val)*100, 1)
pie_labels = paste(pie_labels, pie_percent) # add percents to labels
pie_labels = paste(pie_labels,"%",sep="") # ad % to labels
pie(pie_val, labels = pie_labels, col=rainbow(length(pie_labels)), main = "Percentage of Titles by IMDb Score")

#Genres R-code with IMDb score (Powerpoint)

netfilx1 = netflix_data[!is.na(netflix_data$imdb_score) & !is.na(netflix_data$primary_g1) & !(netflix_data$primary_g1 %in% c('Talk-Show','Game-Show','Film-Noir')),c("primary_g1","imdb_score")] 

#genres having more than 10 movies
genre_count = netfilx1 %>%
  count(primary_g1, sort = TRUE)
genre_count2 = genre_count[(genre_count$n >= 20),]
genre_count2

#actors to compare
genre_compare1=inner_join(netfilx1, genre_count2, by = c("primary_g1"="primary_g1"))

# To determine which genres are in the top 5 to add to the code later
genre_compare1 %>% 
  group_by(primary_g1) %>%
  summarise(mean_bo = mean(imdb_score, na.rm = TRUE)) %>%
  arrange(-mean_bo) %>% 
  slice(1:5)

genre_compare1 = genre_compare1[genre_compare1$primary_g1 %in% c('Documentary','Biography', 'Short', 'Crime', 'Animation'),]

genre_compare1 = genre_compare1 %>%
  mutate(highlight01 = ifelse(genre_compare1$primary_g1 %in% c('Documentary','Biography', 'Short', 'Crime', 'Animation'),"Yes","No"))

genre_compare1 %>%
ggplot(aes(reorder(primary_g1,imdb_score),imdb_score,fill = highlight01))+
geom_boxplot() +
stat_summary(fun.y="mean", shape=7, size = 0.2, color = 'white')+
xlab("Genres") +
ylab("IMDb Ratings")+
scale_fill_manual(values = c("#1f3d7a"))+
theme_fivethirtyeight()+
guides(fill = FALSE)+
theme(text = element_text(family = "Garamond", size = 14))+
theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
theme(axis.title = element_text(size = 20, face = "bold"))+
labs(title = "Mean IMDb Ratings by Genres")+
theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# Genre by IMDb Score - ANOVA
#### Significant
genre_compare1 %>% 
  group_by(primary_g1) %>%
  summarise(mean_imdb = mean(imdb_score, na.rm = TRUE)) %>%
  arrange(-mean_imdb) %>% 
  slice(1:5)

imdb_genre_high.aov <- aov(imdb_score ~ primary_g1, data = genre_compare1[genre_compare1$primary_g1 %in% c('Documentary','Biography', 'Short', 'Crime', 'Animation'),]) 
summary(imdb_genre_high.aov)
TukeyHSD(imdb_genre_high.aov)

#Actors R-code with IMDb score (Powerpoint)

Imdb_actors1 = netflix_data[!is.na(netflix_data$imdb_score) & !is.na(netflix_data$actors),c("actors","imdb_score")]

#actors having more than 10 movies
actors_count = Imdb_actors1 %>%
  count(actors, sort = TRUE)
actors_count2 = actors_count[(actors_count$n >= 10),]
actors_count2

#actors having at least over 8 IMDb score one movie
actors_max_score = distinct(Imdb_actors1[(Imdb_actors1$imdb_score >= 8),], actors)
actors_max_score

#actors to compare
actors_compare1=inner_join(Imdb_actors1, actors_count2, by = c("actors"="actors"))
actors_compare2=inner_join(actors_compare1, actors_max_score, by = c("actors"="actors"))

actors_compare2 = actors_compare2[actors_compare2$actors %in% c('Jennifer Connelly','Nawazuddin Siddiqui', 'Gary Oldman', 'Brad Pitt', 'Woody Harrelson'),]

actors_compare2 = actors_compare2 %>%
  mutate(highlight01 = ifelse(actors_compare2$actors %in% c('Jennifer Connelly','Nawazuddin Siddiqui', 'Gary Oldman', 'Brad Pitt', 'Woody Harrelson'),"Yes","No"))

actors_compare2 %>%
  ggplot(aes(reorder(actors,imdb_score), imdb_score,fill = highlight01))+
  geom_boxplot() +
  stat_summary(fun.y="mean", shape=17, size = 0.2, color = 'white')+
  xlab("Actors") +
  ylab("IMDb Ratings") +
  scale_fill_manual(values = c("#004d4d"))+
  theme_fivethirtyeight()+
  guides(fill = "none")+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
  theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Mean IMDb Ratings by Actors")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# Actors by IMDb Score - ANOVA
#### insignificant
actors_compare2 %>% 
  group_by(actors) %>%
  summarise(mean_imdb = mean(imdb_score, na.rm = TRUE)) %>%
  arrange(-mean_imdb) %>% 
  slice(1:5)

imdb_actor_high.aov <- aov(imdb_score ~ actors, data = actors_compare2[actors_compare2$actors %in% c('Jennifer Connelly','Nawazuddin Siddiqui', 'David Strathairn', 'Brad Pitt', 'Morgan Freeman'),]) 
summary(imdb_actor_high.aov)
TukeyHSD(imdb_actor_high.aov)


###Languages R-code with IMDb score

Imdb_lang1 = netflix_data[!is.na(netflix_data$imdb_score) & !is.na(netflix_data$languages),c("languages","imdb_score")] 

#genres having more than 20 movies
lang_count = Imdb_lang1 %>%
  count(languages, sort = TRUE)

lang_count2 = lang_count[(lang_count$n >= 50),]
lang_count2

#genres to compare
lang_compare1=inner_join(Imdb_lang1, lang_count2, by = c("languages"="languages"))

lang_compare1 %>% 
  group_by(languages) %>%
  summarise(mean_bo = mean(imdb_score, na.rm = TRUE)) %>%
  arrange(-mean_bo) %>% 
  slice(1:5)

lang_compare1 = lang_compare1[lang_compare1$languages %in% c('Romanian','Korean', 'Tamil', 'Japanese', 'Filipino'),]

lang_compare1 = lang_compare1 %>%
  mutate(highlight01 = ifelse(lang_compare1$languages %in% c('Romanian','Korean', 'Tamil', 'Japanese', 'Filipino'),"Yes","No"))

lang_compare1 %>%
  ggplot(aes(reorder(languages,imdb_score),imdb_score,fill = highlight01))+
  geom_boxplot() +
  stat_summary(fun.y="mean", shape=17, size = 0.2, color = 'white')+
  xlab("Languages") +
  ylab("IMDb Ratings")+
  scale_fill_manual(values = c("#993333"))+
  theme_fivethirtyeight()+
  guides(fill = FALSE)+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
  theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Mean IMDb Ratings by Languages")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# Languages by Boxoffice - ANOVA
# Insignificant
lang_compare1 %>% 
  group_by(languages) %>%
  summarise(mean_bo = mean(imdb_score, na.rm = TRUE)) %>%
  arrange(-mean_bo) %>% 
  slice(1:5)

imdb_lang_high.aov <- aov(imdb_score ~ languages, data = lang_compare1[lang_compare1$languages %in% c('Romanian','Korean', 'Tamil', 'Japanese', 'Filipino'),]) 
summary(imdb_lang_high.aov)
TukeyHSD(imdb_lang_high.aov)

# p value greater than 0.05, ANOVA analysis is insignificant

###Directors R-code with IMDb score
Imdb_dir1 = netflix_data[!is.na(netflix_data$imdb_score) & !is.na(netflix_data$director),c("director","imdb_score")]

#directors having more than 10 movies
dir_count = Imdb_dir1 %>%
  count(director, sort = TRUE)
dir_count2 = dir_count[(dir_count$n >= 10),]
dir_count2

#directors having at least over 8 IMDb score one movie
dir_max_score = distinct(Imdb_dir1[(Imdb_dir1$imdb_score >= 8),], director)
dir_max_score

#director to compare
dir_compare1=inner_join(Imdb_dir1, dir_count2, by = c("director"="director"))
dir_compare2=inner_join(dir_compare1, dir_max_score, by = c("director"="director"))

dir_compare2 %>% 
  group_by(director) %>%
  summarise(mean_bo = mean(imdb_score, na.rm = TRUE)) %>%
  arrange(-mean_bo) %>%
  slice(1:5)

dir_compare2 = dir_compare2[dir_compare2$director %in% c('Hayao Miyazaki','Martin Scorsese', 'Charles Chaplin', 'Steven Spielberg', 'François Truffaut'),]

dir_compare2 = dir_compare2 %>%
  mutate(highlight01 = ifelse(dir_compare2$director %in% c('Hayao Miyazaki','Martin Scorsese', 'Charles Chaplin', 'Steven Spielberg', 'François Truffaut'),"Yes","No"))

dir_compare2 %>%
  ggplot(aes(reorder(director,imdb_score), imdb_score,fill = highlight01))+
  geom_boxplot() +
  stat_summary(fun.y="mean", shape=17, size = 0.2, color = 'white')+
  xlab("Directors") +
  ylab("IMDb Ratings") +
  scale_fill_manual(values = c("#cc7a00"))+
  theme_fivethirtyeight()+
  guides(fill = "none")+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
  theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Mean IMDb Ratings by Directors")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# Directors by IMDb Score - ANOVA
##### SIGNIFICANT 

dir_compare2 %>% 
  group_by(director) %>%
  summarise(mean_bo = mean(imdb_score, na.rm = TRUE)) %>%
  arrange(-mean_bo) %>%
  slice(1:5)

imdb_dir_high.aov <- aov(imdb_score ~ director, data = dir_compare2[dir_compare2$director %in% c('Hayao Miyazaki','Martin Scorsese', 'Charles Chaplin', 'Steven Spielberg', 'François Truffaut'),]) 
summary(imdb_dir_high.aov)
TukeyHSD(imdb_dir_high.aov)

# p value greater than 0.05, ANOVA analysis is insignificant

##########Awards received with actors, genre, directors, actors

# Genre R-code with Awards received

netfilx1 = netflix_data[!is.na(netflix_data$awards_received) & !is.na(netflix_data$primary_g1) & !(netflix_data$primary_g1 %in% c('Talk-Show','Game-Show','Film-Noir')),c("primary_g1","awards_received")]

netflix1 = netfilx1 %>%
  mutate(highlight01 = ifelse(netfilx1$primary_g1 %in% c('Biography', 'History', 'Drama', 'Crime', 'Adventure'),"Yes","No"))

netflix2 = netflix1%>%
  group_by(primary_g1) %>%
  summarise(sum_awards = sum(awards_received))

netflix2 = netflix2 %>%
  mutate(highlight01 = ifelse(netflix2$primary_g1 %in% c('Drama','Biography', 'Comedy', 'Action', 'Crime'),"Yes","No"))

netflix2

netflix2[!(netflix2$primary_g1 %in% c('Music','War','Western','Sci-Fi','Romance','Musical','Short','History','Thriller')),] %>%
  ggplot(aes(reorder(primary_g1,sum_awards),sum_awards,fill = highlight01)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=sum_awards), vjust=-0.3, color="black",
            position = position_dodge(0.9), size=3.5)+
  xlab("Genres") +
  ylab("Awards Received")+
  scale_fill_manual(values = c("dark grey","#1f3d7a"))+
  guides(fill = "none")+
  theme_fivethirtyeight()+
  guides(fill = "none")+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
  theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Total Awards Received by Genres")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
theme(axis.text.x=element_text(size=10, angle=45,hjust = 1))

## Genre - Awards received ANOVA

# awards_received_top_5_genre = netflix_data[!is.na(netflix_data$awards_received) & !is.na(netflix_data$primary_g1),c("primary_g1","awards_received")]
# s = awards_received_top_5_genre[awards_received_top_5_genre$primary_g1 %in% c('Biography', 'History', 'Drama', 'Crime', 'Advanture'),]
# awards_received_genre_high.aov <- aov(awards_received ~ primary_g1, data = s)
# summary(awards_received_genre_high.aov)
# TukeyHSD(awards_received_genre_high.aov)


# R-Code Actors with Awards Received

awards_received_actors1 = netflix_data[!is.na(netflix_data$awards_received) & !is.na(netflix_data$actors),c("actors","awards_received")]

#actors having more than 10 movies
actors_count = awards_received_actors1 %>%
  count(actors, sort = TRUE)
actors_count2 = actors_count[(actors_count$n >= 10),]

#actors having at least over 10 awards one movie
actors_max_score = distinct(awards_received_actors1[(awards_received_actors1$awards_received >= 10),], actors)

#actors to compare
actors_compare1=inner_join(awards_received_actors1, actors_count2, by = c("actors"="actors"))
actors_compare2=inner_join(actors_compare1, actors_max_score, by = c("actors"="actors"))

actors_compare3 = actors_compare2 %>%
  group_by(actors) %>%
  summarise(sum_awards = sum(awards_received))

actors_compare3 = actors_compare3 %>%
  mutate(highlight01 = ifelse(actors_compare3$actors %in% c('Jennifer Lawrence', 'Liam Neeson', 'Woody Harrelson', 'Bradley Cooper', 'Colin Firth'),"Yes","No"))

actors_compare3 %>%
  ggplot(aes(reorder(actors,sum_awards),sum_awards,fill = highlight01)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=sum_awards), vjust=-0.3, color="black",
            position = position_dodge(0.9), size=3.5)+
  xlab("Actors") +
  ylab("Awards Received")+
  scale_fill_manual(values = c("dark grey","#004d4d"))+
  guides(fill = "none")+
  theme_fivethirtyeight()+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
  theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Total Awards Received by Actors")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
theme(axis.text.x=element_text(size=10, angle=45,hjust = 1))

# awards_received_actor_high.aov <- aov(awards_received ~ actors, data = actors_compare2[actors_compare2$actors %in% c('Jennifer Lawrence', 'Liam Neeson', 'Hwang Jung-min', 'Colin Firth', 'Robert Downey Jr.'),])
# summary(awards_received_actor_high.aov)
# TukeyHSD(awards_received_actor_high.aov)

# Director R-code with Awards received + ANOVA

awards_received_director1 = netflix_data[!is.na(netflix_data$awards_received) & !is.na(netflix_data$director),c("director","awards_received")]

#directors having more than 10 movies
director_count = awards_received_director1 %>%
  count(director, sort = TRUE)
director_count2 = director_count[(director_count$n >= 10),]

#directors having at least over 10 award one movie
director_max_score = distinct(awards_received_director1[(awards_received_director1$awards_received >= 10),], director)

#directors to compare
director_compare1 = inner_join(awards_received_director1, director_count2, by = c("director"="director"))
director_compare2 = inner_join(director_compare1, director_max_score, by = c("director"="director"))


director_compare3 = director_compare2 %>%
  group_by(director) %>%
  summarise(sum_awards = sum(awards_received)) %>% 
  arrange(-sum_awards)

director_compare2 %>%
  group_by(director) %>%
  summarise(sum_awards = sum(awards_received))

director_compare3 = director_compare3 %>%
mutate(highlight01 = ifelse(director_compare3$director %in% c('Martin Scorsese', 'George Lucas', 'Pedro Almodóvar', 'Steven Spielberg', 'Clint Eastwood'),"Yes","No"))

director_compare3 %>%
  ggplot(aes(reorder(director,sum_awards),sum_awards,fill = highlight01)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=sum_awards), vjust=-0.3, color="black",
            position = position_dodge(0.9), size=3.5)+
  xlab("Directors") +
  ylab("Awards Received")+
  scale_fill_manual(values = c("grey", "#cc7a00"))+
  guides(fill = "none")+
  theme_fivethirtyeight()+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
  theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Total Awards Received by Directors")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
theme(axis.text.x=element_text(size=10, angle=45,hjust = 1))


#Languages R-code with Awards received + ANOVA

awards_received_languages1 = netflix_data[!is.na(netflix_data$awards_received) & !is.na(netflix_data$languages),c("languages","awards_received")]

#languages having more than 50 movies
languages_count = awards_received_languages1 %>%
  count(languages, sort = TRUE)
languages_count2 = languages_count[(languages_count$n >= 50),]


#countries to compare
languages_compare1=inner_join(awards_received_languages1, languages_count2, by = c("languages"="languages"))

languages_compare2 = languages_compare1 %>%
  group_by(languages) %>%
  summarise(sum_awards = sum(awards_received))

languages_compare2=languages_compare2%>%
  mutate(highlight01 = ifelse(languages_compare2$languages %in% c('English', 'Chinese', 'Korean', 'Hindi', 'Spanish'),"Yes","No"))


languages_compare2 %>%
  ggplot(aes(reorder(languages,sum_awards),sum_awards,fill = highlight01)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=sum_awards), vjust=-0.3, color="black",
            position = position_dodge(0.9), size=3.5)+
  xlab("Languages") +
  scale_y_continuous(name="Awards Received", labels = comma, limits = c(0, 30000))+
  scale_fill_manual(values = c("grey", "#993333"))+
  guides(fill = "none")+
  theme_fivethirtyeight()+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
  theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Total Awards Received by Languages")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
theme(axis.text.x=element_text(size=10, angle=45,hjust = 1))


################## Rotten Tomatoes Score with Genres, Actors, Directors and Languages

#####Rotten Tomatoes Values histogram

ggplot(data = netflix_data, aes(x = rotten_tomatoes_score, na.rm = TRUE)) + geom_histogram(bins = 101, fill = "#004d99", color = "white") +
  scale_x_continuous(labels = comma, breaks = scales::pretty_breaks(n = 20))+
  xlab("Rotten Tomatoes Score") +
  ylab("Count")+
  theme_fivethirtyeight()+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", face = "bold", hjust =1, size=14, angle = 45))+
  theme(axis.text.y = element_text(colour = "black", face = "bold", size=14))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Rotten Tomatoes Score Distribution")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

mean(netflix_data$rotten_tomatoes_score, na.rm = TRUE)
median(netflix_data$rotten_tomatoes_score, na.rm = TRUE)
sd(netflix_data$rotten_tomatoes_score, na.rm = TRUE)


mean(netflix_data$imdb_score, na.rm = TRUE)
################## Rotten Tomatoes Score with Genres, Actors, Directors and Languages

#Genres R-code with Rotten Tomatoes score

netfilx1 = netflix_data[!is.na(netflix_data$rotten_tomatoes_score) & !is.na(netflix_data$primary_g1),c("primary_g1","rotten_tomatoes_score")] 

#genres having more than 10 movies
genre_count = netfilx1 %>%
  count(primary_g1, sort = TRUE)
genre_count2 = genre_count[(genre_count$n >= 20),]
genre_count2

#genres to compare
genre_compare1=inner_join(netfilx1, genre_count2, by = c("primary_g1"="primary_g1"))

# To determine which genres are in the top 5 to add to the code later
# Differs from IMdb only with short, here it is drama instead of short

genre_compare1 %>% 
  group_by(primary_g1) %>%
  summarise(mean_rt = mean(rotten_tomatoes_score, na.rm = TRUE)) %>%
  arrange(-mean_rt) %>% 
  slice(1:5)

genre_compare1 = genre_compare1[genre_compare1$primary_g1 %in% c('Documentary','Biography', 'Drama', 'Crime', 'Animation'),]

genre_compare1 = genre_compare1 %>%
  mutate(highlight01 = ifelse(genre_compare1$primary_g1 %in% c('Documentary','Biography', 'Drama', 'Crime', 'Animation'),"Yes","No"))

genre_compare1 %>%
  ggplot(aes(reorder(primary_g1,rotten_tomatoes_score),rotten_tomatoes_score,fill = highlight01))+
  geom_boxplot() +
  stat_summary(fun.y="mean", shape=7, size = 0.2, color = 'white')+
  xlab("Genres") +
  ylab("Rotten Tomatoes Score")+
  scale_fill_manual(values = c("#1f3d7a"))+
  theme_fivethirtyeight()+
  guides(fill = FALSE)+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
  theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Rotten Tomatoes Mean Scores by Genres")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# Genre by RT Score - ANOVA
#### Significant
genre_compare1 %>% 
  group_by(primary_g1) %>%
  summarise(mean_rt = mean(rotten_tomatoes_score, na.rm = TRUE)) %>%
  arrange(-mean_rt) %>% 
  slice(1:5)

rt_genre_high.aov <- aov(rotten_tomatoes_score ~ primary_g1, data = genre_compare1[genre_compare1$primary_g1 %in% c('Documentary','Biography', 'Drama', 'Crime', 'Animation'),]) 
summary(rt_genre_high.aov)
TukeyHSD(rt_genre_high.aov)

#Genres R-code with IMDb score

netfilx1 = netflix_data[!is.na(netflix_data$rotten_tomatoes_score) & !is.na(netflix_data$primary_g1),c("primary_g1","rotten_tomatoes_score")] 

#genres having more than 10 movies
genre_count = netfilx1 %>%
  count(primary_g1, sort = TRUE)
genre_count2 = genre_count[(genre_count$n >= 20),]
genre_count2

#genres to compare
genre_compare1=inner_join(netfilx1, genre_count2, by = c("primary_g1"="primary_g1"))

# To determine which genres are in the top 5 to add to the code later
# Differs from IMdb only with short, here it is drama instead of short

genre_compare1 %>% 
  group_by(primary_g1) %>%
  summarise(mean_rt = mean(rotten_tomatoes_score, na.rm = TRUE)) %>%
  arrange(-mean_rt) %>% 
  slice(1:5)

genre_compare1 = genre_compare1[genre_compare1$primary_g1 %in% c('Documentary','Biography', 'Drama', 'Crime', 'Animation'),]

genre_compare1 = genre_compare1 %>%
  mutate(highlight01 = ifelse(genre_compare1$primary_g1 %in% c('Documentary','Biography', 'Drama', 'Crime', 'Animation'),"Yes","No"))

genre_compare1 %>%
  ggplot(aes(reorder(primary_g1,rotten_tomatoes_score),rotten_tomatoes_score,fill = highlight01))+
  geom_boxplot() +
  stat_summary(fun.y="mean", shape=7, size = 0.2, color = 'white')+
  xlab("Genres") +
  ylab("IMDb Score")+
  scale_fill_manual(values = c("#1f3d7a"))+
  theme_fivethirtyeight()+
  guides(fill = FALSE)+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
  theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Rotten Tomatoes Mean Scores by Genres")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# Genre by RT Score - ANOVA
#### Significant
genre_compare1 %>% 
  group_by(primary_g1) %>%
  summarise(mean_rt = mean(rotten_tomatoes_score, na.rm = TRUE)) %>%
  arrange(-mean_rt) %>% 
  slice(1:5)

rt_genre_high.aov <- aov(rotten_tomatoes_score ~ primary_g1, data = genre_compare1[genre_compare1$primary_g1 %in% c('Documentary','Biography', 'Drama', 'Crime', 'Animation'),]) 
summary(rt_genre_high.aov)
TukeyHSD(rt_genre_high.aov)

#Actors R-code with IMDb score

rt_actors1 = netflix_data[!is.na(netflix_data$rotten_tomatoes_score) & !is.na(netflix_data$actors),c("actors","rotten_tomatoes_score")]

#actors having more than 10 movies
actors_count = rt_actors1 %>%
  count(actors, sort = TRUE)
actors_count2 = actors_count[(actors_count$n >= 10),]
actors_count2

#actors having at least over 8 IMDb score one movie
actors_max_score = distinct(rt_actors1[(rt_actors1$rotten_tomatoes_score >= 60),], actors)
actors_max_score

#actors to compare
actors_compare1=inner_join(rt_actors1, actors_count2, by = c("actors"="actors"))
actors_compare2=inner_join(actors_compare1, actors_max_score, by = c("actors"="actors"))

# took 1:6, to account for missing value
actors_compare2 %>% 
  group_by(actors) %>%
  summarise(mean_rt = mean(rotten_tomatoes_score, na.rm = TRUE)) %>%
  arrange(-mean_rt) %>% 
  slice(1:6)

actors_compare2 = actors_compare2[actors_compare2$actors %in% c('Maggie Cheung','Priyanka Chopra', 'Nawazuddin Siddiqui', 'Emily Blunt', 'Jean-Paul Belmondo'),]

actors_compare2 = actors_compare2 %>%
  mutate(highlight01 = ifelse(actors_compare2$actors %in% c('Maggie Cheung','Priyanka Chopra', 'Nawazuddin Siddiqui', 'Emily Blunt', 'Jean-Paul Belmondo'),"Yes","No"))

actors_compare2 %>%
  ggplot(aes(reorder(actors,rotten_tomatoes_score), rotten_tomatoes_score,fill = highlight01))+
  geom_boxplot() +
  stat_summary(fun.y="mean", shape=17, size = 0.2, color = 'white')+
  xlab("Actors") +
  ylab("Rotten Tomatoes") +
  scale_fill_manual(values = c("#004d4d"))+
  theme_fivethirtyeight()+
  guides(fill = "none")+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
  theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Rotten Tomatoes Mean Scores by Actors")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# Actors by Rotten Tomato - ANOVA
#### insignificant
actors_compare2 %>% 
  group_by(actors) %>%
  summarise(mean_rt = mean(rotten_tomatoes_score, na.rm = TRUE)) %>%
  arrange(-mean_rt) %>% 
  slice(1:6)

rt_actor_high.aov <- aov(rotten_tomatoes_score ~ actors, data = actors_compare2[actors_compare2$actors %in% c('Maggie Cheung','Priyanka Chopra', 'Nawazuddin Siddiqui', 'Emily Blunt', 'Jean-Paul Belmondo'),]) 
summary(rt_actor_high.aov)
TukeyHSD(rt_actor_high.aov)


###Languages R-code with IMDb score

rt_lang1 = netflix_data[!is.na(netflix_data$rotten_tomatoes_score) & !is.na(netflix_data$languages),c("languages","rotten_tomatoes_score")] 

#genres having more than 20 movies
lang_count = rt_lang1 %>%
  count(languages, sort = TRUE)

lang_count2 = lang_count[(lang_count$n >= 50),]
lang_count2

#genres to compare
lang_compare1=inner_join(rt_lang1, lang_count2, by = c("languages"="languages"))

lang_compare1 %>% 
  group_by(languages) %>%
  summarise(mean_rt = mean(rotten_tomatoes_score, na.rm = TRUE)) %>%
  arrange(-mean_rt) %>% 
  slice(1:5)

lang_compare1 = lang_compare1[lang_compare1$languages %in% c('Japanese','Korean', 'Spanish', 'German', 'Chinese'),]

lang_compare1 = lang_compare1 %>%
  mutate(highlight01 = ifelse(lang_compare1$languages %in% c('Japanese','Korean', 'Spanish', 'German', 'Chinese'),"Yes","No"))

lang_compare1 %>%
  ggplot(aes(reorder(languages,rotten_tomatoes_score),rotten_tomatoes_score,fill = highlight01))+
  geom_boxplot() +
  stat_summary(fun.y="mean", shape=17, size = 0.2, color = 'white')+
  xlab("Languages") +
  ylab("Rotten Tomatoes")+
  scale_fill_manual(values = c("#993333"))+
  theme_fivethirtyeight()+
  guides(fill = FALSE)+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
  theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Rotten Tomatoes Mean Scores by Languages")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# Languages by Boxoffice - ANOVA
# Insignificant
lang_compare1 %>% 
  group_by(languages) %>%
  summarise(mean_rt = mean(rotten_tomatoes_score, na.rm = TRUE)) %>%
  arrange(-mean_rt) %>% 
  slice(1:5)

rt_lang_high.aov <- aov(rotten_tomatoes_score ~ languages, data = lang_compare1[lang_compare1$languages %in% c('Japanese','Korean', 'Spanish', 'German', 'Chinese'),]) 
summary(rt_lang_high.aov)
TukeyHSD(rt_lang_high.aov)

# p value greater than 0.05, ANOVA analysis is insignificant

###Directors R-code with Rotten tomatoes score
rt_dir1 = netflix_data[!is.na(netflix_data$rotten_tomatoes_score) & !is.na(netflix_data$director),c("director","rotten_tomatoes_score")]

#directors having more than 10 movies
dir_count = rt_dir1 %>%
  count(director, sort = TRUE)
dir_count2 = dir_count[(dir_count$n >= 10),]
dir_count2

#directors having at least over 8 IMDb score one movie
dir_max_score = distinct(rt_dir1[(rt_dir1$rotten_tomatoes_score >= 60),], director)
dir_max_score

#director to compare
dir_compare1=inner_join(rt_dir1, dir_count2, by = c("director"="director"))
dir_compare2=inner_join(dir_compare1, dir_max_score, by = c("director"="director"))

dir_compare2 %>% 
  group_by(director) %>%
  summarise(mean_rt = mean(rotten_tomatoes_score, na.rm = TRUE)) %>%
  arrange(-mean_rt) %>%
  slice(1:5)

dir_compare2 = dir_compare2[dir_compare2$director %in% c('Hayao Miyazaki','Martin Scorsese', 'Jay Karas', 'Steven Spielberg', 'François Truffaut'),]

dir_compare2 = dir_compare2 %>%
  mutate(highlight01 = ifelse(dir_compare2$director %in% c('Hayao Miyazaki','Martin Scorsese', 'Jay Karas', 'Steven Spielberg', 'François Truffaut'),"Yes","No"))

dir_compare2 %>%
  ggplot(aes(reorder(director,rotten_tomatoes_score), rotten_tomatoes_score,fill = highlight01))+
  geom_boxplot() +
  stat_summary(fun.y="mean", shape=17, size = 0.2, color = 'white')+
  xlab("Directors") +
  ylab("Rotten Tomatoes Score") +
  scale_fill_manual(values = c("#cc7a00"))+
  theme_fivethirtyeight()+
  guides(fill = "none")+
  theme(text = element_text(family = "Garamond", size = 14))+
  theme(axis.text.x = element_text(colour = "black", size=14, face = "bold", angle=45, hjust =1))+
  theme(axis.text.y = element_text(colour = "black", size=14, face = "bold"))+
  theme(axis.title = element_text(size = 20, face = "bold"))+
  labs(title = "Rotten Tomatoes Mean Scores by Directors")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# Directors by IMDb Score - ANOVA
##### INSIGNIFICANT

dir_compare2 %>% 
  group_by(director) %>%
  summarise(mean_rt = mean(rotten_tomatoes_score, na.rm = TRUE)) %>%
  arrange(-mean_rt) %>%
  slice(1:5)

rt_dir_high.aov <- aov(rotten_tomatoes_score ~ director, data = dir_compare2[dir_compare2$director %in% c('Hayao Miyazaki','Martin Scorsese', 'Jay Karas', 'Steven Spielberg', 'François Truffaut'),]) 
summary(rt_dir_high.aov)
TukeyHSD(rt_dir_high.aov)

# p value greater than 0.05, ANOVA analysis is insignificant

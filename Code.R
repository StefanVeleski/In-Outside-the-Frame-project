####Power law plots for Goodreads and Open Syllabus Data####
library(ggpubr)
library(tidyverse)
#Filtered dataset containing the Beetle and Dracula
highlight_df <- Main_dataset %>% 
    filter(Rank1== 30 | Rank1 ==2)
#Goodreads non log
options(scipen=10000)
df <- Main_dataset
A <- ggplot(df, aes(x=Rank1, y=Ratings)) +
    geom_point(alpha = 0.8, size = 3, color = '#00BA38') +
    geom_point(data=highlight_df,
               aes(x=Rank1, y=Ratings),
               color = '#FF0000',
               size = 4) +
    labs(title = "Distribution of present day popularity", 
         x = "Rank", 
         y ="Number of Ratings")

#Goodreads log
options(scipen=10000)

B <- ggplot(df, aes(x=Rank1, y=Ratings)) +
    geom_point(alpha = 0.8, size = 3, color = '#00BA38') +
    scale_x_log10()+
    geom_point(data=highlight_df,
               aes(x=Rank1, y=Ratings),
               color = '#FF0000',
               size = 4) +
    labs(title = "Distribution of present day popularity (log)", 
         x = "Rank (log)", 
         y ="Number of Ratings")

#Open Syllabus non log
options(scipen=10000)
C <- ggplot(df, aes(x=Rank2, y=Syllabi)) +
    geom_point(alpha = 0.8, size = 3, color = '#619CFF') +
    geom_point(data=highlight_df,
               aes(x=Rank2, y=Syllabi),
               color = '#FF0000',
               size = 4) +
    labs(title = "Distribution of critical prestige", 
         x = "Rank", 
         y ="Number of Open Syllabus entries")
#Open Syllabus log
D <- ggplot(df, aes(x=Rank2, y=Syllabi)) +
    geom_point(alpha = 0.8, size = 3, color = '#619CFF') +
    scale_x_log10()+
    geom_point(data=highlight_df,
               aes(x=Rank2, y=Syllabi),
               color = '#FF0000',
               size = 4) +
    labs(title = "Distribution of critical prestige (log)", 
         x = "Rank (log)", 
         y ="Number of Open Syllabus entries")
#Combining all of these together
ggarrange(A, B, C, D,
          labels = c("1", "2", "3", "4"),
          ncol = 2, nrow = 2)

####Scatterplot of film adaptations of Dracula and The Beetle with ggplot####
library(tidyverse)
ggplot(Dracula_adaptations, aes(x=Dracula_adaptations$Year, y=Dracula_adaptations$ImdB)) +
    geom_point(alpha = 0.5, size = 3, color = 'dimgray') +
    geom_rug(alpha = 1/2, position = "jitter") +
    labs(title = "Film adaptations of Dracula", 
         x = "Year", 
         y ="Number of IMDB Ratings")
####Scatterplot of film adaptations of Dracula and The Beetle with ggstatsplot####
library(ggstatsplot)

ggstatsplot::ggscatterstats(
    data = Dracula_adaptations,
    x = Year,
    y = ImdB,
    xlab = "Year", # label for x axis
    ylab = "Number of IMDB Ratings", # label for y axis
    label.var = Title, # variable for labeling data points
    label.expression = "ImdB > 20000 | Year < 1923", # expression that decides which points to label
    title = "Film adaptations of Dracula and the Beetle", # title text for the plot
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "density", # type of marginal distribution to be displayed
    xfill = "dimgray", # color fill for x-axis marginal distribution
    yfill = "dimgray" # color fill for y-axis marginal distribution
)

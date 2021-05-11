####Loading in the data####
library(tidyverse)
Dracula_adaptations <- read_csv("Datasets/Dracula adaptations.csv")
Main_dataset <- read_csv("Datasets/Main dataset.csv")

####Power law plots for Goodreads and Open Syllabus Data####
library(ggpubr)
library(ggrepel)
library(ggforce)
#Filtered dataset containing the Beetle and Dracula
highlight_df <- Main_dataset %>% 
    filter(Rank1== 30 | Rank1 ==2)

#Goodreads non log
options(scipen=10000)
A <- ggplot(Main_dataset, aes(x=Rank1, y=Ratings)) +
    geom_point(alpha = 0.8, size = 3, color = 'gray48') +
    geom_point(data=highlight_df,
               aes(x=Rank1, y=Ratings),
               color = 'gray20',
               size = 4) +
    labs(title = "Distribution of present day popularity", 
         x = "Rank", 
         y ="Number of Goodreads Ratings")

A <- A +  geom_label_repel(data = highlight_df,(aes(label = highlight_df$Title)),
                      box.padding   = 0.35, 
                      point.padding = 0.5,
                      segment.color = 'grey50') +
    theme_classic()

A

#Goodreads log
options(scipen=10000)

B <- ggplot(Main_dataset, aes(x=Rank1, y=Ratings)) +
    geom_point(alpha = 0.8, size = 3, color = 'gray48') +
    scale_x_log10()+
    geom_point(data=highlight_df,
               aes(x=Rank1, y=Ratings),
               color = 'gray20',
               size = 4) +
    labs(title = "Distribution of present day popularity (log)", 
         x = "Rank (log)", 
         y ="Number of Goodreads Ratings")
B <- B +  geom_label_repel(data = highlight_df,(aes(label = highlight_df$Title)),
                           box.padding   = 0.35, 
                           point.padding = 0.5,
                           segment.color = 'grey50') +
    theme_classic()

B
#Open Syllabus non log
options(scipen=10000)
C <- ggplot(Main_dataset, aes(x=Rank2, y=Syllabi)) +
    geom_point(alpha = 0.8, size = 3, color = 'gray58') +
    geom_point(data=highlight_df,
               aes(x=Rank2, y=Syllabi),
               color = 'gray20',
               size = 4) +
    labs(title = "Distribution of critical prestige", 
         x = "Rank", 
         y ="Number of Open Syllabus entries")
C <- C +  geom_label_repel(data = highlight_df,(aes(label = highlight_df$Title)),
                           box.padding   = 0.35, 
                           point.padding = 0.5,
                           segment.color = 'grey50') +
    theme_classic()

C
#Open Syllabus log
D <- ggplot(Main_dataset, aes(x=Rank2, y=Syllabi)) +
    geom_point(alpha = 0.8, size = 3, color = 'gray58') +
    scale_x_log10()+
    geom_point(data=highlight_df,
               aes(x=Rank2, y=Syllabi),
               color = 'gray20',
               size = 4) +
    labs(title = "Distribution of critical prestige (log)", 
         x = "Rank (log)", 
         y ="Number of Open Syllabus entries")

D <- D +  geom_label_repel(data = highlight_df,(aes(label = highlight_df$Title)),
                           box.padding   = 0.35, 
                           point.padding = 0.5,
                           segment.color = 'grey50') +
    theme_classic()

D
#Combining all of these together
composite_plot1 <- ggarrange(A, B, C, D,
          labels = c("1", "2", "3", "4"),
          ncol = 2, nrow = 2)

composite_plot2

#### The same visualization, but with ggforce####
options(scipen=10000)
E <- ggplot(Main_dataset, aes(x=Rank1, y=Ratings)) +
    geom_point(alpha = 0.8, size = 3, color = 'gray48') +
    geom_point(data=highlight_df,
               aes(x=Rank1, y=Ratings),
               color = 'gray20',
               size = 4) +
    labs(title = "Distribution of present day popularity", 
         x = "Rank", 
         y ="Number of Goodreads Ratings")+
    facet_zoom(xlim = c(1, 35))

E <- E +  geom_label_repel(data = highlight_df,(aes(label = highlight_df$Title)),
                           box.padding   = 0.35, 
                           point.padding = 0.5,
                           segment.color = 'grey50')

E

F <- ggplot(Main_dataset, aes(x=Rank2, y=Syllabi)) +
    geom_point(alpha = 0.8, size = 3, color = 'gray48') +
    geom_point(data=highlight_df,
               aes(x=Rank2, y=Syllabi),
               color = 'gray20',
               size = 4) +
    labs(title = "Distribution of critical prestige", 
         x = "Rank", 
         y ="Number of Open Syllabus entries")+
    facet_zoom(xlim = c(1, 35))

F <- F +  geom_label_repel(data = highlight_df,(aes(label = highlight_df$Title)),
                           box.padding   = 0.35, 
                           point.padding = 0.5,
                           segment.color = 'grey50')

F

composite_plot2 <- ggarrange(E,F, ncol = 2, nrow = 1)

composite_plot2


ggsave("plot1.emf",
       plot = composite_plot2, 
       device = emf, 
       dpi = "print",
       width = 8, height = 8)

####Scatterplot of film adaptations of Dracula and The Beetle with ggplot####
ggplot(Dracula_adaptations, aes(x=Dracula_adaptations$Year, y=Dracula_adaptations$ImdB)) +
    geom_point(alpha = 0.5, size = 3, color = 'dimgray') +
    geom_rug(alpha = 1/2, position = "jitter") +
    labs(title = "Film adaptations of Dracula", 
         x = "Year", 
         y ="Number of IMDB Ratings")

####Scatterplot of film adaptations of Dracula and The Beetle with ggstatsplot####
library(ggstatsplot)

ggscatterstats(
    data = Dracula_adaptations,
    x = Year,
    y = ImdB,
    xlab = "Year", # label for x axis
    ylab = "Number of IMDB Ratings", # label for y axis
    label.var = Title, # variable for labeling data points
    label.expression = "ImdB > 20000 | Year < 1923", # expression that decides which points to label
    title = "Film adaptations of Dracula and the Beetle", # title text for the plot
    results.subtitle = "FALSE",
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "densigram", # type of marginal distribution to be displayed
    xfill = "dimgray", # color fill for x-axis marginal distribution
    yfill = "dimgray" # color fill for y-axis marginal distribution
)
ggstatsplot

#### Another implementation, with ggplot not ggstatsplot####
library(ggExtra)
library(ggrepel)

plot_dracula <- ggplot(Dracula_adaptations, aes(x=Year, y=ImdB, shape = `Book?`)) +
    geom_point(alpha = 0.5, size = 3, color = 'dimgray') +
    labs(title = "Film adaptations of Dracula and the Beetle", 
         x = "Year", 
         y ="Number of IMDB Ratings") +
    theme(legend.title = element_blank(), 
          legend.position="bottom", 
          legend.direction = "horizontal") +
    geom_label_repel((aes(label = Dracula_adaptations$Title)),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50')

plot_dracula <- ggMarginal(plot_dracula, type="histogram")

plot_dracula


# Let's just label these items.
ix_label <- c(66,2,3,68,60,69)
Dracula_adaptations$Title[-ix_label] <- ""
Dracula_adaptations$Title[ix_label] <- rownames(Dracula_adaptation)[ix_label]

options(ggrepel.max.overlaps = Inf) 

plot2 <- ggplot(Dracula_adaptations, aes(Year, ImdB, shape = `Book?`, label = Title)) +
    geom_point(alpha = 0.5, size = 3, color = ifelse(Dracula_adaptations$Title == "", "gray48", "gray20"))+
    geom_label_repel(aes(label = Dracula_adaptations$Title),
    box.padding = 0.35, 
    point.padding = 0.5,
    segment.color = 'grey50') +
    theme(legend.title = element_blank(), 
          legend.position="bottom", 
          legend.direction = "horizontal")
plot2

plot2 <- ggMarginal(plot2, type="histogram")

plot2

ggsave("plot2.emf",
       plot = plot2, 
       device = emf, 
       dpi = "print",
       width = 9, height = 6)
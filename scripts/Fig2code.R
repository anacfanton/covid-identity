
setwd("/Users/kyraprats/Documents/Yale PhD/Writing Collab")

library(ggplot2)
library(viridis)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(dplyr)
library(ggpubr)
library(cowplot)

covid <- read.csv('CovidBarrierSummaryData.csv', header = TRUE)



# Subset based on the question numbers from the survey (27, 28, 31, 32), excluding NAs

covid27 <- covid[!is.na(covid$Count27), ]
covid2831 <- covid[!is.na(covid$Count28), ]
covid32 <- covid[!is.na(covid$Count32), ]


##### Figure 2 #####
# A
positions27 <- c("Yes", "No")
covid1 <-
  ggplot(covid27,
         aes(y = Count27, x = Answer27, fill = Answer27)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  coord_flip() +
  scale_fill_viridis_d(direction = -1) +
  geom_text(aes(label = Percent27), hjust = 0, size = 5) +
  scale_x_discrete(limits = positions27) +
  ylim(0, 200) +
  labs(x = " ", y = "Count") +
  ggtitle("Has COVID-19 impacted your writing habits?") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5), legend.position = "none", 
        panel.border = element_rect(color = "black", fill=NA, size=0.75), 
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
covid1
# B
positions28 <- c("Much less time", "Less time", "No change", "More time", "Much more time")
covid2 <-
  ggplot(covid2831,
         aes(y = Count28, x = Answer28, fill = Answer28)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  coord_flip() +
  scale_fill_viridis_d() +
  geom_text(aes(label = Percent28), hjust = 0, size = 5) +
  scale_x_discrete(labels = positions28) +
  ylim(0, 200) +
  labs(x = " ", y = "Count") +
  ggtitle("Has COVID-19 impacted the amount of time you have for writing?") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5), legend.position = "none", 
        panel.border = element_rect(color = "black", fill=NA, size=0.75), 
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
covid2
# C
positions29 <- c("Much less productive", "Less productive", "No change", 
                 "More productive", "Much more productive")
covid3 <-
  ggplot(covid2831,
         aes(y = Count29, x = Answer29, fill = Answer29)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  coord_flip() +
  scale_fill_viridis_d() +
  geom_text(aes(label = Percent29), hjust = 0, size = 5) +
  scale_x_discrete(labels = positions29) +
  ylim(0, 200) +
  labs(x = " ", y = "Count") +
  ggtitle("Has COVID-19 affected your writing productivity?") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5), legend.position = "none", 
        panel.border = element_rect(color = "black", fill=NA, size=0.75), 
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
covid3
# D
positions30 <- c("Much less motivated", "Less motivated", "No change", 
                 "More motivated", "Much more motivated")
covid4 <-
  ggplot(covid2831,
         aes(y = Count30, x = Answer30, fill = Answer30)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  coord_flip() +
  scale_fill_viridis_d() +
  geom_text(aes(label = Percent30), hjust = 0, size = 5) +
  scale_x_discrete(labels = positions30) +
  ylim(0, 200) +
  labs(x = " ", y = "Count") +
  ggtitle("Has COVID-19 affected your motivation to write?") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5), legend.position = "none", 
        panel.border = element_rect(color = "black", fill=NA, size=0.75), 
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
covid4

CovidFig2 <- plot_grid(covid1, covid2, covid3, covid4,
                       labels = c("A", "B", "C", "D"), label_size = 14, vjust = 1, align = "v",
                       ncol = 1, nrow = 4)
CovidFig2
ggsave(CovidFig2, plot = CovidFig2, device = "pdf",
       scale = 1, width = 10, height = 10, units = c("in"),
       dpi = 600, limitsize = TRUE)

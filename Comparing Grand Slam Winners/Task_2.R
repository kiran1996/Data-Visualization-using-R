library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(plot.matrix)
require(scales)
library(ggthemes)
library(ggrepel)
library(ggExtra)
library(gridExtra)

#Task-2 - Figure-1
grand_slam <- read.csv("grand_slam_data.csv")
grand_slam[160,2] <- "Australian Open"
grand_slam[164,2] <- "Australian Open"

Num_wins <- grand_slam %>%
  group_by(winner,tournament) %>%
  summarise(Num_wins = n()) #%>%
#pivot_wider(names_from = tournament, values_from = Num_wins, values_fill=0)

Max_wins_tournament <- Num_wins %>% group_by(tournament) %>% top_n(1,Num_wins)
Num_wins$label <- ifelse(Num_wins$Num_wins %in% Max_wins_tournament$Num_wins & Num_wins$winner %in% Max_wins_tournament$winner,1,0)

Num_wins %>%
  #group_by(tournament) %>%
  filter(Num_wins > 2) %>%
  ggplot(aes(x=Num_wins,y=winner, color = label)) +
  geom_segment(aes(x=0, y=winner, xend = Num_wins, yend = winner)) +
  geom_point() + 
  scale_color_gradient(low = "black", high = "red") +
  ggtitle("Comparison of winners of four grand slam tournament") +
  facet_grid(. ~ tournament)

#Task - 2 - Figure - 2
Recent_wins <- grand_slam[1:40,2:4]
Aus_open_wins <- Recent_wins %>%
                  filter(tournament == "Australian Open")
Aus_open_wins <- Aus_open_wins[,-1]
French_open_wins <- Recent_wins %>%
  filter(tournament == "French Open")
French_open_wins <- French_open_wins[,-1]
U.S._open_wins <- Recent_wins %>%
  filter(tournament == "U.S. Open")
U.S._open_wins <- U.S._open_wins[,-1]
Wimbledon_wins <- Recent_wins %>%
  filter(tournament == "Wimbledon")
Wimbledon_wins <- Wimbledon_wins[,-1]

#Australian Open matrix
winner_runnerup_names <- unique(c(Aus_open_wins[,1], Aus_open_wins[,2]))

match_matrix <- matrix(0, nrow=length(winner_runnerup_names),
                       ncol=length(winner_runnerup_names))
colnames(match_matrix) <- winner_runnerup_names
rownames(match_matrix) <- winner_runnerup_names

for(i in 1:nrow(Recent_wins)) {
  winner <- Aus_open_wins[i, "winner"]
  runner_up <- Aus_open_wins[i, "runner_up"]
  row <- which(rownames(match_matrix) == winner)
  column <- which(colnames(match_matrix) == runner_up)
  
  match_matrix[row,column] <- match_matrix[row,column] + 1
  match_matrix[column,row] <- match_matrix[column,row] + 1
}

diag(match_matrix) <- 0
par(mfrow = c(3,4))
dev.off()
plot(match_matrix, col = topo.colors,
      main = "matrix of how many times they met each other on Australian Open", xlab = "", ylab = "",
      cex.axis = 0.55, las = 2)

#French Open matrix
winner_runnerup_names <- unique(c(French_open_wins[,1], French_open_wins[,2]))

match_matrix <- matrix(0, nrow=length(winner_runnerup_names),
                       ncol=length(winner_runnerup_names))
colnames(match_matrix) <- winner_runnerup_names
rownames(match_matrix) <- winner_runnerup_names

for(i in 1:nrow(Recent_wins)) {
  winner <- French_open_wins[i, "winner"]
  runner_up <- French_open_wins[i, "runner_up"]
  row <- which(rownames(match_matrix) == winner)
  column <- which(colnames(match_matrix) == runner_up)
  
  match_matrix[row,column] <- match_matrix[row,column] + 1
  match_matrix[column,row] <- match_matrix[column,row] + 1
}

diag(match_matrix) <- 0
par(mfrow = c(3,4))
dev.off()
plot(match_matrix, col = topo.colors,
           main = "matrix of how many times they met each other on Australian Open", xlab = "", ylab = "",
           cex.axis = 0.55, las = 2)

#U.S. Open matrix
winner_runnerup_names <- unique(c(U.S._open_wins[,1], U.S._open_wins[,2]))

match_matrix <- matrix(0, nrow=length(winner_runnerup_names),
                       ncol=length(winner_runnerup_names))
colnames(match_matrix) <- winner_runnerup_names
rownames(match_matrix) <- winner_runnerup_names

for(i in 1:nrow(Recent_wins)) {
  winner <- U.S._open_wins[i, "winner"]
  runner_up <- U.S._open_wins[i, "runner_up"]
  row <- which(rownames(match_matrix) == winner)
  column <- which(colnames(match_matrix) == runner_up)
  
  match_matrix[row,column] <- match_matrix[row,column] + 1
  match_matrix[column,row] <- match_matrix[column,row] + 1
}

diag(match_matrix) <- 0
par(mfrow = c(3,4))
dev.off()
plot(match_matrix, col = topo.colors,
           main = "matrix of how many times they met each other on Australian Open", xlab = "", ylab = "",
           cex.axis = 0.55, las = 2)

#Wimbledon matrix
winner_runnerup_names <- unique(c(Wimbledon_wins[,1], Wimbledon_wins[,2]))

match_matrix <- matrix(0, nrow=length(winner_runnerup_names),
                       ncol=length(winner_runnerup_names))
colnames(match_matrix) <- winner_runnerup_names
rownames(match_matrix) <- winner_runnerup_names

for(i in 1:nrow(Recent_wins)) {
  winner <- Wimbledon_wins[i, "winner"]
  runner_up <- Wimbledon_wins[i, "runner_up"]
  row <- which(rownames(match_matrix) == winner)
  column <- which(colnames(match_matrix) == runner_up)
  
  match_matrix[row,column] <- match_matrix[row,column] + 1
  match_matrix[column,row] <- match_matrix[column,row] + 1
}

diag(match_matrix) <- 0
par(mfrow = c(3,4))
dev.off()
plot(match_matrix, col = topo.colors,
           main = "matrix of how many times they met each other on Australian Open", xlab = "", ylab = "",
           cex.axis = 0.55, las = 2)


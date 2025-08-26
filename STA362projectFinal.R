
# this project was done in collaboration with Logan Galiotos and was done as
# part of the final project in clustering for the course Mutlivariate Statistics

#These are all the libraries I have loaded in order to run the code
library(dplyr)
library(tidyverse)
library(Matrix)
library(ggforce)
library(gridExtra)
library(psych)
library(mvtnorm)
library(plotly)
library(HSAUR2)
library(GGally)
library(MASS)
library(vegan)
library(ggnetwork)
library(ggraph)
library(igraph)
library(ggrepel)
library(ggdendro)
library(mclust)
library(ggbiplot)
#additional libraries loaded:
library(knitr)

# https://www.kaggle.com/datasets/vivovinco/2023-2024-nba-player-stats

#loading the csv into R
nba <- read_csv2("Sta 362/2023-2024 NBA Player Stats - Regular.csv")

ncol(nba)
nrow(nba)

#Removes double entry of players on multiple teams during the season.
nba_clean <- nba %>%
  group_by(Player) %>%
  # If "TOT" exists for the Player, keep that row; otherwise keep the first row
  filter(
    if (any(Tm == "TOT")) {
      Tm == "TOT"
    } else {
      row_number() == 1
    }
  ) %>%
  ungroup()

nrow(nba_clean)

# numeric columns not listed as numeric
# subset of numeric columns
nba_subset <- nba_clean[, -c(1, 2, 3, 5)]

# Convert each column in the subset to numeric
nba_numeric <- as.data.frame(
  lapply(nba_subset, function(x) as.numeric(as.character(x)))
)

# Now nba_numeric is a data frame where each column is numeric
str(nba_numeric)

#naming each row by player name
nba_subset2 <- nba_numeric
rownames(nba_subset2) <- nba_clean$Player

# Renaming columns to be more clear

colnames(nba_subset2) <- c("Age" , "Games.Played", "Games.Started", "Avg.Minutes.Played", 
                           "Avg.Field.Goals", "Avg.FG.Attempts", "Field.Goal.%","Avg.3-Pointers",
                           "Avg.3-Point.Attempts","3-Point.%","Avg.2-Pointers","Avg.2-Point.Attempts",
                           "2-Point.%","Effective.FG.%","Avg.Free.Throws.Per.Game",
                           "Avg.Free.Throw.Attempts","Free.Throw.%","Avg.Offensive.Rebounds",
                           "Avg.Defensive.Rebounds","Total.Rebounds.Per.Game","Avg.Assists.Per.Game",
                           "Avg.Steals.Per.Game","Avg.Blocks.Per.Game","Avg.Turnovers.Per.Game",
                           "Avg.Fouls.Per.Game","Avg.Points.Per.Game")

# removing columns that aren't useful to the analysis: the numbers of these columns are already
# captured by the associated columns that show shot percentage
nba_subset3 <- nba_subset2[,-c(5,6,8,9,11,12,15,16)]

# We are scaling the data because we have columns on very different scales, such as age
# and field goal percentage. 
snba <- scale(nba_subset3)


# Agglomerative With Scaled Data

#Chose to do complete linkage

hclust(dist(snba), method = "complete")$height

hclust(dist(snba), method = "complete")$merge

sclust_complete <- hclust(dist(snba, method = "euclidean"), method = "complete")

# Order presented on x-axis
sclust_complete$order

# Merging operations performed in clustering
sclust_complete$merge

# Height (linkage) at each merging operation
sclust_complete$height

# Observation names
sclust_complete$labels

#The dendrogram
ggdendrogram(sclust_complete) +
  theme(panel.grid.major.y = element_line(color = "gray"),axis.text.x = element_blank()) + ggtitle("Figure 1")

#cut so that we get 5 clusters

# Where do we make the cuts?
ggdendrogram(sclust_complete) +
  theme(panel.grid.major.y = element_line(color = "gray"),axis.text.x = element_blank()) +
  annotate("segment", x = -Inf, xend = Inf, y = 11, yend = 11, color = "red") + ggtitle("Figure 2")

unique(cutree(sclust_complete, h = 11))

sclusters <- cutree(sclust_complete, h = 11)

table(sclusters)

scluster_df <- data.frame(Observation = names(sclusters),Cluster=sclusters)


#Allows us to look at features from only players in cluster 1
cluster1_obs <- scluster_df[scluster_df$Cluster == 1, "Observation" ]

cluster1_data <- nba_subset3[rownames(nba_subset3) %in% cluster1_obs, ]

nrow(cluster1_data)

#Allows us to look at features from only players in cluster 2
cluster2_obs <- scluster_df[scluster_df$Cluster == 2, "Observation" ]

cluster2_data <- nba_subset3[rownames(nba_subset3) %in% cluster2_obs, ]

nrow(cluster2_data)

#Allows us to look at features from only players in cluster 3
cluster3_obs <- scluster_df[scluster_df$Cluster == 3, "Observation" ]

cluster3_data <- nba_subset3[rownames(nba_subset3) %in% cluster3_obs, ]

nrow(cluster3_data)

#Allows us to look at features from only players in cluster 4
cluster4_obs <- scluster_df[scluster_df$Cluster == 4, "Observation" ]

cluster4_data <- nba_subset3[rownames(nba_subset3) %in% cluster4_obs, ]

nrow(cluster4_data)

#Allows us to look at features from only players in cluster 5
cluster5_obs <- scluster_df[scluster_df$Cluster == 5, "Observation" ]

cluster5_data <- nba_subset3[rownames(nba_subset3) %in% cluster5_obs, ]

nrow(cluster5_data)

ncol(cluster1_data)

Five_cluster_structure <- data.frame(Cluster=1:5,Age=rep(NA,5),Games.Played=rep(NA,5),
                                     Games.Started=rep(NA,5),Avg.Minutes.Played=rep(NA,5),
                                     Field.Goal.Percent=rep(NA,5),`3Point.Percent`=rep(NA,5),`2Point.Percent`=rep(NA,5),
                                     Effective.FG.Percent=rep(NA,5),Free.Throw.Percent=rep(NA,5),
                                     Avg.Offensive.Rebound=rep(NA,5),Avg.Defensive.Rebound=rep(NA,5),Rebounds.Per.Game=rep(NA,5),
                                     Avg.Assists.Per.Game=rep(NA,5),Avg.Steals.Per.Game=rep(NA,5),Avg.Blocks.Per.Game=rep(NA,5),
                                     Avg.Turnovers.Per.Game=rep(NA,5),Avg.Fouls.Per.Game=rep(NA,5),Avg.Points.Per.Game=rep(NA,5))







for (i in 2:19){
  Five_cluster_structure[1,i]<-mean(cluster1_data[,i-1])
}

for (i in 2:19){
  Five_cluster_structure[2,i]<-mean(cluster2_data[,i-1])
}

for (i in 2:19){
  Five_cluster_structure[3,i]<-mean(cluster3_data[,i-1])
}

for (i in 2:19){
  Five_cluster_structure[4,i]<-mean(cluster4_data[,i-1])
}

for (i in 2:19){
  Five_cluster_structure[5,i]<-mean(cluster5_data[,i-1])
}

library(xtable)

fivedf <- t(Five_cluster_structure)

xtable(fivedf)

nrow_df <- data.frame(Cluster=1:5, Rows=c(465,88,19,19,55))

# creates a nice table for displaying data
xtable(nrow_df)

#Now, we will re-examine the data using only three clusters


# Where do we make the cuts?
ggdendrogram(sclust_complete) +
  theme(panel.grid.major.y = element_line(color = "gray"),axis.text.x = element_blank()) +
  annotate("segment", x = -Inf, xend = Inf, y = 12, yend = 12, color = "red") +ggtitle("Figure 3")

unique(cutree(sclust_complete, h = 12))

sclusters <- cutree(sclust_complete, h = 12)

table(sclusters)

scluster_df <- data.frame(Observation = names(sclusters),Cluster=sclusters)


#Allows us to look at features from only players in cluster 1
cluster1_obs <- scluster_df[scluster_df$Cluster == 1, "Observation" ]

cluster1_data <- nba_subset3[rownames(nba_subset3) %in% cluster1_obs, ]

nrow(cluster1_data)

#Allows us to look at features from only players in cluster 2
cluster2_obs <- scluster_df[scluster_df$Cluster == 2, "Observation" ]

cluster2_data <- nba_subset3[rownames(nba_subset3) %in% cluster2_obs, ]

nrow(cluster2_data)

#Allows us to look at features from only players in cluster 3
cluster3_obs <- scluster_df[scluster_df$Cluster == 3, "Observation" ]

cluster3_data <- nba_subset3[rownames(nba_subset3) %in% cluster3_obs, ]

nrow(cluster3_data)


Three_cluster_structure <- data.frame(Cluster=1:3,Age=rep(NA,3),Games.Played=rep(NA,3),
                                     Games.Started=rep(NA,3),Avg.Minutes.Played=rep(NA,3),
                                     Field.Goal.Percent=rep(NA,3),`3Point.Percent`=rep(NA,3),`2Point.Percent`=rep(NA,3),
                                     Effective.FG.Percent=rep(NA,3),Free.Throw.Percent=rep(NA,3),
                                     Avg.Offensive.Rebound=rep(NA,3),Avg.Defensive.Rebound=rep(NA,3),Rebounds.Per.Game=rep(NA,3),
                                     Avg.Assists.Per.Game=rep(NA,3),Avg.Steals.Per.Game=rep(NA,3),Avg.Blocks.Per.Game=rep(NA,3),
                                     Avg.Turnovers.Per.Game=rep(NA,3),Avg.Fouls.Per.Game=rep(NA,3),Avg.Points.Per.Game=rep(NA,3))





for (i in 2:19){
  Three_cluster_structure[1,i]<-mean(cluster1_data[,i-1])
}

for (i in 2:19){
  Three_cluster_structure[2,i]<-mean(cluster2_data[,i-1])
}

for (i in 2:19){
  Three_cluster_structure[3,i]<-mean(cluster3_data[,i-1])
}

threedf <- t(Three_cluster_structure)

xtable(threedf)


# K-Means Clustering



#getting the WGSS for each k
kmX <- data.frame(K = 1:4, WGSS = sapply(1:4, function(z) kmeans(nba_subset3, centers = z)$tot.withinss))

plot(kmX)


k1 <- kmeans(nba_subset3, centers = 1)
k2 <- kmeans(nba_subset3, centers = 2)
k3 <- kmeans(nba_subset3, centers = 3)
k4 <- kmeans(nba_subset3, centers = 4)
k5 <- kmeans(nba_subset3, centers = 5)

spca <- prcomp(nba_subset3, scale = TRUE)
print(spca$rotation)

spca$x

ggplot() +
  geom_line(aes(x = 1:length(spca$sdev), y = spca$sdev^2 / sum(spca$sdev^2))) +
  geom_point(aes(x = 1:length(spca$sdev), y = spca$sdev^2 / sum(spca$sdev^2)), size = 2) +
  theme_bw() +
  labs(x = "Principal Component", y = "Variance Explained", title="Figure 4")


# Plotting WGSS against number of clusters
wgss <- data.frame(K = 1:10, WGSS = sapply(1:10, function(z) kmeans(nba_subset3, centers = z)$tot.withinss))
ggplot(wgss) +
  geom_point(aes(x = K, y = WGSS)) +
  geom_line(aes(x = K, y = WGSS)) +
  labs(title="Figure 5") +
  theme_minimal()

# K-Means 3 clusters

kmeanscluster_df <- data.frame(
  Observation = rownames(spca$x),        
  PC1 = spca$x[,1],
  PC2 = spca$x[,2],
  Cluster = factor(k3$cluster)
)

spca$x

ggplot() +
  geom_text(aes(x = spca$x[,1], y = spca$x[,2], color = factor(k3$cluster), label = nba_clean$Pos)) +
  geom_point(aes(x = k3$centers %*% spca$rotation[,1], y = k3$centers %*% spca$rotation[,2], color = factor(1:3))) +
  scale_color_discrete("Cluster") +
  labs(x = "Coordinate 1", y = "Coordinate 2") +
  theme_minimal()

#Option without centers:

## Principal components 1 and 2

#added position label here
ggplot() +
  geom_text(aes(x = spca$x[,1], y = spca$x[,2], color = factor(k3$cluster), label = nba_clean$Pos)) +
  scale_color_discrete("Cluster") +
  labs(x = "PC1", y = "PC2") +
  theme_minimal()

#just points
ggplot() +
  geom_point(aes(x = spca$x[,1], y = spca$x[,2], color = factor(k3$cluster)), alpha = 0.7) +
  scale_color_discrete("Cluster") +
  labs(x = "PC1", y = "PC2") +
  theme_minimal()


ggplot_pca(spca, c(1,2), points_size = 1, points_alpha = 0.001) +
  geom_point(aes(x = spca$x[,1]/3, y = spca$x[,2]/3, color = factor(k3$cluster)), alpha = 0.7) +
  scale_color_discrete("Cluster") +
  labs(x = "PC1", y = "PC2",title="Figure 6") +
  theme_minimal()

## Principal components 1 and 3

ggplot() +
  geom_point(aes(x = spca$x[,1], y = spca$x[,3], color = factor(k3$cluster)), alpha = 0.7) +
  scale_color_discrete("Cluster") +
  labs(x = "PC1", y = "PC3") +
  theme_minimal()


ggplot_pca(spca, c(1,3), points_size = 1, points_alpha = 0.001) +
  geom_point(aes(x = spca$x[,1]/3, y = spca$x[,3]/3, color = factor(k3$cluster)), alpha = 0.7) +
  scale_color_discrete("Cluster") +
  labs(x = "PC1", y = "PC3") +
  theme_minimal()

## Principal components 2 and 3

ggplot() +
  geom_point(aes(x = spca$x[,2], y = spca$x[,3], color = factor(k3$cluster)), alpha = 0.7) +
  scale_color_discrete("Cluster") +
  labs(x = "PC2", y = "PC3") +
  theme_minimal()


ggplot_pca(spca, c(2,3), points_size = 1, points_alpha = 0.001) +
  geom_point(aes(x = spca$x[,2]/3, y = spca$x[,3]/3, color = factor(k3$cluster)), alpha = 0.7) +
  scale_color_discrete("Cluster") +
  labs(x = "PC2", y = "PC3") +
  theme_minimal()


# Interpret arrows and do 2 other graphs for three dimensions 

#K-means 5 clusters

## Principal components 1 and 2
##pos
ggplot() +
  geom_text(aes(x = spca$x[,1], y = spca$x[,2], color = factor(k5$cluster), label = nba_clean$Pos)) +
  scale_color_discrete("Cluster") +
  labs(x = "PC1", y = "PC2") +
  theme_minimal()


##points
ggplot() +
  geom_point(aes(x = spca$x[,1], y = spca$x[,2], color = factor(k5$cluster)), alpha = 0.7) +
  scale_color_discrete("Cluster") +
  labs(x = "PC1", y = "PC2") +
  theme_minimal()


ggplot_pca(spca, c(1,2), points_size = 1, points_alpha = 0.001) +
  geom_point(aes(x = spca$x[,1]/3, y = spca$x[,2]/3, color = factor(k5$cluster)), alpha = 0.7) +
  scale_color_discrete("Cluster") +
  labs(x = "PC1", y = "PC2") +
  theme_minimal()

## Principal components 1 and 3

ggplot() +
  geom_point(aes(x = spca$x[,1], y = spca$x[,3], color = factor(k5$cluster)), alpha = 0.7) +
  scale_color_discrete("Cluster") +
  labs(x = "PC1", y = "PC3") +
  theme_minimal()


ggplot_pca(spca, c(1,3), points_size = 1, points_alpha = 0.001) +
  geom_point(aes(x = spca$x[,1]/3, y = spca$x[,3]/3, color = factor(k5$cluster)), alpha = 0.7) +
  scale_color_discrete("Cluster") +
  labs(x = "PC1", y = "PC3") +
  theme_minimal()

## Principal components 2 and 3

ggplot() +
  geom_point(aes(x = spca$x[,2], y = spca$x[,3], color = factor(k5$cluster)), alpha = 0.7) +
  scale_color_discrete("Cluster") +
  labs(x = "PC2", y = "PC3") +
  theme_minimal()


ggplot_pca(spca, c(2,3), points_size = 1, points_alpha = 0.001) +
  geom_point(aes(x = spca$x[,2]/3, y = spca$x[,3]/3, color = factor(k5$cluster)), alpha = 0.7) +
  scale_color_discrete("Cluster") +
  labs(x = "PC2", y = "PC3") +
  theme_minimal()



{
  gah_k5_points <- ggbiplot(spca, choices=1:2, obs.scale=1, var.scale=1,
                groups=factor(k5$cluster), var.axes=TRUE) +
    scale_color_discrete("Cluster");
  for(i in which(sapply(g$layers, function(l) inherits(l$geom, "GeomSegment"))))
    g$layers[[i]]$aes_params$alpha <- 0.25;
  print(gah_k5_points)
  }

{

  # Compute percent variance explained for each PC
  def_ex_var <- function(pca) {
    var <- pca$sdev^2 / sum(pca$sdev^2)
    return(var)
  }
  var_exp <- def_ex_var(spca)
  
  # Helper: produces a biplot with semi-transparent arrows and axis labels including % variance
  faded_biplot <- function(pca,
                           clusters,
                           dims            = 1:2,
                           alpha           = 0.25,
                           title           = NULL,
                           label_positions = FALSE) {
    # base ggbiplot
    g <- ggbiplot(pca,
                  choices    = dims,
                  obs.scale  = 1,
                  var.scale  = 1,
                  groups     = factor(clusters),
                  var.axes   = TRUE) +
      scale_color_discrete("Cluster") +
      labs(
        x     = paste0("PC", dims[1], " (", round(var_exp[dims[1]]*100,1), "%)"),
        y     = paste0("PC", dims[2], " (", round(var_exp[dims[2]]*100,1), "%)"),
        title = title
      ) +
      theme_minimal()
    
    # fade the arrow layer(s)
    segs <- which(sapply(g$layers, function(l) inherits(l$geom, "GeomSegment")))
    for(i in segs) {
      g$layers[[i]]$aes_params$alpha <- alpha
    }
    
    if(label_positions) {
      # hide point layer(s)
      pts <- which(sapply(g$layers, function(l) inherits(l$geom, "GeomPoint")))
      for(i in pts) g$layers[[i]]$aes_params$alpha <- 0
      # add text labels for positions
      scores <- as.data.frame(pca$x[, dims])
      colnames(scores) <- c("PC1","PC2")
      scores$label   <- nba_clean$Pos
      scores$cluster <- factor(clusters)
      g <- g +
        geom_text(
          data    = scores,
          aes(x = PC1, y = PC2, label = label, color = cluster),
          size  = 3, vjust = -0.5
        )
    }
    return(g)
  }
  
  # Cluster assignments
  clusters_h5 <- cutree(sclust_complete, h = 11)
  clusters_h3 <- cutree(sclust_complete, h = 12)
  clusters_k3 <- k3$cluster
  clusters_k5 <- k5$cluster
  
  # PC pairs to plot
  pc_pairs <- list(c(1,2), c(1,3), c(2,3))
  
  # Generate grids: Points & Labels for each method
  # Hierarchical k=5
  hc5_pts  <- lapply(pc_pairs, function(d)
    faded_biplot(spca, clusters_h5, dims = d, title = paste0("HC k=5: PC", d[1]," vs PC", d[2])))
  hc5_lbls <- lapply(pc_pairs, function(d)
    faded_biplot(spca, clusters_h5, dims = d, title = paste0("HC k=5: PC", d[1]," vs PC", d[2]), label_positions = TRUE))
  # Hierarchical k=3
  hc3_pts  <- lapply(pc_pairs, function(d)
    faded_biplot(spca, clusters_h3, dims = d, title = paste0("HC k=3: PC", d[1]," vs PC", d[2])))
  hc3_lbls <- lapply(pc_pairs, function(d)
    faded_biplot(spca, clusters_h3, dims = d, title = paste0("HC k=3: PC", d[1]," vs PC", d[2]), label_positions = TRUE))
  # K-means k=3
  k3_pts   <- lapply(pc_pairs, function(d)
    faded_biplot(spca, clusters_k3, dims = d, title = paste0("K-means k=3: PC", d[1]," vs PC", d[2])))
  k3_lbls  <- lapply(pc_pairs, function(d)
    faded_biplot(spca, clusters_k3, dims = d, title = paste0("K-means k=3: PC", d[1]," vs PC", d[2]), label_positions = TRUE))
  # K-means k=5
  k5_pts   <- lapply(pc_pairs, function(d)
    faded_biplot(spca, clusters_k5, dims = d, title = paste0("K-means k=5: PC", d[1]," vs PC", d[2])))
  k5_lbls  <- lapply(pc_pairs, function(d)
    faded_biplot(spca, clusters_k5, dims = d, title = paste0("K-means k=5: PC", d[1]," vs PC", d[2]), label_positions = TRUE))
  
  # Plot in 2x2 grids: Points
  grid.arrange(grobs = hc5_pts, ncol = 2)
  grid.arrange(grobs = hc3_pts, ncol = 2)
  grid.arrange(grobs = k3_pts,  ncol = 2)
  grid.arrange(grobs = k5_pts,  ncol = 2)
  # Plot in 2x2 grids: Labels
  grid.arrange(grobs = hc5_lbls, ncol = 2)
  grid.arrange(grobs = hc3_lbls, ncol = 2)
  grid.arrange(grobs = k3_lbls,  ncol = 2)
  grid.arrange(grobs = k5_lbls,  ncol = 2)
  
}

{pdf("all_biplots.pdf", width = 10, height = 8)

# point‐based grids
grid.arrange(grobs = hc5_pts,  ncol = 2)
grid.arrange(grobs = hc3_pts,  ncol = 2)
grid.arrange(grobs = k3_pts,   ncol = 2)
grid.arrange(grobs = k5_pts,   ncol = 2)

# label‐based grids
grid.arrange(grobs = hc5_lbls, ncol = 2)
grid.arrange(grobs = hc3_lbls, ncol = 2)
grid.arrange(grobs = k3_lbls,  ncol = 2)
grid.arrange(grobs = k5_lbls,  ncol = 2)

dev.off()
}

{# all eight lists of plots:
all_plots <- c(
  hc5_pts,   # 3 plots
  hc5_lbls,  # 3 plots
  hc3_pts,   # 3 plots
  hc3_lbls,  # 3 plots
  k3_pts,    # 3 plots
  k3_lbls,   # 3 plots
  k5_pts,    # 3 plots
  k5_lbls    # 3 plots
)

pdf("all_biplots_each_page.pdf", width = 10, height = 8)
for(p in all_plots) {
  print(p)    # each print() call creates a new page
}
dev.off()}







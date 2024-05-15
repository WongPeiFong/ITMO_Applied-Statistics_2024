library(vegan)
data(BCI)
data(BCI.env)
data1 = as.data.frame(BCI)
tree_counts = data.frame(data1)
colnames(tree_counts) <- rownames(tree_counts)
rownames(tree_counts) <- 1:nrow(tree_counts)
tree_counts <- as.data.frame(sapply(tree_counts, as.numeric))
str(tree_counts)
summary(tree_counts)
species_richness <- apply(tree_counts, 1, function(x) sum(x > 0))
plot(sort(species_richness), type = "l", xlab = "Number of Plots", ylab = "Species Richness",
     main = "Species Accumulation Curve")
hist(tree_counts[,1], xlab = "Tree Counts", ylab = "Frequency", main = "Histogram of Tree Counts")
plot(BCI.env$UTM.EW, BCI.env$UTM.NS, xlab = "UTM East-West", ylab = "UTM North-South",
     main = "Spatial Distribution of Plots")
text(BCI.env$UTM.EW, BCI.env$UTM.NS, labels = rownames(tree_counts), cex = 0.7)
plot(BCI.env$Precipitation, species_richness, xlab = "Precipitation (mm/year)",
     ylab = "Species Richness", main = "Species Richness vs. Precipitation")
pca_result <- prcomp(tree_counts, scale. = TRUE)
summary(pca_result)

biplot(pca_result, scale = 0)


plot(pca_result, type = "l", main = "Scree Plot of PCA")


plot(pca_result$x[,1], pca_result$x[,2], xlab = "PC1", ylab = "PC2", 
     main = "PCA Scores Plot")
data_df <- as.data.frame(BCI)

env_df <- as.data.frame(BCI.env)


pca_result <- prcomp(data_df, scale. = TRUE)

par(mfrow=c(2, 2))

plot(pca_result$x[,1], pca_result$x[,2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA Scores: PC1 vs. PC2 with Precipitation",
     col = env_df$Precipitation, pch = 20)

legend("topright", legend = "Precipitation", fill = topo.colors(length(unique(env_df$Precipitation))))


plot(pca_result$x[,1], pca_result$x[,2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA Scores: PC1 vs. PC2 with Elevation",
     col = env_df$Elevation, pch = 20)


legend("topright", legend = "Elevation", fill = topo.colors(length(unique(env_df$Elevation))))


plot(pca_result$x[,1], pca_result$x[,2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA Scores: PC1 vs. PC2 with Habitat",
     col = as.numeric(env_df$Habitat), pch = 20)


legend("topright", legend = unique(env_df$Habitat), fill = rainbow(length(unique(env_df$Habitat))))

plot(pca_result$x[,1], pca_result$x[,2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA Scores: PC1 vs. PC2 with Environmental Heterogeneity",
     col = env_df$EnvHet, pch = 20)
legend("topright", legend = "EnvHet", fill = topo.colors(length(unique(env_df$EnvHet))))

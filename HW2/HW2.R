library(golubEsets)
library(cluster)
data(Golub_Merge)
golub <- data.frame(Golub_Merge)[1:7129]
group_vector <- Golub_Merge$ALL.AML

evaluate_clustering <- function(distance_matrix, method, true_labels) {
  cluster_result <- method(distance_matrix)
  
  correlation <- cor(as.numeric(cluster_result), as.numeric(true_labels))
  return(correlation)
}

experiment_clustering <- function(data, true_labels) {
  distance_metrics <- c("euclidean", "manhattan", "cosine")
  clustering_methods <- list(
    kmeans = function(x) kmeans(x, centers = length(unique(true_labels)))$cluster,
    hclust = function(x) cutree(hclust(x, method = "complete"), k = length(unique(true_labels))),
    dbscan = function(x) dbscan(x)$cluster,
    specc = function(x) specc(x, centers = length(unique(true_labels)))$cluster
  )
  
  results <- list()
  
  for (metric in distance_metrics) {
    distance_matrix <- dist(data, method = metric)
    for (method_name in names(clustering_methods)) {
      method <- clustering_methods[[method_name]]
      correlation <- evaluate_clustering(distance_matrix, method, true_labels)
      results[[paste(metric, method_name, sep = "_")]] <- correlation
    }
  }
  
  return(results)
}

clustering_results <- experiment_clustering(golub, group_vector)

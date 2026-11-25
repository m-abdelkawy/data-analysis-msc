# ==============================================================================
# Week 5 Activity: Customer Segmentation with K-Means & Hierarchical Clustering
# ==============================================================================
# Objective : Segment customers using Age and Annual_Spend, determine optimal K,
#             compare K-Means and Hierarchical (Complete Linkage) clustering,
#             and profile the resulting segments on their original scale.
# Dataset   : ../datasets/customer_segmentation.csv
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Setup & Configuration
# ------------------------------------------------------------------------------
required_pkgs <- c("tidyverse", "patchwork", "scales", "cluster")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, dependencies = TRUE)
}

library(tidyverse)
library(patchwork)
library(scales)
library(cluster)

# Global configuration constants
DATA_PATH       <- "../datasets/customer_segmentation.csv"
CLUSTERING_VARS <- c("Age", "Annual_Spend")
K_OPTIMAL       <- 3L
K_RANGE         <- 1:10
SEED            <- 123L
CLUSTER_PALETTE <- c("1" = "#2196F3", "2" = "#FF5722", "3" = "#4CAF50")

# Consistent global ggplot2 theme
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey35", size = 10),
      panel.grid.minor = element_blank(),
      legend.position  = "top"
    )
)

# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

#' Summarize clustering features by cluster assignment on the original scale
profile_clusters <- function(data, cluster_col) {
  data |>
    group_by({{ cluster_col }}) |>
    summarise(
      n = n(),
      pct = round(100 * n() / nrow(data), 1),
      across(
        all_of(CLUSTERING_VARS),
        list(
          mean   = \(x) round(mean(x), 2),
          median = \(x) round(median(x), 2),
          min    = \(x) round(min(x), 2),
          max    = \(x) round(max(x), 2)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
}

#' Scatter plot of Age vs. Annual_Spend colored by cluster
plot_clusters <- function(data, cluster_col, title, subtitle = NULL, centroids = NULL) {
  p <- ggplot(data, aes(x = Age, y = Annual_Spend, color = {{ cluster_col }})) +
    geom_point(size = 2.8, alpha = 0.75) +
    scale_color_manual(values = CLUSTER_PALETTE, name = "Cluster") +
    scale_y_continuous(labels = label_dollar()) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = "Age (Years)",
      y        = "Annual Spend"
    )
  
  if (!is.null(centroids)) {
    p <- p +
      geom_point(
        data        = centroids,
        aes(x = Age, y = Annual_Spend),
        inherit.aes = FALSE,
        shape       = 4,       # 'X' marker
        size        = 5,
        stroke      = 2,
        color       = "black"
      )
  }
  
  p
}

# ------------------------------------------------------------------------------
# 1. Data Loading & Inspection
# ------------------------------------------------------------------------------
stopifnot("Dataset file not found at DATA_PATH." = file.exists(DATA_PATH))

customers <- read_csv(DATA_PATH, show_col_types = FALSE) |>
  mutate(Preferred_Product_Category = as.factor(Preferred_Product_Category))

cat("\n=== Dataset Structure ===\n")
glimpse(customers)

cat("\n=== Missing Values per Column ===\n")
print(colSums(is.na(customers)))

# Fail fast if clustering variables contain NAs
if (anyNA(customers[CLUSTERING_VARS])) {
  stop("Missing values detected in clustering features. Handle NAs before scaling.")
}

cat("\n=== Summary Statistics (Numeric Variables) ===\n")
customers |>
  select(where(is.numeric), -CustomerID) |>
  summary() |>
  print()

# ------------------------------------------------------------------------------
# 2. Exploratory Data Analysis (EDA)
# ------------------------------------------------------------------------------
p_age <- ggplot(customers, aes(x = Age)) +
  geom_histogram(bins = 20, fill = "#4E79A7", color = "white", alpha = 0.9) +
  labs(title = "Distribution of Age", x = "Age (Years)", y = "Count")

p_spend <- ggplot(customers, aes(x = Annual_Spend)) +
  geom_histogram(bins = 20, fill = "#F28E2B", color = "white", alpha = 0.9) +
  scale_x_continuous(labels = label_dollar()) +
  labs(title = "Distribution of Annual Spend", x = "Annual Spend", y = "Count")

p_raw_scatter <- ggplot(customers, aes(x = Age, y = Annual_Spend)) +
  geom_point(color = "grey35", alpha = 0.7, size = 2.5) +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "Age vs. Annual Spend", x = "Age (Years)", y = "Annual Spend")

eda_panel <- (p_age | p_spend | p_raw_scatter) +
  plot_annotation(
    title = "Exploratory Data Analysis: Clustering Feature Distributions",
    theme = theme(plot.title = element_text(face = "bold", size = 15))
  )

print(eda_panel)

# ------------------------------------------------------------------------------
# 3. Preprocessing: Z-Score Standardization
# ------------------------------------------------------------------------------
# Distance-based clustering is scale-sensitive (Age ~ 19-73 vs. Spend ~ $1k-$20k).
# scale() standardizes each column to mean = 0 and sample SD = 1.
scaled_mat <- customers |>
  select(all_of(CLUSTERING_VARS)) |>
  scale()

# Preserve scaling attributes for back-transforming centroids to original units
feature_means <- attr(scaled_mat, "scaled:center")
feature_sds   <- attr(scaled_mat, "scaled:scale")

cat("\n=== Scaled Features Summary ===\n")
print(round(summary(scaled_mat), 4))

# ------------------------------------------------------------------------------
# 4. K-Means Clustering & Optimal K Selection (Elbow Method)
# ------------------------------------------------------------------------------
# Evaluate WCSS (Total Within-Cluster Sum of Squares) across K = 1..10
elbow_tbl <- tibble(k = K_RANGE) |>
  mutate(
    wcss = map_dbl(k, \(k_val) {
      set.seed(SEED)
      kmeans(scaled_mat, centers = k_val, nstart = 25, iter.max = 300)$tot.withinss
    })
  )

cat("\n=== Elbow Method: WCSS by K ===\n")
print(elbow_tbl, n = Inf)

wcss_at_k <- elbow_tbl |> filter(k == K_OPTIMAL) |> pull(wcss)

p_elbow <- ggplot(elbow_tbl, aes(x = k, y = wcss)) +
  geom_line(color = "#2196F3", linewidth = 1) +
  geom_point(color = "#2196F3", size = 3) +
  geom_vline(xintercept = K_OPTIMAL, linetype = "dashed", color = "#D32F2F", linewidth = 0.8) +
  annotate(
    "label",
    x     = K_OPTIMAL + 1.8,
    y     = wcss_at_k + 45,
    label = sprintf("Chosen Elbow: K = %d\nWCSS = %.2f", K_OPTIMAL, wcss_at_k),
    color = "#D32F2F",
    size  = 3.6,
    fontface = "bold"
  ) +
  scale_x_continuous(breaks = K_RANGE) +
  labs(
    title    = "Elbow Method for Optimal Number of Clusters",
    subtitle = "Total Within-Cluster Sum of Squares (WCSS) across K = 1 to 10",
    x        = "Number of Clusters (K)",
    y        = "Within-Cluster Sum of Squares (WCSS)"
  )

print(p_elbow)

# Fit final K-Means model with K = 3
set.seed(SEED)
km_fit <- kmeans(scaled_mat, centers = K_OPTIMAL, nstart = 25, iter.max = 300)

customers <- customers |>
  mutate(KMeans_Cluster = factor(km_fit$cluster))

# Back-transform centroids from Z-scores to original Age / Annual_Spend units
km_centroids_orig <- km_fit$centers |>
  sweep(2, feature_sds, "*") |>
  sweep(2, feature_means, "+") |>
  as_tibble() |>
  mutate(KMeans_Cluster = factor(row_number()))

cat("\n=== K-Means Results (K = 3) ===\n")
cat(sprintf("Total WCSS: %.2f\n", km_fit$tot.withinss))
cat("\nScaled Centroids:\n")
print(round(km_fit$centers, 3))

cat("\n=== K-Means Cluster Profiles (Original Scale) ===\n")
km_profiles <- profile_clusters(customers, KMeans_Cluster)
print(km_profiles, width = Inf)

p_kmeans <- plot_clusters(
  data        = customers,
  cluster_col = KMeans_Cluster,
  title       = sprintf("K-Means Clustering (K = %d)", K_OPTIMAL),
  subtitle    = "Black 'X' markers indicate cluster centroids",
  centroids   = km_centroids_orig
)

print(p_kmeans)

# ------------------------------------------------------------------------------
# 5. Hierarchical Clustering (Agglomerative, Complete Linkage)
# ------------------------------------------------------------------------------
dist_mat <- dist(scaled_mat, method = "euclidean")
hc_fit   <- hclust(dist_mat, method = "complete")

# Plot dendrogram with visual cut highlighting the 3 clusters
plot(
  hc_fit,
  labels = FALSE,
  hang   = -1,
  main   = "Dendrogram — Hierarchical Clustering (Complete Linkage)",
  xlab   = "Customers",
  sub    = "",
  ylab   = "Euclidean Distance (Complete Linkage)"
)
rect.hclust(hc_fit, k = K_OPTIMAL, border = unname(CLUSTER_PALETTE))

# Cut tree into K = 3 clusters
customers <- customers |>
  mutate(HC_Cluster = factor(cutree(hc_fit, k = K_OPTIMAL)))

cat("\n=== Hierarchical Cluster Profiles (Original Scale) ===\n")
hc_profiles <- profile_clusters(customers, HC_Cluster)
print(hc_profiles, width = Inf)

p_hc <- plot_clusters(
  data        = customers,
  cluster_col = HC_Cluster,
  title       = sprintf("Hierarchical Clustering (K = %d)", K_OPTIMAL),
  subtitle    = "Agglomerative clustering with complete linkage"
)

print(p_hc)

# ------------------------------------------------------------------------------
# 6. Method Comparison: K-Means vs. Hierarchical Clustering
# ------------------------------------------------------------------------------
# 6a. Side-by-side visual comparison
comparison_panel <- (p_kmeans | p_hc) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Customer Segmentation Comparison: K-Means vs. Hierarchical",
    theme = theme(plot.title = element_text(face = "bold", size = 15))
  )

print(comparison_panel)

# 6b. Quantitative evaluation via Mean Silhouette Width (range: -1 to 1; higher is better)
sil_km <- mean(silhouette(km_fit$cluster, dist_mat)[, "sil_width"])
sil_hc <- mean(silhouette(as.integer(customers$HC_Cluster), dist_mat)[, "sil_width"])

comparison_metrics <- tibble(
  Method               = c("K-Means (K = 3)", "Hierarchical - Complete (K = 3)"),
  Mean_Silhouette      = round(c(sil_km, sil_hc), 4)
)

cat("\n=== Clustering Quality Comparison (Silhouette Score) ===\n")
print(comparison_metrics)

# 6c. Cross-tabulation of cluster assignments (note: cluster IDs are arbitrary)
cat("\n=== Cross-Tabulation: K-Means (Rows) vs. Hierarchical (Columns) ===\n")
print(table(KMeans = customers$KMeans_Cluster, Hierarchical = customers$HC_Cluster))


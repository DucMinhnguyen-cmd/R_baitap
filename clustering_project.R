# =========================================================================
# CHIẾN LƯỢC PHÂN CỤM ĐA TẦNG TRONG PHÁT HIỆN GIAN LẬN THẺ TÍN DỤNG
# =========================================================================

# Load các thư viện cần thiết
# install.packages(c("kohonen", "factoextra", "dbscan", "mclust", "ggplot2", "dplyr"))

library(dplyr)
library(ggplot2)
library(cluster)     
library(factoextra)  
library(dbscan)      
library(mclust)      
library(kohonen)     

# =========================================================================
# PHẦN 1: ĐÁNH GIÁ ĐỊNH LƯỢNG TRÊN DỮ LIỆU THỰC TẾ
# =========================================================================
cat("1. ĐANG TIỀN XỬ LÝ DỮ LIỆU...\n")
creditcard <- read.csv("D:\\Program\\R\\creditcard.csv")

# Kiểm tra và xử lý trùng lặp
creditcard <- creditcard[!duplicated(creditcard),]

labels_true <- creditcard$Class
data_features <- creditcard %>% select(-Time, -Class)

# Chuẩn hóa dữ liệu (Z-score)
data_scaled <- scale(data_features)

# Lấy mẫu 5000 dòng để tăng tốc độ chạy nghiệm
set.seed(42)
sample_indices <- sample(1:nrow(data_scaled),)
df_sample <- data_scaled[sample_indices, ]
labels_sample <- labels_true[sample_indices]

# Hàm tính Inertia (WCSS)
compute_wcss <- function(data, cluster_assignment) { 
  valid_idx <- cluster_assignment > 0
  if (sum(valid_idx) == 0) return(NA)
  data_valid <- data[valid_idx, , drop = FALSE]
  cl_valid <- cluster_assignment[valid_idx]
  centers <- aggregate(data_valid, by=list(cl_valid), FUN=mean)[, -1]
  wcss <- 0
  unique_cl <- unique(cl_valid)
  for (i in 1:length(unique_cl)) {
    c_id <- unique_cl[i]
    cluster_points <- data_valid[cl_valid == c_id, , drop = FALSE]
    center <- centers[i, ]
    wcss <- wcss + sum(rowSums(sweep(cluster_points, 2, as.numeric(center))^2))
  }
  return(wcss)
}

# --- PHƯƠNG PHÁP ELBOW ---
cat("2. ĐANG VẼ BIỂU ĐỒ ELBOW...\n")
p_elbow <- fviz_nbclust(df_sample, kmeans, method = "wss", k.max = 10) +
  geom_vline(xintercept = 2, linetype = 2, color = "red") +
  labs(title = "Phương pháp Elbow", x = "Số lượng cụm (k)", y = "Inertia (Total WCSS)")
print(p_elbow)

# Chạy các thuật toán trên dữ liệu thật (k=2)
cat("3. ĐANG CHẠY 5 THUẬT TOÁN PHÂN CỤM...\n")
k_optimal <- 2

# 1. K-Means
km_clusters <- kmeans(df_sample, centers = k_optimal, nstart = 25)$cluster

# 2. Hierarchical
hc_clusters <- cutree(hclust(dist(df_sample, method = "euclidean"), method = "ward.D2"), k = k_optimal)

# 3. DBSCAN
db_clusters <- dbscan(df_sample, eps = 4.5, minPts = 10)$cluster 

# 4. GMM
gmm_clusters <- Mclust(df_sample, G = k_optimal, verbose = FALSE)$classification

# 5. SOM
set.seed(42)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
som_model <- som(df_sample, grid = som_grid, rlen = 100)
som_hc <- hclust(dist(som_model$codes[[1]]))
som_node_clusters <- cutree(som_hc, k = k_optimal)
som_clusters <- som_node_clusters[som_model$unit.classif]

# Đánh giá 3 chỉ số
evaluate_clustering <- function(algo_name, data, clusters) {
  valid_idx <- clusters > 0
  n_clusters <- length(unique(clusters[valid_idx]))
  if (n_clusters > 1 && n_clusters < nrow(data)) {
    inertia <- compute_wcss(data, clusters)
    sil_obj <- silhouette(clusters[valid_idx], dist(data[valid_idx, ]))
    sil_score <- mean(sil_obj[, 3])
  } else {
    inertia <- NA; sil_score <- NA
  }
  return(data.frame(Algorithm = algo_name, Inertia = round(inertia, 2), Silhouette_Score = round(sil_score, 4)))
}

results <- rbind(
  evaluate_clustering("K-Means", df_sample, km_clusters),
  evaluate_clustering("Hierarchical", df_sample, hc_clusters),
  evaluate_clustering("DBSCAN", df_sample, db_clusters),
  evaluate_clustering("GMM", df_sample, gmm_clusters),
  evaluate_clustering("SOM", df_sample, som_clusters)
)
print(results)


# =========================================================================
# PHẦN 2: TRỰC QUAN HÓA ĐẠT CHUẨN BÀI BÁO KHOA HỌC
# =========================================================================
cat("\n4. ĐANG XUẤT CÁC BIỂU ĐỒ TRỰC QUAN HÓA...\n")

# -------------------------------------------------------------------------
# HÌNH 1: Biểu đồ Silhouette Score (Dữ liệu thật)
# -------------------------------------------------------------------------
results_clean <- results[!is.na(results$Silhouette_Score), ]
p_sil <- ggplot(results_clean, aes(x = reorder(Algorithm, Silhouette_Score), y = Silhouette_Score, fill = Algorithm)) +
  geom_bar(stat = "identity", width = 0.5, color="black") +
  geom_text(aes(label = Silhouette_Score), hjust = -0.2, size = 5, fontface="bold") +
  coord_flip() + 
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Đánh giá Chất lượng Phân cụm (Silhouette Score)",
       subtitle = "Đánh giá trên tập dữ liệu thẻ tín dụng thực tế (N=5000)",
       x = "Thuật toán", y = "Silhouette Score") +
  theme(legend.position = "none")

print(p_sil)

# -------------------------------------------------------------------------
# TẠO DỮ LIỆU MÔ PHỎNG 2D (MINH HỌA BẢN CHẤT CÁC THUẬT TOÁN)
# -------------------------------------------------------------------------
set.seed(123)
normal_pts <- data.frame(X = rnorm(800, 0, 1), Y = rnorm(800, 0, 1), Label = "Normal")
fraud_pts1 <- data.frame(X = rnorm(20, 4, 0.5), Y = rnorm(20, 4, 0.5), Label = "Fraud")
fraud_pts2 <- data.frame(X = rnorm(15, -4, 0.8), Y = rnorm(15, 3, 0.8), Label = "Fraud")

df_sim <- rbind(normal_pts, fraud_pts1, fraud_pts2)
df_sim_features <- df_sim[, c("X", "Y")]

# --- K-MEANS ---
km_sim <- kmeans(df_sim_features, centers = 3, nstart = 25)
p_km_sim <- fviz_cluster(km_sim, data = df_sim_features, geom = "point", ellipse.type = "convex", palette = "jco", ggtheme = theme_bw(), main = "Phân cụm bằng K-Means (Ranh giới lồi)")
print(p_km_sim)

# --- DBSCAN ---
db_sim <- dbscan(df_sim_features, eps = 0.6, minPts = 5)
p_dbscan_sim <- fviz_cluster(db_sim, data = df_sim_features, geom = "point", ellipse.type = "convex", palette = "npg", ggtheme = theme_bw(), main = "Phân cụm bằng DBSCAN (Điểm nhiễu màu đen)")
print(p_dbscan_sim)

# --- HIERARCHICAL ---
set.seed(42)
df_tree <- df_sim_features[sample(1:nrow(df_sim_features), 80), ] # Lấy 80 điểm vẽ cây cho khỏi rối
hc_sim <- hclust(dist(df_tree, method = "euclidean"), method = "ward.D2")
p_dend <- fviz_dend(hc_sim, k = 3, cex = 0.5, k_colors = c("#2E9FDF", "#E7B800", "#FC4E07"), rect = TRUE, main = "Dendrogram (Hierarchical Clustering)")
print(p_dend)

# --- GMM (Gaussian Mixture Model) ---
gmm_sim <- Mclust(df_sim_features, G = 3, verbose = FALSE)
gmm_list <- list(data = df_sim_features, cluster = gmm_sim$classification)
p_gmm_sim <- fviz_cluster(gmm_list, geom = "point", ellipse.type = "norm", palette = "Dark2", ggtheme = theme_bw(), main = "Phân cụm bằng GMM (Phân phối chuẩn Elip)")
print(p_gmm_sim)

# --- SOM (Self-Organizing Map) ---
# SOM thường được visualize bằng đồ thị Mapping của Base R thay vì ggplot
som_grid_sim <- somgrid(xdim = 4, ydim = 4, topo = "hexagonal")
som_model_sim <- som(scale(df_sim_features), grid = som_grid_sim, rlen = 100)
# Mở một cửa sổ plot mới để vẽ SOM
plot(som_model_sim, type = "mapping", pch = 20, main = "Bản đồ phân phối SOM (Self-Organizing Map)")
cat("\nĐã hoàn tất toàn bộ tiến trình!\n")
# ==============================================================
# PHẦN 1: THIẾT LẬP MÔI TRƯỜNG & KHÁM PHÁ DỮ LIỆU LONGLEY
# ==============================================================

# Thay đổi ngôn ngữ hệ thống sang tiếng Anh để tránh lỗi encoding
# khi hiển thị output (đặc biệt quan trọng trên Windows)
Sys.setlocale("LC_ALL", "English")
Sys.setenv(LANGUAGE = "en")

# ------------------------------------------------------------------
# Dataset 'longley': Dữ liệu kinh tế vĩ mô Mỹ giai đoạn 1947-1962
# Gồm 7 biến: GNP.deflator, GNP, Unemployed, Armed.Forces,
#              Population, Year, Employed (biến mục tiêu)
# ------------------------------------------------------------------

# Xem thống kê mô tả: min, max, mean, median, Q1, Q3 cho từng biến
datasets::longley
View(longley)
summary(longley)

# Kiểm tra cấu trúc dữ liệu: kiểu dữ liệu và số quan sát/biến
str(longley)


# ==============================================================
# PHẦN 2: PHÂN TÍCH TƯƠNG QUAN (LONGLEY)
# ==============================================================

# Tính ma trận tương quan Pearson giữa TẤT CẢ các cặp biến
# Giá trị gần 1 hoặc -1 → tương quan mạnh; gần 0 → tương quan yếu
cor(longley)

# Tính riêng hệ số tương quan giữa GNP và Employed
# Dùng để kiểm tra trước khi đưa vào mô hình hồi quy đơn
cor(longley$GNP, longley$Employed)

# --- Cài đặt package (chỉ chạy lần đầu) ---
install.packages("corrplot")
install.packages("ggcorrplot")

library(corrplot)
library(ggcorrplot)

# --- Tính ma trận tương quan ---
cor_matrix <- cor(longley)
print(round(cor_matrix, 3))

# Tương quan riêng giữa GNP và Employed
cor(longley$GNP, longley$Employed)


# ============================================================
# CÁCH 1: corrplot (nhanh, đẹp, phổ biến nhất)
# ============================================================
corrplot(cor_matrix,
         method  = "color",      # tô màu ô
         # type    = "under",      # chỉ hiện tam giác trên
         addCoef.col = "black",  # hiện số hệ số
         number.cex  = 0.8,      # cỡ chữ số
         tl.col  = "black",      # màu nhãn biến
         tl.srt  = 45,           # xoay nhãn 45 độ
         col     = COL2("RdBu"), # thang màu đỏ-xanh
         title   = "Ma trận tương quan - Longley",
         mar     = c(0,0,1,0))

# ============================================================
# CÁCH 2: ggcorrplot (tích hợp ggplot2, dễ tuỳ chỉnh)
# ============================================================
ggcorrplot(cor_matrix,
           method   = "square",   # hoặc "circle"
           type     = "upper",    # tam giác trên
           lab      = TRUE,       # hiện hệ số
           lab_size = 3,
           colors   = c("#C0392B", "white", "#2980B9"),
           title    = "Ma trận tương quan - Longley",
           ggtheme  = theme_minimal())


# ==============================================================
# PHẦN 3: HỒI QUY TUYẾN TÍNH ĐƠN BIẾN (Simple Linear Regression)
# ==============================================================

# Xây mô hình: GNP = β0 + β1 * Employed + ε
# lm(formula, data): hàm tuyến tính cơ bản trong R
# formula dạng: biến_phụ_thuộc ~ biến_độc_lập

model<-lm(GNP~Employed, data=longley)

# In tóm tắt ngắn gọn (chỉ hiện coefficients)
model

# In kết quả đầy đủ: R², p-value, F-statistic, Std. Error, t-value
# R² (Multiple R-squared): tỉ lệ phương sai GNP được giải thích bởi Employed
# p-value < 0.05 → mô hình có ý nghĩa thống kê
summary(model)

# Trích xuất toàn bộ vector hệ số hồi quy: Intercept (β0) và slope (β1)
model$coefficients

# Lấy riêng hệ số chặn β0 (Intercept): giá trị GNP khi Employed = 0
model$coefficients[[1]]

# Lấy riêng hệ số góc β1 (slope): GNP thay đổi bao nhiêu khi Employed tăng 1 đơn vị
model$coefficients[[2]]

# Xem lại data frame gốc được dùng để khớp mô hình (gồm biến Y và X)
model$model

# ==============================================================
# PHẦN 4: HỒI QUY TUYẾN TÍNH ĐA BIẾN (Multiple Linear Regression)
# ==============================================================

# Xây mô hình với 2 biến độc lập:
# GNP = β0 + β1 * Employed + β2 * Armed.Forces + ε
# Dùng dấu '+' để thêm nhiều biến độc lập vào mô hình
model1 <- lm(GNP ~ Employed + Armed.Forces, data = longley)

# Kiểm tra từng hệ số có ý nghĩa không (p-value từng biến)
# Adjusted R²: phiên bản R² đã điều chỉnh theo số biến (dùng để so sánh mô hình)
summary(model1)

# Trích xuất vector hệ số: β0, β1 (Employed), β2 (Armed.Forces)
model1$coefficients

# β0: Intercept (hằng số)
model1$coefficients[[1]]

# β1: Hệ số của biến Employed trong mô hình đa biến
model1$coefficients[[2]]



# Xây mô hình với tất cả biến độc lập:
# Dùng dấu '.' để lấy tất cả
model2 <- lm(GNP ~ ., data = longley)

# Kiểm tra từng hệ số có ý nghĩa không (p-value từng biến)
# Adjusted R²: phiên bản R² đã điều chỉnh theo số biến (dùng để so sánh mô hình)
summary(model2)

# Trích xuất vector hệ số: β0, β1 (Employed), β2 (Armed.Forces)
model2$coefficients

# β0: Intercept (hằng số)
model2$coefficients[[1]]

# β1: Hệ số của biến Employed trong mô hình đa biến
model2$coefficients[[2]]


# ==============================================================
# PHẦN 5: KHÁM PHÁ DỮ LIỆU BOSTON (Dataset từ package MASS)
# ==============================================================

library(MASS)   # Package chứa nhiều dataset thống kê kinh điển
data(Boston)    # Tải dataset Boston Housing (giá nhà Boston, 506 quan sát, 14 biến)
# Biến mục tiêu: medv (median value of homes, đơn vị $1000)

# Kiểm tra kiểu dữ liệu và cấu trúc các cột
str(Boston)

# Thống kê mô tả toàn bộ dataset
summary(Boston)

# ==============================================================
# PHẦN 6: PHÂN TÍCH TƯƠNG QUAN (BOSTON)
# ==============================================================

# Vẽ ma trận scatter plot (pairs plot) cho tất cả cặp biến
# Giúp nhìn nhanh quan hệ tuyến tính/phi tuyến giữa các biến
pairs(Boston)

# Tính ma trận tương quan cho toàn bộ Boston dataset
correlation <- cor(Boston)

# In hàng tương quan của biến mục tiêu 'medv' với tất cả biến còn lại
# → Xác định biến nào tương quan mạnh nhất với giá nhà
print(correlation["medv", ])

# ==============================================================
# PHẦN 7: TRỰC QUAN HÓA VÀ MÔ HÌNH (BOSTON)
# ==============================================================

# Vẽ scatter plot giữa:
#   rm   (x): số phòng ngủ trung bình mỗi căn nhà
#   medv (y): giá trị nhà trung bình (nghìn USD)
# abline(): vẽ đường hồi quy tuyến tính lên biểu đồ (màu đỏ)
# Lưu ý: abline() phải đặt SAU plot() và TRONG cùng lệnh plot()
plot(Boston$rm, Boston$medv,
     xlab = "So phong trung binh (rm)",
     ylab = "Gia nha trung binh (medv)",
     main = "Moi quan he giua rm va medv",
     abline(lm(medv ~ rm, data = Boston), col = "red"))

# ==============================================================
# PHẦN 8: HỒI QUY TUYẾN TÍNH ĐA BIẾN ĐẦY ĐỦ VỚI ĐÁNH GIÁ MÔ HÌNH
# (Sử dụng Boston Housing Dataset - Train/Test Split)
# ==============================================================

# Cài đặt các gói nếu chưa có, sau đó tải vào phiên làm việc
# require() trả về FALSE nếu gói chưa được cài → kích hoạt install.packages()
if (!require(MASS))  install.packages("MASS")   # Dataset Boston, các hàm thống kê
if (!require(caret)) install.packages("caret")  # Công cụ ML: chia dữ liệu, cross-validation

library(MASS)    # Cung cấp bộ dữ liệu Boston (506 quan sát, 14 biến)
library(caret)   # Cung cấp createDataPartition() để chia train/test có phân tầng

# ------------------------------------------------------------------
# BƯỚC 1: TẢI VÀ CHUẨN BỊ DỮ LIỆU
# ------------------------------------------------------------------

data(Boston)

# Tách ma trận đặc trưng X (13 biến dự báo) và vector mục tiêu y
# Các biến X gồm: crim, zn, indus, chas, nox, rm, age, dis, rad, tax,
#                 ptratio, black, lstat
X <- Boston[, 1:13]   # Tất cả cột trừ cột cuối (medv)
y <- Boston$medv      # medv: Median value of owner-occupied homes ($1000s)

dataset_name <- "Boston housing"

# ------------------------------------------------------------------
# BƯỚC 2: KHÁM PHÁ DỮ LIỆU (Exploratory Data Analysis)
# ------------------------------------------------------------------

# In thông tin tổng quan về tập dữ liệu
cat("Thông tin về tập dữ liệu", dataset_name, ":\n")
cat("Số mẫu:", nrow(X), "\n")       # Số quan sát (hàng)
cat("Số đặc trưng:", ncol(X), "\n\n") # Số biến độc lập (cột)

# Xem 6 hàng đầu để kiểm tra dữ liệu có vẻ hợp lý không
cat("Năm hàng đầu tiên của dữ liệu:\n")
print(head(X))

# Thống kê mô tả: phát hiện outlier, missing values, phân phối lệch
cat("\nThống kê mô tả về dữ liệu:\n")
print(summary(X))

# ------------------------------------------------------------------
# BƯỚC 3: PHÂN CHIA DỮ LIỆU TRAIN / TEST (80% - 20%)
# ------------------------------------------------------------------

# Đặt seed để kết quả có thể tái lập (reproducibility)
# Bất kỳ con số nào đều được; 42 là quy ước phổ biến
set.seed(42)

# createDataPartition(): chia có phân tầng (stratified) theo y
# p = 0.8 → 80% dùng để huấn luyện, 20% để kiểm tra
# list = FALSE → trả về vector chỉ số (index), không phải list
train_indices <- createDataPartition(y, p = 0.8, list = FALSE)

# Chia X và y theo chỉ số train
X_train <- X[train_indices, ]    # Ma trận đặc trưng tập huấn luyện
X_test  <- X[-train_indices, ]   # Ma trận đặc trưng tập kiểm tra (dấu '-' = loại trừ)
y_train <- y[train_indices]      # Nhãn mục tiêu tập huấn luyện
y_test  <- y[-train_indices]     # Nhãn mục tiêu tập kiểm tra

# Kiểm tra kích thước sau khi chia: dim() trả về (số hàng, số cột)
cat("\nKích thước tập huấn luyện:", dim(X_train), "\n")
cat("Kích thước tập kiểm tra:",   dim(X_test),  "\n")

# ------------------------------------------------------------------
# BƯỚC 4-5: XÂY DỰNG VÀ HUẤN LUYỆN MÔ HÌNH HỒI QUY TUYẾN TÍNH
# ------------------------------------------------------------------

# Công thức 'y_train ~ .' nghĩa là:
#   y_train là biến phụ thuộc
#   dấu '.' = sử dụng TẤT CẢ các cột còn lại trong data = X_train làm biến độc lập
# → Mô hình: medv = β0 + β1*crim + β2*zn + ... + β13*lstat + ε
model <- lm(y_train ~ ., data = X_train)

# Kết quả summary gồm:
#   - Coefficients: hệ số, Std. Error, t-value, p-value từng biến
#   - Multiple R²: mức độ giải thích phương sai của mô hình trên tập train
#   - F-statistic: kiểm định toàn bộ mô hình có ý nghĩa không
summary(model)

# ------------------------------------------------------------------
# BƯỚC 6: PHÂN TÍCH HỆ SỐ HỒI QUY
# ------------------------------------------------------------------

# coef(model): vector hệ số gồm Intercept + 13 biến
# [-1]: bỏ Intercept, chỉ lấy hệ số của các biến độc lập
coef_df <- data.frame(
  feature     = names(coef(model)[-1]),   # Tên biến
  coefficient = coef(model)[-1]           # Giá trị hệ số tương ứng
)

# Sắp xếp giảm dần theo giá trị hệ số
# Hệ số dương lớn → biến tác động mạnh chiều tăng giá nhà
# Hệ số âm lớn   → biến tác động mạnh chiều giảm giá nhà
coef_df <- coef_df[order(coef_df$coefficient, decreasing = TRUE), ]

cat("\nHệ số hồi quy (sắp xếp theo độ lớn):\n")
print(coef_df)

# In riêng Intercept β0: giá trị medv dự đoán khi tất cả biến X = 0
cat("Hệ số chặn (Intercept):", coef(model)[1], "\n")

# ------------------------------------------------------------------
# BƯỚC 7: DỰ ĐOÁN TRÊN TẬP KIỂM TRA
# ------------------------------------------------------------------

# predict() áp dụng mô hình đã huấn luyện lên dữ liệu mới (X_test)
# newdata phải có đúng tên cột như dữ liệu train
y_pred <- predict(model, newdata = X_test)

# ------------------------------------------------------------------
# BƯỚC 8: ĐÁNH GIÁ MÔ HÌNH BẰNG CÁC CHỈ SỐ HỒI QUY
# ------------------------------------------------------------------

# MSE (Mean Squared Error): trung bình bình phương sai số
# Nhạy cảm với outlier; đơn vị là $1000² → khó diễn giải trực tiếp
mse <- mean((y_test - y_pred)^2)
mse

# RMSE (Root MSE): căn bậc hai của MSE
# Cùng đơn vị với y ($1000) → dễ diễn giải hơn MSE
rmse <- sqrt(mse)
rmse

# R² (Coefficient of Determination): tỉ lệ phương sai y_test được giải thích bởi mô hình
# R² = 1 → dự đoán hoàn hảo; R² = 0 → mô hình không tốt hơn dự đoán bằng mean(y)
# Công thức: R² = 1 - SS_res / SS_tot
r2 <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

cat("\nĐánh giá mô hình trên tập kiểm tra:\n")
cat("Mean Squared Error (MSE):",         round(mse,  4), "\n")
cat("Root Mean Squared Error (RMSE):",   round(rmse, 4), "\n")
cat("R-squared (R²):",                   round(r2,   4), "\n")

# ------------------------------------------------------------------
# BƯỚC 9: TRỰC QUAN HÓA KẾT QUẢ DỰ ĐOÁN
# ------------------------------------------------------------------

# Scatter plot: trục x = giá thực tế, trục y = giá dự đoán
# Nếu mô hình hoàn hảo → tất cả điểm nằm trên đường y = x
# pch = 16: điểm đặc hình tròn; col = "blue": màu xanh
plot(y_test, y_pred,
     main = "So sánh giữa giá trị thực tế và dự đoán",
     xlab = "Giá trị thực tế (medv)",
     ylab = "Giá trị dự đoán (medv)",
     pch  = 16,
     col  = "blue")

# Vẽ đường tham chiếu y = x (slope = 1, intercept = 0)
# lwd = 2: độ dày nét vẽ; lty = 2: nét đứt
# Điểm càng gần đường này → dự đoán càng chính xác
abline(0, 1, col = "red", lwd = 2, lty = 2)

# Thêm lưới để dễ đọc giá trị trên biểu đồ
grid()


# ==============================================================
# PHẦN MỞ RỘNG: SO SÁNH NHIỀU MÔ HÌNH REGRESSION
# ==============================================================

# Cài thêm package cần thiết
if (!require(randomForest)) install.packages("randomForest")
if (!require(e1071))       install.packages("e1071")   # SVM
if (!require(FNN))         install.packages("FNN")     # KNN

library(randomForest)
library(e1071)
library(FNN)

# --------------------------------------------------------------
# CHUẨN HÓA DỮ LIỆU (CẦN CHO KNN, SVM)
# --------------------------------------------------------------
preProcValues <- preProcess(X_train, method = c("center", "scale"))

X_train_scaled <- predict(preProcValues, X_train)
X_test_scaled  <- predict(preProcValues, X_test)

# --------------------------------------------------------------
# HÀM ĐÁNH GIÁ MODEL
# --------------------------------------------------------------
evaluate_model <- function(y_true, y_pred) {
  mse  <- mean((y_true - y_pred)^2)
  rmse <- sqrt(mse)
  mae  <- mean(abs(y_true - y_pred))
  r2   <- 1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
  
  return(c(MSE = mse, RMSE = rmse, MAE = mae, R2 = r2))
}

# --------------------------------------------------------------
# 1. LINEAR REGRESSION
# --------------------------------------------------------------
lm_model <- lm(y_train ~ ., data = X_train)
lm_pred  <- predict(lm_model, newdata = X_test)

lm_metrics <- evaluate_model(y_test, lm_pred)

# --------------------------------------------------------------
# 2. KNN REGRESSION
# --------------------------------------------------------------
knn_pred <- knn.reg(
  train = X_train_scaled,
  test  = X_test_scaled,
  y     = y_train,
  k     = 5
)$pred

knn_metrics <- evaluate_model(y_test, knn_pred)

# --------------------------------------------------------------
# 3. SVM REGRESSION
# --------------------------------------------------------------
svm_model <- svm(
  x = X_train_scaled,
  y = y_train,
  type = "eps-regression"
)

svm_pred <- predict(svm_model, X_test_scaled)

svm_metrics <- evaluate_model(y_test, svm_pred)

# --------------------------------------------------------------
# 4. RANDOM FOREST
# --------------------------------------------------------------
rf_model <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 100
)

rf_pred <- predict(rf_model, X_test)

rf_metrics <- evaluate_model(y_test, rf_pred)

# --------------------------------------------------------------
# TẠO BẢNG SO SÁNH
# --------------------------------------------------------------
results <- data.frame(
  Model = c("Linear Regression", "KNN", "SVM", "Random Forest"),
  MSE   = c(lm_metrics["MSE"], knn_metrics["MSE"], svm_metrics["MSE"], rf_metrics["MSE"]),
  RMSE  = c(lm_metrics["RMSE"], knn_metrics["RMSE"], svm_metrics["RMSE"], rf_metrics["RMSE"]),
  MAE   = c(lm_metrics["MAE"], knn_metrics["MAE"], svm_metrics["MAE"], rf_metrics["MAE"]),
  R2    = c(lm_metrics["R2"], knn_metrics["R2"], svm_metrics["R2"], rf_metrics["R2"])
)

# Sắp xếp theo RMSE (nhỏ nhất là tốt nhất)
results <- results[order(results$RMSE), ]

cat("\n===== BẢNG SO SÁNH MODEL =====\n")
print(results)

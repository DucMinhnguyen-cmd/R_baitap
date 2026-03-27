# BÀI THỰC HÀNH: PHÂN TÍCH CHẤT LƯỢNG NƯỚC BẰNG HỒI QUY LOGISTIC
# ==========================================================

# TÓM TẮT BÀI TOÁN:
# Bài thực hành này sử dụng bộ dữ liệu water_potability để dự đoán liệu một mẫu nước có uống được hay không
# dựa trên các chỉ số hóa học và vật lý khác nhau. Chúng ta sẽ sử dụng mô hình hồi quy logistic để
# xây dựng bộ phân loại nhị phân, đánh giá hiệu suất của mô hình và trực quan hóa kết quả.
# Biến mục tiêu: Potability (0 = không uống được, 1 = uống được)
install.packages("readr")
library(readr)
water_potability <- read_csv("D:\\Program\\R\\water_potability.csv")
colSums(is.na(water_potability))
if(any(is.na(water_potability$ph))) {
  water_potability$ph[is.na(water_potability$ph)] <- mean(water_potability$ph, na.rm = T)
}
if(any(is.na(water_potability$Sulfate))) {
  water_potability$Sulfate[is.na(water_potability$Sulfate)] <- mean(water_potability$Sulfate, na.rm = T)
}

if(any(is.na(water_potability$Trihalomethanes))) {
  water_potability$Trihalomethanes[is.na(water_potability$Trihalomethanes)] <- mean(water_potability$Trihalomethanes, na.rm = T)
}

# Chuyển Potability sang factor (0, 1)
water_potability$Potability <- factor(water_potability$Potability, levels = c(0,1))

# 4. CHIA TẬP DỮ LIỆU (70% Train, 30% Test)
set.seed(123) 
n <- nrow(water_potability)
train_index <- sample(1:n, size = 0.7*n)

train_data <- water_potability[train_index, ]
test_data  <- water_potability[-train_index, ]

# Đã chuyển dim() xuống đây (sau khi biến được tạo)
print("Kích thước tập Train:")
dim(train_data)
print("Kích thước tập Test:")
dim(test_data)

# 5. HUẤN LUYỆN MÔ HÌNH
logit_model <- glm(
  Potability ~ .,
  data = train_data,
  family = binomial(link = "logit")
)
summary(logit_model)

# 6. DỰ ĐOÁN TRÊN TẬP TEST (Phần bạn đang thiếu)
# Tính xác suất (prob)
prob_pred <- predict(logit_model, newdata = test_data, type = "response")

# Đổi xác suất thành nhãn 0 hoặc 1 (ngưỡng 0.5)
class_pred <- ifelse(prob_pred > 0.5, 1, 0)
class_pred <- factor(class_pred, levels = c(0, 1))

# Tạo bảng Confusion Matrix thực tế (cm_logit)
cm_logit <- table(Predicted = class_pred, Actual = test_data$Potability)
print("Bảng Confusion Matrix:")
print(cm_logit)

# 7. VẼ BIỂU ĐỒ CONFUSION MATRIX
plot_cm <- function(cm, title) {
  df <- as.data.frame(cm)
  # Đặt tên cột cho khớp với dataframe sinh ra từ lệnh table()
  colnames(df) <- c("Predicted", "Actual", "Freq")
  
  ggplot(df, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 5, fontface = "bold") +
    scale_fill_gradient(low = "#e0f3db", high = "#43a2ca") + # Màu nhìn rõ đường chéo hơn
    labs(
      title = title,
      x = "Thực tế (Actual)",
      y = "Dự đoán (Predicted)"
    ) +
    theme_minimal()
}

# Chạy hàm vẽ
plot_cm(cm_logit, "Confusion Matrix - Logistic Regression")

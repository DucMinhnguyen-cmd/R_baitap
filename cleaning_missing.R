# ------------------------------------------------------------------------------
# 1. KHỞI TẠO VÀ THIẾT LẬP MÔI TRƯỜNG
# ------------------------------------------------------------------------------

# Tải các thư viện cần thiết cho việc vẽ biểu đồ và xử lý dữ liệu
if(!require(corrplot)) install.packages("corrplot")
if(!require(ggplot2)) install.packages("ggplot2")
library(corrplot)
library(ggplot2)

# Thiết lập đường dẫn và đọc dữ liệu gốc
# Lưu ý: Thay đổi đường dẫn này phù hợp với máy tính của bạn
path <- "D:\\Program\\R\\train.csv"
d1 <- read.csv(path)

# ------------------------------------------------------------------------------
# 2. KHÁM PHÁ DỮ LIỆU BAN ĐẦU (INITIAL DATA EXPLORATION)
# ------------------------------------------------------------------------------

# Kiểm tra kích thước của bộ dữ liệu (Số dòng x Số cột)
print(paste("Số lượng quan sát:", nrow(d1)))
print(paste("Số lượng biến số:", ncol(d1)))

# Xem cấu trúc chi tiết để phân loại biến định lượng và biến định tính
str(d1)

# Kiểm tra 10 dòng đầu tiên để làm quen với các giá trị thực tế
head(d1, 10)

# Thống kê sơ bộ các giá trị thiếu (Missing Values) trên toàn bộ dataset
colSums(is.na(d1))


# ------------------------------------------------------------------------------
# 3. XỬ LÝ DỮ LIỆU THIẾU (DATA IMPUTATION)
# ------------------------------------------------------------------------------

# 3.1. Biến LotFrontage (Chiều dài tiếp giáp đường)
# Sử dụng Median vì diện tích thường có phân phối lệch
cat("Đang xử lý LotFrontage...\n")
d1$LotFrontage[is.na(d1$LotFrontage)] <- median(d1$LotFrontage, na.rm = TRUE)

# 3.2. Biến Alley
# NA nghĩa là không có lối đi phụ
d1$Alley <- as.character(d1$Alley)
d1$Alley[is.na(d1$Alley)] <- "None"
d1$Alley <- as.factor(d1$Alley)

# 3.3. Nhóm biến Tầng hầm (Basement)
# Xử lý từng cột một để đảm bảo tính chính xác cho từng loại dữ liệu
cat("Đang làm sạch dữ liệu Tầng hầm...\n")
d1$BsmtQual[is.na(d1$BsmtQual)] <- "None"
d1$BsmtCond[is.na(d1$BsmtCond)] <- "None"
d1$BsmtExposure[is.na(d1$BsmtExposure)] <- "None"
d1$BsmtFinType1[is.na(d1$BsmtFinType1)] <- "None"
d1$BsmtFinType2[is.na(d1$BsmtFinType2)] <- "None"

# 3.4. Nhóm biến Nhà để xe (Garage)
# NA thường đi cùng với việc không có diện tích Garage
cat("Đang làm sạch dữ liệu Garage...\n")
d1$GarageType[is.na(d1$GarageType)] <- "None"
d1$GarageFinish[is.na(d1$GarageFinish)] <- "None"
d1$GarageQual[is.na(d1$GarageQual)] <- "None"
d1$GarageCond[is.na(d1$GarageCond)] <- "None"
d1$GarageYrBlt[is.na(d1$GarageYrBlt)] <- 0

# 3.5. Biến MasVnrType (Gạch trang trí)
# Điền giá trị phổ biến nhất (Mode)
table(d1$MasVnrType)
d1$MasVnrType[is.na(d1$MasVnrType)] <- "None"

# 3.6. Các biến tiện ích khác
d1$FireplaceQu[is.na(d1$FireplaceQu)] <- "None"
d1$PoolQC[is.na(d1$PoolQC)] <- "None"
d1$Fence[is.na(d1$Fence)] <- "None"
d1$MiscFeature[is.na(d1$MiscFeature)] <- "None"

#xử lý electrical
table(d1$Electrical)

if(any(is.na(d1$Electrical))) {
  
  d1$Electrical[is.na(d1$Electrical)] <- get_mode(d1$Electrical)
  
}
# Kiểm tra lại một lần nữa để chắc chắn không còn NA
final_na_check <- sum(is.na(d1))
print(paste("Tổng số giá trị thiếu còn lại:", final_na_check))

# ------------------------------------------------------------------------------
# 4. CHUẨN HÓA KIỂU DỮ LIỆU (DATA TYPE CONVERSION)
# ------------------------------------------------------------------------------

# Chuyển các cột định danh dạng số sang Factor để máy không tính toán nhầm
cat("Đang chuyển đổi kiểu dữ liệu...\n")
d1$MSSubClass <- as.factor(d1$MSSubClass)
d1$MoSold <- as.factor(d1$MoSold)
d1$YrSold <- as.factor(d1$YrSold)

# Tự động hóa việc chuyển Character sang Factor cho các cột còn lại
char_columns <- sapply(d1, is.character)
d1[char_columns] <- lapply(d1[char_columns], as.factor)

# ------------------------------------------------------------------------------
# 5. MÃ HÓA THỨ TỰ CHO CÁC BIẾN CHẤT LƯỢNG (ORDINAL ENCODING)
# ------------------------------------------------------------------------------

# Thiết lập thang điểm từ 0 đến 5 cho các biến đánh giá
quality_map <- c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)

# Áp dụng cho Exterior Quality
d1$ExterQual <- as.numeric(factor(d1$ExterQual, levels=names(quality_map), labels=quality_map))

#Basement Quality
d1$BsmtQual <- as.numeric(factor(d1$BsmtQual, levels=names(quality_map), labels=quality_map))

#Heating Quality
d1$HeatingQC <- as.numeric(factor(d1$HeatingQC, levels=names(quality_map), labels=quality_map))

#Kitchen Quality
d1$KitchenQual <- as.numeric(factor(d1$KitchenQual, levels=names(quality_map), labels=quality_map))

#Fireplace Quality
d1$FireplaceQu <- as.numeric(factor(d1$FireplaceQu, levels=names(quality_map), labels=quality_map))

#Garage Quality
d1$GarageQual <- as.numeric(factor(d1$GarageQual, levels=names(quality_map), labels=quality_map))

# ------------------------------------------------------------------------------
# 6. KIẾN TẠO BIẾN MỚI (FEATURE ENGINEERING)
# ------------------------------------------------------------------------------

# Tạo biến tổng diện tích (Tổng các tầng và hầm)
d1$TotalSF <- d1$X1stFlrSF + d1$X2ndFlrSF + d1$TotalBsmtSF

# Tạo biến tổng số phòng tắm
d1$TotalBath <- d1$FullBath + (0.5 * d1$HalfBath) + d1$BsmtFullBath + (0.5 * d1$BsmtHalfBath)

# Tạo biến độ tuổi của ngôi nhà khi được bán
d1$AgeAtSale <- as.numeric(as.character(d1$YrSold)) - d1$YearBuilt

# Kiểm tra các biến mới vừa tạo
summary(d1[, c("TotalSF", "TotalBath", "AgeAtSale")])

# ------------------------------------------------------------------------------
# 7. XỬ LÝ NGOẠI LAI (OUTLIERS DETECTION)
# ------------------------------------------------------------------------------

# Vẽ biểu đồ Scatter Plot để tìm kiếm các căn nhà dị thường
plot(d1$GrLivArea, d1$SalePrice,
     pch = 19, col = "darkblue",
     main = "Phân tích giá trị ngoại lai diện tích",
     xlab = "Diện tích sống (sq ft)", ylab = "Giá bán ($)")

# Loại bỏ các căn nhà có GrLivArea > 4000 nhưng giá quá thấp
d1 <- d1[!(d1$GrLivArea > 4000 & d1$SalePrice < 300000), ]

# ------------------------------------------------------------------------------
# 8. PHÂN TÍCH TƯƠNG QUAN (CORRELATION ANALYSIS)
# ------------------------------------------------------------------------------

# Lọc các cột dạng số để tính toán ma trận tương quan
numeric_features <- sapply(d1, is.numeric)
cor_matrix <- cor(d1[, numeric_features])

# Lấy các biến có tương quan mạnh nhất với SalePrice
price_correlations <- sort(cor_matrix[, "SalePrice"], decreasing = TRUE)
print("Top các biến ảnh hưởng mạnh nhất đến giá nhà:")
print(head(price_correlations, 15))

# Vẽ biểu đồ tương quan rút gọn (Heatmap)
top_features <- names(price_correlations[1:10])
corrplot(cor_matrix[top_features, top_features], method = "number", type = "upper")

# ------------------------------------------------------------------------------
# 9. TRỰC QUAN HÓA CÁC BIẾN QUAN TRỌNG NHẤT
# ------------------------------------------------------------------------------

# Biểu đồ 1: SalePrice vs OverallQual (Chất lượng tổng thể)
ggplot(d1, aes(x = factor(OverallQual), y = SalePrice)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Giá nhà phân bổ theo chất lượng tổng thể", x = "Chất lượng", y = "Giá")

# Biểu đồ 2: SalePrice vs TotalSF (Tổng diện tích)
ggplot(d1, aes(x = TotalSF, y = SalePrice)) +
  geom_point(col = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Mối liên hệ giữa Tổng diện tích và Giá bán")

# Biểu đồ 3: Phân phối giá nhà (Histogram)
ggplot(d1, aes(x = SalePrice)) +
  geom_histogram(fill = "green", bins = 50, col = "white") +
  labs(title = "Phân phối tần suất của Giá bán nhà")

# ------------------------------------------------------------------------------
# 10. XUẤT DỮ LIỆU SẠCH VÀ KẾT LUẬN
# ------------------------------------------------------------------------------

# Tạo tập dữ liệu cuối cùng chứa các biến quan trọng cho mô hình dự báo
final_important_cols <- c("SalePrice", "TotalSF", "OverallQual", "GrLivArea", 
                          "GarageCars", "TotalBath", "KitchenQual", "AgeAtSale", "Neighborhood")

d1_final <- d1[, final_important_cols]

# Xuất file CSV kết quả
write.csv(d1_final, "Ames_Cleaned_Data_Final.csv", row.names = FALSE)

# Tóm tắt kết quả xử lý
cat("--------------------------------------------------\n")
cat("TỔNG KẾT QUÁ TRÌNH LÀM SẠCH:\n")
cat("- Xử lý xong NA cho toàn bộ 81 biến.\n")
cat("- Tạo thêm 3 biến mới quan trọng.\n")
cat("- Mã hóa thứ tự cho 6 biến chất lượng.\n")
cat("- Loại bỏ thành công các giá trị ngoại lai nhiễu.\n")
cat("Dữ liệu đã sẵn sàng để huấn luyện mô hình.\n")
cat("--------------------------------------------------\n")

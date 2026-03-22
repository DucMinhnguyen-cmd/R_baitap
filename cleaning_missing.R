#đọc dữ liệu
d1<-read.csv("D:\\Program\\R\\train.csv")
View(d1)
#tìm số lượng Na
colSums((is.na(d1)))
summary(d1)
#sửa đổi cột LotFrontage bằng median
if(any(is.na(d1$LotFrontage))) {
  d1$LotFrontage[is.na(d1$LotFrontage)] <- median(d1$LotFrontage, na.rm = TRUE)
}
#cột alley điền "no alley" 
if(any(is.na(d1$Alley))) {
  d1$Alley[is.na(d1$Alley)]<-"None"
}
#xem tổng quan cột MasVnrType so với 8 mising values
table(d1$MasVnrType)
if(any(is.na(d1$MasVnrType))) {
  d1$MasVnrType[is.na(d1$MasVnrType)]<-names(table(d1$MasVnrType))[which.max(table(d1$MasVnrType))]
}

#sử dụng None với BsmtQual, bsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2
cols_to_mode <- c("BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2")

d1[cols_to_mode] <- lapply(d1[cols_to_mode], function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- "None"
  }
  return(x)
})
#cho none fireplaceQu
if(any(is.na(d1$FireplaceQu))) {
  d1$FireplaceQu[is.na(d1$FireplaceQu)]<-"None"
}
#cũng như vạy nhưng vớ GarageType, GarageFinish, GarageQual,  GarageCond
cols_to_mode <- c("GarageType", "GarageFinish", "GarageQual",  "GarageCond")

d1[cols_to_mode] <- lapply(d1[cols_to_mode], function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <-"None"
  }
  return(x)
})
#tính garageỶBlt
if(any(is.na(d1$GarageYrBlt))) {
  d1$GarageYrBlt[is.na(d1$GarageYrBlt)] <- "None"
}
colSums(is.na(d1))

#điền giá trị "none" vào poolQC, fence, miscfeature

cols_to_mode <- c("Fence", "PoolQC", "MiscFeature")

d1[cols_to_mode] <- lapply(d1[cols_to_mode], function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- "None"
  }
  return(x)
})

table(d1$Electrical)
if(any(is.na(d1$Electrical))) {
  d1$Electrical[is.na(d1$Electrical)] <- get_mode(d1$Electrical)
}

# Chuyển các biến số mang ý nghĩa phân loại sang Factor
cols_to_factor <- c("MSSubClass", "MoSold", "YrSold")
d1[cols_to_factor] <- lapply(d1[cols_to_factor], as.factor)

#Chuyển tự động toàn bộ các cột dạng Chuỗi (Character) sang Factor
char_cols <- sapply(d1, is.character)
# Ép kiểu hàng loạt sang factor
d1[char_cols] <- lapply(d1[char_cols], as.factor)

# 3. Trực quan hóa Outliers (Tùy chọn: bạn chạy dòng này để xem biểu đồ)
# Cửa sổ Plots sẽ hiện ra một vài điểm nằm tách biệt hẳn ở góc dưới bên phải
plot(d1$GrLivArea, d1$SalePrice, 
     main = "Biểu đồ Phân tán: Diện tích sống vs Giá nhà", 
     xlab = "Diện tích sống (GrLivArea)", 
     ylab = "Giá nhà (SalePrice)", 
     col = "blue", pch = 16)

# 4. Loại bỏ các giá trị ngoại lai gây nhiễu
# Xóa những căn nhà có diện tích > 4000 nhưng giá < 300.000
d1 <- d1[!(d1$GrLivArea > 4000 & d1$SalePrice < 300000), ]

# 5. Kiểm tra lại kích thước dữ liệu (sẽ thấy hụt đi vài dòng so với 1460 ban đầu)
dim(d1)
colSums(is.na(d1))

# Định nghĩa bảng xếp hạng chất lượng
qual_levels <- c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)

# Danh sách các cột có chung cách đánh giá này
cols_to_fix <- c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", 
                 "HeatingQC", "KitchenQual", "FireplaceQu", "GarageQual", "GarageCond")

# Dùng vòng lặp để chuyển đổi hàng loạt
for (col in cols_to_fix) {
  d1[[col]] <- as.numeric(factor(d1[[col]], 
                                 levels = names(qual_levels), 
                                 labels = qual_levels))
}

#Kiểm tra tương quan với biến mục tiêu SalePrice
# Chỉ lấy những cột là dạng số (numeric)
numeric_vars <- sapply(d1, is.numeric)
cor_data <- cor(d1[, numeric_vars])

# Lấy những biến có tương quan > 0.5 với SalePrice (những biến quan trọng nhất)
significant_vars <- sort(cor_data[,"SalePrice"], decreasing = TRUE)
print(significant_vars[significant_vars > 0.5])

d1$TotalSF <- d1$X1stFlrSF + d1$X2ndFlrSF + d1$TotalBsmtSF
important_cols <- c("SalePrice", "TotalSF", "GrLivArea", "OverallQual", "KitchenQual", "ExterQual")

# Tạo dataframe mới gọn gàng hơn
d1_clean <- d1[, important_cols]

# Xem thử 5 dòng đầu của bộ dữ liệu "tinh gọn"
head(d1_clean)

# 1. Tính toán tương quan chỉ riêng cho SalePrice
cor_values <- cor(d1[, sapply(d1, is.numeric)])["SalePrice", ]
cor_values <- sort(cor_values, decreasing = TRUE)

# 2. Lấy Top 10 (bỏ chính nó ở vị trí số 1)
top_10 <- cor_values[2:11]

# 3. Vẽ biểu đồ cột nằm ngang cho cực kỳ dễ đọc nhãn
barplot(top_10, 
        las = 2, 
        col = "steelblue", 
        horiz = TRUE, 
        main = "10 Yếu tố ảnh hưởng mạnh nhất đến Giá Nhà",
        xlab = "Độ mạnh của sự ảnh hưởng (Tương quan)",
        cex.names = 0.8) # Chỉnh cỡ chữ tên cột cho vừa

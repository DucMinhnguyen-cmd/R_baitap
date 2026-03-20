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
#cột alley do thiếu >20% nên sẽ phải fill bằng cách điền "no alley" 
if(any(is.na(d1$Alley))) {
  d1$Alley[is.na(d1$Alley)]<-"None"
}
#xem tổng quan cột MasVnrType so với 8 mising values
table(d1$MasVnrType)
if(any(is.na(d1$MasVnrType))) {
 d1$MasVnrType[is.na(d1$MasVnrType)]<-names(table(d1$MasVnrType))[which.max(table(d1$MasVnrType))]
}

#sử dụng yếu vị với BsmtQual, bsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2
# 1. Định nghĩa hàm tìm Yếu vị
get_mode <- function(x) {
  x <- x[!is.na(x)] # Bỏ qua NA khi đếm
  if (length(x) == 0) return(NA) 
  bang_tan_suat <- table(x)
  return(names(bang_tan_suat)[which.max(bang_tan_suat)])
}
cols_to_mode <- c("BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2")

d1[cols_to_mode] <- lapply(d1[cols_to_mode], function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- get_mode(x)
  }
  return(x)
})
#cũng như vạy nhưng với FireplaceQu, GarageType, GarageFinish, GarageQual,  GarageCond
cols_to_mode <- c("FireplaceQu", "GarageType", "GarageFinish", "GarageQual",  "GarageCond")

d1[cols_to_mode] <- lapply(d1[cols_to_mode], function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- get_mode(x)
  }
  return(x)
})
#tính trung vị garageỶBlt
if(any(is.na(d1$GarageYrBlt))) {
  d1$GarageYrBlt[is.na(d1$GarageYrBlt)] <- median(d1$GarageYrBlt, na.rm = T)
}
colSums(is.na(d1))

#điền giá trị "none" vào poolQC
if(any(is.na(d1$PoolQC))) {
  d1$PoolQC[is.na(d1$PoolQC)] <- "None"
}
#
cols_to_mode <- c("Fence")

d1[cols_to_mode] <- lapply(d1[cols_to_mode], function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- get_mode(x)
  }
  return(x)
})
#
if(any(is.na(d1$MiscFeature))) {
  d1$MiscFeature[is.na(d1$MiscFeature)] <- "None"
}

colSums(is.na(d1))

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

#sử dụng yếu vị với BsmtQual



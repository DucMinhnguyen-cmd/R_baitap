# ------------------------------------------------------------------------------
# Bài tập 1: Bar Chart
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu sau:
subjects <- c("Toán", "Lý", "Hóa", "Văn", "Anh")
scores <- c(8, 7.5, 9, 8.5, 7)

# Yêu cầu:
# 1. Vẽ bar chart cơ bản
# 2. Thêm tiêu đề "Điểm thi của bạn"
# 3. Tô màu khác nhau cho mỗi môn
# 4. Thêm giá trị điểm lên đầu mỗi cột
# 5. Vẽ bar chart ngang
barplot(scores,
        names.arg = subjects,
        main="điểm các môn",
        xlab="các môn học",
        ylab="điểm môn",
        ylim=c(0,10))
barplot(scores,
        main="điểm các môn",
        xlab = "các môn học",
        ylab = "điểm môn",
        col = c( 1, 2, 3, 4, 5),
        horiz= TRUE)

# ------------------------------------------------------------------------------
# Bài tập 2: Histogram
# ------------------------------------------------------------------------------

# Tạo dữ liệu: Điểm thi của 100 sinh viên
set.seed(2024)
exam_scores <- rnorm(100, mean = 70, sd = 10)

# Yêu cầu:
# 1. Vẽ histogram với 10 bins
# 2. Thêm tiêu đề và nhãn trục phù hợp
# 3. Tô màu xanh lam
# 4. Thêm đường thẳng đứng màu đỏ tại vị trí điểm trung bình
# 5. Vẽ histogram khác với 20 bins, so sánh sự khác biệt
hist(exam_scores,
     main="điểm thi 100 sinh viên",
     xlab="sinh vien",
     ylab="diem thi")
hist(exam_scores,
     main="histogram with 10 bins",
     xlab="sinh vien",
     breaks= 10,
     col = "lightblue")
abline(v = mean(exam_scores), col = "red", lwd = 2, lty = 2)
hist(exam_scores,
     main="histogram with 20 bins",
     xlab = "sinh vien",
     breaks= 20,
     col = "lightgreen")
abline(v = mean(exam_scores), col = "red", lwd = 2, lty = 2)
# ------------------------------------------------------------------------------
# Bài tập 3: Box Plot
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu iris
data("iris")
# Yêu cầu:
# 1. Vẽ box plot so sánh Petal.Length giữa 3 loài
# 2. Tô màu khác nhau cho mỗi loài
# 3. Thêm tiêu đề phù hợp
# 4. Nhìn vào biểu đồ và trả lời:
#    - Loài nào có petal dài nhất?
#    - Loài nào có độ biến thiên lớn nhất?
#    - Có outliers không? Ở loài nào?
# Vẽ Boxplot
boxplot(Petal.Length ~ Species, 
        data = iris,
        main = "So sánh chiều dài cánh hoa (Petal Length) giữa các loài",
        xlab = "Loài (Species)",
        ylab = "Chiều dài cánh hoa (cm)",
        col = c("lightblue", "lightgreen", "lightpink"), # Tô màu khác nhau cho 3 loài
        border = "darkgray")
# ------------------------------------------------------------------------------
# Bài tập 4: Scatter Plot
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu mtcars

# Yêu cầu:
# 1. Vẽ scatter plot giữa hp (horsepower) và mpg
# 2. Tô màu các điểm theo số cy-lanh (cyl)
# 3. Thêm đường hồi quy tuyến tính
# 4. Thêm legend giải thích màu
# 5. Nhận xét về mối quan hệ giữa hp và mpg
# Chuẩn bị dữ liệu: Chuyển biến 'cyl' thành dạng factor để ggplot hiểu là biến phân loại (tô màu riêng biệt)
library(ggplot2)

# Chuyển cyl sang factor
mtcars$cyl <- as.factor(mtcars$cyl)

ggplot(mtcars, aes(x = hp, y = mpg)) +  # Không để color ở đây
  geom_point(aes(color = cyl), size = 3) +  # Chỉ tô màu cho các điểm
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Vẽ 1 đường chung (màu đen)
  labs(
    title = "Đường hồi quy tuyến tính chung cho toàn bộ dữ liệu",
    x = "Mã lực (hp)",
    y = "Số dặm mỗi gallon (mpg)"
  ) +
  theme_minimal()
# ------------------------------------------------------------------------------
# Bài tập 5: Nhiều biểu đồ
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu mtcars
library(ggplot2)
# Yêu cầu:
# Tạo một figure với 4 biểu đồ (2x2) để phân tích biến hp:
# 1. Histogram của hp
# 2. Box plot của hp
# 3. Box plot so sánh hp theo cyl
# 4. Scatter plot hp vs mpg
data(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
# 1. Histogram của hp
p1 <- ggplot(mtcars, aes(x = hp)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "white") +
  labs(title = "Histogram of HP", x = "Horsepower") +
  theme_minimal()

# 2. Box plot của hp (tổng quát)
p2 <- ggplot(mtcars, aes(y = hp)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot of HP", y = "Horsepower") +
  theme_minimal()

# 3. Box plot so sánh hp theo cyl
p3 <- ggplot(mtcars, aes(x = cyl, y = hp, fill = cyl)) +
  geom_boxplot() +
  labs(title = "HP by Cylinder", x = "Cylinders", y = "Horsepower") +
  theme_minimal() +
  theme(legend.position = "none") # Ẩn legend cho gọn

# 4. Scatter plot hp vs mpg
p4 <- ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "darkred") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "HP vs MPG Scatter", x = "Horsepower", y = "Miles per Gallon") +
  theme_minimal()
# Chia màn hình thành lưới 2 hàng, 2 cột
par(mfrow = c(2, 2))

# 1. Histogram của hp
hist(mtcars$hp, col = "skyblue", main = "Histogram of HP", xlab = "Horsepower")

# 2. Box plot của hp
boxplot(mtcars$hp, col = "lightgreen", main = "Boxplot of HP", ylab = "Horsepower")

# 3. Box plot so sánh hp theo cyl
boxplot(hp ~ cyl, data = mtcars, col = c("orange", "tomato", "darkred"), 
        main = "HP by Cylinder", xlab = "Cylinders", ylab = "Horsepower")

# 4. Scatter plot hp vs mpg
plot(mtcars$hp, mtcars$mpg, col = "blue", pch = 19,
     main = "HP vs MPG", xlab = "Horsepower", ylab = "MPG")
abline(lm(mpg ~ hp, data = mtcars), col = "red", lwd = 2) # Thêm đường hồi quy

# Trả lại trạng thái màn hình bình thường (1x1) sau khi vẽ xong
par(mfrow = c(1, 1))caption = "Nguồn dữ liệu: mtcars"
  )
# ------------------------------------------------------------------------------
# Bài tập 6: Tổng hợp
# ------------------------------------------------------------------------------

# Tạo dữ liệu bán hàng của 4 quý
Q1 <- c(100, 120, 110, 130)
Q2 <- c(150, 140, 160, 155)
Q3 <- c(180, 170, 190, 185)
Q4 <- c(200, 210, 195, 220)
products <- c("Sản phẩm A", "Sản phẩm B", "Sản phẩm C", "Sản phẩm D")

# Tạo ma trận doanh thu (hàng là Sản phẩm, cột là Quý)
sales_matrix <- matrix(c(Q1, Q2, Q3, Q4), nrow = 4, byrow = FALSE)
colnames(sales_matrix) <- c("Q1", "Q2", "Q3", "Q4")
rownames(sales_matrix) <- products

# Yêu cầu:
# 1. Vẽ grouped bar chart so sánh doanh thu 4 quý
# 2. Vẽ line plot cho từng sản phẩm qua 4 quý
# 3. Tính tổng doanh thu mỗi quý, vẽ bar chart
# 4. Tạo figure 2x2 hiển thị:
#    - Grouped bar chart
#    - Line plot tất cả sản phẩm
#    - Pie chart tổng doanh thu mỗi quý
#    - Bar chart tổng doanh thu mỗi sản phẩm
# Thiết lập khung hình 2 hàng, 2 cột
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1)) 

# --- 1. Grouped Bar Chart ---
barplot(sales_matrix, 
        beside = TRUE, 
        col = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
        main = "Doanh thu 4 quý theo sản phẩm",
        ylab = "Doanh thu", 
        legend.text = products,
        args.legend = list(x = "topleft", bty = "n", cex = 0.8))

# --- 2. Line Plot (Tất cả sản phẩm qua 4 quý) ---
# Vẽ sản phẩm A trước
plot(sales_matrix[1,], type = "b", col = "#E41A1C", lwd = 2, ylim = c(100, 250),
     xaxt = "n", xlab = "Quý", ylab = "Doanh thu", main = "Xu hướng theo sản phẩm")
axis(1, at = 1:4, labels = c("Q1", "Q2", "Q3", "Q4"))
# Vẽ thêm các sản phẩm còn lại
lines(sales_matrix[2,], type = "b", col = "#377EB8", lwd = 2)
lines(sales_matrix[3,], type = "b", col = "#4DAF4A", lwd = 2)
lines(sales_matrix[4,], type = "b", col = "#984EA3", lwd = 2)

# --- 3. Pie Chart (Tổng doanh thu mỗi quý) ---
total_per_quarter <- colSums(sales_matrix)
pie(total_per_quarter, 
    labels = paste(names(total_per_quarter), ":", total_per_quarter),
    col = terrain.colors(4),
    main = "Tỷ trọng doanh thu theo Quý")

# --- 4. Bar Chart (Tổng doanh thu mỗi sản phẩm) ---
total_per_product <- rowSums(sales_matrix)
barplot(total_per_product, 
        col = "orange", 
        main = "Tổng doanh thu theo Sản phẩm",
        names.arg = c("Sp A", "Sp B", "Sp C", "Sp D"),
        ylab = "Doanh thu")

# Trả lại cấu hình mặc định
par(mfrow = c(1, 1))
# ==============================================================================
# TÀI LIỆU THAM KHẢO
# ==============================================================================

# 1. R Graphics Cookbook: https://r-graphics.org/
# 2. Quick-R Graphics: https://www.statmethods.net/graphs/
# 3. R Documentation: ?plot, ?hist, ?boxplot, ?barplot
# 4. R Color Chart: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# ==============================================================================
# TỔNG KẾT
# ==============================================================================

# Những điểm cần nhớ:

# 1. ✅ Chọn biểu đồ phù hợp với loại dữ liệu:
#    - Bar chart: Dữ liệu phân loại
#    - Histogram: Phân phối dữ liệu liên tục
#    - Box plot: So sánh nhóm, tìm outliers
#    - Scatter plot: Mối quan hệ giữa 2 biến
#    - Line plot: Xu hướng theo thời gian
#    - Pie chart: Tỷ lệ phần trăm (ít nhóm)

# 2. ✅ Luôn thêm tiêu đề và nhãn trục rõ ràng

# 3. ✅ Sử dụng màu sắc hợp lý:
#    - Không quá nhiều màu
#    - Màu có ý nghĩa (đỏ = cảnh báo, xanh lá = tốt)
#    - Đảm bảo đọc được khi in đen trắng

# 4. ✅ Box plot giúp:
#    - Thấy trung vị, Q1, Q3
#    - Phát hiện outliers
#    - So sánh nhiều nhóm

# 5. ✅ Histogram vs Bar chart:
#    - Histogram: Dữ liệu liên tục, không có khoảng cách giữa cột
#    - Bar chart: Dữ liệu phân loại, có khoảng cách

# 6. ✅ Sử dụng par(mfrow) để vẽ nhiều biểu đồ cùng lúc

# 7. ✅ Lưu biểu đồ: png(), pdf(), jpeg() + dev.off()

# Quy trình vẽ biểu đồ tốt:
# 1. Xác định mục đích: Muốn truyền đạt thông tin gì?
# 2. Chọn loại biểu đồ phù hợp
# 3. Vẽ biểu đồ cơ bản
# 4. Thêm tiêu đề, nhãn, màu sắc
# 5. Kiểm tra xem biểu đồ có dễ hiểu không
# 6. Lưu lại nếu cần

# Lưu ý quan trọng:
# - Biểu đồ phải đơn giản, dễ hiểu
# - Không thêm quá nhiều thông tin vào một biểu đồ
# - Luôn nghĩ về người xem
# - "A picture is worth a thousand words" - Một hình ảnh đáng giá ngàn lời
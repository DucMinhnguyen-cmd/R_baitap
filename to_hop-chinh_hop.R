
# ==============================================================================
# BÀI 10: ĐẠI SỐ TỔ HỢP VÀ XÁC SUẤT CƠ BẢN
# ==============================================================================
# Mục tiêu học tập:
# - Hiểu và tính được giai thừa, hoán vị, chỉnh hợp, tổ hợp
# - Phân biệt được hoán vị, chỉnh hợp và tổ hợp
# - Áp dụng đại số tổ hợp vào bài toán xác suất thực tế
# - Sử dụng R để tính toán các bài toán tổ hợp
# - Giải quyết được các bài toán đếm trong thực tế
# ==============================================================================

# ------------------------------------------------------------------------------
# 10.1 Giai thừa (Factorial)
# ------------------------------------------------------------------------------

# 10.1.1 Giai thừa là gì?
#
# Giai thừa của một số nguyên dương n, ký hiệu là n!, là tích của tất cả 
# các số nguyên dương từ 1 đến n.
#
# Công thức:
# n! = 1 × 2 × 3 × ... × n
#
# Quy ước đặc biệt:
# 0! = 1  (theo quy ước)

# 10.1.2 Ví dụ tính giai thừa
#
# Ví dụ 1: Tính 5!
# 5! = 1 × 2 × 3 × 4 × 5 = 120
#
# Ví dụ 2: Tính 3!
# 3! = 1 × 2 × 3 = 6
#
# Ví dụ 3: Tính 0!
# 0! = 1  (theo quy ước)

# 10.1.3 Tính giai thừa trong R

# Hàm factorial() có sẵn trong R

# Tính 5!
factorial(5)  # Kết quả: 120

# Tính 0!
factorial(0)  # Kết quả: 1

# Tính 10!
factorial(10)  # Kết quả: 3,628,800

# Tính giai thừa cho nhiều số cùng lúc
n <- 1:10
factorial(n)
# Kết quả: 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800

# 10.1.4 Ứng dụng của giai thừa
#
# Câu hỏi: Có bao nhiêu cách sắp xếp 5 quyển sách khác nhau trên kệ?
#
# Giải thích:
# - Vị trí thứ 1: có 5 lựa chọn
# - Vị trí thứ 2: có 4 lựa chọn (đã dùng 1 quyển)
# - Vị trí thứ 3: có 3 lựa chọn
# - Vị trí thứ 4: có 2 lựa chọn
# - Vị trí thứ 5: có 1 lựa chọn
#
# Đáp án: 5 × 4 × 3 × 2 × 1 = 5! = 120 cách

factorial(5)  # 120

# 10.1.5 Viết hàm tính giai thừa

# Tự viết hàm tính giai thừa (để hiểu logic)
my_factorial <- function(n) {
  if (n < 0) {
    return("Không tính được giai thừa của số âm")
  }
  if (n == 0 || n == 1) {
    return(1)
  }
  
  result <- 1
  for (i in 2:n) {
    result <- result * i
  }
  
  return(result)
}

# Test
my_factorial(5)   # 120
my_factorial(0)   # 1
my_factorial(10)  # 3628800

# ------------------------------------------------------------------------------
# 10.2 Hoán vị (Permutation)
# ------------------------------------------------------------------------------

# 10.2.1 Hoán vị là gì?
#
# Hoán vị là cách sắp xếp TẤT CẢ các phần tử theo thứ tự.
#
# Đặc điểm:
# - Sắp xếp TẤT CẢ n phần tử
# - CÓ QUAN TÂM ĐẾN THỨ TỰ
#
# Ký hiệu: P(n)
#
# Công thức:
# P(n) = n!

# 10.2.2 Ví dụ về hoán vị
#
# Ví dụ 1: Có bao nhiêu cách sắp xếp 3 người A, B, C vào 3 ghế?
#
# Giải thích:
# - Sắp xếp TẤT CẢ 3 người
# - ABC, ACB, BAC, BCA, CAB, CBA → 6 cách
#
# P(3) = 3! = 6
#
# Ví dụ 2: Có bao nhiêu cách sắp xếp 5 quyển sách trên kệ?
# P(5) = 5! = 120

# 10.2.3 Tính hoán vị trong R

# Hoán vị của n phần tử = n!

# Ví dụ 1: Sắp xếp 3 người
factorial(3)  # 6

# Ví dụ 2: Sắp xếp 5 quyển sách
factorial(5)  # 120

# Ví dụ 3: Sắp xếp 7 học sinh
factorial(7)  # 5040

# ------------------------------------------------------------------------------
# 10.3 Chỉnh hợp (Arrangement)
# ------------------------------------------------------------------------------

# 10.3.1 Chỉnh hợp là gì?
#
# Chỉnh hợp là cách chọn và sắp xếp MỘT PHẦN các phần tử theo thứ tự.
#
# Đặc điểm:
# - Chọn r phần tử từ n phần tử
# - CÓ QUAN TÂM ĐẾN THỨ TỰ
# - Ví dụ: ABC khác CBA
#
# Ký hiệu: A(n, r) hoặc P(n, r)
#
# Công thức:
# A(n, r) = n! / (n - r)!
#
# Trong đó:
# - n: tổng số phần tử
# - r: số phần tử được chọn

# 10.3.2 Giải thích công thức
#
# A(n, r) = n! / (n - r)!
#
# - Vị trí thứ 1: có n lựa chọn
# - Vị trí thứ 2: có (n-1) lựa chọn
# - Vị trí thứ 3: có (n-2) lựa chọn
# - ...
# - Vị trí thứ r: có (n-r+1) lựa chọn
#
# Tổng cộng: n × (n-1) × (n-2) × ... × (n-r+1)
#
# Ví dụ: A(5, 3) = 5 × 4 × 3 = 60

# 10.3.3 Ví dụ về chỉnh hợp
#
# Ví dụ 1: Có 5 người, chọn 3 người và xếp vào 3 vị trí: 
# Lớp trưởng, Lớp phó, Thư ký. Hỏi có bao nhiêu cách?
#
# Phân tích:
# - Chọn 3 người từ 5 người
# - Có quan tâm thứ tự (vị trí khác nhau)
# - Dùng CHỈNH HỢP
#
# A(5, 3) = 5! / (5-3)! = 5! / 2! = 120 / 2 = 60
#
# Ví dụ 2: Có bao nhiêu số có 4 chữ số khác nhau được tạo từ 
# 1, 2, 3, 4, 5, 6?
#
# Phân tích:
# - Chọn 4 chữ số từ 6 chữ số
# - Có thứ tự (1234 khác 4321)
# - Dùng CHỈNH HỢP
#
# A(6, 4) = 6! / 2! = 360

# 10.3.4 Tính chỉnh hợp trong R

# Viết hàm tính chỉnh hợp
chinh_hop <- function(n, r) {
  factorial(n) / factorial(n - r)
}

# Ví dụ 1: Chọn 3 người từ 5 người để xếp vào 3 vị trí
chinh_hop(5, 3)  # 60

# Ví dụ 2: Tạo số 4 chữ số từ 6 chữ số
chinh_hop(6, 4)  # 360

# Ví dụ 3: Chọn 2 chữ cái từ 26 chữ cái (có thứ tự)
chinh_hop(26, 2)  # 650

# 10.3.5 Bài tập thực hành
#
# Bài 1: Một lớp có 30 học sinh. Giáo viên muốn chọn 3 học sinh làm 
# lớp trưởng, lớp phó, thủ quỹ. Hỏi có bao nhiêu cách chọn?

# Có thứ tự: lớp trưởng ≠ lớp phó ≠ thủ quỹ
chinh_hop(30, 3)  # 24,360 cách

# Bài 2: Có bao nhiêu cách xếp 3 người vào 10 ghế được đánh số từ 1-10?

chinh_hop(10, 3)  # 720 cách

# ------------------------------------------------------------------------------
# 10.4 Tổ hợp (Combination)
# ------------------------------------------------------------------------------

# 10.4.1 Tổ hợp là gì?
#
# Tổ hợp là cách chọn các phần tử KHÔNG QUAN TÂM ĐẾN THỨ TỰ.
#
# Đặc điểm:
# - Chọn r phần tử từ n phần tử
# - KHÔNG QUAN TÂM ĐẾN THỨ TỰ
# - Ví dụ: {A, B, C} = {C, B, A}
#
# Ký hiệu: C(n, r)
#
# Công thức:
# C(n, r) = n! / (r! × (n - r)!)
#
# Trong đó:
# - n: tổng số phần tử
# - r: số phần tử được chọn

# 10.4.2 Phân biệt Hoán vị, Chỉnh hợp và Tổ hợp
#
# | Thuật ngữ      | Ký hiệu  | Công thức        | Chọn      | Thứ tự  |
# |----------------|----------|------------------|-----------|---------|
# | Hoán vị        | P(n)     | n!               | TẤT CẢ n  | Có      |
# | Chỉnh hợp      | A(n,r)   | n!/(n-r)!        | r từ n    | Có      |
# | Tổ hợp         | C(n,r)   | n!/(r!(n-r)!)    | r từ n    | Không   |
#
# Mẹo nhớ:
# - Hoán vị: Xếp TẤT CẢ (All)
# - Chỉnh hợp: Chọn MỘT PHẦN + có VỊ TRÍ (Arrangement)
# - Tổ hợp: Chọn MỘT PHẦN + KHÔNG VỊ TRÍ (Combination)

# 10.4.3 Ví dụ về tổ hợp
#
# Ví dụ 1: Từ 5 học sinh, chọn 3 học sinh vào đội thi. 
# Hỏi có bao nhiêu cách chọn?
#
# Phân tích:
# - Chọn 3 học sinh từ 5 học sinh
# - Không quan tâm thứ tự (chọn A, B, C = chọn C, B, A)
# - Dùng TỔ HỢP
#
# C(5, 3) = 5! / (3! × 2!) = 120 / (6 × 2) = 10 cách
#
# Ví dụ 2: Trong một bộ bài 52 lá, có bao nhiêu cách rút 5 lá bài?
# C(52, 5) = 52! / (5! × 47!) = 2,598,960 cách

# 10.4.4 Tính tổ hợp trong R

# Hàm choose() có sẵn trong R

# Ví dụ 1: Chọn 3 học sinh từ 5 học sinh
choose(5, 3)  # 10

# Ví dụ 2: Chọn 5 lá bài từ 52 lá
choose(52, 5)  # 2,598,960

# Ví dụ 3: Chọn 3 câu hỏi từ 20 câu
choose(20, 3)  # 1,140

# Tính tổ hợp cho nhiều giá trị
n <- 10
r <- 0:10
results <- data.frame(n = n, r = r, combinations = choose(n, r))
print(results)

# 10.4.5 So sánh Chỉnh hợp và Tổ hợp

# Ví dụ: Từ 5 phần tử, chọn 3 phần tử

# Chỉnh hợp (có thứ tự)
chinh_hop(5, 3)  # 60

# Tổ hợp (không thứ tự)
choose(5, 3)     # 10

# Mối quan hệ: A(n,r) = C(n,r) × r!
chinh_hop(5, 3) == choose(5, 3) * factorial(3)  # TRUE
# 60 = 10 × 6

# 10.4.6 Bài tập thực hành
#
# Bài 1: Một lớp có 30 học sinh. Chọn 5 học sinh để tham gia hoạt động 
# ngoại khóa (không phân biệt vai trò). Hỏi có bao nhiêu cách chọn?

# Không có thứ tự → Tổ hợp
choose(30, 5)  # 142,506 cách

# Bài 2: Trong một đề thi có 10 câu, sinh viên phải chọn 6 câu để làm. 
# Hỏi có bao nhiêu cách chọn?

# Không quan tâm thứ tự → Tổ hợp
choose(10, 6)  # 210 cách

# ------------------------------------------------------------------------------
# 10.5 So sánh và Lựa chọn
# ------------------------------------------------------------------------------

# 10.5.1 Bảng tổng hợp
#
# | Tình huống                      | Công thức          | Ví dụ                         |
# |---------------------------------|--------------------|-------------------------------|
# | Xếp TẤT CẢ n phần tử            | P(n) = n!          | Xếp 5 người vào 5 ghế         |
# | Chọn r từ n, CÓ VỊ TRÍ          | A(n,r) = n!/(n-r)! | Chọn 3 từ 5 vào 3 vị trí      |
# | Chọn r từ n, KHÔNG VỊ TRÍ       | C(n,r)             | Chọn 3 từ 5 thành nhóm        |

# 10.5.2 Cách nhận biết
#
# Các từ khóa:
#
# 1. Hoán vị (P):
#    - "Sắp xếp TẤT CẢ"
#    - "Có bao nhiêu cách xếp n phần tử"
#
# 2. Chỉnh hợp (A):
#    - "Chọn... vào các VỊ TRÍ"
#    - "Lớp trưởng, Lớp phó, Thư ký"
#    - "Ghế số 1, ghế số 2, ghế số 3"
#
# 3. Tổ hợp (C):
#    - "Chọn NHÓM"
#    - "Đội thi", "Ban tổ chức"
#    - "Không phân biệt vai trò"

# 10.5.3 Ví dụ minh họa
#
# Bài toán: 5 học sinh: A, B, C, D, E
#
# Câu 1: Có bao nhiêu cách SẮP XẾP 5 học sinh thành 1 hàng?
# → Hoán vị:
factorial(5)  # 120

# Câu 2: Có bao nhiêu cách chọn 3 học sinh làm 
# Lớp trưởng, Lớp phó, Thư ký?
# → Chỉnh hợp: A làm LT ≠ A làm LP
chinh_hop(5, 3)  # 60

# Câu 3: Có bao nhiêu cách chọn 3 học sinh vào đội thi?
# → Tổ hợp: Chọn A,B,C = Chọn C,B,A
choose(5, 3)  # 10

# So sánh kết quả
cat("Hoán vị P(5) =", factorial(5), "\n")
cat("Chỉnh hợp A(5,3) =", chinh_hop(5, 3), "\n")
cat("Tổ hợp C(5,3) =", choose(5, 3), "\n")

# ------------------------------------------------------------------------------
# 10.6 Ứng dụng trong Xác suất
# ------------------------------------------------------------------------------

# 10.6.1 Xác suất là gì?
#
# Xác suất là khả năng xảy ra của một sự kiện.
#
# Công thức:
# P(A) = Số kết quả thuận lợi / Tổng số kết quả có thể
#
# Giá trị: 0 ≤ P(A) ≤ 1
# - P(A) = 0: Sự kiện không thể xảy ra
# - P(A) = 1: Sự kiện chắc chắn xảy ra
# - 0 < P(A) < 1: Sự kiện có thể xảy ra

# 10.6.2 Ví dụ về xác suất

# Ví dụ 1: Tung đồng xu
# Mặt ngửa hoặc mặt sấp
# P(Ngửa) = 1/2 = 0.5

prob_heads <- 1/2
cat("Xác suất mặt ngửa:", prob_heads, "\n")

# Ví dụ 2: Gieo xúc xắc
# 6 mặt: 1, 2, 3, 4, 5, 6
# P(ra số 6) = 1/6

prob_six <- 1/6
cat("Xác suất ra số 6:", round(prob_six, 3), "\n")  # 0.167

# 10.6.3 Ứng dụng tổ hợp trong xác suất

# Ví dụ 1: Xác suất rút 4 quân Át từ bộ bài 52 lá
#
# Phân tích:
# - Tổng số cách chọn 4 lá: C(52, 4)
# - Số cách chọn 4 quân Át: C(4, 4) = 1
# - Xác suất = 1 / C(52, 4)

# Tổng số cách chọn 4 lá
total_ways <- choose(52, 4)
cat("Tổng số cách chọn 4 lá:", total_ways, "\n")

# Số cách chọn 4 quân Át
ace_ways <- choose(4, 4)
cat("Số cách chọn 4 Át:", ace_ways, "\n")

# Xác suất
prob_4_aces <- ace_ways / total_ways
cat("Xác suất rút 4 quân Át:", prob_4_aces, "\n")
cat("Xác suất:", format(prob_4_aces, scientific = FALSE), "\n")
# Rất nhỏ: 0.000003694...

# Ví dụ 2: Xác suất có đúng 2 quân Át trong 5 lá bài
#
# Phân tích:
# - Chọn 2 Át từ 4 quân Át: C(4, 2)
# - Chọn 3 lá khác từ 48 lá còn lại: C(48, 3)
# - Tổng số cách: C(52, 5)

# Số cách chọn 2 Át và 3 lá khác
ways_2_aces <- choose(4, 2) * choose(48, 3)
cat("Số cách chọn 2 Át và 3 lá khác:", ways_2_aces, "\n")

# Tổng số cách chọn 5 lá
total_ways_5 <- choose(52, 5)
cat("Tổng số cách chọn 5 lá:", total_ways_5, "\n")

# Xác suất
prob_2_aces <- ways_2_aces / total_ways_5
cat("Xác suất có đúng 2 quân Át:", round(prob_2_aces, 4), "\n")  # 0.0399

# Ví dụ 3: Xổ số - Chọn 6 số từ 45 số

# Tổng số cách chọn 6 số từ 45 số
total_combinations <- choose(45, 6)
cat("Tổng số tổ hợp:", total_combinations, "\n")

# Xác suất trúng jackpot (chọn đúng 6 số)
prob_jackpot <- 1 / total_combinations
cat("Xác suất trúng jackpot:", prob_jackpot, "\n")
cat("Tỷ lệ: 1 trên", total_combinations, "\n")
# 1 trên 8,145,060

# 10.6.4 Ví dụ thực tế khác

# Ví dụ 4: Một lớp có 30 sinh viên, trong đó có 12 nữ. 
# Chọn ngẫu nhiên 5 sinh viên. Tính xác suất có đúng 3 sinh viên nữ.

# Tổng số cách chọn 5 sinh viên từ 30
total <- choose(30, 5)

# Số cách chọn 3 nữ từ 12 nữ
female_ways <- choose(12, 3)

# Số cách chọn 2 nam từ 18 nam
male_ways <- choose(18, 2)

# Số cách chọn 3 nữ và 2 nam
favorable <- female_ways * male_ways

# Xác suất
prob <- favorable / total
cat("Xác suất có đúng 3 nữ:", round(prob, 4), "\n")

# ------------------------------------------------------------------------------
# 10.7 Tam giác Pascal
# ------------------------------------------------------------------------------

# 10.7.1 Tam giác Pascal là gì?
#
# Tam giác Pascal là một cách sắp xếp các số tổ hợp thành hình tam giác.
#
# Đặc điểm:
# - Hàng thứ n chứa các giá trị C(n, 0), C(n, 1), ..., C(n, n)
# - Mỗi số = tổng 2 số ở hàng trên
#
# Ví dụ:
#          1              (hàng 0)
#        1   1            (hàng 1)
#      1   2   1          (hàng 2)
#    1   3   3   1        (hàng 3)
#  1   4   6   4   1      (hàng 4)

# 10.7.2 Vẽ tam giác Pascal trong R

# Hàm vẽ tam giác Pascal
pascal_triangle <- function(n) {
  for (i in 0:n) {
    # In khoảng trắng để căn giữa
    cat(rep(" ", n - i), sep = "")
    
    # In các giá trị tổ hợp
    for (j in 0:i) {
      cat(choose(i, j), " ")
    }
    cat("\n")
  }
}

# Vẽ tam giác Pascal với 6 hàng
pascal_triangle(6)

# 10.7.3 Tính chất của tam giác Pascal

# Tính chất 1: Tổng các số trong hàng n = 2^n
n <- 5
row_sum <- sum(choose(n, 0:n))
cat("Tổng hàng", n, ":", row_sum, "\n")
cat("2^", n, "=", 2^n, "\n")
# Tổng hàng 5: 32
# 2^5 = 32

# Tính chất 2: Đối xứng: C(n, r) = C(n, n-r)
n <- 10
r <- 3
cat("C(10, 3) =", choose(n, r), "\n")
cat("C(10, 7) =", choose(n, n-r), "\n")
# Cả hai đều = 120

# ------------------------------------------------------------------------------
# 10.8 Bài tập nâng cao
# ------------------------------------------------------------------------------

# Bài tập 1: Chọn đội bóng
#
# Một câu lạc bộ có 20 thành viên, trong đó có 8 nữ. Cần chọn 1 đội bóng 
# chuyền gồm 6 người, trong đó phải có ít nhất 2 nữ. Hỏi có bao nhiêu cách chọn?

# Giải:
# Ít nhất 2 nữ = 2 nữ + 3 nữ + 4 nữ + 5 nữ + 6 nữ

# Cách 1: Tính từng trường hợp
case_2f <- choose(8, 2) * choose(12, 4)  # 2 nữ, 4 nam
case_3f <- choose(8, 3) * choose(12, 3)  # 3 nữ, 3 nam
case_4f <- choose(8, 4) * choose(12, 2)  # 4 nữ, 2 nam
case_5f <- choose(8, 5) * choose(12, 1)  # 5 nữ, 1 nam
case_6f <- choose(8, 6) * choose(12, 0)  # 6 nữ, 0 nam

total_ways <- case_2f + case_3f + case_4f + case_5f + case_6f
cat("Tổng số cách:", total_ways, "\n")

# Cách 2: Tổng - Bù
# Tổng các cách - (0 nữ + 1 nữ)
total_all <- choose(20, 6)
case_0f <- choose(8, 0) * choose(12, 6)
case_1f <- choose(8, 1) * choose(12, 5)
total_ways_2 <- total_all - case_0f - case_1f
cat("Tổng số cách (cách 2):", total_ways_2, "\n")

# Bài tập 2: Chia nhóm
#
# Một lớp có 24 học sinh. Giáo viên muốn chia thành 3 nhóm: 
# Nhóm A (8 người), Nhóm B (8 người), Nhóm C (8 người). 
# Hỏi có bao nhiêu cách chia?

# Lưu ý: Các nhóm có thể hoán đổi cho nhau
# Công thức: C(24,8) × C(16,8) × C(8,8) / 3!

ways <- choose(24, 8) * choose(16, 8) * choose(8, 8)
# Chia cho 3! vì 3 nhóm hoán đổi cho nhau
total <- ways / factorial(3)
cat("Số cách chia:", total, "\n")

# ==============================================================================
# BÀI TẬP THỰC HÀNH
# ==============================================================================

# ------------------------------------------------------------------------------
# Bài tập 1: Phân biệt P, A, C
# ------------------------------------------------------------------------------

# Xác định mỗi bài toán sau dùng Hoán vị, Chỉnh hợp hay Tổ hợp, sau đó tính:
#
# 1. Có bao nhiêu cách sắp xếp 6 quyển sách trên kệ?
# 2. Có bao nhiêu cách chọn 3 học sinh từ 15 học sinh vào đội thi 
#    (không phân biệt vai trò)?
# 3. Có bao nhiêu cách chọn 3 học sinh từ 15 học sinh để làm 
#    Lớp trưởng, Lớp phó, Thư ký?
# 4. Có bao nhiêu số có 4 chữ số khác nhau từ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9?

# Đáp án:

# 1. Hoán vị - xếp TẤT CẢ
factorial(6)  # 720

# 2. Tổ hợp - chọn KHÔNG VỊ TRÍ
choose(15, 3)  # 455

# 3. Chỉnh hợp - chọn CÓ VỊ TRÍ
chinh_hop(15, 3)  # 2730

# 4. Chỉnh hợp - chọn và xếp
chinh_hop(10, 4)  # 5040

# ------------------------------------------------------------------------------
# Bài tập 2: Bài toán đếm
# ------------------------------------------------------------------------------

# 1. Một đội bóng đá có 15 cầu thủ. Huấn luyện viên cần chọn:
#    - 1 thủ môn từ 2 thủ môn
#    - 4 hậu vệ từ 6 hậu vệ
#    - 4 tiền vệ từ 5 tiền vệ
#    - 2 tiền đạo từ 2 tiền đạo
#    Hỏi có bao nhiêu cách chọn đội hình?

# Mỗi vị trí dùng Tổ hợp (không phân biệt thứ tự trong vị trí)
ways <- choose(2, 1) * choose(6, 4) * choose(5, 4) * choose(2, 2)
cat("Số cách chọn đội hình:", ways, "\n")

thu_mon<-choose(2,1)
hau_ve<-choose(6,4)
tien_ve <- choose(5, 4)
tien_dao <- choose(2, 2)
ways<-thu_mon * hau_ve * tien_dao * tien_ve
cat(sprintf("Thủ môn: %d cách\nHậu vệ: %d cách\nTiền vệ: %d cách\nTiền đạo: %d cách\n", 
            thu_mon, hau_ve, tien_ve, tien_dao))
cat("so cach chon doi hinh:", ways,"cach\n")
# 2. Có bao nhiêu cách chia 12 học sinh thành 3 nhóm, mỗi nhóm 4 người?
# Số cách chọn 4 từ 12, sau đó 4 từ 8, sau đó 4 từ 4
# Chia cho factorial(3) vì 3 nhóm có kích thước bằng nhau và không tên gọi
so_cach <- (choose(12, 4) * choose(8, 4) * choose(4, 4)) / factorial(3)

print(so_cach)
# ------------------------------------------------------------------------------
# Bài tập 3: Xác suất
# ------------------------------------------------------------------------------

# 1. Rút ngẫu nhiên 3 lá bài từ bộ bài 52 lá. Tính xác suất:
#    - Cả 3 lá đều là át
#    - Có đúng 2 lá át
#    - Có ít nhất 1 lá át
total_ways <- choose(52, 3)

# - Cả 3 lá đều là át (Có 4 lá át trong bộ bài)
p_3_at <- choose(4, 3) / total_ways

# - Có đúng 2 lá át (Chọn 2 từ 4 lá át, và 1 từ 48 lá còn lại)
p_2_at <- (choose(4, 2) * choose(48, 1)) / total_ways

# - Có ít nhất 1 lá át (Dùng biến cố đối: 1 - Xác suất không có lá át nào)
p_it_nhat_1_at <- 1 - (choose(48, 3) / total_ways)

cat("BT1 - 3 Át:", p_3_at, "\nBT1 - 2 Át:", p_2_at, "\nBT1 - Ít nhất 1 Át:", p_it_nhat_1_at)
# 2. Một hộp có 10 viên bi, trong đó 6 viên đỏ và 4 viên xanh. 
#    Lấy ngẫu nhiên 3 viên. Tính xác suất:
#    - Cả 3 viên đều đỏ
#    - Có đúng 2 viên đỏ
#    - Có ít nhất 1 viên đỏ
total_bi <- choose(10, 3)

# - Cả 3 viên đều đỏ (Chọn 3 từ 6 viên đỏ)
p_3_do <- choose(6, 3) / total_bi

# - Có đúng 2 viên đỏ (Chọn 2 từ 6 đỏ, và 1 từ 4 xanh)
p_2_do <- (choose(6, 2) * choose(4, 1)) / total_bi

# - Có ít nhất 1 viên đỏ (1 - Xác suất cả 3 viên đều xanh)
p_it_nhat_1_do <- 1 - (choose(4, 3) / total_bi)

cat("BT2 - 3 Đỏ:", p_3_do, "\nBT2 - 2 Đỏ:", p_2_do, "\nBT2 - Ít nhất 1 Đỏ:", p_it_nhat_1_do)
# 3. Một lớp có 25 sinh viên, trong đó 10 nữ. 
#    Chọn ngẫu nhiên 4 sinh viên. Tính xác suất:
#    - Cả 4 đều là nữ
#    - Có đúng 2 nữ
#    - Có ít nhất 1 nữ
total_sv <- choose(25, 4)

# - Cả 4 đều là nữ (Chọn 4 từ 10 nữ)
p_4_nu <- choose(10, 4) / total_sv

# - Có đúng 2 nữ (Chọn 2 từ 10 nữ, và 2 từ 15 nam)
p_2_nu <- (choose(10, 2) * choose(15, 2)) / total_sv

# - Có ít nhất 1 nữ (1 - Xác suất cả 4 đều là nam)
p_it_nhat_1_nu <- 1 - (choose(15, 4) / total_sv)

cat("BT3 - 4 Nữ:", p_4_nu, "\nBT3 - 2 Nữ:", p_2_nu, "\nBT3 - Ít nhất 1 Nữ:", p_it_nhat_1_nu)
# ------------------------------------------------------------------------------
# Bài tập 4: Ứng dụng thực tế
# ------------------------------------------------------------------------------

# 1. Xổ số: Trong xổ số Mega, người chơi chọn 6 số từ 1-45. Tính:
#    - Tổng số tổ hợp có thể
#    - Xác suất trúng jackpot (đúng cả 6 số)
#    - Xác suất trúng giải 2 (đúng 5 số)
# - Tổng số tổ hợp có thể (Chọn 6 số từ 45 số)
tong_to_hop <- choose(45, 6)

# - Xác suất trúng jackpot (Chỉ có duy nhất 1 bộ số đúng cả 6)
p_jackpot <- 1 / tong_to_hop

# - Xác suất trúng giải 2 (Đúng đúng 5 số trong 6 số trúng, và 1 số sai trong 39 số còn lại)
# Công thức: (Chọn 5 từ 6 số trúng) * (Chọn 1 từ 39 số trượt) / Tổng số cách
p_giai_2 <- (choose(6, 5) * choose(39, 1)) / tong_to_hop
# 2. Mật khẩu: Có bao nhiêu mật khẩu dài 6 ký tự, gồm chữ số 0-9, trong đó:
#    - Các ký tự có thể lặp lại?
mk_lap <- 10^6
#    - Các ký tự không được lặp lại?
factorial(10) / factorial(10 - 6)
# ==============================================================================
# CÂU HỎI ÔN TẬP
# ==============================================================================

# 1. Phân biệt Hoán vị, Chỉnh hợp và Tổ hợp?
# 2. Khi nào dùng công thức P(n), A(n,r), C(n,r)?
# 3. Tại sao C(n,r) = C(n, n-r)?
# 4. Giải thích ý nghĩa của tam giác Pascal?
# 5. Trong xác suất, tổ hợp được dùng như thế nào?
# 6. Cho ví dụ thực tế về mỗi loại: P, A, C?
# 7. Tính A(10, 3) và C(10, 3), giải thích tại sao khác nhau?
# 8. Nêu mối quan hệ giữa A(n,r) và C(n,r)?

# ==============================================================================
# TÀI LIỆU THAM KHẢO
# ==============================================================================

# 1. R Documentation: ?factorial, ?choose
# 2. Toán học rời rạc: Giáo trình Đại học
# 3. Xác suất thống kê: Giáo trình cơ bản
# 4. Wolfram MathWorld: http://mathworld.wolfram.com/

# ==============================================================================
# TỔNG KẾT
# ==============================================================================

# Những điểm cần nhớ:
#
# 1. ✅ Giai thừa (n!): Sắp xếp tất cả n phần tử
#    - Công thức: n! = 1 × 2 × 3 × ... × n
#    - Quy ước: 0! = 1
#
# 2. ✅ Hoán vị P(n): Sắp xếp TẤT CẢ
#    - Công thức: P(n) = n!
#    - Xếp TẤT CẢ n phần tử
#    - Ví dụ: Xếp 5 người vào 5 ghế
#
# 3. ✅ Chỉnh hợp A(n,r): Chọn và sắp xếp CÓ THỨ TỰ
#    - Công thức: A(n,r) = n! / (n-r)!
#    - Chọn r từ n, CÓ VỊ TRÍ
#    - ABC ≠ CBA
#
# 4. ✅ Tổ hợp C(n,r): Chọn KHÔNG THỨ TỰ
#    - Công thức: C(n,r) = n! / (r!(n-r)!)
#    - Chọn r từ n, KHÔNG VỊ TRÍ
#    - {A,B,C} = {C,B,A}
#
# 5. ✅ Cách nhận biết:
#    - "Sắp xếp TẤT CẢ" → Hoán vị (P)
#    - "Chọn vào VỊ TRÍ" → Chỉnh hợp (A)
#    - "Chọn NHÓM" → Tổ hợp (C)
#
# 6. ✅ Trong xác suất:
#    - P(A) = Số trường hợp thuận lợi / Tổng số trường hợp
#    - Thường dùng tổ hợp để đếm

# Công thức quan trọng:
#
# Giai thừa:     n! = 1 × 2 × 3 × ... × n
# Hoán vị:       P(n) = n!
# Chỉnh hợp:     A(n,r) = n! / (n-r)!
# Tổ hợp:        C(n,r) = n! / (r! × (n-r)!)
# Mối quan hệ:   A(n,r) = C(n,r) × r!
# Xác suất:      P(A) = |A| / |Ω|

# Lưu ý quan trọng:
# - LUÔN phân tích kỹ đề bài: chọn TẤT CẢ hay MỘT PHẦN?
# - KIỂM TRA xem có quan tâm VỊ TRÍ hay không?
# - VẼ SƠ ĐỒ hoặc liệt kê các trường hợp nhỏ để hiểu bài toán
# - SỬ DỤNG R để tính toán và kiểm tra kết quả
# - THỰC HÀNH nhiều bài tập để thành thạo

# Cập nhật: Tháng 3/2026

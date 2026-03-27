#load dữ liệu
alcohol<- read.csv("D:\\Program\\R\\student-alcohol.csv")
View(alcohol)
#xem 5 dòng đầu tiên
head(alcohol)
#kiểm tra cấu trúc
str(alcohol)
#tóm tắt thống kê
summary(alcohol)

#loại bỏ cột đầu tiên (có thể là cột ID không cần thiết)
head(alcohol[,-1])
alcohol<-alcohol[,-1] #xóa cột 1


#[,-1] lấy tất cả các dòng, loại bỏ cột 1
#luôn kiểm tra trước khi xóa để tránh mất dữ liệu quan trọng

#2.3 xử lý missing data
#tìm các dòng dữ liệu thiếu
alcohol[!complete.cases(alcohol), ]
#đếm số dòng bị thiếu
length(alcohol[!complete.cases(alcohol), ])
#xử lý biến số numeric - age
#kiểm tra phân bố tuổi
summary(alcohol$age)
#tính trung vị (bỏ qua NA)
median(alcohol$age, na.rm=TRUE)
#điền missing values bằng median
alcohol$age[is.na(alcohol$age)]<- median(alcohol$age, na.rm=TRUE)
#kiểm tra xem còn NA không
alcohol$age[is.na(alcohol$age)]

#điền giá trị "other" cho dòng 63(Mjob bị thiếu)
alcohol$Mjob[63]<-"other"

#2.4 bước 3 chuyển đổi categorical data
str(alcohol)
#biến nhị phân đơn giản(binary variables)
#school: GP/MS
summary(factor(alcohol$school))
alcohol$school<- factor(alcohol$school,
                       levels =c("GP", "MS"),
                       labels =c("Gabriel Pereira", "Mousino da Silveira"))
#sex gioi tinh
summary(factor(alcohol$sex))
alcohol$sex<-factor(alcohol$sex,
                    levels=c("F", "M"),
                    labels=c("female", "male"))

#address
summary(factor(alcohol$famsize))
alcohol$famsize<-factor(alcohol$famsize,
               levels=c("GT3","LE3"),
               labels=c("more than 3","less than 3"))
#ordinal factors bien co thu tu
#mother education
summary(factor(alcohol$Medu))
alcohol$Medu<-factor(alcohol$Medu,
                     levels=c(0,1,2,3,4),
                     labels=c("none","primary","secondary","high","university"),
                     ordered = TRUE )
#reason to choose this school
alcohol$reason<-factor(alcohol$reason)

#check
str(alcohol)

#chạy thư viện
library(VIM)

#kiem tra cau truc du lieu
View(chorizonDL)

head(chorizonDL)
#kiem tra cau truc
str(chorizonDL)
#thong ke
summary(chorizonDL)
#tim kiem missing
chorizonDL[!complete.cases(chorizonDL), ]
length(chorizonDL[!complete.cases(chorizonDL), ])

bt<-read.csv("D:\\Program\\R\\data_cleaned_2021.csv", stringsAsFactors = FALSE)
str(bt)
summary(bt)
colSums(is.na(bt))

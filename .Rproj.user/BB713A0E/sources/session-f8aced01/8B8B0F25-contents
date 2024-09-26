#Thuật toán phân lớp
#1. Tiền xử lý
#xóa các dòng có giá trị rỗng (NA)
CustomerData <- na.omit(CustomerData)

# Load thư viện dplyr
library(dplyr)

# Xóa các dòng có tuổi không nằm trong khoảng từ 18 đến 50
CustomerData <- CustomerData %>%
  filter(Tuoi >= 18 & Tuoi <= 50)

#Tạo cột Phạm vi Tuổi để chia ra 2 nhóm tuổi chưa ổn định tài chính và đã ổn định tài chính
CustomerData$`Pham vi Tuoi` <- cut(CustomerData$Tuoi, 
                                       breaks = c(17, 34, 50),
                                       labels = c("18-34", "35-50"))
# Di chuyển cột từ cuối lên vị trí thứ 6
CustomerData <- CustomerData %>%
  select(1:5, `Pham vi Tuoi`, 6:(ncol(CustomerData) - 1), `KH trung thanh`)

##Lưu lại bộ dữ liệu sau tiền xử lý
library(openxlsx)
write.xlsx(CustomerData,"CustomerDatatienxulytrenR.xlsx")

#2. Chạy thuật toán
#Cài đặt và sử dụng các thư viện
install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

# Tạo mô hình cây ra quyết định
tree <- rpart(`KH trung thanh` ~ `Gioi tinh` + `Pham vi Tuoi` + `Tong chi` +
                `So lan mua hang` + `Phuong thuc thanh toan` +
                `Danh gia don hang`,
              data = CustomerData)

# In cây ra quyết định
plot(tree)
text(tree)
prp(tree)
prp(tree, extra = 1, box.col = "lightgreen", branch.lty = 1)


#Thuật toán gom cụm
#1. Tiền xử lý

#Cài đặt và tải các thư viện
install.packages("factoextra")
install.packages("plotly")
install.packages("fastDummies")
library(factoextra)
library(ggplot2)
library(plotly)
library(dplyr)
library(fastDummies)

#Xem cấu trúc CustomerData
str(CustomerData)

#Chuyển dạng dữ liệu của cột 'Pham vi Tuoi' từ Factor thành character
CustomerData$`Pham vi Tuoi` <- as.character(CustomerData$`Pham vi Tuoi`)

#Chọn các cột chạy gom cụm
DataGomcum <- CustomerData[, c(4, 6, 10:15)]

#Bắt đầu xây dataset
dataset <- DataGomcum %>% select_if(is.numeric)
character<- DataGomcum %>% select_if(is.character)

#Chuyển dữ liệu từ số sang chữ
character <- dummy_cols(character,
                        remove_most_frequent_dummy = TRUE)

#gán vào dataset (kiểu số) các data đã được chuyển từ chữ sang số
dataset <- cbind(dataset, character[,5:9])

#scale the dataset
set.seed(40)
dataset [, 1:9] <- scale(dataset[, 1:9])

##Lưu lại bộ dữ liệu sau tiền xử lý
write.xlsx(dataset,"CustomerDatatienxulyGomcumtrenR.xlsx")

#2. Chạy thuật toán gom cụm
set.seed(50)
clusters <- kmeans(dataset, centers = 6, iter.max = 10)
clusters$centers
dataset <- cbind(dataset, clusters$cluster)

#Visualize gom cụm
fviz_cluster(list(data=dataset, cluster=clusters$cluster))
fviz_cluster(clusters, data = dataset, geom = "point", ellipse = TRUE)
fviz_cluster(clusters, data = dataset, geom="point",ellipse.type="euclid")


#Thuật toán kết hợp
#1. Tiền xử lý

# Xóa các dòng trùng lặp dựa trên cột "Mã hóa đơn" và "Tên sản phẩm"
InvoiceData <- InvoiceData %>%distinct(`Ma hoa don`, `Ten san pham`, .keep_all = TRUE)

# Chọn cột mã hóa đơn và cột tên sản phẩm của bộ dữ liệu InvoieData
selected_data <- InvoiceData %>% select("Ma hoa don", "Ten san pham")

#Load thư viện cần
install.packages("caret")
library(caret)

#Mã hóa one-hot cho thuộc tính Tên sản phẩm
onehot <- dummyVars(~ `Ten san pham`, data = selected_data)
data_oneHot <- predict(onehot, selected_data) %>% as.data.frame() 

#chọn cột có kiểu dữ liệu numeric (Ma hoa don), kết hợp với cột đã được mã hóa dạng one-hot
df_final <- selected_data %>% 
  select_if(is.numeric) %>% 
  bind_cols(data_oneHot)

#Đổi tên cột
colnames(df_final) <- c("Ma hoa don","Dau goi thao duoc",	
"Dau rua mat",	"Hair serum",	"Kem chong nang",	"Kem tay",	"Kem xa u toc",
"Lipscrub",	"Mat na ngu moi",	"Son duong","Son moi",	"Sua duong the",
"Sua rua mat",	"Sua tam",	"Xit thom Body Mist")

#Gom nhóm các dòng có cùng Mã hóa đơn
grouped_data <- df_final %>%
  group_by(`Ma hoa don`) %>%
  summarise_all(sum)

write.csv(grouped_data,"InvoiceDatasautienxulyKH.csv")

# Chạy thuật toán kết hợp Apriori
#Sử dụng thư viện
install.packages("arules")
install.packages("arulesViz")
library("arules")
library("arulesViz")

#Truyền vào đọc dữ liệu từ file csv với kiểu dữ liệu các cột là factor
mydata <- read.csv(file.choose(), header = T, colClasses = "factor")

rules <- apriori(mydata, parameter = list(support = 0.5, confidence = 0.75))

# In các luật kết hợp với điều kiện
sortrules <- sort(rules, by = 'lift')[1:20]
inspect(sortrules)

#Visualize 1
plot(sortrules, jitter = 0)

#Visualize 2
plot(sortrules, method = "matrix", measure = "lift")

#Visualize 3
plot(sortrules, method = "matrix", engine = "3d", measure = "lift")

#Visualize 4
plot(sortrules, method = "grouped")

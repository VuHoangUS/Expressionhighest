setwd("D:/baitapR")
row_data <- read.csv("rows.csv", header = TRUE, stringsAsFactors = FALSE)
columns_data <- read.csv("columns.csv", header = TRUE, stringsAsFactors = FALSE)
expression_data <- read.csv("expression.csv", header = FALSE, stringsAsFactors = FALSE)

# Loại bỏ cột đầu tiên (header) của expression
expression_data <- expression_data[,-1]

# Chuyển cột "top_level_structure_abbreviation" thành danh sách hàng
column_names <- columns_data$top_level_structure_abbreviation

# Thêm hàng "top_level_structure_abbreviation" vào expression_data
expression_data <- rbind(column_names, expression_data)

# Gán hàng thứ 1 làm header
library(data.table)
setnames(expression_data, as.character(expression_data[1,]))

# Loại bỏ hàng số 1
expression_data <- expression_data[-1,]

# Tìm số lớn nhất của "NP"
max_NP <- max(sapply(expression_data[, grepl("^NP", names(expression_data))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "ACx"
max_ACx <- max(sapply(expression_data[, grepl("^ACx", names(expression_data))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "VZ"
max_VZ <- max(sapply(expression_data[, grepl("^VZ", names(expression_data))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "THM"
max_THM <- max(sapply(expression_data[, grepl("^THM", names(expression_data))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "BN"
max_BN <- max(sapply(expression_data[, grepl("^BN", names(expression_data))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))


# Sử dụng hàm 'which' để tìm tất cả các vị trí của giá trị trong ma trận
positions_NP <- which(expression_data == max_NP, arr.ind = TRUE)
positions_ACx <- which(expression_data == max_ACx, arr.ind = TRUE)
positions_VZ <- which(expression_data == max_VZ, arr.ind = TRUE)
positions_THM <- which(expression_data == max_THM, arr.ind = TRUE)
positions_BN <- which(expression_data == max_BN, arr.ind = TRUE)


# Vị trí của các max
cat("    ","Tọa độ của max_NP", positions_NP,"
    ",
"Tọa độ của max_ACx", positions_ACx,"
    ",
"Tọa độ của max_VZ", positions_VZ,"
    ",
"Tọa độ của max_THM", positions_THM,"
    ",
"Tọa độ của max_BN", positions_BN)


# Thêm cột "gene.name" của row_data vào expression_data
expression_data_1 <- cbind(row_data$gene.name, expression_data)

# Gọi gene biểu hiện mạnh nhất
value <- expression_data_1[759, 1]
cat("Gen biểu hiện mạnh nhất trên các vùng não là", value)

# Tạo data frame của expression_highest
Gene_in_top_level_structure <- c("Neuronatin / Neural plate", "Neuronatin / Allocortex", "Neuronatin / Ventricular zone", "Neuronatin / Thalamus", "Neuronatin / Basal nuclei")
Value <- c(max_NP, max_ACx, max_VZ, max_THM, max_BN)
expression_highest <- data.frame(Gene_in_top_level_structure, Value)
expression_highest

# Vẽ đồ thị hình hộp
names <- expression_highest$Gene_in_top_level_structure
barplot(Value, 
col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
ylim = c(0, 16), space = 0.5,
ylab = "log2 RPKM", xlab = "Gene / Vùng não", main ="Biểu đồ thể hiện mức độ biểu hiện của gene biểu hiện mạnh nhất trên từng vùng não (log2 RPKM)", names.arg = names)

# Thêm Value vào đồ thị
text(1, 13, max_NP)
text(2.5, 12, max_ACx)
text(4, 12, max_VZ)
text(5.5, 12.6, max_THM)
text(7, 13, max_BN)


-------------------------------------------------------
Task 2:

# Chuyển cột "donor_age" thành danh sách hàng
column_ages <- columns_data$donor_age

# Thêm hàng "donor_age" vào expression_data
expression_data_2 <- rbind(column_ages, expression_data)

# Độ tuổi biểu hiện mạnh nhất của các gene ở các vùng não khác nhau
cat("    ","Độ tuổi biểu hiện mạnh nhất của gene Neuronatin ở vùng Neural plate là", expression_data_2[1, 16],"
    ",
"Độ tuổi biểu hiện mạnh nhất của gene Neuronatin ở vùng Allocortex là", expression_data_2[1, 168],"
    ",
"Độ tuổi biểu hiện mạnh nhất của gene Neuronatin ở vùng Ventricular zone là", expression_data_2[1, 28],"
    ",
"Độ tuổi biểu hiện mạnh nhất của gene Neuronatin ở vùng Thalamus là", expression_data_2[1, 45],"
    ",
"Độ tuổi biểu hiện mạnh nhất của gene Neuronatin ở vùng Basal nuclei là", expression_data_2[1, 103])

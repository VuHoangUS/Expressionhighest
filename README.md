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

# Thêm cột "id" của row_data vào expression_data
expression_data <- cbind(row_data$id, expression_data)


# Sử dụng hàm 'which' để tìm tất cả các vị trí của giá trị trong ma trận
positions_NP <- which(expression_data == max_NP, arr.ind = TRUE)
positions_ACx <- which(expression_data == max_ACx, arr.ind = TRUE)
positions_VZ <- which(expression_data == max_VZ, arr.ind = TRUE)
positions_THM <- which(expression_data == max_THM, arr.ind = TRUE)
positions_BN <- which(expression_data == max_BN, arr.ind = TRUE)











max_np <- max(apply(expression[, np_columns, drop = FALSE], 1, max))

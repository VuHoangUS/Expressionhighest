setwd("D:/baitapR")
row_data <- read.csv("rows.csv", header = TRUE, stringsAsFactors = FALSE)
columns_data <- read.csv("columns.csv", header = TRUE, stringsAsFactors = FALSE)
expression_data <- read.csv("expression.csv", header = FALSE, stringsAsFactors = FALSE)

# Loại bỏ cột đầu tiên (header) của expression
expression_data <- expression_data[,-1]

unique_values <- unique(columns_data$structure_abbreviation)
print(unique_values)

unique_values_1 <- unique(columns_data$structure_name)
print(unique_values_1)

# Chuyển cột "structure_abbreviation" thành danh sách hàng
column_names <- columns_data$structure_abbreviation

# Thêm hàng "structure_abbreviation" vào expression_data
expression_data_1 <- rbind(column_names, expression_data)

# Gán hàng thứ 1 làm header
library(data.table)
setnames(expression_data_1, as.character(expression_data_1[1,]))

# Loại bỏ hàng số 1
expression_data_1 <- expression_data_1[-1,]


# Thêm cột "gene.name" của row_data vào expression_data_1
expression_data_1 <- cbind(row_data$gene.name, expression_data_1)



# 1. DFC
# Lấy tất cả giá trị từ các cột bắt đầu bằng "DFC"
values_DFC <- sapply(expression_data_1[, grepl("^DFC", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_DFC <- max(values_DFC[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_DFC > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_DFC
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não DFC là")
print(result)


top_10_gen_name_DFC <- character(10)
# Tìm tên gene của 10 max DFC
for (i in top_10_max_rows) {
gen_max_DFC <- expression_data_1[i, 1]
cat("Tên gen hàng", i, "là", gen_max_DFC, "\n")
top_10_gen_name_DFC <- c(top_10_gen_name_DFC, gen_max_DFC)
}


# Tìm tên gene của 10 max DFC và lưu vào top_10_gen_name_DFC
top_10_gen_name_DFC <- character(10)

for (i in 1:10) {
  gen_max_DFC <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_DFC[i] <- gen_max_DFC
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_DFC, "\n")
}

Dorsolateral_prefrontal_cortex <- data.frame(Gene_name = top_10_gen_name_DFC, Value = top_10_max_values)
Dorsolateral_prefrontal_cortex


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Dorsolateral prefrontal cortex", xaxt = "n",
        names.arg = top_10_gen_name_DFC, las=2)


text(x = (1:length(top_10_max_values) * 1.2) -0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_DFC,
                  xpd = NA,
                  ## Rotate the labels by 45 degrees.
                  srt = 40, adj=1.1,
                  cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}


#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

# 2. VFC
# Lấy tất cả giá trị từ các cột bắt đầu bằng "VFC"
values_VFC <- sapply(expression_data_1[, grepl("^VFC", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_VFC <- max(values_VFC[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_VFC > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_VFC
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não VFC là")
print(result)


top_10_gen_name_VFC <- character(10)
# Tìm tên gene của 10 max VFC
for (i in top_10_max_rows) {
  gen_max_VFC <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_VFC, "\n")
  top_10_gen_name_VFC <- c(top_10_gen_name_VFC, gen_max_VFC)
}


# Tìm tên gene của 10 max VFC và lưu vào top_10_gen_name_VFC
top_10_gen_name_VFC <- character(10)

for (i in 1:10) {
  gen_max_VFC <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_VFC[i] <- gen_max_VFC
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_VFC, "\n")
}

Ventrolateral_prefrontal_cortex <- data.frame(Gene_name = top_10_gen_name_VFC, Value = top_10_max_values)
Ventrolateral_prefrontal_cortex


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Ventrolateral prefrontal cortex", xaxt = "n",
        names.arg = top_10_gen_name_VFC, las=2)


text(x = (1:length(top_10_max_values) * 1.2) -0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_VFC,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------


# 3. MFC
# Lấy tất cả giá trị từ các cột bắt đầu bằng "MFC"
values_MFC <- sapply(expression_data_1[, grepl("^MFC", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_MFC <- max(values_MFC[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_MFC > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_MFC
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não MFC là")
print(result)


top_10_gen_name_MFC <- character(10)
# Tìm tên gene của 10 max MFC
for (i in top_10_max_rows) {
  gen_max_MFC <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_MFC, "\n")
  top_10_gen_name_MFC <- c(top_10_gen_name_MFC, gen_max_MFC)
}


# Tìm tên gene của 10 max MFC và lưu vào top_10_gen_name_MFC
top_10_gen_name_MFC <- character(10)

for (i in 1:10) {
  gen_max_MFC <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_MFC[i] <- gen_max_MFC
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_MFC, "\n")
}

Anterior_cingulate_cortex <- data.frame(Gene_name = top_10_gen_name_MFC, Value = top_10_max_values)
Anterior_cingulate_cortex


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Anterior cingulate cortex", xaxt = "n",
        names.arg = top_10_gen_name_MFC, las=2)


text(x = (1:length(top_10_max_values) * 1.2) -0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_MFC,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------


# 4. OFC
# Lấy tất cả giá trị từ các cột bắt đầu bằng "OFC"
values_OFC <- sapply(expression_data_1[, grepl("^OFC", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_OFC <- max(values_OFC[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_OFC > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_OFC
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não OFC là")
print(result)


top_10_gen_name_OFC <- character(10)
# Tìm tên gene của 10 max OFC
for (i in top_10_max_rows) {
  gen_max_OFC <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_OFC, "\n")
  top_10_gen_name_OFC <- c(top_10_gen_name_OFC, gen_max_OFC)
}


# Tìm tên gene của 10 max OFC và lưu vào top_10_gen_name_OFC
top_10_gen_name_OFC <- character(10)

for (i in 1:10) {
  gen_max_OFC <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_OFC[i] <- gen_max_OFC
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_OFC, "\n")
}

Orbital_frontal_cortex <- data.frame(Gene_name = top_10_gen_name_OFC, Value = top_10_max_values)
Orbital_frontal_cortex


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Orbital frontal cortex", xaxt = "n",
        names.arg = top_10_gen_name_OFC, las=2)


text(x = (1:length(top_10_max_values) * 1.2) -0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_OFC,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# 5. M1C_S1C
# Lấy tất cả giá trị từ các cột bắt đầu bằng "M1C_S1C"
values_M1C.S1C <- sapply(expression_data_1[, grepl("^M1C.S1C", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_M1C.S1C <- max(values_M1C.S1C[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_M1C.S1C > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_M1C.S1C
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não M1C_S1C là")
print(result)


top_10_gen_name_M1C.S1C <- character(10)
# Tìm tên gene của 10 max M1C_S1C
for (i in top_10_max_rows) {
  gen_max_M1C.S1C <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_M1C.S1C, "\n")
  top_10_gen_name_M1C.S1C <- c(top_10_gen_name_M1C.S1C, gen_max_M1C.S1C)
}


# Tìm tên gene của 10 max M1C_S1C và lưu vào top_10_gen_name_M1C_S1C
top_10_gen_name_M1C.S1C <- character(10)

for (i in 1:10) {
  gen_max_M1C.S1C <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_M1C.S1C[i] <- gen_max_M1C.S1C
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_M1C.S1C, "\n")
}

primary_motor_sensory_cortex <- data.frame(Gene_name = top_10_gen_name_M1C.S1C, Value = top_10_max_values)
primary_motor_sensory_cortex


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Primary motor-sensory cortex", xaxt = "n",
        names.arg = top_10_gen_name_M1C.S1C, las=2)


text(x = (1:length(top_10_max_values) * 1.2) -0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_M1C.S1C,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------


# 6. PCx
# Lấy tất cả giá trị từ các cột bắt đầu bằng "PCx"
values_PCx <- sapply(expression_data_1[, grepl("^PCx", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_PCx <- max(values_PCx[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_PCx > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_PCx
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não PCx là")
print(result)


top_10_gen_name_PCx <- character(10)
# Tìm tên gene của 10 max PCx
for (i in top_10_max_rows) {
  gen_max_PCx <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_PCx, "\n")
  top_10_gen_name_PCx <- c(top_10_gen_name_PCx, gen_max_PCx)
}


# Tìm tên gene của 10 max PCx và lưu vào top_10_gen_name_PCx
top_10_gen_name_PCx <- character(10)

for (i in 1:10) {
  gen_max_PCx <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_PCx[i] <- gen_max_PCx
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_PCx, "\n")
}

parietal_neocortex <- data.frame(Gene_name = top_10_gen_name_PCx, Value = top_10_max_values)
parietal_neocortex


old_par <- par(mar = c(15, 8, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Parietal neocortex", xaxt = "n",
        names.arg = top_10_gen_name_PCx, las=2)


text(x = (1:length(top_10_max_values) * 1.2) -0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_PCx,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# 7. STC
# Lấy tất cả giá trị từ các cột bắt đầu bằng "STC"
values_STC <- sapply(expression_data_1[, grepl("^STC", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_STC <- max(values_STC[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_STC > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_STC
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não STC là")
print(result)


top_10_gen_name_STC <- character(10)
# Tìm tên gene của 10 max STC
for (i in top_10_max_rows) {
  gen_max_STC <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_STC, "\n")
  top_10_gen_name_STC <- c(top_10_gen_name_STC, gen_max_STC)
}


# Tìm tên gene của 10 max STC và lưu vào top_10_gen_name_STC
top_10_gen_name_STC <- character(10)

for (i in 1:10) {
  gen_max_STC <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_STC[i] <- gen_max_STC
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_STC, "\n")
}

parietal_neocortex <- data.frame(Gene_name = top_10_gen_name_STC, Value = top_10_max_values)
parietal_neocortex


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Parietal neocortex", xaxt = "n",
        names.arg = top_10_gen_name_STC, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_STC,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------


# 8. ITC
# Lấy tất cả giá trị từ các cột bắt đầu bằng "ITC"
values_ITC <- sapply(expression_data_1[, grepl("^ITC", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_ITC <- max(values_ITC[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_ITC > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_ITC
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não ITC là")
print(result)


top_10_gen_name_ITC <- character(10)
# Tìm tên gene của 10 max ITC
for (i in top_10_max_rows) {
  gen_max_ITC <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_ITC, "\n")
  top_10_gen_name_ITC <- c(top_10_gen_name_ITC, gen_max_ITC)
}


# Tìm tên gene của 10 max ITC và lưu vào top_10_gen_name_ITC
top_10_gen_name_ITC <- character(10)

for (i in 1:10) {
  gen_max_ITC <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_ITC[i] <- gen_max_ITC
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_ITC, "\n")
}

Inferolateral_temporal_cortex <- data.frame(Gene_name = top_10_gen_name_ITC, Value = top_10_max_values)
Inferolateral_temporal_cortex


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Inferolateral temporal cortex", xaxt = "n",
        names.arg = top_10_gen_name_ITC, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_ITC,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# 9. Ocx
# Lấy tất cả giá trị từ các cột bắt đầu bằng "Ocx"
values_Ocx <- sapply(expression_data_1[, grepl("^Ocx", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_Ocx <- max(values_Ocx[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_Ocx > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_Ocx
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não Ocx là")
print(result)


top_10_gen_name_Ocx <- character(10)
# Tìm tên gene của 10 max Ocx
for (i in top_10_max_rows) {
  gen_max_Ocx <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_Ocx, "\n")
  top_10_gen_name_Ocx <- c(top_10_gen_name_Ocx, gen_max_Ocx)
}


# Tìm tên gene của 10 max Ocx và lưu vào top_10_gen_name_Ocx
top_10_gen_name_Ocx <- character(10)

for (i in 1:10) {
  gen_max_Ocx <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_Ocx[i] <- gen_max_Ocx
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_Ocx, "\n")
}

occipital_neocortex <- data.frame(Gene_name = top_10_gen_name_Ocx, Value = top_10_max_values)
occipital_neocortex


old_par <- par(mar = c(16, 8, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Occipital neocortex", xaxt = "n",
        names.arg = top_10_gen_name_Ocx, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_Ocx,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 45, adj=1.1,
     cex = 0.8)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------


# 10. HIP
# Lấy tất cả giá trị từ các cột bắt đầu bằng "HIP"
values_HIP <- sapply(expression_data_1[, grepl("^HIP", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_HIP <- max(values_HIP[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_HIP > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_HIP
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não HIP là")
print(result)


top_10_gen_name_HIP <- character(10)
# Tìm tên gene của 10 max HIP
for (i in top_10_max_rows) {
  gen_max_HIP <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_HIP, "\n")
  top_10_gen_name_HIP <- c(top_10_gen_name_HIP, gen_max_HIP)
}


# Tìm tên gene của 10 max HIP và lưu vào top_10_gen_name_HIP
top_10_gen_name_HIP <- character(10)

for (i in 1:10) {
  gen_max_HIP <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_HIP[i] <- gen_max_HIP
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_HIP, "\n")
}

hippocampus <- data.frame(Gene_name = top_10_gen_name_HIP, Value = top_10_max_values)
hippocampus


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Hippocampus", xaxt = "n",
        names.arg = top_10_gen_name_HIP, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_HIP,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------


# 11. AMY
# Lấy tất cả giá trị từ các cột bắt đầu bằng "AMY"
values_AMY <- sapply(expression_data_1[, grepl("^AMY", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_AMY <- max(values_AMY[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_AMY > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_AMY
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não AMY là")
print(result)


top_10_gen_name_AMY <- character(10)
# Tìm tên gene của 10 max AMY
for (i in top_10_max_rows) {
  gen_max_AMY <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_AMY, "\n")
  top_10_gen_name_AMY <- c(top_10_gen_name_AMY, gen_max_AMY)
}


# Tìm tên gene của 10 max AMY và lưu vào top_10_gen_name_AMY
top_10_gen_name_AMY <- character(10)

for (i in 1:10) {
  gen_max_AMY <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_AMY[i] <- gen_max_AMY
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_AMY, "\n")
}

amygdaloid_complex <- data.frame(Gene_name = top_10_gen_name_AMY, Value = top_10_max_values)
amygdaloid_complex


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Amygdaloid complex", xaxt = "n",
        names.arg = top_10_gen_name_AMY, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_AMY,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# 12. LGE
# Lấy tất cả giá trị từ các cột bắt đầu bằng "LGE"
values_LGE <- sapply(expression_data_1[, grepl("^LGE", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_LGE <- max(values_LGE[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_LGE > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_LGE
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não LGE là")
print(result)


top_10_gen_name_LGE <- character(10)
# Tìm tên gene của 10 max LGE
for (i in top_10_max_rows) {
  gen_max_LGE <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_LGE, "\n")
  top_10_gen_name_LGE <- c(top_10_gen_name_LGE, gen_max_LGE)
}


# Tìm tên gene của 10 max LGE và lưu vào top_10_gen_name_LGE
top_10_gen_name_LGE <- character(10)

for (i in 1:10) {
  gen_max_LGE <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_LGE[i] <- gen_max_LGE
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_LGE, "\n")
}

lateral_ganglionic_eminence <- data.frame(Gene_name = top_10_gen_name_LGE, Value = top_10_max_values)
lateral_ganglionic_eminence


old_par <- par(mar = c(15, 8, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Lateral ganglionic eminence", xaxt = "n",
        names.arg = top_10_gen_name_LGE, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_LGE,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------


# 13. MGE
# Lấy tất cả giá trị từ các cột bắt đầu bằng "MGE"
values_MGE <- sapply(expression_data_1[, grepl("^MGE", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_MGE <- max(values_MGE[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_MGE > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_MGE
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não MGE là")
print(result)


top_10_gen_name_MGE <- character(10)
# Tìm tên gene của 10 max MGE
for (i in top_10_max_rows) {
  gen_max_MGE <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_MGE, "\n")
  top_10_gen_name_MGE <- c(top_10_gen_name_MGE, gen_max_MGE)
}


# Tìm tên gene của 10 max MGE và lưu vào top_10_gen_name_MGE
top_10_gen_name_MGE <- character(10)

for (i in 1:10) {
  gen_max_MGE <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_MGE[i] <- gen_max_MGE
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_MGE, "\n")
}

medial_ganglionic_eminence <- data.frame(Gene_name = top_10_gen_name_MGE, Value = top_10_max_values)
medial_ganglionic_eminence


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Medial ganglionic eminence", xaxt = "n",
        names.arg = top_10_gen_name_MGE, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_MGE,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# 14. CGE
# Lấy tất cả giá trị từ các cột bắt đầu bằng "CGE"
values_CGE <- sapply(expression_data_1[, grepl("^CGE", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_CGE <- max(values_CGE[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_CGE > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_CGE
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não CGE là")
print(result)


top_10_gen_name_CGE <- character(10)
# Tìm tên gene của 10 max CGE
for (i in top_10_max_rows) {
  gen_max_CGE <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_CGE, "\n")
  top_10_gen_name_CGE <- c(top_10_gen_name_CGE, gen_max_CGE)
}


# Tìm tên gene của 10 max CGE và lưu vào top_10_gen_name_CGE
top_10_gen_name_CGE <- character(10)

for (i in 1:10) {
  gen_max_CGE <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_CGE[i] <- gen_max_CGE
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_CGE, "\n")
}

caudal_ganglionic_eminence <- data.frame(Gene_name = top_10_gen_name_CGE, Value = top_10_max_values)
caudal_ganglionic_eminence


old_par <- par(mar = c(15, 8, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Caudal ganglionic eminence", xaxt = "n",
        names.arg = top_10_gen_name_CGE, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_CGE,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# 15. DTH
# Lấy tất cả giá trị từ các cột bắt đầu bằng "DTH"
values_DTH <- sapply(expression_data_1[, grepl("^DTH", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_DTH <- max(values_DTH[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_DTH > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_DTH
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não DTH là")
print(result)


top_10_gen_name_DTH <- character(10)
# Tìm tên gene của 10 max DTH
for (i in top_10_max_rows) {
  gen_max_DTH <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_DTH, "\n")
  top_10_gen_name_DTH <- c(top_10_gen_name_DTH, gen_max_DTH)
}


# Tìm tên gene của 10 max DTH và lưu vào top_10_gen_name_DTH
top_10_gen_name_DTH <- character(10)

for (i in 1:10) {
  gen_max_DTH <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_DTH[i] <- gen_max_DTH
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_DTH, "\n")
}

dorsal_thalamus <- data.frame(Gene_name = top_10_gen_name_DTH, Value = top_10_max_values)
dorsal_thalamus


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Dorsal thalamus", xaxt = "n",
        names.arg = top_10_gen_name_DTH, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_DTH,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# 16. URL
# Lấy tất cả giá trị từ các cột bắt đầu bằng "URL"
values_URL <- sapply(expression_data_1[, grepl("^URL", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_URL <- max(values_URL[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_URL > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_URL
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não URL là")
print(result)


top_10_gen_name_URL <- character(10)
# Tìm tên gene của 10 max URL
for (i in top_10_max_rows) {
  gen_max_URL <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_URL, "\n")
  top_10_gen_name_URL <- c(top_10_gen_name_URL, gen_max_URL)
}


# Tìm tên gene của 10 max URL và lưu vào top_10_gen_name_URL
top_10_gen_name_URL <- character(10)

for (i in 1:10) {
  gen_max_URL <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_URL[i] <- gen_max_URL
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_URL, "\n")
}

upper_rhombic_lip <- data.frame(Gene_name = top_10_gen_name_URL, Value = top_10_max_values)
upper_rhombic_lip


old_par <- par(mar = c(15, 8, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Upper rhombic lip", xaxt = "n",
        names.arg = top_10_gen_name_URL, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_URL,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# 17. TCx
# Lấy tất cả giá trị từ các cột bắt đầu bằng "TCx"
values_TCx <- sapply(expression_data_1[, grepl("^TCx", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))
values_TCx <- unname(values_TCx)
values_TCx <- data.frame(Value = values_TCx)

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_TCx <- max(values_TCx[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_TCx > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_TCx
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não TCx là")
print(result)


top_10_gen_name_TCx <- character(10)
# Tìm tên gene của 10 max TCx
for (i in top_10_max_rows) {
  gen_max_TCx <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_TCx, "\n")
  top_10_gen_name_TCx <- c(top_10_gen_name_TCx, gen_max_TCx)
}


# Tìm tên gene của 10 max TCx và lưu vào top_10_gen_name_TCx
top_10_gen_name_TCx <- character(10)

for (i in 1:10) {
  gen_max_TCx <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_TCx[i] <- gen_max_TCx
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_TCx, "\n")
}

TCx <- data.frame(Gene_name = top_10_gen_name_TCx, Value = top_10_max_values)
TCx


old_par <- par(mar = c(15, 8, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Temporal neocortex", xaxt = "n",
        names.arg = top_10_gen_name_TCx, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_TCx,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------


# 18. M1C
# Lấy tất cả giá trị từ các cột bắt đầu bằng "M1C"
values_M1C <- sapply(expression_data_1[, grepl("^M1C", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_M1C <- max(values_M1C[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_M1C > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_M1C
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não M1C là")
print(result)


top_10_gen_name_M1C <- character(10)
# Tìm tên gene của 10 max M1C
for (i in top_10_max_rows) {
  gen_max_M1C <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_M1C, "\n")
  top_10_gen_name_M1C <- c(top_10_gen_name_M1C, gen_max_M1C)
}


# Tìm tên gene của 10 max M1C và lưu vào top_10_gen_name_M1C
top_10_gen_name_M1C <- character(10)

for (i in 1:10) {
  gen_max_M1C <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_M1C[i] <- gen_max_M1C
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_M1C, "\n")
}

primary_motor_cortex <- data.frame(Gene_name = top_10_gen_name_M1C, Value = top_10_max_values)
primary_motor_cortex


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Primary motor cortex", xaxt = "n",
        names.arg = top_10_gen_name_M1C, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_M1C,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------


# 19. S1C
# Lấy tất cả giá trị từ các cột bắt đầu bằng "S1C"
values_S1C <- sapply(expression_data_1[, grepl("^S1C", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_S1C <- max(values_S1C[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_S1C > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_S1C
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não S1C là")
print(result)


top_10_gen_name_S1C <- character(10)
# Tìm tên gene của 10 max S1C
for (i in top_10_max_rows) {
  gen_max_S1C <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_S1C, "\n")
  top_10_gen_name_S1C <- c(top_10_gen_name_S1C, gen_max_S1C)
}


# Tìm tên gene của 10 max S1C và lưu vào top_10_gen_name_S1C
top_10_gen_name_S1C <- character(10)

for (i in 1:10) {
  gen_max_S1C <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_S1C[i] <- gen_max_S1C
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_S1C, "\n")
}

primary_somatosensory_cortex <- data.frame(Gene_name = top_10_gen_name_S1C, Value = top_10_max_values)
primary_somatosensory_cortex


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Primary somatosensory cortex", xaxt = "n",
        names.arg = top_10_gen_name_S1C, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_S1C,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# 20. IPC
# Lấy tất cả giá trị từ các cột bắt đầu bằng "IPC"
values_IPC <- sapply(expression_data_1[, grepl("^IPC", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_IPC <- max(values_IPC[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_IPC > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_IPC
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não IPC là")
print(result)


top_10_gen_name_IPC <- character(10)
# Tìm tên gene của 10 max IPC
for (i in top_10_max_rows) {
  gen_max_IPC <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_IPC, "\n")
  top_10_gen_name_IPC <- c(top_10_gen_name_IPC, gen_max_IPC)
}


# Tìm tên gene của 10 max IPC và lưu vào top_10_gen_name_IPC
top_10_gen_name_IPC <- character(10)

for (i in 1:10) {
  gen_max_IPC <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_IPC[i] <- gen_max_IPC
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_IPC, "\n")
}

posteroventral_parietal_cortex <- data.frame(Gene_name = top_10_gen_name_IPC, Value = top_10_max_values)
posteroventral_parietal_cortex


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Posteroventral parietal cortex", xaxt = "n",
        names.arg = top_10_gen_name_IPC, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_IPC,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# 21. A1C
# Lấy tất cả giá trị từ các cột bắt đầu bằng "A1C"
values_A1C <- sapply(expression_data_1[, grepl("^A1C", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_A1C <- max(values_A1C[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_A1C > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_A1C
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não A1C là")
print(result)


top_10_gen_name_A1C <- character(10)
# Tìm tên gene của 10 max A1C
for (i in top_10_max_rows) {
  gen_max_A1C <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_A1C, "\n")
  top_10_gen_name_A1C <- c(top_10_gen_name_A1C, gen_max_A1C)
}


# Tìm tên gene của 10 max A1C và lưu vào top_10_gen_name_A1C
top_10_gen_name_A1C <- character(10)

for (i in 1:10) {
  gen_max_A1C <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_A1C[i] <- gen_max_A1C
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_A1C, "\n")
}

primary_auditory_cortex <- data.frame(Gene_name = top_10_gen_name_A1C, Value = top_10_max_values)
primary_auditory_cortex


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Primary auditory cortex", xaxt = "n",
        names.arg = top_10_gen_name_A1C, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_A1C,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# 22. V1C
# Lấy tất cả giá trị từ các cột bắt đầu bằng "V1C"
values_V1C <- sapply(expression_data_1[, grepl("^V1C", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_V1C <- max(values_V1C[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_V1C > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_V1C
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não V1C là")
print(result)


top_10_gen_name_V1C <- character(10)
# Tìm tên gene của 10 max V1C
for (i in top_10_max_rows) {
  gen_max_V1C <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_V1C, "\n")
  top_10_gen_name_V1C <- c(top_10_gen_name_V1C, gen_max_V1C)
}


# Tìm tên gene của 10 max V1C và lưu vào top_10_gen_name_V1C
top_10_gen_name_V1C <- character(10)

for (i in 1:10) {
  gen_max_V1C <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_V1C[i] <- gen_max_V1C
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_V1C, "\n")
}

primary_visual_cortex <- data.frame(Gene_name = top_10_gen_name_V1C, Value = top_10_max_values)
primary_visual_cortex


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Primary visual cortex", xaxt = "n",
        names.arg = top_10_gen_name_V1C, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_V1C,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# 23. STR
# Lấy tất cả giá trị từ các cột bắt đầu bằng "STR"
values_STR <- sapply(expression_data_1[, grepl("^STR", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_STR <- max(values_STR[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_STR > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_STR
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não STR là")
print(result)


top_10_gen_name_STR <- character(10)
# Tìm tên gene của 10 max STR
for (i in top_10_max_rows) {
  gen_max_STR <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_STR, "\n")
  top_10_gen_name_STR <- c(top_10_gen_name_STR, gen_max_STR)
}


# Tìm tên gene của 10 max STR và lưu vào top_10_gen_name_STR
top_10_gen_name_STR <- character(10)

for (i in 1:10) {
  gen_max_STR <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_STR[i] <- gen_max_STR
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_STR, "\n")
}

striatum <- data.frame(Gene_name = top_10_gen_name_STR, Value = top_10_max_values)
striatum


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Striatum", xaxt = "n",
        names.arg = top_10_gen_name_STR, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_STR,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# 24. CB
# Lấy tất cả giá trị từ các cột bắt đầu bằng "CB"
values_CB <- sapply(expression_data_1[, grepl("^CB", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_CB <- max(values_CB[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_CB > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_CB
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não CB là")
print(result)


top_10_gen_name_CB <- character(10)
# Tìm tên gene của 10 max CB
for (i in top_10_max_rows) {
  gen_max_CB <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_CB, "\n")
  top_10_gen_name_CB <- c(top_10_gen_name_CB, gen_max_CB)
}


# Tìm tên gene của 10 max CB và lưu vào top_10_gen_name_CB
top_10_gen_name_CB <- character(10)

for (i in 1:10) {
  gen_max_CB <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_CB[i] <- gen_max_CB
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_CB, "\n")
}

cerebellum <- data.frame(Gene_name = top_10_gen_name_CB, Value = top_10_max_values)
cerebellum


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Cerebellum", xaxt = "n",
        names.arg = top_10_gen_name_CB, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_CB,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# 25. CBC
# Lấy tất cả giá trị từ các cột bắt đầu bằng "CBC"
values_CBC <- sapply(expression_data_1[, grepl("^CBC", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_CBC <- max(values_CBC[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_CBC > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_CBC
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não CBC là")
print(result)


top_10_gen_name_CBC <- character(10)
# Tìm tên gene của 10 max CBC
for (i in top_10_max_rows) {
  gen_max_CBC <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_CBC, "\n")
  top_10_gen_name_CBC <- c(top_10_gen_name_CBC, gen_max_CBC)
}


# Tìm tên gene của 10 max CBC và lưu vào top_10_gen_name_CBC
top_10_gen_name_CBC <- character(10)

for (i in 1:10) {
  gen_max_CBC <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_CBC[i] <- gen_max_CBC
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_CBC, "\n")
}

cerebellar_cortex <- data.frame(Gene_name = top_10_gen_name_CBC, Value = top_10_max_values)
cerebellar_cortex


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Cerebellar cortex", xaxt = "n",
        names.arg = top_10_gen_name_CBC, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_CBC,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# 26. MD
# Lấy tất cả giá trị từ các cột bắt đầu bằng "MD"
values_MD <- sapply(expression_data_1[, grepl("^MD", names(expression_data_1))], function(col) as.numeric(gsub("[^0-9.-]", "", col)))

# Tạo một vector để lưu trữ 10 giá trị lớn nhất
top_10_max_values <- numeric(10)

# Tạo một vector để lưu trữ hàng tương ứng với 10 giá trị lớn nhất
top_10_max_rows <- character(10)

# Lặp qua từng hàng của dữ liệu
for (i in 1:nrow(expression_data_1)) {
  # Lấy giá trị lớn nhất trong hàng hiện tại
  max_value_MD <- max(values_MD[i, ], na.rm = TRUE)
  
  # Kiểm tra xem giá trị lớn nhất có lớn hơn giá trị nào trong top 10 không
  if (max_value_MD > min(top_10_max_values)) {
    # Tìm vị trí của giá trị nhỏ nhất trong top 10
    min_index <- which.min(top_10_max_values)
    
    # Cập nhật giá trị lớn nhất và tên hàng tương ứng
    top_10_max_values[min_index] <- max_value_MD
    top_10_max_rows[min_index] <- rownames(expression_data_1)[i]
  }
}

# Sắp xếp top_10_max_values theo thứ tự giảm dần
sorted_indices <- order(top_10_max_values, decreasing = TRUE)
top_10_max_values <- top_10_max_values[sorted_indices]
top_10_max_rows <- top_10_max_rows[sorted_indices]

# In ra 10 giá trị lớn nhất và tên hàng tương ứng
result <- data.frame(Row = top_10_max_rows, Max_Value = top_10_max_values)
cat("Vị trí hàng và giá trị của 10 gen biểu hiện mạnh nhất của vùng não MD là")
print(result)


top_10_gen_name_MD <- character(10)
# Tìm tên gene của 10 max MD
for (i in top_10_max_rows) {
  gen_max_MD <- expression_data_1[i, 1]
  cat("Tên gen hàng", i, "là", gen_max_MD, "\n")
  top_10_gen_name_MD <- c(top_10_gen_name_MD, gen_max_MD)
}


# Tìm tên gene của 10 max MD và lưu vào top_10_gen_name_MD
top_10_gen_name_MD <- character(10)

for (i in 1:10) {
  gen_max_MD <- expression_data_1[top_10_max_rows[i], 1]
  top_10_gen_name_MD[i] <- gen_max_MD
  cat("Tên gen hàng", top_10_max_rows[i], "là", gen_max_MD, "\n")
}

mediodorsal_nucleus_of_thalamus <- data.frame(Gene_name = top_10_gen_name_MD, Value = top_10_max_values)
mediodorsal_nucleus_of_thalamus


old_par <- par(mar = c(15, 6, 2, 0), xpd = NA)

# Tạo một vector chứa các giá trị width mới (2/3 của giá trị mặc định)

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 14),
        ylab = "log2 RPKM", main ="Mediodorsal nucleus of thalamus", xaxt = "n",
        names.arg = top_10_gen_name_MD, las=2)


text(x = (1:length(top_10_max_values) * 1.2) - 0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_MD,
     xpd = NA,
     ## Rotate the labels by 45 degrees.
     srt = 40, adj=1.1,
     cex = 0.95)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.5, top_10_max_values[i], cex=0.8)
}

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------


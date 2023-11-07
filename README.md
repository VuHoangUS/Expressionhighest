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


old_par <- par(mar = c(15, 6, 2, 0))

barplot(top_10_max_values, 
        col = hsv(seq(0,1 - 1/12, length.out = 12), 0.5 , 1),
        ylim = c(0, 12),
        ylab = "log2 RPKM", main ="Dorsolateral_prefrontal_cortex", xaxt = "n",
        names.arg = top_10_gen_name_DFC, las=2)

axis(side = 1, labels = FALSE)
text(x = (1:length(top_10_max_values) * 1.2) -0.45,
     y = par("usr")[3],             
     labels = top_10_gen_name_DFC,
                  xpd = NA,
                  ## Rotate the labels by 45 degrees.
                  srt = 40, adj=1.1,
                  cex = 1)

x_positions <- seq(0.7, 0.7 + 1.2*(10 - 1), by = 1.2)

for (i in 1:10) {
  text(x = x_positions[i], top_10_max_values[i] + 0.4, top_10_max_values[i], cex=1)
}


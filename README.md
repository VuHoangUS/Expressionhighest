setwd("D:/baitapR")
rows <- read.csv ("Rows.csv", header=TRUE)
str(rows)
attach(rows)
columns <- read.csv ("Columns.csv", header=TRUE)
str(columns)
attach(columns)
expression <- read.csv ("Expression.csv", header=TRUE)
str(expression)
attach(expression)


# Tạo một dòng mới từ cột "top_level_structure_abbreviation" của columns
new_row <- t(columns$top_level_structure_abbreviation)

# Loại bỏ hàng đầu tiên (header) khỏi rows và columns
rows <- rows[-1, ]
columns <- columns[-1, ]

# Thêm dòng mới vào đầu expression
expression <- rbind(data.frame(ID = "top_level_structure_abbreviation", expression))

# Thêm cột ID từ rows_df vào expression_df
expression <- rows$ID

write.csv(expression_df, "path/to/combined_expression.csv", row.names = FALSE)


# Lấy danh sách các cột mang kí tự "NP" hoặc "HM"
np_columns <- grep("^NP", colnames(expression))
vz_columns <- grep("^VZ", colnames(expression))

max_np <- max(apply(expression[, np_columns, drop = FALSE], 1, max))

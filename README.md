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

# Tìm số lớn nhất của "DFC"
max_DFC <- max(sapply(expression_data_1[, grepl("^DFC", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "VFC"
max_VFC <- max(sapply(expression_data_1[, grepl("^VFC", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "MFC"
max_MFC <- max(sapply(expression_data_1[, grepl("^MFC", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "OFC"
max_OFC <- max(sapply(expression_data_1[, grepl("^OFC", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "M1C-S1C"
max_M1C_S1C <- max(sapply(expression_data_1[, grepl("^M1C.S1C", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "PCx"
max_PCx <- max(sapply(expression_data_1[, grepl("^PCx", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "STC"
max_STC <- max(sapply(expression_data_1[, grepl("^STC", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "ITC"
max_ITC <- max(sapply(expression_data_1[, grepl("^ITC", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "Ocx"
max_Ocx <- max(sapply(expression_data_1[, grepl("^Ocx", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "HIP"
max_HIP <- max(sapply(expression_data_1[, grepl("^HIP", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "AMY"
max_AMY <- max(sapply(expression_data_1[, grepl("^AMY", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "LGE"
max_LGE <- max(sapply(expression_data_1[, grepl("^LGE", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "MGE"
max_MGE <- max(sapply(expression_data_1[, grepl("^MGE", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "CGE"
max_CGE <- max(sapply(expression_data_1[, grepl("^CGE", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "DTH"
max_DTH <- max(sapply(expression_data_1[, grepl("^DTH", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "URL"
max_URL <- max(sapply(expression_data_1[, grepl("^URL", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "TCx"
max_TCx <- max(sapply(expression_data_1[, grepl("^TCx", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "M1C"
max_M1C <- max(sapply(expression_data_1[, grepl("^M1C", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "S1C"
max_S1C <- max(sapply(expression_data_1[, grepl("^S1C", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "IPC"
max_IPC <- max(sapply(expression_data_1[, grepl("^IPC", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "A1C"
max_A1C <- max(sapply(expression_data_1[, grepl("^A1C", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "V1C"
max_V1C <- max(sapply(expression_data_1[, grepl("^V1C", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "STR"
max_STR <- max(sapply(expression_data_1[, grepl("^STR", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "CB"
max_CB <- max(sapply(expression_data_1[, grepl("^CB", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "CBC"
max_CBC <- max(sapply(expression_data_1[, grepl("^CBC", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))
# Tìm số lớn nhất của "MD"
max_MD <- max(sapply(expression_data_1[, grepl("^MD", names(expression_data_1))], function(col) max(as.numeric(gsub("[^0-9.-]", "", col)), na.rm = TRUE)))


# Sử dụng hàm 'which' để tìm tất cả các vị trí của giá trị trong ma trận
positions_DFC <- which(expression_data_1 == max_DFC, arr.ind = TRUE)
positions_VFC <- which(expression_data_1 == max_VFC, arr.ind = TRUE)
positions_MFC <- which(expression_data_1 == max_MFC, arr.ind = TRUE)
positions_OFC <- which(expression_data_1 == max_OFC, arr.ind = TRUE)
positions_M1C_S1C <- which(expression_data_1 == max_M1C_S1C, arr.ind = TRUE)
positions_PCx <- which(expression_data_1 == max_PCx, arr.ind = TRUE)
positions_STC <- which(expression_data_1 == max_STC, arr.ind = TRUE)
positions_ITC <- which(expression_data_1 == max_ITC, arr.ind = TRUE)
positions_Ocx <- which(expression_data_1 == max_Ocx, arr.ind = TRUE)
positions_HIP <- which(expression_data_1 == max_HIP, arr.ind = TRUE)
positions_AMY <- which(expression_data_1 == max_AMY, arr.ind = TRUE)
positions_LGE <- which(expression_data_1 == max_LGE, arr.ind = TRUE)
positions_MGE <- which(expression_data_1 == max_MGE, arr.ind = TRUE)
positions_CGE <- which(expression_data_1 == max_CGE, arr.ind = TRUE)
positions_DTH <- which(expression_data_1 == max_DTH, arr.ind = TRUE)
positions_URL <- which(expression_data_1 == max_URL, arr.ind = TRUE)
positions_TCx <- which(expression_data_1 == max_TCx, arr.ind = TRUE)
positions_M1C <- which(expression_data_1 == max_M1C, arr.ind = TRUE)
positions_S1C <- which(expression_data_1 == max_S1C, arr.ind = TRUE)
positions_IPC <- which(expression_data_1 == max_IPC, arr.ind = TRUE)
positions_A1C <- which(expression_data_1 == max_A1C, arr.ind = TRUE)
positions_V1C <- which(expression_data_1 == max_V1C, arr.ind = TRUE)
positions_STR <- which(expression_data_1 == max_STR, arr.ind = TRUE)
positions_CB <- which(expression_data_1 == max_CB, arr.ind = TRUE)
positions_CBC <- which(expression_data_1 == max_CBC, arr.ind = TRUE)
positions_MD <- which(expression_data_1 == max_MD, arr.ind = TRUE)


# Vị trí của các max
cat("    ","Tọa độ của max_DFC", positions_DFC,"
    ",
    "Tọa độ của max_VFC", positions_VFC,"
    ",
    "Tọa độ của max_MFC", positions_MFC,"
    ",
    "Tọa độ của max_OFC", positions_OFC,"
    ",
    "Tọa độ của max_M1C_S1C", positions_M1C_S1C,"
    ",
    "Tọa độ của max_PCx", positions_PCx,"
    ",
    "Tọa độ của max_STC", positions_STC,"
    ",
    "Tọa độ của max_ITC", positions_ITC,"
    ",
    "Tọa độ của max_Ocx", positions_Ocx,"
    ",
    "Tọa độ của max_HIP", positions_HIP,"
    ",
    "Tọa độ của max_AMY", positions_AMY,"
    ",
    "Tọa độ của max_LGE", positions_LGE,"
    ",
    "Tọa độ của max_MGE", positions_MGE,"
    ",
    "Tọa độ của max_DTH", positions_DTH,"
    ",
    "Tọa độ của max_URL", positions_URL,"
    ",
    "Tọa độ của max_TCx", positions_TCx,"
    ",
    "Tọa độ của max_M1C", positions_M1C,"
    ",
    "Tọa độ của max_S1C", positions_S1C,"
    ",
    "Tọa độ của max_IPC", positions_IPC,"
    ",
    "Tọa độ của max_A1C", positions_A1C,"
    ",
    "Tọa độ của max_V1C", positions_V1C,"
    ",
    "Tọa độ của max_STR", positions_STR,"
    ",
    "Tọa độ của max_CB", positions_CB,"
    ",
    "Tọa độ của max_CBC", positions_CBC,"
    ",
    "Tọa độ của max_MD", positions_MD)


#Gen biểu hiện mạnh nhất của DFC, VFC, MFC, OFC, M1C_S1C, STC, M1C, S1C, IPC, A1C, V1C
gen_max_1 <- expression_data_1[1102, 1]
cat("Gen biểu hiện mạnh nhất của DFC, VFC, MFC, OFC, M1C_S1C, STC, M1C, S1C, IPC, A1C, V1C là", gen_max_1)

#Gen biểu hiện mạnh nhất của PCx, ITC, Ocx, HIP, AMY, LGE, MGE, DTH, URL, TCx, STR, CB, CBC, MD
gen_max_2 <- expression_data_1[759, 1]
cat("Gen biểu hiện mạnh nhất của PCx, ITC, Ocx, HIP, AMY, LGE, MGE, DTH, URL, TCx, STR, CB, CBC, MD là", gen_max_2)

# Tạo data frame của expression_highest
Structure_name <- c("DFC","VFC","MFC","M1C-S1C","PCx","STC","ITC","Ocx","HIP","AMY","LGE","MGE","CGE","DTH","URL","TCx","M1C","S1C","IPC","A1C","V1C","STR","CB","CBC","MD")
Value <- c(max_DFC, max_VFC, max_MFC, max_M1C_S1C, max_PCx, max_STC, max_ITC, max_Ocx, max_HIP, max_AMY, max_LGE, max_MGE, max_CGE, max_DTH, max_URL, max_TCx, max_M1C, max_S1C, max_IPC, max_A1C, max_V1C, max_STR, max_CB, max_CBC, max_MD)
structure_full_name <- c("DFC: dorsolateral prefrontal cortex", "VFC: ventrolateral prefrontal cortex", "MFC: anterior cingulate cortex", "OFC: orbital frontal cortex", "M1C-S1C: primary motor-sensory cortex", "STC: posterior superior temporal cortex", "M1C: primary motor cortex", "S1C: primary somatosensory cortex", "IPC: posteroventral parietal cortex", "A1C: primary auditory cortex", "V1C: primary visual cortex", "PCx: parietal neocortex", "ITC: inferolateral temporal cortex", "Ocx: occipital neocortex", "HIP: hippocampus", "AMY: amygdaloid complex", "LGE: lateral ganglionic eminence", "MGE: medial ganglionic eminence", "DTH: dorsal thalamus", "URL: upper rhombic lip", "TCx: temporal neocortex", "STR: striatum", "CB: cerebellum", "CBC: cerebellar cortex", "MD: mediodorsal nucleus of thalamus")
expression_highest <- data.frame(Structure_name, Value, structure_full_name)
expression_highest

# Tạo data frame của stathmin-like 2
structure_name_1 <- c("DFC", "VFC", "MFC", "OFC", "M1C-S1C", "STC", "M1C", "S1C", "IPC", "A1C", "V1C")
value_1 <- c(max_DFC, max_VFC, max_MFC, max_OFC, max_M1C_S1C, max_STC, max_M1C, max_S1C, max_IPC, max_A1C, max_V1C)
structure_name_full_1 <- c("DFC: dorsolateral prefrontal cortex", "VFC: ventrolateral prefrontal cortex", "MFC: anterior cingulate cortex", "OFC: orbital frontal cortex", "M1C-S1C: primary motor-sensory cortex", "STC: posterior superior temporal cortex", "M1C: primary motor cortex", "S1C: primary somatosensory cortex", "IPC: posteroventral parietal cortex", "A1C: primary auditory cortex", "V1C: primary visual cortex")
stathmin_like_2 <- data.frame(structure_name_1, value_1, structure_name_full_1)
stathmin_like_2

# Tạo data frame của neuronatin
structure_name_2 <- c("PCx", "ITC", "Ocx", "HIP", "AMY", "LGE", "MGE", "DTH", "URL", "TCx", "STR", "CB", "CBC", "MD")
value_2 <- c(max_PCx, max_IPC, max_Ocx, max_HIP, max_AMY, max_LGE, max_MGE, max_DTH, max_URL, max_TCx, max_STR, max_CB, max_CBC, max_MD)
neuronatin <- data.frame(structure_name_2, value_2)
neuronatin

old.par <- par(mfrow = c(2, 1),
               # chỉnh khoảng cách inside từng đồ thị [bottom, left, top, right]
               mar = c(0.5, 4, 7, 12), 
               # chỉnh khoảng cách multiple plot
               oma = c(2, 2, 0, 7),
               xpd = NA)


# Vẽ đồ thị stathmin-like 2
names_1 <- stathmin_like_2$structure_name_1
barplot(value_1,
        col = hsv(0.7, 0.5 , 1),
        ylim = c(0, 13), space = 0.3,
        ylab = "log2 RPKM", main ="Stathmin-like 2", names.arg = names_1)


# Thêm Value vào đồ thị 1
text(0.8, max_DFC + 0.6, max_DFC, cex=0.9)
text(2.1, max_VFC + 0.6, max_VFC, cex=0.9)
text(3.4, max_MFC + 0.6, max_MFC, cex=0.9)
text(4.7, max_OFC + 0.6, max_OFC, cex=0.9)
text(6, max_M1C_S1C + 0.6, max_M1C_S1C, cex=0.9)
text(7.3, max_STC + 0.6, max_STC, cex=0.9)
text(8.6, max_M1C + 0.6, max_M1C, cex=0.9)
text(9.9, max_S1C + 0.6, max_S1C, cex=0.9)
text(11.2, max_IPC + 0.6, max_IPC, cex=0.9)
text(12.5, max_A1C + 0.6, max_A1C, cex=0.9)
text(13.8, max_V1C + 0.6, max_V1C, cex=0.9)

# Vẽ đồ thị neuronatin
names_2 <- neuronatin$structure_name_2
barplot(value_2,
        col = hsv(0.6, 0.5 , 1),
        ylim = c(0, 13), space = 0.3,
        ylab = "log2 RPKM", main ="Neuronatin", names.arg = names_2)

# Thêm value vào đồ thị 2
text(0.8, max_PCx + 0.6, max_PCx, cex=0.9)
text(2.1, max_ITC + 0.6, max_ITC, cex=0.9)
text(3.4, max_Ocx + 0.6, max_Ocx, cex=0.9)
text(4.7, max_HIP + 0.6, max_HIP, cex=0.9)
text(6, max_AMY + 0.6, max_AMY, cex=0.9)
text(7.3, max_LGE + 0.6, max_LGE, cex=0.9)
text(8.6, max_MGE + 0.6, max_MGE, cex=0.9)
text(9.9, max_DTH + 0.6, max_DTH, cex=0.9)
text(11.2, max_URL + 0.6, max_URL, cex=0.9)
text(12.5, max_TCx + 0.6, max_TCx, cex=0.9)
text(13.8, max_STR + 0.6, max_STR, cex=0.9)
text(15.1, max_CB + 0.6, max_CB, cex=0.9)
text(16.4, max_CBC + 0.6, max_CBC, cex=0.9)
text(17.7, max_MD + 0.6, max_MD, cex=0.9)


legend(17, 40, # trục x, trục y
       y.intersp = 0.3, # vertical spacing
       legend = expression_highest$structure_full_name,
       title = "Vũng não",
       horiz = FALSE, 
       box.lty = 1,
       cex = 1)

library(dplyr)
library(ggplot2)

library(dplyr)
library(ggplot2)

ggplot(stathmin_like_2, aes(structure_name_1, value_1, fill = structure_name_full_1)) +
      geom_col(color="black", size= 0.5, width=0.5)

text(0.8, max_DFC + 0.6, max_DFC, cex=0.9)
text(2.1, max_VFC + 0.6, max_VFC, cex=0.9)
text(3.4, max_MFC + 0.6, max_MFC, cex=0.9)
text(4.7, max_OFC + 0.6, max_OFC, cex=0.9)
text(6, max_M1C_S1C + 0.6, max_M1C_S1C, cex=0.9)
text(7.3, max_STC + 0.6, max_STC, cex=0.9)
text(8.6, max_M1C + 0.6, max_M1C, cex=0.9)
text(9.9, max_S1C + 0.6, max_S1C, cex=0.9)
text(11.2, max_IPC + 0.6, max_IPC, cex=0.9)
text(12.5, max_A1C + 0.6, max_A1C, cex=0.9)
text(13.8, max_V1C + 0.6, max_V1C, cex=0.9)


ggplot(neuronatin, aes(structure_name_2, value_2, fill= structure_name_2)) +
      geom_col(color= "black", size = 0.5, width=0.5)

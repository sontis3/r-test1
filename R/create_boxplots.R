setwd("z:/UBUNTU/00 - Личные папки сотрудников/08 - Овчаренко/Repos/metax/113/metaX_result_POS1/data/")
library("readxl", lib.loc="~/R/win-library/3.5")

# загрузка исх. и списка отбора
neg_src <- read_excel("neg-norm-metaboAnalystInput.xlsx", sheet = "neg-norm-metaboAnalystInput")
ids_list <- read_excel("z:/UBUNTU/00 - Личные папки сотрудников/08 - Овчаренко/Repos/metax/113/Identification_list.xlsx", sheet = "Identification")

# отбор
library("dplyr", lib.loc="~/R/win-library/3.5")
m_neg_src <- merge(neg_src, ids_list, by.x = "Sample", by.y = "Id")

# транспонирование фрейма
n <- m_neg_src$Sample
f <- neg_src[1,-1]
fact <- factor(t(f))
t_neg_src <- t(m_neg_src[, -c(1, 60, 61)])
storage.mode(t_neg_src) <- "numeric"
dft_neg_src <- as.data.frame(t_neg_src)
colnames(dft_neg_src) <- n

# добавление фактора A,B,QC
dft_neg_src$group <- fact

# вывод боксплотов в файлы
setwd("z:/UBUNTU/00 - Личные папки сотрудников/08 - Овчаренко/Repos/metax/113/metaX_result_POS1/data-out/")
library("grDevices", lib.loc="C:/Program Files/R/R-3.5.1/library")
for (i in 1:(ncol(dft_neg_src)-1)) {
  #for (i in 1:2700) {
  png(file = paste("var_", gsub("/", "", colnames(dft_neg_src)[i]), ".png", sep=""))
  boxplot(dft_neg_src[,i] ~ group, dft_neg_src, main=colnames(dft_neg_src)[i])
  dev.off()
}

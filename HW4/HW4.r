library(readxl)
library(vegan)
library(VIF)
library(ImputeRobust)

file_path <- "C:\\Users\\po333\\Downloads\\Grazing_Magierowski_et_al_2015.xls"

sheets <- excel_sheets(file_path)
data_frames <- list()
for (sheet in sheets) {
  data_frames[[sheet]] <- read_excel(file_path, sheet = sheet)
}

fauna_data <- data_frames$fauna[, -1]  # Exclude the first column
env_data <- data_frames$env[, -1]   # Exclude the first column

env_numeric <- sapply(env_data[, -ncol(env_data)], as.numeric, na.rm = TRUE)
env_cleaned <- as.data.frame(na.aggregate(env_numeric, FUN = mean))
env_cleaned$GrazingRank <- data_frames$env$GrazingRank

cca_result <- cca(fauna_data ~ ., data = env_cleaned[, -1])

summary(cca_result)
vif.cca(cca_result)

plot(cca_result, scaling = "sites")
plot(cca_result, scaling = "species")

anova(cca_result, by = "mar")
anova(cca_result, by = "term")

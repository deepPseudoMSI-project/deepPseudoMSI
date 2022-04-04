masstools::setwd_project()
rm(list = ls())
source("code/tools.R")

setwd("data_analysis/metabolome_data/normal_dataset/")
load("expression_data")
load("phenotype_info")
load("sample_info")
load("variable_info")

write.csv(expression_data, "expression_data.csv", row.names = FALSE)
write.csv(phenotype_info, "phenotype_info.csv", row.names = FALSE)
write.csv(sample_info, "sample_info.csv", row.names = FALSE)
write.csv(variable_info, "variable_info.csv", row.names = FALSE)


masstools::setwd_project()
rm(list = ls())
source("code/tools.R")

setwd("data_analysis/metabolome_data/rt_shift_dataset/")
load("mzrtshift_expression_data")
load("mzrtshift_sample_info")
load("mzrtshift_variable_info")

write.csv(mzrtshift_expression_data, "mzrtshift_expression_data.csv", row.names = FALSE)
write.csv(mzrtshift_sample_info, "mzrtshift_sample_info.csv", row.names = FALSE)
write.csv(mzrtshift_variable_info, "mzrtshift_variable_info.csv", row.names = FALSE)


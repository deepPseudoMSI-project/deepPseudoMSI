#to avoind source
# no_exist_function()

masstools::setwd_project()
rm(list = ls())

source("code/tools.R")

setwd("data_analysis/metabolome_data/overlap/")

##load the sample information of metabolomics
sample_info <-
  readr::read_csv("metaData.csv")

###sample_info2 is the information for 808 - 1007 samples
sample_info2 <-
  readxl::read_xlsx("Sample mapping file-subject ID-161010.xlsx")

colnames(sample_info2) <-
  c("sample_id", "GA", "subject_id")

sample_info2 <-
  sample_info2 %>%
  dplyr::arrange(sample_id)

sample_info <-
  sample_info %>%
  dplyr::arrange(sample_id)

sample_info$sample_id <-
  sample_info$sample_id %>%
  paste("X", ., sep = "")

sample_info2$sample_id <-
  sample_info2$sample_id %>%
  paste("X", ., sep = "")

head(sample_info)
head(sample_info2)

##make sure that the sample_info and sample_info2 are same
sample_info$sample_id
sample_info2$sample_id

idx <-
  match(sample_info$sample_id, sample_info2$sample_id)

cbind(sample_info2$subject_id[idx], sample_info$subject_id)

sample_info2$subject_id[idx] == sample_info$subject_id

sample_info2[idx, ]

cbind(sample_info2$GA[idx], sample_info$g_stage)
plot(sample_info2$GA[idx], sample_info$g_stage)

##marker sure that subject ID same with RNA
sample_info$subject_id <-
  case_when(
    nchar(sample_info$subject_id) == 1 ~ paste("DK", "00", sample_info$subject_id, sep = ""),
    nchar(sample_info$subject_id) == 2 ~ paste("DK", "0", sample_info$subject_id, sep = ""),
    nchar(sample_info$subject_id) == 3 ~ paste("DK", sample_info$subject_id, sep = "")
  )

##combine sample_info and sample_info2
sample_info <-
  sample_info %>%
  dplyr::full_join(sample_info2, by = "sample_id") %>%
  mutate(subject_id = case_when(
    !is.na(subject_id.x) ~ subject_id.x,
    is.na(subject_id.x) ~ subject_id.y
  )) %>%
  dplyr::select(-c(subject_id.x, subject_id.y))

sample_info <-
  sample_info %>%
  mutate(g_stage =
           dplyr::case_when(!is.na(g_stage) ~ g_stage,
                            is.na(g_stage) ~ GA))

colnames(sample_info)[15] <-
  "GA(from_sample_info2)"

sample_info <-
  sample_info %>%
  dplyr::select(sample_id, subject_id, dplyr::everything())

##load metabolomics data
load("denmark_rplc_pos6")

peak_table_pos <-
  denmark_rplc_pos6@ms1.data[[1]]

dim(peak_table_pos)

variable_info_pos <- peak_table_pos %>%
  dplyr::select(1:3)

sample_data_pos <-
  peak_table_pos %>%
  dplyr::select(-c(1:3))

dim(sample_data_pos)

match(sample_info$sample_id, colnames(sample_data_pos))

setdiff(colnames(sample_data_pos), sample_info$sample_id)

variable_info <-
  variable_info_pos

sample_data <- 
  sample_data_pos

dim(sample_data)
sum(is.na(sample_data))

expression_data <-
  sample_data %>%
  as.data.frame()

rownames(expression_data) <-
  variable_info$name

setdiff(colnames(expression_data), sample_info$sample_id)

sample_info_pos <- denmark_rplc_pos6@sample.info

sample_info_pos <-
  sample_info_pos %>%
  dplyr::filter(sample.name %in% colnames(expression_data))

sort(colnames(expression_data)) == sort(sample_info_pos$sample.name)

sum(sort(colnames(expression_data)) == sort(sample_info_pos$sample.name))

sample_info_pos <-
  sample_info_pos %>%
  dplyr::select(sample.name, batch) %>%
  dplyr::rename(sample_id = sample.name)

sample_info <-
  sample_info_pos %>%
  dplyr::left_join(sample_info, by = "sample_id")

colnames(sample_info)

dim(sample_info)

dim(expression_data)

expression_data <-
  expression_data %>%
  dplyr::select(sample_info$sample_id)

colnames(expression_data) == sample_info$sample_id

phenotype_info <-
  sample_info %>%
  arrange(subject_id, g_stage) %>%
  dplyr::select(-c(
    sample_id,
    batch,
    g_stage,
    weeks_to_dlvry,
    `GA(from_sample_info2)`
  )) %>%
  dplyr::distinct(subject_id, .keep_all = TRUE)

sample_info <-
  sample_info %>%
  dplyr::select(
    -c(
      labor_onset,
      birth_g_stage,
      age,
      pre_bmi,
      birth_status,
      n_male_birth,
      n_female_birth,
      earlier_preg_with_children,
      birth_weight,
      n_total_birth,
      year_lcms
    )
  )

phenotype_info <- 
  phenotype_info %>% 
  dplyr::filter_all(dplyr::any_vars(!is.na(.)))

sum(colnames(expression_data) == sample_info$sample_id)
sum(rownames(expression_data) == variable_info$name)

##we combine denmark data and mz rt shift data using mz and rt matching 
mzrtshift_sample_info <- sample_info
mzrtshift_expression_data <- expression_data
mzrtshift_variable_info <- variable_info

load("expression_data")
denmark_expression_data <- expression_data 
load("sample_info")
denmark_sample_info <- sample_info 
load("variable_info")
denmark_variable_info <- variable_info 

data1 <- 
  denmark_variable_info[,c(1:3)] %>% 
  dplyr::filter(stringr::str_detect(name, "POS")) %>% 
  dplyr::select(mz, rt)

data2 <-
  mzrtshift_variable_info[,c(1:3)] %>% 
  dplyr::filter(stringr::str_detect(name, "POS")) %>% 
  dplyr::select(mz, rt)

match_result <-
  masstools::mz_rt_match(
    data1 = as.matrix(data1),
    data2 = as.matrix(data2),
    mz.tol = 25,
    rt.tol = 120,
    rt.error.type = "abs"
  )

match_result <-
  match_result %>% 
  as.data.frame() %>% 
  dplyr::group_by(Index1) %>% 
  dplyr::filter(`rt error` == min(`rt error`)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(Index2) %>% 
  dplyr::filter(`rt error` == min(`rt error`)) %>% 
  dplyr::ungroup()

match_result

##venn dragram
library(VennDiagram)
dim(match_result)

dim(data1)
dim(data2)

dim(match_result)

temp_data <- list("Normal dataset" = 
                    c(1:5207, paste("a", 1:(nrow(denmark_expression_data)-5207))),
                  "RT shift dataset" = 
                    c(1:5207, paste("b", 1:(nrow(mzrtshift_expression_data)-5207))))

plot <- venn.diagram(x = temp_data, 
                     filename = NULL, 
                     col = ggsci::pal_aaas()(2),
                     # col = NA,
                     fill = ggsci::pal_aaas(alpha = 0.7)(2)
)


grid.draw(plot)

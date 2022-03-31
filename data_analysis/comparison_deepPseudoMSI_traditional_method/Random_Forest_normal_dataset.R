#to avoind source
# no_exist_function()

##first set the work directory to project folder
masstools::setwd_project()
rm(list = ls())

source("code/tools.R")

load("data_analysis/metabolome_data/normal_dataset/expression_data")
load("data_analysis/metabolome_data/normal_dataset/phenotype_info")
load("data_analysis/metabolome_data/normal_dataset/sample_info")
load("data_analysis/metabolome_data/normal_dataset/variable_info")

load("data_analysis/metabolome_data/rt_shift_dataset/mzrtshift_expression_data")
load("data_analysis/metabolome_data/rt_shift_dataset/mzrtshift_sample_info")
load("data_analysis/metabolome_data/rt_shift_dataset/mzrtshift_variable_info")

setwd("")

##sample_info
image_info <- readr::read_csv("sample_info.csv")

image_info <- image_info %>% 
  dplyr::select(subject_id, class) %>% 
  dplyr::distinct(subject_id, .keep_all = TRUE)

###only positive mode
variable_info <- 
  variable_info %>% 
  dplyr::filter(stringr::str_detect(name, "POS"))

sample_info_old <- sample_info
expression_data_old <- expression_data

sample_info <- 
  sample_info %>% 
  dplyr::filter(stringr::str_detect(subject_id, "DK0")) %>% 
  dplyr::left_join(image_info, by = "subject_id")

expression_data <- 
  expression_data[variable_info$name, sample_info$sample_id]

####prediction model
library(randomForest)

library(Boruta)

library(tidyverse)

class <- sort(unique(sample_info$class))

###get the markers for each class
# marker_rf <- vector(mode = "list", length = length(class))
# for(idx in class){
#   cat(idx, " ")
#   
#   val_id <- 
#   sample_info %>% 
#     dplyr::filter(class == idx) %>% 
#     dplyr::filter(weeks_to_dlvry > 0) %>% 
#     dplyr::pull(sample_id)
#   
#   dis_id <- 
#     sample_info %>% 
#     dplyr::filter(class != idx) %>% 
#     dplyr::filter(weeks_to_dlvry > 0) %>% 
#     dplyr::pull(sample_id)
#     
#   val_y <- 
#     sample_info %>% 
#     dplyr::filter(class == idx) %>%
#     dplyr::filter(weeks_to_dlvry > 0) %>% 
#     dplyr::pull(g_stage) %>% 
#     as.matrix()
#   
#   dis_y <- 
#     sample_info %>% 
#     dplyr::filter(class != idx) %>%
#     dplyr::filter(weeks_to_dlvry > 0) %>% 
#     dplyr::pull(g_stage) %>% 
#     as.matrix()
#   
#   dis_x <- 
#     expression_data[,dis_id] %>% 
#     as.matrix() %>% 
#     t()
#   
#   val_x <- 
#     expression_data[,val_id] %>% 
#     as.matrix() %>% 
#     t()
#   
#   boruta_test <- 
#     Boruta(x = dis_x,
#            y = dis_y, 
#            doTrace = 3, 
#            holdHistory = TRUE)  
# 
#   
#   marker_rf[[idx]] <- 
#     boruta_test$finalDecision[boruta_test$finalDecision == "Confirmed"] %>% 
#     names() %>% 
#     sort()
# }
# 
# save(marker_rf, file = "marker_rf")
# load("marker_rf")

# purrr::reduce(marker_rf, intersect)
# marker_rf

# library(ohicore)
# 
# flower_plot(sample = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
#             value =  lapply(marker_rf, length) %>% unlist(), 
#             start = 90, a = 1, b = 2, labels="core")

##for each class, just print the results


# library(VennDiagram)
# 
# names(marker_rf) <- paste("Module", 1:5, sep = " ")
# 
# plot <- venn.diagram(x = marker_rf, 
#                      filename = NULL, 
#                      col = ggsci::pal_aaas()(5)
#                      # col = NA,
#                      # fill = ggsci::pal_aaas(alpha = 0.5)(5)
#                      )
# 
# grid.draw(plot)

marker_rf <- vector(mode = "list", length = length(class))
predicted_result <- vector(mode = "list", length = length(class))


# for(idx in class){
#   cat(idx, " ")
#   val_id <-
#   sample_info %>%
#     dplyr::filter(class == idx) %>%
#     dplyr::filter(weeks_to_dlvry > 0) %>%
#     dplyr::pull(sample_id)
# 
#   dis_id <-
#     sample_info %>%
#     dplyr::filter(class != idx) %>%
#     dplyr::filter(weeks_to_dlvry > 0) %>%
#     dplyr::pull(sample_id)
# 
#   val_y <-
#     sample_info %>%
#     dplyr::filter(class == idx) %>%
#     dplyr::filter(weeks_to_dlvry > 0) %>%
#     dplyr::pull(g_stage) %>%
#     as.matrix()
# 
#   dis_y <-
#     sample_info %>%
#     dplyr::filter(class != idx) %>%
#     dplyr::filter(weeks_to_dlvry > 0) %>%
#     dplyr::pull(g_stage) %>%
#     as.matrix()
# 
#   dis_x <-
#     expression_data[,dis_id] %>%
#     as.matrix() %>%
#     t()
#   
#   # dis_x <-
#   #   expression_data[marker_rf[[idx]],dis_id] %>%
#   #   as.matrix() %>%
#   #   t()
# 
#   # val_x <-
#   #   expression_data[marker_rf[[idx]],val_id] %>%
#   #   as.matrix() %>%
#   #   t()
#   
#   val_x <-
#     expression_data[,val_id] %>%
#     as.matrix() %>%
#     t()
# 
#   rf_regression <-
#     randomForest(x = dis_x,
#                  y = dis_y[,1],
#                  replace = TRUE,
#                  importance = TRUE,
#                  proximity = TRUE,
#                  mtry = 4)
#   
#   importancce <- 
#   randomForest::importance(rf_regression) %>% 
#     as.data.frame() %>% 
#     tibble::rownames_to_column(var = "name") %>% 
#     dplyr::arrange(dplyr::desc(`%IncMSE`))
#   
#   marker <- 
#     importancce$name[1:800]
#   
#   marker_rf[[idx]] <- marker
#   
#   rf_regression <-
#     randomForest(x = dis_x[,marker],
#                  y = dis_y[,1],
#                  replace = TRUE,
#                  importance = TRUE,
#                  proximity = TRUE,
#                  mtry = 4)
#   
#   predicted_y <-
#     predict(
#       object = rf_regression,
#       newdata = val_x[,marker]
#       # type = "response"
#     )
# 
#   predicted_result[[idx]] <-
#     data.frame(sample_id = val_id,
#                true = val_y,
#                predicted = predicted_y,
#                stringsAsFactors = FALSE)
# }
# 
# save(marker_rf, file = "marker_rf")
# save(predicted_result, file = "predicted_result")
load("marker_rf")
load("predicted_result")

predicted_result <- 
  do.call(rbind, predicted_result)

predicted_result <- 
  predicted_result %>% 
  dplyr::left_join(sample_info[,c("sample_id", "weeks_to_dlvry")],
                   by = "sample_id")

# write.csv(predicted_result, "predicted_result.csv", row.names = FALSE)
predicted_result <- readr::read_csv("predicted_result.csv")

###
rmse <- cal_rmse(predicted_result$true, predicted_result$predicted) %>% 
  round(2)
r2 <- cal_r2(predicted_result$predicted, predicted_result$true) %>% 
  round(2)
median <- median(abs(predicted_result$predicted - predicted_result$true)) %>% 
  round(2)

plot <- 
  predicted_result %>% 
  dplyr::filter(weeks_to_dlvry > 0) %>% 
  dplyr::left_join(sample_info[,c("sample_id","subject_id","class")], by = "sample_id") %>% 
  ggplot(aes(true, predicted)) +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  geom_point(shape = 16, aes(color = as.character(class))) +
  ggsci::scale_color_aaas() +
  guides(color = guide_legend(title = "Fold (cross validation)", 
                              override.aes = list(size = 3))) +
  geom_smooth(color = "skyblue") +
  scale_x_continuous(limits = c(5, 42)) +
  scale_y_continuous(limits = c(5, 42)) +
  labs(x = "Gestational age (GA, Actual)", y = "Gestational age (GA, Predicted)") +
  theme_bw() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 13),
        legend.position = c(0,1), legend.justification = c(0,1),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)) +
  annotate(geom = "text", x = Inf, y = -Inf, 
           label = paste("RMSE:", rmse, "\nMAE:", median, "\nAdjusted R2:", r2), 
           hjust = 1.5, vjust = -1.5)

plot

# ggsave(
#   plot,
#   filename = "validation_plot_all.pdf",
#   width = 7,
#   height = 7,
#   bg = "transparent"
# )

temp_data <-
  predicted_result %>%
  dplyr::filter(weeks_to_dlvry > 0) %>%
  dplyr::left_join(sample_info[, c("sample_id", "subject_id", "class")], 
                   by = "sample_id")

library(plyr)
info_fold <- 
  temp_data %>% 
  plyr::dlply(.variables = .(class)) %>% 
  purrr::map(.f = function(x){
    rmse <- 
      cal_rmse(predicted = x$predicted[x$weeks_to_dlvry > 0], 
               true = x$true[x$weeks_to_dlvry > 0])
    r2 <- 
      cal_r2(
        predicted = x$predicted[x$weeks_to_dlvry > 0], 
        true = x$true[x$weeks_to_dlvry > 0]
      )
    
    median <- 
      median(
        abs(x$predicted[x$weeks_to_dlvry > 0] -
              x$true[x$weeks_to_dlvry > 0])
      )
    c(rmse, r2, median)
  }) %>% 
  dplyr::bind_cols() %>% 
  t() %>% 
  as.data.frame()

colnames(info_fold) <- c("rmse", "r2", "median")

info_fold <-
  info_fold %>% 
  tibble::rownames_to_column(var = "fold") %>% 
  dplyr::mutate(fold = paste("Fold", fold, sep = " "))

save(info_fold, file = "info_fold")


plot <- 
  predicted_result %>% 
  dplyr::filter(weeks_to_dlvry > 0) %>% 
  dplyr::left_join(sample_info[,c("sample_id","subject_id","class")], by = "sample_id") %>% 
  dplyr::mutate(fold = paste("Fold", class), sep = " ") %>% 
  ggplot(aes(true, predicted)) +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  geom_point(shape = 16, size = 1.5, aes(color = as.character(fold)), 
             show.legend = FALSE) +
  ggsci::scale_color_aaas() +
  geom_smooth(color = "skyblue") +
  scale_x_continuous(limits = c(5, 42)) +
  scale_y_continuous(limits = c(5, 42)) +
  labs(x = "Gestational age (week, Actual)", 
       y = "Gestational age (week, Predicted)") +
  theme_bw() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 10),
        legend.position = c(0,1), legend.justification = c(0,1),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = "white", size = 10),
        strip.background = element_rect(fill = ggsci::pal_aaas()(10)[1])) +
  facet_wrap(vars(fold), ncol = 3)  +
  geom_text(data = info_fold, 
            aes(x = -Inf, 
                y = Inf, 
                hjust   = -0.1,
                vjust   = 1,
                label = paste("RMSE:", round(rmse, 2), 
                              "\nMAE:", round(median, 2), 
                              "\nAdjusted R2:", round(r2, 2))
            )) 

plot

ggsave(
  plot,
  filename = "validation_plot_for_each_fold.pdf",
  width = 9,
  height = 7,
  bg = "transparent"
)

ggsave(
  plot,
  filename = "validation_plot_for_each_fold.png",
  width = 9,
  height = 7,
  bg = "transparent"
)



####for each person
temp_data <- 
  predicted_result %>% 
  dplyr::filter(weeks_to_dlvry > 0) %>% 
  dplyr::left_join(sample_info[,c("sample_id","subject_id","class")], 
                   by = "sample_id") 

library(plyr)

info_subject <- 
  temp_data %>% 
  plyr::dlply(.variables = .(subject_id)) %>% 
  purrr::map(.f = function(x){
    rmse <- 
      cal_rmse(predicted = x$predicted[x$weeks_to_dlvry > 0], 
               true = x$true[x$weeks_to_dlvry > 0])
    r2 <- 
      cal_r2(
        predicted = x$predicted[x$weeks_to_dlvry > 0], 
        true = x$true[x$weeks_to_dlvry > 0]
      )
    
    median <- 
      median(
        abs(x$predicted[x$weeks_to_dlvry > 0] -
              x$true[x$weeks_to_dlvry > 0])
      )
    
    
    c(rmse, r2, median)
  }) %>% 
  dplyr::bind_cols() %>% 
  t() %>% 
  as.data.frame()

colnames(info_subject) <- c("rmse", "r2", "median")

info_subject <-
  info_subject %>% 
  tibble::rownames_to_column(var = "subject_id") %>% 
  dplyr::left_join(sample_info[,c("subject_id", "class")] %>% 
                     dplyr::distinct(subject_id, .keep_all = TRUE), by = "subject_id")


save(info_subject, file = "info_subject")


plot <- 
  predicted_result %>% 
  dplyr::filter(weeks_to_dlvry > 0) %>% 
  dplyr::left_join(sample_info[,c("sample_id","subject_id","class")], 
                   by = "sample_id") %>% 
  ggplot(aes(true, predicted)) +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  geom_point(shape = 16, size = 1, 
             aes(color = as.character(class)), show.legend = FALSE) +
  geom_smooth(color = "skyblue", size = 0.5) +
  ggsci::scale_color_aaas() +
  scale_x_continuous(limits = c(5, 42)) +
  scale_y_continuous(limits = c(5, 42)) +
  labs(x = "Gestational age (week, Actual)", 
       y = "Gestational age (week, Predicted)") +
  theme_bw() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 10),
        legend.position = c(0,1), legend.justification = c(0,1),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = "white", size = 10),
        strip.background = element_rect(fill = ggsci::pal_aaas()(2)[1])) +
  facet_wrap(vars(subject_id), ncol = 6) +
  geom_text(data = info_subject, 
            size = 2.5,
            aes(x = -Inf, 
                y = Inf, 
                hjust   = -0.1,
                vjust   = 1,
                label = paste("RMSE:", round(rmse, 2), 
                              "\nMAE:", round(median, 2), 
                              "\nAdjusted R2:", round(r2, 2))
            )) 


plot

ggsave(
  plot,
  filename = "validation_plot_subject_id.pdf",
  width = 9,
  height = 7,
  bg = "transparent"
)

ggsave(
  plot,
  filename = "validation_plot_subject_id.png",
  width = 9,
  height = 7,
  bg = "transparent"
)



plot <- 
  info_subject %>% 
  dplyr::mutate(subject_id = factor(subject_id, levels = rev(info_subject$subject_id))) %>% 
  tidyr::pivot_longer(cols = -c(subject_id, class), 
                      names_to = "measure", values_to = "value") %>% 
  dplyr::mutate(measure = factor(measure, levels = c("rmse", "median", "r2"))) %>% 
  ggplot(aes(x = value, y = subject_id)) +
  geom_segment(aes(x = 0, y = subject_id, 
                   xend = value, yend = subject_id, color = as.character(class)), 
               show.legend = FALSE) +
  geom_point(aes(color = as.character(class)), 
             shape = 16, size = 3,
             show.legend = FALSE) +
  ggsci::scale_color_aaas() +
  labs(x = "", y = "Subject ID") +
  facet_wrap(vars(measure), ncol = 3, scales = "free_x") +
  theme_bw() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 10),
        legend.position = c(0,1), legend.justification = c(0,1),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = "white", size = 10),
        strip.background = element_rect(fill = ggsci::pal_aaas()(10)[8]))

plot

ggsave(plot, filename = "rmse_for_each_preson.pdf", width = 7, height = 7,
       bg = "transparent")

ggsave(plot, filename = "rmse_for_each_preson.png", width = 7, height = 7,
       bg = "transparent")




##external validation dataset
sum(marker_rf[[1]] %in% external_variable_info$name2)
sum(marker_rf[[2]] %in% external_variable_info$name2)
sum(marker_rf[[3]] %in% external_variable_info$name2)
sum(marker_rf[[4]] %in% external_variable_info$name2)
sum(marker_rf[[5]] %in% external_variable_info$name2)

external_predicted_result <- vector(mode = "list", length = length(class))

# for(idx in class){
#   cat(idx, " ")
#   dis_id <-
#     sample_info %>%
#     dplyr::filter(class != idx) %>%
#     dplyr::filter(weeks_to_dlvry > 0) %>%
#     dplyr::pull(sample_id)
# 
#   dis_y <-
#     sample_info %>%
#     dplyr::filter(class != idx) %>%
#     dplyr::filter(weeks_to_dlvry > 0) %>%
#     dplyr::pull(g_stage) %>%
#     as.matrix()
# 
#   marker <- marker_rf[[idx]]
# 
#   marker <- marker[marker %in% external_variable_info$name2]
# 
#   dis_x <-
#     expression_data[marker,dis_id] %>%
#     as.matrix() %>%
#     t()
# 
#   val_idx <-
#     match(marker, external_variable_info$name2)
# 
#   val_x <-
#     external_expression_data[val_idx,] %>%
#     as.matrix() %>%
#     t()
# 
# colnames(val_x) <-
#    external_variable_info$name2[val_idx]
# 
#   rf_regression <-
#     randomForest(x = dis_x,
#                  y = dis_y[,1],
#                  replace = TRUE,
#                  importance = TRUE,
#                  proximity = TRUE,
#                  mtry = 4)
# 
#   predicted_y <-
#     predict(
#       object = rf_regression,
#       newdata = val_x
#       # type = "response"
#     )
# 
#   external_predicted_result[[idx]] <-
#     data.frame(sample_id = names(predicted_y),
#                # true = val_y,
#                predicted = predicted_y,
#                stringsAsFactors = FALSE)
# }
# save(external_predicted_result, file = "external_predicted_result")

load("external_predicted_result")

external_predicted_result <- 
  external_predicted_result %>% 
  do.call(cbind, .)

external_predicted_result <- 
  external_predicted_result[,c(1,2,4,6,8,10)]

colnames(external_predicted_result)[-1] <- paste("model", 1:5, sep = "_")

external_predicted_result$mean <- 
  external_predicted_result %>% 
  apply(1, function(x){
    mean(as.numeric(x[-1]))
  })

write.csv(external_predicted_result,
          "external_predicted_result.csv",
          row.names = FALSE)

##predict samples afer birth
after_birth_predicted_result <- vector(mode = "list", length = length(class))

# for(idx in class){
#   cat(idx, " ")
#   dis_id <-
#     sample_info %>%
#     dplyr::filter(class != idx) %>%
#     dplyr::filter(weeks_to_dlvry > 0) %>%
#     dplyr::pull(sample_id)
# 
#   dis_y <-
#     sample_info %>%
#     dplyr::filter(class != idx) %>%
#     dplyr::filter(weeks_to_dlvry > 0) %>%
#     dplyr::pull(g_stage) %>%
#     as.matrix()
# 
#   marker <- marker_rf[[idx]]
# 
#   dis_x <-
#     expression_data[marker,dis_id] %>%
#     as.matrix() %>%
#     t()
# 
#   val_y <-
#     sample_info %>%
#     dplyr::filter(weeks_to_dlvry < 0) %>%
#     dplyr::pull(g_stage)
# 
#   val_id <-
#     sample_info %>%
#     dplyr::filter(weeks_to_dlvry < 0) %>%
#     dplyr::pull(sample_id)
# 
#   val_x <-
#     expression_data[marker, val_id] %>%
#     as.matrix() %>%
#     t()
# 
#   rf_regression <-
#     randomForest(x = dis_x,
#                  y = dis_y[,1],
#                  replace = TRUE,
#                  importance = TRUE,
#                  proximity = TRUE,
#                  mtry = 4)
# 
#   predicted_y <-
#     predict(
#       object = rf_regression,
#       newdata = val_x
#       # type = "response"
#     )
# 
#   after_birth_predicted_result[[idx]] <-
#     data.frame(sample_id = names(predicted_y),
#                true = val_y,
#                predicted = predicted_y,
#                stringsAsFactors = FALSE)
# }
# 
# save(after_birth_predicted_result, file = "after_birth_predicted_result")

load("after_birth_predicted_result")


after_birth_predicted_result <- 
  after_birth_predicted_result %>%
  do.call(cbind, .)

after_birth_predicted_result <- 
  after_birth_predicted_result[,c(1,2,3, 6, 9, 12, 15)]

colnames(after_birth_predicted_result)[-c(1:2)] <- paste("model", 1:5, sep = "_")


after_birth_predicted_result$mean <- 
  after_birth_predicted_result %>% 
  apply(1, function(x){
    mean(as.numeric(x[-c(1:2)]))
  })



plot <- 
  after_birth_predicted_result %>% 
  ggplot(aes(true, mean)) +
  # geom_abline(slope = 1, intercept = 0, color = "grey") +
  geom_point(shape = 16, size = 3, color = "black") +
  geom_smooth(color = "skyblue") +
  # scale_x_continuous(limits = c(5, 42)) +
  # scale_y_continuous(limits = c(5, 42)) +
  labs(x = "Gestational age (GA, Actual)", 
       y = "Gestational age (GA, Predicted)") +
  theme_bw() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 13),
        legend.position = c(0,1), legend.justification = c(0,1),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))

plot

ggsave(
  plot,
  filename = "validation_plot_after_birth.pdf",
  width = 7,
  height = 7,
  bg = "transparent"
)

ggsave(
  plot,
  filename = "validation_plot_after_birth.png",
  width = 7,
  height = 7,
  bg = "transparent"
)


# write.csv(after_birth_predicted_result, 
#           "after_birth_predicted_result.csv", 
#           row.names = FALSE)

# ###predict 200 sample from other cohort
# val_id <- 
# sample_info_old %>% 
#   dplyr::filter(!sample_id %in% sample_info$sample_id) %>% 
#   dplyr::filter(!is.na(g_stage)) %>% 
#   dplyr::filter(stringr::str_detect(subject_id, "P|C")) %>% 
#   pull(sample_id)
# 
# 
# ##predict samples afer birth
# validation200_predicted_result <- vector(mode = "list", length = length(class))
# for(idx in class){
#   cat(idx, " ")
#   dis_id <-
#     sample_info %>%
#     dplyr::filter(class != idx) %>%
#     dplyr::filter(weeks_to_dlvry > 0) %>%
#     dplyr::pull(sample_id)
# 
#   dis_y <-
#     sample_info %>%
#     dplyr::filter(class != idx) %>%
#     dplyr::filter(weeks_to_dlvry > 0) %>%
#     dplyr::pull(g_stage) %>%
#     as.matrix()
# 
#   marker <- marker_rf[[idx]]
# 
#   dis_x <-
#     expression_data[marker,dis_id] %>%
#     as.matrix() %>%
#     t()
# 
#   val_y <-
#     sample_info_old[which(sample_info_old$sample_id %in% val_id),] %>% 
#     dplyr::pull(g_stage)
# 
#   val_x <-
#     expression_data_old[marker, val_id] %>%
#     as.matrix() %>%
#     t()
# 
#   rf_regression <-
#     randomForest(x = dis_x,
#                  y = dis_y[,1],
#                  replace = TRUE,
#                  importance = TRUE,
#                  proximity = TRUE,
#                  mtry = 4)
# 
#   predicted_y <-
#     predict(
#       object = rf_regression,
#       newdata = val_x
#       # type = "response"
#     )
# 
#   validation200_predicted_result[[idx]] <-
#     data.frame(sample_id = names(predicted_y),
#                true = val_y,
#                predicted = predicted_y,
#                stringsAsFactors = FALSE)
# }
# 
# 
# save(validation200_predicted_result,
#      file = "validation200_predicted_result")
# 
load("validation200_predicted_result")


validation200_predicted_result <-
  validation200_predicted_result %>%
  do.call(cbind, .)

validation200_predicted_result <-
  validation200_predicted_result[,c(1,2,3, 6, 9, 12, 15)]

colnames(validation200_predicted_result)[-c(1:2)] <- paste("model", 1:5, sep = "_")


validation200_predicted_result$mean <-
  validation200_predicted_result %>%
  apply(1, function(x){
    mean(as.numeric(x[-c(1:2)]))
  })

# write.csv(validation200_predicted_result, 
#           "validation200.csv", 
#           row.names = FALSE)


plot <-
  validation200_predicted_result %>%
  ggplot(aes(true, mean)) +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  geom_point(shape = 16, size = 3, color = "black") +
  geom_smooth(color = "skyblue") +
  scale_x_continuous(limits = c(5, 42)) +
  scale_y_continuous(limits = c(5, 42)) +
  labs(x = "Gestational age (GA, Actual)",
       y = "Gestational age (GA, Predicted)") +
  theme_bw() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 13),
        legend.position = c(0,1), legend.justification = c(0,1),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))

plot

ggsave(
  plot,
  filename = "validation200_plot.pdf",
  width = 7,
  height = 7,
  bg = "transparent"
)

ggsave(
  plot,
  filename = "validation200_plot.png",
  width = 7,
  height = 7,
  bg = "transparent"
)


cal_rmse(predicted = validation200_predicted_result$mean,
         true = validation200_predicted_result$true)

cal_r2(predicted = validation200_predicted_result$mean,
       true = validation200_predicted_result$true)

###200 result from deepMet
masstools::setwd_project()
validation200_predicted_result_cnn <- 
  readr::read_csv("data_analysis_20200609/external_deepmet/external_validation_dataset2.csv")
validation200_predicted_result_cnn
validation200_predicted_result_cnn$image <- 
  stringr::str_replace(validation200_predicted_result_cnn$image, ".png", "") %>% 
  paste("X", ., sep = "")
dim(validation200_predicted_result)
dim(validation200_predicted_result_cnn)

validation200_predicted_result_cnn$mean <- 
  apply(validation200_predicted_result_cnn[,-1], 1, mean)

validation200_predicted_result_cnn <- 
  validation200_predicted_result_cnn %>% 
  dplyr::left_join(validation200_predicted_result[,1:2], by = c("image" = "sample_id")) %>% 
  dplyr::filter(!is.na(true))

plot <- 
  validation200_predicted_result_cnn %>% 
  ggplot(aes(x = true, y = mean)) +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  geom_point(shape = 16, size = 3, color = "black") +
  geom_smooth(color = "skyblue") +
  scale_x_continuous(limits = c(5, 42)) +
  scale_y_continuous(limits = c(5, 42)) +
  labs(x = "Gestational age (GA, Actual)",
       y = "Gestational age (GA, Predicted)") +
  theme_bw() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 13),
        legend.position = c(0,1), legend.justification = c(0,1),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))

plot

ggsave(
  plot,
  filename = "validation200_plo_cnnt.pdf",
  width = 7,
  height = 7,
  bg = "transparent"
)

ggsave(
  plot,
  filename = "validation200_plot_cnn.png",
  width = 7,
  height = 7,
  bg = "transparent"
)


cal_rmse(predicted = validation200_predicted_result_cnn$mean,
         true = validation200_predicted_result_cnn$true)

cal_r2(predicted = validation200_predicted_result_cnn$mean,
       true = validation200_predicted_result_cnn$true)

cal_rmse(predicted = validation200_predicted_result$mean,
         true = validation200_predicted_result$true)

cal_r2(predicted = validation200_predicted_result$mean,
       true = validation200_predicted_result$true)


# 
# 
# 
# write.csv(validation200_predicted_result, 
#           "validation200.csv", 
#           row.names = FALSE)


info1 <- after_birth_predicted_result[,c(1,2)]
# info2 <- validation200_predicted_result[,c(1,2)]

info <- info1

# write.csv(info, "info.csv", row.names = FALSE)



####predict the samples from the mz and rt shift
sum(marker_rf[[1]] %in% mzrtshift_variable_info$name2)
sum(marker_rf[[2]] %in% mzrtshift_variable_info$name2)
sum(marker_rf[[3]] %in% mzrtshift_variable_info$name2)
sum(marker_rf[[4]] %in% mzrtshift_variable_info$name2)
sum(marker_rf[[5]] %in% mzrtshift_variable_info$name2)

mzrtshift_predicted_result <- vector(mode = "list", length = length(class))

# for(idx in class){
#   cat(idx, " ")
#   
#   dis_id <-
#     sample_info %>%
#     dplyr::filter(class != idx) %>%
#     dplyr::filter(weeks_to_dlvry > 0) %>%
#     dplyr::pull(sample_id)
#   
#   dis_y <-
#     sample_info %>%
#     dplyr::filter(class != idx) %>%
#     dplyr::filter(weeks_to_dlvry > 0) %>%
#     dplyr::pull(g_stage) %>%
#     as.matrix()
#   
#   marker <- marker_rf[[idx]]
#   
#   marker <- marker[marker %in% mzrtshift_variable_info$name2]
#   
#   dis_x <-
#     expression_data[marker,dis_id] %>%
#     as.matrix() %>%
#     t()
#   
#   val_idx <-
#     match(marker, mzrtshift_variable_info$name2)
#   
#   val_x <-
#     mzrtshift_expression_data[val_idx,
#                               which(colnames(mzrtshift_expression_data) %in% sample_info$sample_id[sample_info$class == idx & sample_info$weeks_to_dlvry >0 ])] %>%
#     as.matrix() %>%
#     t()
#   
#   colnames(val_x) <-
#     mzrtshift_variable_info$name2[val_idx]
#   
#   val_y <- 
#    sample_info$g_stage[match(rownames(val_x), sample_info$sample_id)]
#   
#   rf_regression <-
#     randomForest(x = dis_x,
#                  y = dis_y[,1],
#                  replace = TRUE,
#                  importance = TRUE,
#                  proximity = TRUE,
#                  mtry = 4)
#   
#   predicted_y <-
#     predict(
#       object = rf_regression,
#       newdata = val_x
#       # type = "response"
#     )
#   
#   mzrtshift_predicted_result[[idx]] <-
#     data.frame(sample_id = names(predicted_y),
#                true = val_y,
#                predicted = predicted_y,
#                stringsAsFactors = FALSE)
# }
# 
# save(mzrtshift_predicted_result, file = "mzrtshift_predicted_result")

load("mzrtshift_predicted_result")

mzrtshift_predicted_result <- 
  mzrtshift_predicted_result %>% 
  do.call(rbind, .)

# write.csv(mzrtshift_predicted_result,
#           "mzrtshift_predicted_result.csv",
#           row.names = FALSE)


plot <- 
  mzrtshift_predicted_result %>% 
  dplyr::left_join(sample_info[,c("sample_id","subject_id","class")], by = "sample_id") %>% 
  ggplot(aes(true, predicted)) +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  geom_point(shape = 16, aes(color = as.character(class))) +
  ggsci::scale_color_aaas() +
  guides(color = guide_legend(title = "Fold (cross validation)", 
                              override.aes = list(size = 3))) +
  geom_smooth(color = "skyblue") +
  scale_x_continuous(limits = c(5, 42)) +
  scale_y_continuous(limits = c(5, 42)) +
  labs(x = "Gestational age (GA, Actual)", y = "Gestational age (GA, Predicted)") +
  theme_bw() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 13),
        legend.position = c(0,1), legend.justification = c(0,1),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))

plot

ggsave(
  plot,
  filename = "mzrtshift_validation_plot_all.pdf",
  width = 7,
  height = 7,
  bg = "transparent"
)

ggsave(
  plot,
  filename = "mzrtshift_validation_plot_all.png",
  width = 7,
  height = 7,
  bg = "transparent"
)


###
cal_rmse(mzrtshift_predicted_result$true, mzrtshift_predicted_result$predicted)

cor.test(mzrtshift_predicted_result$true, mzrtshift_predicted_result$predicted, method = "spearman")

cal_r2(mzrtshift_predicted_result$predicted, mzrtshift_predicted_result$true)

idx <- 5
plot <- 
  mzrtshift_predicted_result %>% 
  dplyr::left_join(sample_info[,c("sample_id","subject_id","class")], by = "sample_id") %>% 
  dplyr::filter(class == idx) %>% 
  ggplot(aes(true, predicted)) +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  geom_point(shape = 16, color = ggsci::pal_aaas()(5)[idx], size = 3) +
  geom_smooth(color = "skyblue") +
  scale_x_continuous(limits = c(5, 42)) +
  scale_y_continuous(limits = c(5, 42)) +
  labs(x = "Gestational age (GA, Actual)", y = "Gestational age (GA, Predicted)") +
  theme_bw() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 13),
        legend.position = c(0,1), legend.justification = c(0,1),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))

plot

ggsave(
  plot,
  filename = paste("mzrtshift_validation_plot_class", idx, ".pdf", sep = ""),
  width = 7,
  height = 7,
  bg = "transparent"
)

ggsave(
  plot,
  filename = paste("mzrtshift_validation_plot_class", idx, ".png", sep = ""),
  width = 7,
  height = 7,
  bg = "transparent"
)


temp_data <-
  mzrtshift_predicted_result %>% 
  dplyr::left_join(sample_info[,c("sample_id","subject_id","class")], by = "sample_id") %>% 
  dplyr::filter(class == idx)

cal_rmse(temp_data$true,
         temp_data$predicted)

cal_r2(predicted = temp_data$predicted, 
       true = temp_data$true)

cor.test(temp_data$true,
         temp_data$predicted, method = "spearman")

plot <- 
  mzrtshift_predicted_result %>% 
  dplyr::left_join(sample_info[,c("sample_id","subject_id","class")], by = "sample_id") %>% 
  ggplot(aes(true, predicted)) +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  geom_point(shape = 16, size = 1, color = "black") +
  geom_smooth(color = "skyblue", size = 0.5) +
  scale_x_continuous(limits = c(5, 42)) +
  scale_y_continuous(limits = c(5, 42)) +
  labs(x = "Gestational age (GA, Actual)", 
       y = "Gestational age (GA, Predicted)") +
  theme_bw() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 13),
        legend.position = c(0,1), legend.justification = c(0,1),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)) +
  facet_wrap(vars(subject_id), ncol = 6)

plot

ggsave(
  plot,
  filename = "mzrtshift_validation_plot_subject_id.pdf",
  width = 9,
  height = 7,
  bg = "transparent"
)

ggsave(
  plot,
  filename = "mzrtshift_validation_plot_subject_id.png",
  width = 9,
  height = 7,
  bg = "transparent"
)


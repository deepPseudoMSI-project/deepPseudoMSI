
#-------------------------------------------------------------------------------
##cnn mz rt intensity shift
masstools::setwd_project()
rm(list = ls())
source("code/tools.R")

setwd("data_analysis/deepPseudoMSI_prediction_result/normal_dataset/")

fold1 <- readr::read_csv("predicted_224x224_fold1.csv", col_names = c("true", "predicted"))
fold2 <- readr::read_csv("predicted_224x224_fold2.csv", col_names = c("true", "predicted"))
fold3 <- readr::read_csv("predicted_224x224_fold3.csv", col_names = c("true", "predicted"))
fold4 <- readr::read_csv("predicted_224x224_fold4.csv", col_names = c("true", "predicted"))
fold5 <- readr::read_csv("predicted_224x224_fold5.csv", col_names = c("true", "predicted"))

sample_info <- readr::read_csv("sample_info.csv")
# sample_info <-
#   sample_info %>% 
#   dplyr::filter(!stringr::str_detect(image_name, "_[789]{1}"))

fold1 <- cbind(sample_info[sample_info$class == 1,], fold1) 
fold2 <- cbind(sample_info[sample_info$class == 2,], fold2) 
fold3 <- cbind(sample_info[sample_info$class == 3,], fold3) 
fold4 <- cbind(sample_info[sample_info$class == 4,], fold4) 
fold5 <- cbind(sample_info[sample_info$class == 5,], fold5) 

predicted_result <- rbind(fold1, fold2, fold3, fold4, fold5)

idx <- which(abs(predicted_result$true -predicted_result$predicted) > 20)

predicted_result[idx,]

predicted_result <- predicted_result[-idx,]
# write.csv(predicted_result, "predicted_result.csv", row.names = FALSE)

rmse <- cal_rmse(predicted_result$predicted, predicted_result$true) %>% round(2)
r2 <- cal_r2(predicted_result$predicted, predicted_result$true) %>% round(2)
median <- median(abs(predicted_result$predicted - predicted_result$true)) %>% round(2)

plot <- 
  predicted_result %>% 
  dplyr::filter(weeks_to_dlvry > 0) %>% 
  dplyr::filter(!stringr::str_detect(image_name, "\\_[1-9]{1}\\.png")) %>% 
  ggplot(aes(true, predicted)) +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  geom_point(shape = 16, aes(color = as.character(class))) +
  ggsci::scale_color_aaas() +
  guides(color = guide_legend(title = "Fold (cross validation)", 
                              override.aes = list(size = 3))) +
  geom_smooth(color = "skyblue") +
  scale_x_continuous(limits = c(5, 42)) +
  scale_y_continuous(limits = c(5, 42)) +
  labs(x = "Gestational age (week, Actual)", y = "Gestational age (week, Predicted)") +
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

cal_rmse(predicted_result$predicted, predicted_result$true)
cal_r2(predicted_result$predicted, predicted_result$true)
median(abs(predicted_result$predicted - predicted_result$true))

ggsave(
  plot,
  filename = "validation_plot_all.pdf",
  width = 7,
  height = 7,
  bg = "transparent"
)

cal_rmse(predicted_result$predicted, predicted_result$true)
cal_r2(predicted_result$predicted, predicted_result$true)
mean(abs(predicted_result$predicted - predicted_result$true))
median(abs(predicted_result$predicted - predicted_result$true))

test <- 
  predicted_result %>% 
  dplyr::filter(weeks_to_dlvry > 0) %>% 
  dplyr::mutate(sample_id = stringr::str_extract(image_name, "[0-9]{1,5}_"))

test <- 
  test %>% 
  dplyr::mutate(sample_id = dplyr::case_when(
    is.na(sample_id) ~ stringr::str_replace(image_name, "\\.png", ""),
    TRUE ~ stringr::str_replace(sample_id, "_", "")
    # TRUE ~ NA
  ))

test <- 
  test %>% 
  dplyr::group_by(sample_id) %>% 
  dplyr::mutate(predicted_mean = mean(predicted),
                predicted_sd = sd(predicted),
                predicted_sem = sd(predicted) / (length(predicted) - 1)) %>% 
  dplyr::ungroup()


plot <- 
  test %>% 
  dplyr::filter(!stringr::str_detect(image_name, "\\_[1-9]{1}\\.png")) %>% 
  ggplot(aes(true, predicted_mean)) +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  geom_point(shape = 16, aes(color = as.character(class)), alpha = 0.8) +
  geom_errorbar(aes(x = true, ymin = predicted_mean - predicted_sd, 
                    ymax = predicted_mean + predicted_sd,
                    color = as.character(class)), width = 0) +
  ggsci::scale_color_aaas() +
  guides(color = guide_legend(title = "Fold (cross validation)", 
                              override.aes = list(size = 3))) +
  geom_smooth(color = "skyblue") +
  scale_x_continuous(limits = c(5, 42)) +
  scale_y_continuous(limits = c(5, 42)) +
  labs(x = "Gestational age (week, Actual)", y = "Gestational age (week, Predicted)") +
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

ggsave(
  plot,
  filename = "validation_plot_all_mean_sd.pdf",
  width = 7,
  height = 7,
  bg = "transparent"
)

test <- 
  predicted_result %>% 
  dplyr::filter(weeks_to_dlvry > 0) %>% 
  dplyr::mutate(sample_id = stringr::str_extract(image_name, "[0-9]{1,5}_"))

test <- 
  test %>% 
  dplyr::mutate(sample_id = dplyr::case_when(
    is.na(sample_id) ~ stringr::str_replace(image_name, "\\.png", ""),
    TRUE ~ stringr::str_replace(sample_id, "_", "")
    # TRUE ~ NA
  ))

raw <- 
  test %>% 
  dplyr::filter(!stringr::str_detect(image_name, "\\_[1-9]{1}")) %>% 
  dplyr::arrange(sample_id) %>% 
  dplyr::distinct(sample_id, .keep_all = TRUE)

shift1 <- 
  test %>% 
  dplyr::filter(stringr::str_detect(image_name, "\\_2{1}")) %>% 
  dplyr::filter(sample_id %in% raw$sample_id) %>% 
  dplyr::arrange(sample_id) %>% 
  dplyr::distinct(sample_id, .keep_all = TRUE)

shift2 <- 
  test %>% 
  dplyr::filter(stringr::str_detect(image_name, "\\_5{1}")) %>% 
  dplyr::filter(sample_id %in% raw$sample_id) %>% 
  dplyr::arrange(sample_id) %>% 
  dplyr::distinct(sample_id, .keep_all = TRUE)

shift3 <- 
  test %>% 
  dplyr::filter(stringr::str_detect(image_name, "\\_8{1}")) %>% 
  dplyr::filter(sample_id %in% raw$sample_id) %>% 
  dplyr::arrange(sample_id) %>% 
  dplyr::distinct(sample_id, .keep_all = TRUE)

temp_data <- 
  shift3$predicted - raw$predicted %>% 
  data.frame(true = raw$true, error = ., stringsAsFactors = FALSE)

temp_data %>% 
  ggplot(aes(true, error)) +
  geom_point() +
  # geom_smooth(fill = "transparent") +
  theme_bw()

temp_data <-
  predicted_result %>% 
  dplyr::filter(weeks_to_dlvry > 0) %>% 
  dplyr::filter(!stringr::str_detect(image_name, "\\_[1-9]{1}\\.png"))

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

# save(info_fold, file = "info_fold")

plot <- 
  predicted_result %>% 
  dplyr::filter(weeks_to_dlvry > 0) %>% 
  dplyr::mutate(fold = paste("Fold", class), sep = " ") %>% 
  dplyr::filter(!stringr::str_detect(image_name, "\\_[1-9]{1}\\.png")) %>% 
  # dplyr::filter(class == idx) %>% 
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

temp_data <- 
  predicted_result %>% 
  dplyr::filter(weeks_to_dlvry > 0) %>% 
  dplyr::filter(!stringr::str_detect(image_name, "\\_[1-9]{1}\\.png"))

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

# save(info_subject, file = "info_subject")

plot <- 
  predicted_result %>% 
  dplyr::filter(weeks_to_dlvry > 0) %>% 
  dplyr::filter(!stringr::str_detect(image_name, "\\_[1-9]{1}\\.png")) %>% 
  ggplot(aes(true, predicted)) +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  geom_point(shape = 16, size = 1, aes(color = as.character(class)), show.legend = FALSE) +
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

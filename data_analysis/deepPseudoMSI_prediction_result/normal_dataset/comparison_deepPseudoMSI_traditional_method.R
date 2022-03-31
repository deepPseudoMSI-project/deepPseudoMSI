#to avoind source
# no_exist_function()

masstools::setwd_project()
rm(list = ls())
source("code/tools.R")

##result from deepPseudoMSI
deeppseudomsi_normal_dataset_prediction_result <-
  readr::read_csv(
    "data_analysis/deepPseudoMSI_prediction_result/normal_dataset/deeppseudomsi_normal_dataset_prediction_result.csv"
  )

deeppseudomsi_rt_shift_dataset_prediction_result <-
  readr::read_csv(
    "data_analysis/deepPseudoMSI_prediction_result/rt_shift_dataset/deeppseudomsi_rt_shift_dataset_prediction_result.csv"
  )

traditional_normal_dataset_prediction_result <-
  readr::read_csv("data_analysis/traditional_method_prediction_result/normal_dataset/traditional_normal_dataset_prediction_result.csv")

traditional_rt_shift_dataset_predicted_result <-
  read_csv("data_analysis/traditional_method_prediction_result/rt_shift_dataset/traditional_rt_shift_dataset_predicted_result.csv")

setwd("data_analysis/comparison_deepPseudoMSI_traditional_method/")

plot(
  deeppseudomsi_normal_dataset_prediction_result$true,
  deeppseudomsi_normal_dataset_prediction_result$predicted
)

plot(
  traditional_rt_shift_dataset_predicted_result$true,
  traditional_rt_shift_dataset_predicted_result$predicted
)

error1 <-
  deeppseudomsi_normal_dataset_prediction_result$true - deeppseudomsi_normal_dataset_prediction_result$predicted

error2 <-
  deeppseudomsi_rt_shift_dataset_prediction_result$true - deeppseudomsi_rt_shift_dataset_prediction_result$predicted

error3 <-
  traditional_normal_dataset_prediction_result$true - traditional_normal_dataset_prediction_result$predicted

error4 <-
  traditional_rt_shift_dataset_predicted_result$true - traditional_rt_shift_dataset_predicted_result$predicted

error4 %>%
  density() %>%
  plot()

error <- rbind(
  error1 %>% data.frame(
    class = "deepMet1",
    error = .,
    stringsAsFactors = FALSE
  ),
  error2 %>% data.frame(
    class = "deepMet2",
    error = .,
    stringsAsFactors = FALSE
  ),
  error3 %>% data.frame(
    class = "Peak1",
    error = .,
    stringsAsFactors = FALSE
  ),
  error4 %>% data.frame(
    class = "Peak2",
    error = .,
    stringsAsFactors = FALSE
  )
)

plot <-
  error %>%
  ggplot() +
  geom_density(mapping = aes(x = error, color = class), alpha = 0.5) +
  ggsci::scale_fill_aaas() +
  ggsci::scale_color_aaas() +
  labs(x = "Error (Actual - Predicted)", y = "Density") +
  guides(color = guide_legend(title = "Class")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.105)) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    plot.background = element_rect(fill = "transparent",  color = NA),
    panel.background = element_rect(fill = "transparent",  color = NA),
    legend.background = element_rect(fill = "transparent",  color = NA),
    legend.position = c(0, 1),
    legend.justification = c(0, 1)
  )

plot

ggsave(plot, filename = "compare_deepmet_tranditional_density.pdf", width = 7, height = 7,
       bg = "transparent")

library(ggalluvial)

error1 <-
  deeppseudomsi_normal_dataset_prediction_result$true - deeppseudomsi_normal_dataset_prediction_result$predicted

error2 <-
  deeppseudomsi_rt_shift_dataset_prediction_result$true - deeppseudomsi_rt_shift_dataset_prediction_result$predicted

error3 <-
  traditional_normal_dataset_prediction_result$true - traditional_normal_dataset_prediction_result$predicted

error4 <-
  traditional_rt_shift_dataset_predicted_result$true - traditional_rt_shift_dataset_predicted_result$predicted

error1 <-
  error1 %>%
  abs() %>%
  data.frame(
    sample_name = deeppseudomsi_normal_dataset_prediction_result$sample_id,
    error1 = .,
    stringsAsFactors = FALSE
  )

error2 <-
  error2 %>%
  abs() %>%
  data.frame(
    sample_name = deeppseudomsi_rt_shift_dataset_prediction_result$sample_id,
    error2 = .,
    stringsAsFactors = FALSE
  )

error3 <-
  error3 %>%
  abs() %>%
  data.frame(
    sample_name = traditional_normal_dataset_prediction_result$sample_id,
    error3 = .,
    stringsAsFactors = FALSE
  )

error4 <-
  error4 %>%
  abs() %>%
  data.frame(
    sample_name = traditional_rt_shift_dataset_predicted_result$sample_id,
    error4 = .,
    stringsAsFactors = FALSE
  )


dim(error4)

error <-
  error1 %>%
  dplyr::inner_join(error2, by = "sample_name") %>%
  dplyr::inner_join(error3, by = "sample_name") %>%
  dplyr::inner_join(error4, by = "sample_name")

error <-
  error %>%
  dplyr::mutate(
    error1_class =
      case_when(
        error1 >= 0 & error1 < 2 ~ "0,2",
        error1 >= 2 & error1 < 5 ~ "2,5",
        error1 >= 5 ~ ">5"
      ),
    error2_class =
      case_when(
        error2 >= 0 & error2 < 2 ~ "0,2",
        error2 >= 2 & error2 < 5 ~ "2,5",
        error2 >= 5 ~ ">5"
      ),
    error3_class =
      case_when(
        error3 >= 0 & error3 < 2 ~ "0,2",
        error3 >= 2 & error3 < 5 ~ "2,5",
        error3 >= 5 ~ ">5"
      ),
    error4_class =
      case_when(
        error4 >= 0 & error4 < 2 ~ "0,2",
        error4 >= 2 & error4 < 5 ~ "2,5",
        error4 >= 5 ~ ">5"
      ),
  )

plot <-
  error %>%
  dplyr::select(sample_name, error1_class:error4_class) %>%
  tidyr::pivot_longer(cols = -sample_name) %>%
  dplyr::mutate(freq = 1) %>%
  dplyr::mutate(value = factor(value, levels = c(">5", "2,5", "0,2"))) %>%
  as.data.frame() %>%
  ggplot(aes(
    x = name,
    y = freq,
    stratum = value,
    alluvium = sample_name,
    fill = value,
    label = value
  )) +
  scale_x_discrete(expand = c(.1, .1)) +
  ggsci::scale_fill_jama() +
  ggalluvial::geom_flow(show.legend = TRUE) +
  labs(x = "", y = "") +
  ggalluvial::geom_stratum(alpha = 1) +
  # geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    plot.background = element_rect(fill = "transparent",
                                   color = NA),
    panel.background = element_rect(fill = "transparent",
                                    color = NA),
    legend.background = element_rect(fill = "transparent",
                                     color = NA)
  )
plot

ggsave(plot, filename = "sankey_deepmet_transtational.pdf",
       width = 8, height = 7, bg = "transparent")

table(error$error1_class) / nrow(error)
table(error$error2_class) / nrow(error)
table(error$error3_class) / nrow(error)
table(error$error4_class) / nrow(error)

sum(error$error3_class == '0,2' &
      error$error4_class == '2,5') * 100 / nrow(error)

sum(error$error3_class == '2,5' &
      error$error4_class == '>5') * 100 / nrow(error)

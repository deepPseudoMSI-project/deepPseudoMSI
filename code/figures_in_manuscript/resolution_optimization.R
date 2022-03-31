##
# no_function()

##224 * 224
masstools::setwd_project()
rm(list = ls())
source("code/tools.R")
setwd("data_analysis/resolution_optimization/224_224/")

library(tidyverse)
library(tidyr)

training <- readr::read_csv("results_224x224.csv")
test <- readr::read_csv("validation_224x224.csv")

plot <- 
  training %>% 
  as.data.frame() %>% 
  tidyr::pivot_longer(cols = -epoch, names_to = "class", values_to = "rmse") %>% 
  ggplot(aes(epoch, rmse, color = class)) +
  geom_point(shape = 16, size = 2) +
  geom_line() +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 1, width = 1), 
                              title = "Dataset")) +
  ggsci::scale_color_aaas() +
  labs(x = "Epoch", y = "Root Mean Square Error (RMSE)") +
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(1,1), legend.justification = c(1,1),
        legend.background = element_rect(fill = "transparent", color = NA)
  ) 

plot

ggsave(plot, filename = "training_data.pdf",
       width = 7, height = 7, bg = "transparent")

plot1 <- 
  test %>% 
  dplyr::mutate(true = `True g_stage`, predicted = `predicted g_stage`) %>% 
  ggplot(aes(true, predicted)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(shape = 16, color = ggsci::pal_aaas()(2)[2], size = 3) +
  geom_smooth() +
  theme_bw() +
  scale_x_continuous(limits = c(8, 42)) +
  scale_y_continuous(limits = c(8, 42)) +
  labs(x = "Gestation age (week, Actual)", y = "Gestation age (week, Predicted)") +
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(1,1), legend.justification = c(1,1),
        legend.background = element_rect(fill = "transparent", color = NA)
  ) 

plot1

ggsave(plot1, filename = "validation_data.pdf",
       width = 7, height = 7, bg = "transparent")

cal_rmse <- function(predicted, true){
  sqrt(mean((predicted-true)^2))
}

cal_r2 <- function(predicted, true){
  1 - sum((predicted-true)^2)/sum((mean(true) - true)^2)
}

cal_rmse(predicted = test$`predicted g_stage`,
         true = test$`True g_stage`)

cal_r2(predicted = test$`predicted g_stage`,
       true = test$`True g_stage`)

mean(abs(test$`predicted g_stage` - 
           test$`True g_stage`))

median(abs(test$`predicted g_stage` - 
             test$`True g_stage`))

##1024 * 1024
masstools::setwd_project()
setwd("data_analysis/resolution_optimization/1024_1024/")

training <- readr::read_csv("training_results.csv")
test <- readr::read_csv("validation_results.csv")

plot <- 
  training %>% 
  as.data.frame() %>% 
  tidyr::pivot_longer(cols = -epoch, names_to = "class", values_to = "rmse") %>% 
  ggplot(aes(epoch, rmse, color = class)) +
  geom_point(shape = 16, size = 2) +
  geom_line() +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 1, width = 1), 
                              title = "Dataset")) +
  ggsci::scale_color_aaas() +
  labs(x = "Epoch", y = "Root Mean Square Error (RMSE)") +
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(1,1), legend.justification = c(1,1),
        legend.background = element_rect(fill = "transparent", color = NA)
  ) 

plot

ggsave(plot, filename = "training_data.pdf",
       width = 7, height = 7, bg = "transparent")

plot1 <- 
  test %>% 
  dplyr::mutate(true = `True g_stage`, predicted = `predicted g_stage`) %>% 
  ggplot(aes(true, predicted)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(shape = 16, color = ggsci::pal_aaas()(2)[2], size = 3) +
  geom_smooth() +
  theme_bw() +
  scale_x_continuous(limits = c(8, 42)) +
  scale_y_continuous(limits = c(8, 42)) +
  labs(x = "Gestation age (week, Actual)", y = "Gestation age (week, Predicted)") +
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(1,1), legend.justification = c(1,1),
        legend.background = element_rect(fill = "transparent", color = NA)
  ) 

plot1

ggsave(plot1, filename = "validation_data.pdf",
       width = 7, height = 7, bg = "transparent")

cal_rmse(predicted = test$`predicted g_stage`,
         true = test$`True g_stage`)

cal_r2(predicted = test$`predicted g_stage`,
       true = test$`True g_stage`)

mean(abs(test$`predicted g_stage` - 
           test$`True g_stage`))

median(abs(test$`predicted g_stage` - 
             test$`True g_stage`))

cal_rmse(predicted = test$`predicted g_stage`,
         true = test$`True g_stage`)

cal_r2(predicted = test$`predicted g_stage`,
       true = test$`True g_stage`)

median(abs(test$`predicted g_stage` - 
             test$`True g_stage`))

##compare 224 and 1024
##224 * 224
masstools::setwd_project()
setwd("data_analysis/resolution_optimization/")

training224 <- readr::read_csv("224_224/results_224x224.csv")
test224 <- readr::read_csv("224_224/validation_224x224.csv")

training1024 <- readr::read_csv("1024_1024/training_results.csv")
test1024 <- readr::read_csv("1024_1024/validation_results.csv")

cal_rmse(predicted = test224$`predicted g_stage`,
         true = test224$`True g_stage`)

cal_r2(predicted = test224$`predicted g_stage`,
       true = test224$`True g_stage`)

median(abs(test224$`predicted g_stage` - 
             test224$`True g_stage`))

cal_rmse(predicted = test1024$`predicted g_stage`,
         true = test1024$`True g_stage`)

cal_r2(predicted = test1024$`predicted g_stage`,
       true = test1024$`True g_stage`)

median(abs(test1024$`predicted g_stage` - 
             test1024$`True g_stage`))

error224 <-  test224$`True g_stage` - test224$`predicted g_stage`
error1024 <- test1024$`True g_stage` - test1024$`predicted g_stage`

error <-
  rbind(
    data.frame(
      class = "224×224",
      error = error224,
      stringsAsFactors = FALSE
    ),
    data.frame(
      class = "1024×1024",
      error = error1024,
      stringsAsFactors = FALSE
    )
  )

plot <- 
  error %>% 
  ggplot() +
  geom_density(mapping = aes(x = error, fill = class, color = class), 
               alpha = 0.5) +
  ggsci::scale_fill_aaas() +
  ggsci::scale_color_aaas() +
  labs(x = "Error (Actual - Predicted)", y = "Density") +
  guides(color = guide_legend(title = "Class")) +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 0.11)) +
  theme_bw() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        plot.background = element_rect(fill = "transparent",  color = NA),
        panel.background = element_rect(fill = "transparent",  color = NA),
        legend.background = element_rect(fill = "transparent",  color = NA),
        legend.position = c(0,1), legend.justification = c(0,1))

plot

ggsave(plot, filename = "224vs1024.pdf",
       width = 7, height = 7, bg = "transparent")




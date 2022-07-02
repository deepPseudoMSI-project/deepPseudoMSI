##
no_function()

##224 * 224
masstools::setwd_project()
rm(list = ls())
source("code/tools.R")

setwd("data_analysis/public_case_study/MTBLS1129/")

library(tidyverse)
library(tidyr)

# result1 <- readr::read_csv("densenet/predicted_224x224_fold1.csv")
# result2 <- readr::read_csv("densenet/predicted_224x224_fold2.csv")
# result3 <- readr::read_csv("densenet/predicted_224x224_fold3.csv")
#
# result1 <- result1[-nrow(result1),]
# result2 <- result2[-nrow(result2),]
# result3 <- result3[-nrow(result3),]
#
# result <-
#   rbind(result1,
#         result2,
#         result3)
#
# table(result$`ground truth`, result$predicted)

result1 <- readr::read_csv("vgg/predicted_224x224_fold1.csv")
result2 <- readr::read_csv("vgg/predicted_224x224_fold2.csv")
result3 <- readr::read_csv("vgg/predicted_224x224_fold3.csv")

result1 <- result1[-nrow(result1),]
result2 <- result2[-nrow(result2),]
result3 <- result3[-nrow(result3),]

result <-
  rbind(result1,
        result2,
        result3)

table(result$`ground truth`, result$predicted)

###accuracy
accuracy <-
  sum(result$`ground truth` == result$predicted) / nrow(result)
accuracy

##specificity true negative
specificity <-
  sum(result$`ground truth` == 0 &
        result$predicted == 0) / (
          sum(result$`ground truth` == 0 &
                result$predicted == 0) + sum(result$`ground truth` == 1 &
                                               result$predicted == 0)
        )
specificity

##Sensitivity true positive
sensitivity <-
  sum(result$`ground truth` == 1 &
        result$predicted == 1) / (
          sum(result$`ground truth` == 1 &
                result$predicted == 1) + sum(result$`ground truth` == 0 &
                                               result$predicted == 1)
        )
sensitivity

c(sensitivity, specificity, accuracy)


temp_data <-
  table(result$`ground truth`, result$predicted) %>%
  as.data.frame() %>%
  dplyr::mutate(result = case_when(Var1 == Var2 ~ "Correct",
                                   Var1 != Var2 ~ "Wrong"))

temp_data$Freq[temp_data$result == "Wrong"] <-
  temp_data$Freq[temp_data$result == "Wrong"] * -1

temp_data$ratio[temp_data$result == "Correct"] <-
  temp_data$Freq[temp_data$result == "Correct"] / max(temp_data$Freq[temp_data$result == "Correct"])
temp_data$ratio[temp_data$result == "Wrong"] <-
  temp_data$Freq[temp_data$result == "Wrong"] / max(abs(temp_data$Freq[temp_data$result == "Wrong"]))

plot <-
  temp_data %>%
  ggplot(aes(Var1, Var2)) +
  geom_tile(aes(fill = ratio),
            color = "black",
            show.legend = FALSE) +
  scale_fill_gradient2(
    low = ggsci::pal_aaas()(n = 10)[1],
    mid = "white",
    high = ggsci::pal_aaas()(n = 10)[2],
    midpoint = 0
  ) +
  geom_text(aes(label = Freq), color = "white") +
  theme_light() +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +
  scale_y_discrete(expand = expansion(mult = c(0, 0))) +
  labs(x = "", y = "")

plot

ggsave(plot,
       filename = "result.pdf",
       width = 7,
       height = 7)

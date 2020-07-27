library(dplyr)
library(tidyr)

dataset <- data.frame(id=1:5,
                      gender = c("Male","Male","Female", "Female", "Female"),
                      y = c(9,4,7,6,3),
                      stringsAsFactors = FALSE
                      )

value_perms <- gtools::permutations(n=length(dataset$y), r=length(dataset$y), v=dataset$y, set = FALSE) %>%
  t() %>%
  as_tibble() %>%
  bind_cols(select(dataset, id, gender)) %>%
  tidyr::gather("replicate", "y", -id, -gender)

gender_perms <- gtools::permutations(n=length(dataset$gender), r=length(dataset$gender), v=dataset$gender, set = FALSE) %>%
  t() %>%
  as_tibble() %>%
  bind_cols(select(dataset, id, y)) %>%
  tidyr::gather("replicate", "gender", -id, -y)

group_by(value_perms, replicate, gender) %>%
  summarise(sample_mean = mean(y)) %>%
  ungroup() %>%
  summarise(mean_of_means = mean(sample_mean))

group_by(gender_perms, replicate, gender) %>%
  summarise(sample_mean = mean(y)) %>%
  ungroup() %>%
  summarise(mean_of_means = mean(sample_mean))
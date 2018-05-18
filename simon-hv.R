library(tidyverse)
source("create_groups.R")


simon_hv <- read_csv("~/HV Matrix.csv")

simon_hv <- simon_hv[,-1]
simon_hv <- as.matrix(simon_hv)
rownames(simon_hv) <- colnames(simon_hv)

grouped_taxa_97 <- create_groups(simon_hv, 97)

write_rds(grouped_taxa_97, "hv_97.rds")
print("done with 97")

write_csv(grouped_taxa_97[[2]], "hv_97.csv")
write_csv(grouped_taxa_88[[2]], "hv_88.csv")
write_csv(grouped_taxa81[[2]], "hv_81.csv")

grouped_taxa_88 <- create_groups(simon_hv, 88)
write_rds(grouped_taxa_88, "hv_88.rds")

grouped_taxa81 <- create_groups(simon_hv, 81)
write_rds(grouped_taxa81, "hv_81.rds")

list_97 <- grouped_taxa_97[[1]]
name_list_97 <- lapply(list_97, function(x){
  paste(colnames(simon_hv)[x], collapse = ", ")
})
lapply(name_list_97, write, "hv_97.txt", append = TRUE)

list_88 <- grouped_taxa_88[[1]]
name_list_88 <- lapply(list_88, function(x){
  paste(colnames(simon_hv)[x], collapse = ", ")
})
lapply(name_list_88, write, "hv_88.txt", append = TRUE)

list_81 <- grouped_taxa81[[1]]
name_list_81 <- lapply(list_81, function(x){
  paste(colnames(simon_hv)[x], collapse = ", ")
})
lapply(name_list_81, write, "hv_81.txt", append = TRUE)



simon_hv2 <- simon_hv
simon_hv2[lower.tri(simon_hv)] <- NA

similarities <- data_frame(scores = as.vector(simon_hv2)) %>%
  filter(!is.na(scores))


ggplot(data = similarities) +
  geom_histogram(aes(x = scores), fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "HV Percent Similarity")

ggplot(data = similarities %>% filter(scores > 80)) +
  geom_histogram(aes(x = scores), fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "HV Percent Similarity", subtitle = "Above 80%")

ggplot(data = similarities %>% filter(scores > 80 & scores != 100)) +
  geom_histogram(aes(x = scores), fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "HV Percent Similarity", subtitle = "Above 80% - No 100%")

ggplot(data = similarities %>% filter(scores > 90 & scores != 100)) +
  geom_histogram(aes(x = scores), fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "HV Percent Similarity", subtitle = "Above 90% - No 100%")


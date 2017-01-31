library(tidyverse)

distance <- read_csv("distance_matrix_ALL.csv")
distance <- as.data.frame(distance)
row.names(distance) <- distance$X1

distance <- distance %>%
  select(-X1)

idMat <- data.matrix(distance)

groups97 <- create_groups(idMat, 97)
groups89 <- create_groups(idMat, 89)
groups81 <- create_groups(idMat, 81)

saveRDS(groups97, "groups97.rds")
saveRDS(groups89, "groups89.rds")
saveRDS(groups81, "groups81.rds")


library(tidyverse)

distance <- read_csv("distance_matrix_ALL.csv")
distance <- as.data.frame(distance)
row.names(distance) <- distance$X1

distance <- distance %>%
  select(-X1)

idMat <- data.matrix(distance)

groups97 <- create_groups(idMat, 97)
groups88 <- create_groups(idMat, 88)
groups81 <- create_groups(idMat, 81)
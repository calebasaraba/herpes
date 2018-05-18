library(tidyverse)
source("create_groups.R")

astro_mat <- read_csv("astv.csv")

rownames(astro_mat) <- astro_mat$X1
astro_mat <- select(astro_mat, -X1)

print("loaded data")
astro_mat <- as.matrix(astro_mat)
str(astro_mat)

grouped_taxa_93 <- create_groups(astro_mat, 93)

write_rds(grouped_taxa_93, "astv_93.rds")
print("done with 93")

grouped_taxa_83 <- create_groups(astro_mat, 83)
write_rds(grouped_taxa_83, "astv_83.rds")
print("done with 83")

write_csv(grouped)

df93 <- grouped_taxa_93[[2]]

df83 <- grouped_taxa_83[[2]]

write_csv(df93, "astv_93.csv")

write_csv(df83, "astv_83.csv")
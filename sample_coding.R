library(stringi)
library(tidyverse)

distance <- read_csv("distance_matrix_ALL.csv")
distance <- as.data.frame(distance)
row.names(distance) <- distance$X1

distance <- distance %>%
  select(-X1)

idMat <- data.matrix(distance)
ugly_names <- rownames(idMat)

create_codes <- function(prefix, code_length){
  nums <- c(1:(10^code_length-1))
  num_codes <- stri_pad_left(nums, width = code_length, pad = "0")
  sample_codes <- stri_paste(prefix, num_codes, sep ="-")
  sample_codes
}

pbz_codes <- create_codes("PBZ", 4)
pdf_codes <- create_codes("PDF", 4)

codes <- list(pbz_codes, pdf_codes)

enumerate_samples <- function(sequence_names, code_list){
  enumerated <- sequence_names
  for (k in code_list){
    for (j in k){
      locations <- which(stri_detect_regex(sequence_names, j))
      for (i in seq_along(locations)){
        enumerated[locations[i]] <- stri_paste(j, i, sep = ".")  
      }
    }
  }
  lookup <- data.frame(sequence_names, enumerated)
  lookup
}

clean_lookup <- enumerate_samples(ugly_names, codes)

rownames(idMatc) <- clean_lookup$enumerated
colnames(idMatc) <- clean_lookup$enumerated


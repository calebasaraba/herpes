testfunc <- function(x){
r <- row(x)
c <- col(x)
rname <- rownames(r)
cname <- colnames(c)
final <- c(rname,cname)
return final
}

df <-
  data.frame(
    Sample1 = character(),Sample2 = character(), Similarity = int() stringsAsFactors = FALSE
  )


insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r + 1,nrow(existingDF) + 1),] <-
    existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

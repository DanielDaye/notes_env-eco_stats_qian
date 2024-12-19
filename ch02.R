library(utils)


# 2.3.1 Functions for Creating Data
?read.table
everglades_precip <- read.table("book_data/EvergladesP.txt", header = TRUE)

#' simplifies table structure to a single vector (preserves MONTH since it is stored
#' as a header)
?unlist
unlist(everglades_precip[,-c(1,14)])

rep(everglades_precip[,1], 12)

names(everglades_precip)

eData <- data.frame(precip = unlist(everglades_precip[,-c(1,14)]),
                    year = rep(everglades_precip[,1], 12),
                    month = rep(names(everglades_precip[,-c(1,14)]), 86))
head(eData)

# 2.3.2 Simulation Example

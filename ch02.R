library(lattice)
library(tidyverse)
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
# Returns quantiles for given distribution parameters
?qnorm
qnorm(p = 0.9, mean = 2, sd = 0.75, lower.tail = TRUE)

viol_sim <- function(n = 10, nsims = 1000, mu = 2, sigma = 0.75, cr = 3) {
  #print(n)
  temp <- numeric()
  for (i in 1:nsims) {
    temp[i] <- sum(rnorm(n, mu, sigma) > cr) > 0.1*n
  }
  return(mean(temp))
}
viol_sim()
viol_sim()

# 2.4.2
read_csv("book_data/EUSE_NAT_ENV.csv")
read_csv("book_data/EUSE_USGSReportData.csv")

# 2.4.4
tab <- data.frame(TP = c(20.1,21.5,30,15.2,31,12,20,25,19,11,14,21),
                  site = c(rep("s1",3),rep("s2",3),rep("s3",3),rep("s4",3)),
                  day = c(rep(c("d1","d2","d3"),4)))
tab
tapply(tab$TP, tab$site, mean)
tapply(tab$TP, tab$day, mean)

# Info from Fig 2.3
id <- as.numeric(ordered(c(1,1,2,2,2)))
idtbl <- table(id)  # idtbl
ns <- max(idtbl)
nr <- max(id)
                  
# 2.5 Exercises
## 1a
area_circle <- 2 * pi * 2
area_circle

## 1b
n_dens <- function(x, mu = 2, sig = 1.25) {
  # Text is missing the ^2 from (1/sqrt(2*pi*sd^2))
  (1/sqrt(2*pi*sig^2)) *exp(-((x-mu)^2/(2*sig^2)))
}

x <- c(0, 4, 0.5)

n_dens(x)
dnorm(x, 2, 1.25)

## 2a
viol_sim(n = 10, mu = 2, sigma = 0.75)
viol_sim(n = 100, mu = 2, sigma = 0.75)

viol_sim(n = 10, mu = 2, sigma = 1)
viol_sim(n = 100, mu = 2, sigma = 1)

## 2b
samp = c(6, 12, 24, 48, 60, 72, 84, 96)
data.frame(samp = samp,
           normal = sapply(samp, viol_sim),
           impaired = sapply(samp, viol_sim, sigma = 1)) %>%
  pivot_longer(normal:impaired, names_to = "status") %>%
  ggplot(mapping = aes(samp, value, col = status)) +
  geom_line() + coord_cartesian(xlim = c(0,100), ylim = c(0, 1), expand = FALSE) +
  theme_bw()

## 3a & 3b
data <- read_csv("book_data/Intelligence.csv") %>%
  mutate(log_body = log(Body.weigh),
         log_brain = log(Brain.weight),
         int = Brain.weight/(Body.weigh^(2/3)))
data %>%
  ggplot(aes(log_body, log_brain)) +
  geom_point()

## 3c
dotplot(Species~int, data = data)

## 3d
# Chinchilla has an error in its data
dotplot(reorder(Species, int) ~ int, data = data)

## 4 - Data not accessible at the URL provided
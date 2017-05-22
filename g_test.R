install.packages("Deducer")

library(Deducer)

mktable <- matrix(c(6,16,0,25),nrow=2)

likelihood.test(mktable, conservative = T)
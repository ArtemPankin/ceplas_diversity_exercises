## simulator of genetic drift

# install.packages("ggplot2")
library(ggplot2)

# options( expressions = 5e5 )

# usage 'drift' function - drift(number_idividuals, starting_allele_frequency,number_generations, number_mutations)

drift <- function(num_samp,start_freq, num_gen,num_rep){c <- ggplot() + 
  ylim(0,1)
drift <- function(){
  alleles <- c(rep("A", num_samp * start_freq), rep("a", num_samp * (1-start_freq)))  
  a <- replicate(num_gen, {
    alleles <<- sample(alleles, num_samp, replace = T)
    length(alleles[alleles == "A"]) / num_samp
  })
  data.frame(generations=seq(1:num_gen),frequency=a)
}
b <- drift()
invisible(sapply(1:num_rep, function(x){
  b <- drift()
  c <<- c + geom_line(data=b,aes(x=generations,y=frequency))
}))
c}

drift(1000,0.001,500,200)


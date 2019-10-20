## simple simulator of genetic drift

# install.packages("ggplot2")
library(ggplot2)

# load the function

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



# usage 'drift' function - drift(number_idividuals, starting_allele_frequency,number_generations, number_mutations)

drift(100,0.5,100,20)



# OPTIONAL - loop 100 times; x sec delay between plots

x <- 1

while(x<100) {print(drift(100,0.5,100,20))
    delay <- 5
    now <- proc.time()[3]
  while(proc.time()[3] < (now + delay)) {dum <- 0}
x<- x+1
    }
# Uncomment and install the packages if you haven't got them
# install.packages("ggplot2")
# install.packages("plyr")
# install.packages("tidyr")
library("ggplot2")
library("plyr")
library("tidyr")

# importing data

data.flowering <- read.table("flowering.data", colClasses = c(rep("factor",3), "integer", rep("factor",2)))

colnames(data.flowering) <- c("genotype","photoperiod","replicate","value","marker1","marker2")

# testing normality of the data with Shapiro test

shapiro.test(subset(data.flowering, photoperiod %in% "LD")$value)

## VISUALIZING DISTRIBUTIONS of the data ...

# ... using density plots

ggplot(data.flowering, aes(x=value, fill = photoperiod)) + 
  geom_density(alpha=0.3)

# ... using histograms

flmeans <- data.flowering %>%
      ddply(c("photoperiod","genotype"), function(x){
        mean(x$value)
        })

ggplot(data=flmeans, aes(x=V1, fill=photoperiod)) + 
  geom_histogram()
  
# ... using box plots

ggplot(data.flowering, aes(y=value,x=photoperiod)) + 
  geom_boxplot(notch=T) + 
  geom_jitter(width=.3, alpha=.1) + 
  theme(legend.position="none") 

### ANALYSIS OF VARIANCE - ANOVA

anova_results <- aov(value ~ genotype*photoperiod, data = data.flowering)

# separately for short & long days

anova_results.sd <- aov(value ~ genotype, data = data.flowering[data.flowering$photoperiod=="SD",])

anova_results.ld <- aov(value ~ genotype, data = data.flowering[data.flowering$photoperiod=="LD",])

## summarizing ANOVA results - Sum of Squares I type

anovaI.all <- summary(anova_results)

# separately for short & long days

anI.sd <- summary(anova_results.sd)

anI.ld <- summary(anova_results.ld)

## summarizing ANOVA results - Sum of Squares III type

anIII.all <- drop1(anova_results,~., test = "F")

# separately for short & long days

anIII.sd <- drop1(anova_results.sd, ~., test="F")

anIII.ld <- drop1(anova_results.ld, ~., test="F")
 

### ASSOCIATION ANALYSIS / MAPPING

association <- aov(value ~ photoperiod * marker1 * marker2, data = data.flowering)

summary(association)

drop1(association,~.,test="F")

## END

# install.packages("ggplot2")
# install.packages("plyr")

library("ggplot2")
library("plyr")

# importing data

data.flowering <- read.table("flowering.data", colClasses = c(rep("factor",3), "integer", rep("factor",2)))

colnames(data.flowering) <- c("genotype","photoperiod","replicate","value","M1","M2")

# testing normality of the data

shapiro.test(subset(data.flowering, photoperiod %in% "LD")$value)

## visualizing distributions

# density plots

ggplot(data.flowering, aes(x=value, fill = photoperiod)) + 
  geom_density(alpha=0.3)

#histogram

flmeans <- ddply(data.flowering, c("photoperiod","genotype"), function(x){mean(x$value)})

ggplot(data=flmeans, aes(x=V1, fill=photoperiod)) + 
  geom_histogram()

# box plots

ggplot(data.flowering, aes(y=value,x=photoperiod)) + 
  geom_boxplot(notch=T) + 
  geom_jitter(width=.3, alpha=.1) + 
  theme(legend.position="none") 

ggplot(data.flowering, aes(y=value,x=photoperiod, fill=replicate)) + 
  geom_boxplot(notch=T) + 
  geom_point(aes(group=replicate),position=position_jitterdodge(jitter.width=.4),alpha=.1) + 
  theme(legend.position="none") 

### calculating ANOVA

anova_results <- aov(value~genotype*photoperiod, data = data.flowering)

# separately for short & long days

anova_results.sd <- aov(value ~ genotype * replicate, data = data.flowering[data.flowering$photoperiod=="SD",])

anova_results.ld <- aov(value ~ genotype * replicate, data = data.flowering[data.flowering$photoperiod=="LD",])

### summarizing ANOVA results - SS I type

anovaI.all <- summary(anova_results)

# separately for short & long days

anI.sd <- summary(anova_results.sd)

anI.ld <- summary(anova_results.ld)


### summarizing ANOVA results - SS III type

anIII.all <- drop1(anova_results,~., test = "F")

# separately for short & long days

anIII.sd <- drop1(anova_results.sd,~.,test="F")

anIII.ld <- drop1(anova_results.ld,~.,test="F")


### association analysis / mapping

association <- aov(value~photoperiod * M1 * M2, data = data.flowering)
summary(association)
drop1(association,~.,test="F")

## END

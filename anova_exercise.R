library("ggplot2")

# import data

data.flowering <- read.table("/biodata/dep_coupland/grp_korff/artem/scripts_git/teaching_notes_ppt/flowering.data")

colnames(data.flowering) <- c("sample","photoperiod","replicate","value")

head(data.flowering)

# convert data types to "factor"

#data.flowering.all$Biol_repl <- as.factor(data.flowering.all$Biol_repl)

# anova calculations

a <- aov(value~sample*photoperiod, data.flowering)

a.sd <- aov(value ~ sample * replicate, data = data.flowering[data.flowering$photoperiod=="SD",])

a.ld <- aov(value ~ sample * replicate, data = data.flowering[data.flowering$photoperiod=="LD",])

# summarizing anova calculations SS I type

anI.all <- summary(a)

anI.sd <- summary(a.sd)
anI.ld <- summary(a.ld)

# summarizing anova calculations SS III type

anIII.all <- drop1(a,~., test = "F")

anIII.sd <- drop1(a.sd,~.,test="F")
anIII.ld <- drop1(a.ld,~.,test="F")

# plotting

# density plots of flowering in two replicates

# for Long days

ggplot(data.flowering, aes(x=value, fill = photoperiod)) + geom_density(alpha=0.3)

# box plot of flowewing time values by biol replicate and photoperiod

ggplot(data.flowering, aes(y=value,x=photoperiod,fill=replicate)) + geom_boxplot(notch=T) + geom_jitter(width=.3, alpha=.1) + theme(legend.position="none") 


#######################################
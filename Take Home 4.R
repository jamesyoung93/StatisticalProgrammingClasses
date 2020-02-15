### Take Home 4

setwd("C:\\data\\")
x <- read.csv("data-takehome4.csv")


# summary(data)
# colnames(data)

# Note: create factors for Stage 1 and Stage 2 ANOVA
x$block <- as.factor(x$block)
x$pot <- as.factor(x$pot)
x$cell <- as.factor(paste(x$block,x$pot, sep='.'))



x$cell <- as.factor(paste(x$block,x$watered, x$fertilizer,sep='.'))
### ANOVA
# Stage 1 between cell analysis
Stage1.aov <- aov(height ~ pot, data=x)
summary(Stage1.aov)
anova(Stage1.aov)

# Stage 2 Partition  Sums of Squares
Stage2.aov <- aov(height ~ (block*fertilizer*watered), data = x)
summary(Stage2.aov)


model.treatments <- aov(height ~ fertilizer+watered + block + Error(block/(watered+fertilizer)), data=x)
summary(model.treatments)






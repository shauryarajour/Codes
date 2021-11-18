library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(dplyr)
library(ggplot2)
library(lattice)
library(ranger)
library(Metrics)

carins <- read.csv(file.choose())
str(carins)
summary(carins)
## Checking Missing Value ##
any(is.na(carins))

histogram::histogram(carins)

library(VIM)

opar <- par(no.readonly = TRUE)
par(bg ="gray63", col="blue", col.axis = "blue", col.lab = "blue", col.main = "blue",col.sub = "blue")
aggr(carins,prop = F,cex.axis = 0.4, numbers = T)
par(opar)

num.data <- subset(carins[-c(2,4,6,7,10,11,21,28,38,39)])
corr <- cor(num.data[,-1])
library(corrplot)

opar2 <- par(no.readonly = TRUE)
corrplot(corr,method = "circle",tl.cex = 0.5,tl.col = "black",number.cex = 0.55,bg = "grey14",
addgrid.col = "gray50", tl.offset = 2,col = colorRampPalette(c("blue1","ivory2","firebrick2"))(100))


carins$ACC_OP_DATE <- as.character(carins$ACC_OP_DATE)
carins$ACC_OP_DATE <- as.Date(carins$ACC_OP_DATE, format="%m/%d/%Y")
carins$FLG_HAS_CC <- as.factor(carins$FLG_HAS_CC)
carins$FLG_HAS_ANY_CHGS <- as.factor(carins$FLG_HAS_ANY_CHGS)
carins$FLG_HAS_NOMINEE <- as.factor(carins$FLG_HAS_NOMINEE)
carins$FLG_HAS_OLD_LOAN <- as.factor(carins$FLG_HAS_OLD_LOAN)

set.seed(1500)
split <- createDataPartition(carins$TARGET,p = 0.7,list = FALSE,times = 1)
View(split)
RFDF.dev <- carins[split,] 
RFDF.holdout <- carins[-split,]

RF <- randomForest(as.factor(TARGET) ~ ., data = RFDF.dev[,-1], 
                   ntree=501, mtry = 7, nodesize = 140,
                   importance=TRUE)
print(RF)




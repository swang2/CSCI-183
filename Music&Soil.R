# For Music Clustering
music.all <- read.csv("~/Documents/CSCI 183/music-all.csv", header=FALSE)
music.all[is.na(music.all)] <- 0
Music2 <- music.all[(,4:73)]
Music2[is.na(Music2)] <- 0
na.omit(Music2)
components <- prcomp(Music2, retx=TRUE, scale=TRUE)
components
sdev <- plot(components$sdev)
x <- data.frame(components$x)
plot(x$PC1, x$PC2)
artists <- ggplot(x, aes(x=PC1, y=PC2, label=music.all$artist))
artists + geom_text()

# For pH modeling
soil <- read.csv("~/Documents/CSCI 183/soil.csv")
soil_a <- data.matrix(soil, rownames.force = NA)
components2 <- prcomp(soil, retx=TRUE, scale=TRUE)
y <- data.frame(components2$x)
install.packages("glmnet")
library(glmnet)
pH <- glmnet(soil_a, y$PC1, family='gaussian')
plot(coef(pH))

cpH <- cv.glmnet(soil_a, y$PC1)
bestlambda <- cpH$lambda.min

plot(coef(cpH, s= "lambda.min"))
bestcoeff <- coef(cpH, s= "lambda.min")
bestcoeff <- abs(bestcoeff)

bestvar <- rownames(bestcoeff)[order(bestcoeff, decreasing=TRUE)][1:10]
print(bestvar)


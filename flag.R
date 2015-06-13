flag <- read.csv("~/Downloads/flag.data", header=FALSE)
names(flag) <- c("Country", "Landmass", "Zone", "Area", "Population", "Language", "Religion", "Bars",
                 "Stripes", "Colours", "Red", "Green", "Blue", "Gold", "White", "Black", "Orange", "Mainhue",
                 "Circles", "Crosses", "Saltires", "Quarters", "Sustars", "Crescent", "Triangle", "Icon",
                 "Animate", "Text", "Topleft", "Botright")

library(ggplot2)
library(reshape2)
#Relgion vs. MainHue
newflag <- flag[,c("Religion","Mainhue")]
Mainhuecols <- c(black= "black", blue= "dodgerblue2", brown= "chocolate4", gold= "gold1", green= "forestgreen",
                 orange= "orange", red= "red", white= "white")
gg <- ggplot(newflag, aes(Religion, fill=Mainhue))
gg <- gg + geom_bar()+ scale_fill_manual(values = Mainhuecols) + ggtitle("Mainhue on flag by religion") +
      theme(legend.position = "none",
      axis.text=element_text(size=14, colour= "black", face= "bold"), 
      axis.title=element_text(size=14),
      plot.title =element_text(size=16,face="bold"))
gg

#Predict probability of colors based on religion
#Create data frame
a <- c("Buddhist", "Christian", "Hindu", "Muslim", "Others")
Rel_Mainhue <- data.frame(Religion = a)
#black
BlackHue <- flag$Mainhue == "black"
glm_black <- glm(BlackHue ~ Religion, data= flag, family= binomial)
predict_black <- predict(glm_black, Rel_Mainhue, type= "response")
Rel_Mainhue$Black <- predict_black
gg_black <- ggplot(Rel_Mainhue, aes(x= Religion, y= Black)) + geom_bar(stat= "identity", fill= "black") + ylim(0,1)
gg_black <- gg_black + ggtitle("Black") + labs(y= "Probability")
gg_black

#red
RedHue <- flag$Mainhue == "red"
glm_red <- glm(RedHue ~ Religion, data= flag, family= binomial)
predict_red <- predict(glm_red, Rel_Mainhue, type= "response")
Rel_Mainhue$Red <- predict_red
gg_red <- ggplot(Rel_Mainhue, aes(x= Religion, y= Red)) + geom_bar(stat= "identity", fill= "red") + ylim(0,1)
gg_red <- gg_red + ggtitle("Red") + labs(y= "Probability")
gg_red

#blue
BlueHue <- flag$Mainhue == "blue"
glm_blue <- glm(BlueHue ~ Religion, data= flag, family= binomial)
predict_blue <- predict(glm_blue, Rel_Mainhue, type= "response")
Rel_Mainhue$Blue <- predict_blue
gg_blue <- ggplot(Rel_Mainhue, aes(x= Religion, y= Blue)) + geom_bar(stat= "identity", fill= "dodgerblue2") + ylim(0,1)
gg_blue <- gg_blue + ggtitle("Blue") + labs(y= "Probability")
gg_blue

#gold
GoldHue <- flag$Mainhue == "gold"
glm_gold <- glm(GoldHue ~ Religion, data= flag, family= binomial)
predict_gold <- predict(glm_gold, Rel_Mainhue, type= "response")
Rel_Mainhue$Gold <- predict_gold
gg_gold <- ggplot(Rel_Mainhue, aes(x= Religion, y= Gold)) + geom_bar(stat= "identity", fill= "gold1") + ylim(0,1)
gg_gold <- gg_gold + ggtitle("Gold") + labs(y= "Probability")
gg_gold

#white
WhiteHue <- flag$Mainhue == "white"
glm_white <- glm(WhiteHue ~ Religion, data= flag, family= binomial)
predict_white <- predict(glm_white, Rel_Mainhue, type= "response")
Rel_Mainhue$White <- predict_white
gg_white <- ggplot(Rel_Mainhue, aes(x= Religion, y= White)) + geom_bar(stat= "identity", fill= "white", colour= "black") + ylim(0,1)
gg_white <- gg_white + ggtitle("White") + labs(y= "Probability")
gg_white

#green
GreenHue <- flag$Mainhue == "green"
glm_green <- glm(GreenHue ~ Religion, data= flag, family= binomial)
predict_green <- predict(glm_green, Rel_Mainhue, type= "response")
Rel_Mainhue$Green <- predict_green
gg_green <- ggplot(Rel_Mainhue, aes(x= Religion, y= Green)) + geom_bar(stat= "identity", fill= "forestgreen") + ylim(0,1)
gg_green <- gg_green + ggtitle("Green") + labs(y= "Probability")
gg_green

#orange
OrangeHue <- flag$Mainhue == "orange"
glm_orange <- glm(OrangeHue ~ Religion, data= flag, family= binomial)
predict_orange <- predict(glm_orange, Rel_Mainhue, type= "response")
Rel_Mainhue$Orange <- predict_orange
gg_orange <- ggplot(Rel_Mainhue, aes(x= Religion, y= Orange)) + geom_bar(stat= "identity", fill= "orange") + ylim(0,1)
gg_orange <- gg_orange + ggtitle("Orange") + labs(y= "Probability")
gg_orange

MainProb <- melt(Rel_Mainhue, id.vars= "Religion")

#graph all of them
main_all <- ggplot(MainProb, aes(x= Religion, y= value, fill= variable))
main_all <- main_all + labs(x= "", y= "Probability")
main_all <- main_all + geom_bar(stat= "identity", position= "dodge") + ylim(0,1) +
            scale_fill_manual(values = Colors) + ggtitle("Probability of Mainhues by Religion") + 
            theme(legend.position = "none",
            axis.text=element_text(size=14, colour= "black", face= "bold"), 
            axis.title=element_text(size=14), 
            plot.title =element_text(size=16,face="bold"))
main_all

#Have red
glm_redcol <- glm(Red ~ Religion, data= flag, family= binomial)
predict_redcol <- predict(glm_redcol, Rel_Colors, type= "response")
Rel_Colors$Red <- predict_redcol
gg_redcol <- ggplot(Rel_Colors, aes(x= Religion, y= Red)) + geom_bar(stat= "identity", fill= "red") + ylim(0,1)
gg_redcol <- gg_redcol + ggtitle("Have Red") + labs(y= "Probability")
gg_redcol

#Have blue
glm_bluecol <- glm(Blue ~ Religion, data= flag, family= binomial)
predict_bluecol <- predict(glm_bluecol, Rel_Colors, type= "response")
Rel_Colors$Blue <- predict_bluecol
gg_bluecol <- ggplot(Rel_Colors, aes(x= Religion, y= Blue)) + geom_bar(stat= "identity", fill= "dodgerblue2") + ylim(0,1)
gg_bluecol <- gg_bluecol + ggtitle("Have Blue") + labs(y= "Probability")
gg_bluecol

#Have green
glm_greencol <- glm(Green ~ Religion, data= flag, family= binomial)
predict_greencol <- predict(glm_greencol, Rel_Colors, type= "response")
Rel_Colors$Green <- predict_greencol
gg_greencol <- ggplot(Rel_Colors, aes(x= Religion, y= Green)) + geom_bar(stat= "identity", fill= "forestgreen") + ylim(0,1)
gg_greencol <- gg_greencol + ggtitle("Have Green") + labs(y= "Probability")
gg_greencol

#Have orange
glm_orangecol <- glm(Orange ~ Religion, data= flag, family= binomial)
predict_orangecol <- predict(glm_orangecol, Rel_Colors, type= "response")
Rel_Colors$Orange <- predict_orangecol
gg_orangecol <- ggplot(Rel_Colors, aes(x= Religion, y= Orange)) + geom_bar(stat= "identity", fill= "orange") + ylim(0,1)
gg_orangecol <- gg_orangecol + ggtitle("Have Orange") + labs(y= "Probability")
gg_orangecol

#Have black
glm_blackcol <- glm(Black ~ Religion, data= flag, family= binomial)
predict_blackcol <- predict(glm_blackcol, Rel_Colors, type= "response")
Rel_Colors$Black <- predict_blackcol
gg_blackcol <- ggplot(Rel_Colors, aes(x= Religion, y= Black)) + geom_bar(stat= "identity", fill= "black") + ylim(0,1)
gg_blackcol <- gg_blackcol + ggtitle("Have Black") + labs(y= "Probability")
gg_blackcol

#Have gold
glm_goldcol <- glm(Gold ~ Religion, data= flag, family= binomial)
predict_goldcol <- predict(glm_goldcol, Rel_Colors, type= "response")
Rel_Colors$Gold <- predict_goldcol
gg_goldcol <- ggplot(Rel_Colors, aes(x= Religion, y= Gold)) + geom_bar(stat= "identity", fill= "gold1") + ylim(0,1)
gg_goldcol <- gg_goldcol + ggtitle("Have Gold") + labs(y= "Probability")
gg_goldcol

#Have white
glm_whitecol <- glm(White ~ Religion, data= flag, family= binomial)
predict_whitecol <- predict(glm_whitecol, Rel_Colors, type= "response")
Rel_Colors$White <- predict_whitecol
gg_whitecol <- ggplot(Rel_Colors, aes(x= Religion, y= White)) + geom_bar(stat= "identity", fill= "white", colour="black") + ylim(0,1)
gg_whitecol <- gg_whitecol + ggtitle("Have White") + labs(y= "Probability")
gg_whitecol

#Probability of each color per religion
Colors <- c(Black= "black", Blue= "dodgerblue2", Gold= "gold1", Green= "forestgreen",
            Orange= "darkorange", Red= "red", White= "white")
Probs <- melt(Rel_Colors, id.vars= "Religion")

#Buddhist
Buddhist_Prob <- Probs[Probs$Religion== "Buddhist",]
Buddhistplot <- ggplot(Buddhist_Prob, aes(x= variable, y= value, fill= variable))
Buddhistplot <- Buddhistplot + labs(x= "", y= "Probability")
Buddhistplot <- Buddhistplot + geom_bar(stat= "identity") + ylim(0,1) + 
                scale_fill_manual(values = Colors) + ggtitle("Buddhist") + 
                theme(legend.position = "none", 
                axis.text=element_text(size=14, colour= "black", face= "bold"), 
                axis.title=element_text(size=14), 
                plot.title =element_text(size=16,face="bold"))
Buddhistplot 

#christian
Christian_Prob <- Probs[Probs$Religion== "Christian",]
Christianplot <- ggplot(Christian_Prob, aes(x= variable, y= value, fill= variable))
Christianplot <- Christianplot + labs(x= "", y= "Probability")
Christianplot <- Christianplot + geom_bar(stat= "identity") + ylim(0,1) + 
                 scale_fill_manual(values = Colors) + ggtitle("Christian") + 
                 theme(legend.position = "none",              
                 axis.text=element_text(size=14, colour= "black", face= "bold"), 
                 axis.title=element_text(size=14), 
                 plot.title =element_text(size=16,face="bold"))
Christianplot

#hindu
Hindu_Prob <- Probs[Probs$Religion== "Hindu",]
Hinduplot <- ggplot(Hindu_Prob, aes(x= variable, y= value, fill= variable)) 
Hinduplot <- Hinduplot + labs(x= "", y= "Probability")
Hinduplot <- Hinduplot + geom_bar(stat= "identity") + ylim(0,1) + 
             scale_fill_manual(values = Colors) + ggtitle("Hindu") + 
             theme(legend.position = "none",              
             axis.text=element_text(size=14, colour= "black", face= "bold"), 
             axis.title=element_text(size=14), 
             plot.title =element_text(size=16,face="bold"))
Hinduplot

#muslim
Muslim_Prob <- Probs[Probs$Religion== "Muslim",]
Muslimplot <- ggplot(Muslim_Prob, aes(x= variable, y= value, fill= variable)) 
Muslimplot <- Muslimplot + labs(x= "", y= "Probability")
Muslimplot <- Muslimplot + geom_bar(stat= "identity") + ylim(0,1) + 
              scale_fill_manual(values = Colors) + ggtitle("Muslim") + 
              theme(legend.position = "none",              
              axis.text=element_text(size=14, colour= "black", face= "bold"), 
              axis.title=element_text(size=14), 
              plot.title =element_text(size=16,face="bold"))
Muslimplot

#others
Others_Prob <- Probs[Probs$Religion== "Others",]
Othersplot <- ggplot(Others_Prob, aes(x= variable, y= value, fill= variable, xlab= "Colors", ylab= "Probability")) 
Othersplot <- Othersplot + geom_bar(stat= "identity") + ylim(0,1) + scale_fill_manual(values = Colors) + ggtitle("Others")
Othersplot

#graph all of them
color_all <- ggplot(Probs, aes(x= variable, y= value, fill= variable))
color_all <- color_all + labs(x= "", y= "Probability")
color_all <- color_all + geom_bar(stat= "identity") + ylim(0,1) +
             scale_fill_manual(values = Colors) + ggtitle("Probability of Colors by Religion") + 
             theme(legend.position = "none",
             axis.text=element_text(size=14, colour= "black", face= "bold"), 
             axis.title=element_text(size=14), 
             plot.title =element_text(size=16,face="bold"))
color_all <- color_all + facet_wrap(~ Religion, ncol=2)
color_all

#features
#cross
cross <- glm(Crosses ~ Religion, data= flag, family= "gaussian")
predict_cross <- predict(cross, Rel_features, type= "response")
Rel_features$Crosses <- predict_cross
cross_chris <- ggplot(Rel_features, aes(x= Religion, y= Crosses)) + geom_bar(stat= "identity") + ylim(0,2)
cross_chris <- cross_chris + ggtitle("Crosses") + labs(y= "Probability")
cross_chris

#saltire
saltire <- glm(Saltires ~ Religion, data= flag, family= "gaussian")
predict_saltire <- predict(saltire, Rel_features, type= "response")
Rel_features$Saltires <- predict_saltire
saltired <- ggplot(Rel_features, aes(x= Religion, y= Saltires)) + geom_bar(stat= "identity") + ylim(0,2)
saltired <- saltired + ggtitle("Saltires") + labs(y= "Probability")
saltired

#sunstars
sustars <- flag$Sustars == 1
sunstars <- glm(sustars ~ Religion, data= flag, family= "gaussian")
predict_sunstars <- predict(sunstars, Rel_features, type= "response")
Rel_features$Sunstars <- predict_sunstars
gg_sunstars <- ggplot(Rel_features, aes(x= Religion, y= Sunstars)) + geom_bar(stat= "identity") + ylim(0,2)
gg_sunstars <- gg_sunstars + ggtitle("Sunstars") + labs(y= "Probability")
gg_sunstars

#crescent
crescent <- glm(Crescent ~ Religion, data= flag, family= "gaussian")
predict_crescent <- predict(crescent, Rel_features, type= "response")
Rel_features$Crescent <- predict_crescent
crescent <- ggplot(Rel_features, aes(x= Religion, y= Crescent)) + geom_bar(stat= "identity") + ylim(0,2)
crescent <- crescent + ggtitle("Crescents") + labs(y= "Probability")
crescent

features <- melt(Rel_features, id.vars= "Religion")
features_all <- ggplot(features, aes(x= variable, y= value, fill= Religion))
features_all <- features_all + labs(x= "", y= "Probability")
features_all<- features_all + geom_bar(stat= "identity", position = "dodge") + ylim(0,1) + 
                ggtitle("Probability of Features by Religion") + 
                theme(legend.text=element_text(size=14, colour= "black"),
                legend.title=element_text(size=14),
                axis.text=element_text(size=14, colour= "black", face= "bold"), 
                axis.title=element_text(size=14), 
                plot.title =element_text(size=16,face="bold"))
features_all




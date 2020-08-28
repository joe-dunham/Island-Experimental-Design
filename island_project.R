require(tidyverse)
require(gridExtra)
setwd("~/The Island Project 101B")
island <- read.csv("theisland.csv")

#Better variable names
names(island)[6:9] <- c("exer", "supp","before", "after")

#Cleaning up
testos <- island %>% select(Age, exer, supp, before, after)
testos[,2] <- as.factor(testos[,2])
testos[,3] <- as.factor(testos[,3])
testos$Age <- as.numeric(as.character((testos$Age)))
testos$before[203] <- 551.4
levels(testos$supp) <- c("Vitamin D", "Mushrooms", "Sugar Tablet")
levels(testos$exer) <- c("Arm Curls", "Running", "Swimming", "None")

#New variables
testos <- testos %>% mutate(dif = after - before)
testos <- testos %>% mutate(percent = (after - before)/before)

#Check means
testos %>% group_by(supp, exer) %>% 
  summarise("Mean" = mean(dif))

#anova test
ano <- aov(aov(dif ~ as.factor(exer)*as.factor(supp), data = testos))
summary(ano)

#Formatting Anova Table
fake_anova <- tibble(" " = c("Supplement", "Exercise", "Interaction", "Residuals"),
                     "DF" = c(2, 3, 6, 216),
                     "Sum Sq" = c(205, 683, 618, 27361),
                     "Mean Sq" = c(102.5, 227.6, 103, 126.7),
                     "F Value" = c("0.809","1.797","0.813"," "),
                     "P Value" = c("0.447", "0.149", "0.560", " "))
grid.table(fake_anova, rows = c("","","",""))

###Graphics

#Boxplots
ggplot(testos, aes(x = supp, y = dif)) +
  geom_boxplot() +
  xlab("Supplement") +
  ylab("Difference (microgram/dl)") +
  ggtitle("Supplement vs Difference in Testosterone Level")
ggplot(testos, aes(x = exer, y = dif)) +
  geom_boxplot() +
  xlab("Exercise") +
  ylab("Difference (microgram/dl)") +
  ggtitle("Exercise vs Difference in Testosterone Level")

testos %>% 
  group_by(exer, supp) %>% 
  summarise(groups = mean(dif)) %>% 
  ggplot(aes(x = exer, y = groups, color = supp)) +
  geom_line(aes(group = supp)) +
  geom_point() +
  ggtitle("Interaction Plot between Supplement and Exercise") +
  xlab("Exercise") +
  ylab("Mean Difference in Testosterone Level (microgram/dl)") +
  labs(color = "Supplement")

testos %>% 
  group_by(supp, exer) %>% 
  summarise(groups = mean(dif)) %>% 
  ggplot(aes(x = supp, y = groups, color = exer)) +
  geom_line(aes(group = exer)) +
  geom_point()
plot(TukeyHSD(ano))


tky = as.data.frame(TukeyHSD(ano)$`as.factor(supp):as.factor(exer)`)
tky$pair = 1:66
tky$sig = rep("Not Significant",66)

# Plot pairwise TukeyHSD comparisons and color by significance level
ggplot(tky, aes(color = sig)) +
  geom_hline(yintercept=0, lwd = 1.5, colour="grey30") +
  geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2) +
  geom_point(aes(pair, diff)) +
  labs(color="") +
  ggtitle("95% Family-Wise Confidence Level") +
  ylab("Difference in Testosterone Level (microgram/dl)") +
  xlab("Pair")

#t.test overall
t.test(testos$dif)

plot(lm(dif ~ as.factor(exer)*as.factor(supp), data = testos))

library(dplyr)

########################Red Wine###########################
wine_red = read.csv(file = 'hw/winequality-red.csv')
head(wine_red)

par(mfrow=c(1,2))
for (i in names(wine_red[,-12]))
{
  boxplot(wine_red[,i] ~ quality, data = wine_red, xlab = 'Red Wine Quality', ylab = i, col=colors()[2:7])
}


wine_red %>%
  group_by(quality) %>%
  summarize(mean = mean(fixed.acidity),
            median = median(fixed.acidity),
            var = var(fixed.acidity),
            min = min(fixed.acidity),
            max = max(fixed.acidity)) %>% View()

wine_red %>%
  group_by(quality) %>%
  summarize(mean = mean(volatile.acidity),
            median = median(volatile.acidity),
            var = var(volatile.acidity),
            min = min(volatile.acidity),
            max = max(volatile.acidity)) %>% View()

wine_red %>%
  group_by(quality) %>%
  summarize(mean = mean(citric.acid),
            median = median(citric.acid),
            var = var(citric.acid),
            min = min(citric.acid),
            max = max(citric.acid)) %>% View()

wine_red %>%
  group_by(quality) %>%
  summarize(mean = mean(residual.sugar),
            median = median(residual.sugar),
            var = var(residual.sugar),
            min = min(residual.sugar),
            max = max(residual.sugar)) %>% View()

wine_red %>%
  group_by(quality) %>%
  summarize(mean = mean(chlorides),
            median = median(chlorides),
            var = var(chlorides),
            min = min(chlorides),
            max = max(chlorides)) %>% View()

wine_red %>%
  group_by(quality) %>%
  summarize(mean = mean(free.sulfur.dioxide),
            median = median(free.sulfur.dioxide),
            var = var(free.sulfur.dioxide),
            min = min(free.sulfur.dioxide),
            max = max(free.sulfur.dioxide)) %>% View()

wine_red %>%
  group_by(quality) %>%
  summarize(mean = mean(total.sulfur.dioxide),
            median = median(total.sulfur.dioxide),
            var = var(total.sulfur.dioxide),
            min = min(total.sulfur.dioxide),
            max = max(total.sulfur.dioxide)) %>% View()

wine_red %>%
  group_by(quality) %>%
  summarize(mean = mean(density),
            median = median(density),
            var = var(density),
            min = min(density),
            max = max(density)) %>% View()

wine_red %>%
  group_by(quality) %>%
  summarize(mean = mean(pH),
            median = median(pH),
            var = var(pH),
            min = min(pH),
            max = max(pH)) %>% View()

wine_red %>%
  group_by(quality) %>%
  summarize(mean = mean(sulphates),
            median = median(sulphates),
            var = var(sulphates),
            min = min(sulphates),
            max = max(sulphates)) %>% View()

wine_red %>%
  group_by(quality) %>%
  summarize(mean = mean(alcohol),
            median = median(alcohol),
            var = var(alcohol),
            min = min(alcohol),
            max = max(alcohol)) %>% View()


str(wine_red)

########################White Wine#########################
wine_white = read.csv(file = 'hw/winequality-white.csv', sep=';')
head(wine_white)

par(mfrow=c(1,2))
for (i in names(wine_white[,-12]))
{
  boxplot(wine_white[,i] ~ quality, data = wine_white, xlab = 'White Wine Quality', ylab = i, col=colors()[2:8])
}

wine_white %>%
  group_by(quality) %>%
  summarize(mean = mean(fixed.acidity),
            median = median(fixed.acidity),
            var = var(fixed.acidity),
            min = min(fixed.acidity),
            max = max(fixed.acidity)) %>% View()

wine_white %>%
  group_by(quality) %>%
  summarize(mean = mean(volatile.acidity),
            median = median(volatile.acidity),
            var = var(volatile.acidity),
            min = min(volatile.acidity),
            max = max(volatile.acidity)) %>% View()

wine_white %>%
  group_by(quality) %>%
  summarize(mean = mean(citric.acid),
            median = median(citric.acid),
            var = var(citric.acid),
            min = min(citric.acid),
            max = max(citric.acid)) %>% View()

wine_white %>%
  group_by(quality) %>%
  summarize(mean = mean(residual.sugar),
            median = median(residual.sugar),
            var = var(residual.sugar),
            min = min(residual.sugar),
            max = max(residual.sugar)) %>% View()

wine_white %>%
  group_by(quality) %>%
  summarize(mean = mean(chlorides),
            median = median(chlorides),
            var = var(chlorides),
            min = min(chlorides),
            max = max(chlorides)) %>% View()

wine_white %>%
  group_by(quality) %>%
  summarize(mean = mean(free.sulfur.dioxide),
            median = median(free.sulfur.dioxide),
            var = var(free.sulfur.dioxide),
            min = min(free.sulfur.dioxide),
            max = max(free.sulfur.dioxide)) %>% View()

wine_white %>%
  group_by(quality) %>%
  summarize(mean = mean(total.sulfur.dioxide),
            median = median(total.sulfur.dioxide),
            var = var(total.sulfur.dioxide),
            min = min(total.sulfur.dioxide),
            max = max(total.sulfur.dioxide)) %>% View()

wine_white %>%
  group_by(quality) %>%
  summarize(mean = mean(density),
            median = median(density),
            var = var(density),
            min = min(density),
            max = max(density)) %>% View()

wine_white %>%
  group_by(quality) %>%
  summarize(mean = mean(pH),
            median = median(pH),
            var = var(pH),
            min = min(pH),
            max = max(pH)) %>% View()

wine_white %>%
  group_by(quality) %>%
  summarize(mean = mean(sulphates),
            median = median(sulphates),
            var = var(sulphates),
            min = min(sulphates),
            max = max(sulphates)) %>% View()

wine_white %>%
  group_by(quality) %>%
  summarize(mean = mean(alcohol),
            median = median(alcohol),
            var = var(alcohol),
            min = min(alcohol),
            max = max(alcohol)) %>% View()

##########################skill craft#################################
skill_d = read.csv('hw/SkillCraft1_Dataset.csv', na.strings = '?')
skill_d<- skill_d[which(skill_d$TotalHours<10000|skill_d$LeagueIndex==8),]
skill_d<- skill_d[,-1]
head(skill_d)

par(mfrow=c(1,2))
for (i in names(skill_d[,-1]))
{
  boxplot(skill_d[,i] ~ LeagueIndex, data = skill_d, xlab = 'League Levels', ylab = i, col=colors()[2:9])
}

bronze=subset(skill_d, LeagueIndex == "1")
silver=subset(skill_d, LeagueIndex == "2")
gold=subset(skill_d, LeagueIndex == "3")
platinum=subset(skill_d, LeagueIndex == "4")
diamond=subset(skill_d, LeagueIndex == "5")
master=subset(skill_d, LeagueIndex == "6")
grandmaster=subset(skill_d, LeagueIndex == "7")
professional=subset(skill_d, LeagueIndex == "8")

plot(x = 0, y = 0, type = 'n', xlim = c(0,10), ylim = c(0,100))
x1 = seq(1, 2, length = 347)
x2<- c()
points(x1, silver$Age, col ='blue')
points(silver$Age, col = 'blue')
str(skill_d)
length(silver$Age)
############################################################
wine_red %>%
  group_by(quality) %>%
  summarize(f.a_value = mean(fixed.acidity))

barplot(table(wine_red$quality))

par(mfrow=c(1,2))
for (i in names(wine_red[,-12]))
{
  plot(wine_red[,i] ~ quality, data = wine_red, xlab = 'Red Wine Quality', ylab = i,col=color.scale(wine_red[,i],na.color='red'))
}


hist(fixed.acidity[quality==4])



for (i in names(wine_red[,-12]))
{
  for (l in i)
  {
    hist(l[4])
  }
}





par(mfrow=c(2,2))
for (i in names(wine_red[,-12]))
{
  plot(wine_red[,i] ~ quality, data = wine_red, col=8)
}



a=table(wine_red$quality)
barplot(table(wine_red$quality), names.arg = c('3lv','4lv','5lv','6lv','7lv','8lv'), col = 3:9)
plot(fixed.acidity ~ quality, data = wine_red, col='red')
boxplot(wine_red[,3] ~ quality, data = wine_red, col=8)
fan.plot(a)
str(wine_red)




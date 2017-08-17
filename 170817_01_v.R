surveys <- read.csv(file = '170817/surveys.csv')
class(surveys)

names(surveys)
match(c('plot_id', 'species_id', 'weight'), names(surveys))


surveys[, match(c('plot_id', 'species_id', 'weight'), names(surveys))]
surveys[,c('plot_id', 'species_id', 'weight')]
surveys[c('plot_id', 'species_id', 'weight')]

head(surveys[surveys$year >= 1995 & surveys$year < 2000, ])

a=surveys[surveys$weight<5, c('species_id', 'sex', 'weight')]
b=surveys[which(surveys$weight<5), c('species_id', 'sex', 'weight')]

surveys_ex <- surveys
surveys_ex$weight_kg <- surveys$weight/1000
surveys_ex <- surveys_ex[!is.na(surveys_ex$weight_kg),]

u = unique(surveys$sex)
length(u)
class(surveys$sex)
levels(surveys$sex)

mean(surveys$weight[surveys$sex == u[1]], na.rm = T)
mean(surveys$weight[surveys$sex == u[2]], na.rm = T)
mean(surveys$weight[surveys$sex == u[3]], na.rm = T)

c = by(data = surveys$weight, INDICES = surveys$sex, FUN = mean, na.rm = TRUE, trim = 0.1)
d = unclass(c)
class(d)


aggregate(formula = weight ~ sex, data = surveys, FUN = mean, na.rm = TRUE)
aggregate(formula = weight ~ sex, data = surveys, FUN = median, na.rm = TRUE)
aggregate(formula = weight ~ sex + species_id, data = surveys, FUN = mean, na.rm = TRUE)

########################3
?cut
x <- rnorm(100)
cut(x, breaks = c(-Inf,-0.3, 0.3, Inf))
as.integer((cut(x, breaks = c(-Inf,-0.3, 0.3, Inf))))

table(surveys$sex, surveys$plot_id)


tmp  <- surveys
tmp <- tmp[order(surveys$plot_id),]
tmp <- tmp[order(tmp$month, decreaing = TRUE)]
head(tmp)

install.packages('dplyr')
library(dplyr)

select(.data = surveys, plot_id, species_id, weight)
head(select(.data = surveys, plot_id, species_id, weight))
select(.data = surveys, plot_id, species_id, weight) %>% head()
filter(.data = surveys, year == 1995) %>% head()

filter(.data = surveys, year >= 1995 & weight > 20) %>% head()
filter(.data = surveys, year >= 1995, weight > 20) %>% head()

surveys %>%
  filter(!is.na(weight)) %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight) %>% head()

surveys_ex <- surveys %>% filter(!is.na(surveys$weight)) %>%
  mutate(weight_kg = weight / 1000)

surveys %>%
  group_by(sex) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE),
            var_weight = var(weight,na.rm = TRUE),
            min_weight = min(weight,na.rm = TRUE),
            median_weight = median(weight,na.rm = TRUE)) %>%
  print(n = 15)


surveys %>%
  arrange(month, plot_id) %>% head()

surveys %>%
  arrange(desc(month), plot_id) %>% head()

###############################################################################

data("airquality")
library(reshape2)
str(airquality)
names(airquality) <- tolower(names(airquality))
aql <- melt(data = airquality, id.vars = c('month', 'day'))
head(aql)

aql <- melt(data= airquality, id.vars = c('month', 'day'),
            variable.name = 'climate_variable',
            value.name = 'climate_value')


aql <- dcast(aql, month + day ~ climate_variable, value.var = 'climate_value')

dcast(aql, month ~ climate_variable, fun.aggregate = mean, na.rm = TRUE, margins = FALSE) %>% head(n=3)

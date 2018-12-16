#Simran Batra
#May 10, 2018

library(knitr)
library(tidyverse)
library(tidytext)
library(margins)
library(stargazer)

#Data management

#Cleaning the happiness and the oil and region data

happy <- read_csv("happy1.csv", skip=2)
oil <- read_csv("oilreligion.csv")

happy <- select(happy, Country, `Happiness score`)

happy <- rename(happy, "hscore"=`Happiness score`)
oil <- rename(oil, "religion"=`I_RELIGION`)

#check for missing data
oil$religion[oil$religion=="-999"] <- NA
oil$oilstate[oil$oilstate=="-999"] <- NA
oil <- mutate(oil, oilstate=fct_recode(as.factor(oilstate), 
"oil-exporting"="1",
"non-oil-exporting"="0"))

#sort
happy <- arrange(happy, hscore)
kable(head(happy, 10))
happy <- arrange(happy, -hscore)
kable(head(happy, 10))

#ID names check
names(happy)
names(oil)
oil <- rename(oil, "country"="icountry")
happy <- rename(happy, "country"="Country")
intersect(names(happy),names(oil))

#Unique ID check
unique.happy <- unique(select(happy, country))
nrow(unique.happy); nrow(happy)
unique.oil <- unique(select(oil, country))
nrow(unique.oil); nrow(oil)

#ID values check
check1 <- anti_join(happy, oil, by="country")
kable(check1)
check2 <- anti_join(oil, happy, by="country")
kable(check2)
happy <- mutate(happy, country=fct_recode(country, 
"USA" = "United States",
"UK" = "United Kingdom",
"UAE" = "United Arab Emirates",
"Taiwan"="Taiwan Province of China",
"Trinidad and Tobago" = "Trinidad & Tobago",
"Hong Kong" = "Hong Kong SAR, China",
"Palestine (Israeli Occupied Territories)" = 
"Palestinian Territories",
"Cote d'Ivoire" = "Ivory Coast",
"Congo, Republic of the" = "Congo (Brazzaville)",
"Congo, Democratic Republic of the" = "Congo (Kinshasa)",
"Burma (Myanmar)" = "Myanmar"
))

#Merge
twodata <- inner_join(happy, oil)

#Cleaning the UN data

un <- read_csv("unindicators.csv")
un <- gather(un, Afghanistan:Zimbabwe, key="country", value=data)
un <- spread(un, key="var", value="data")

#recode
un <- mutate(un, region=fct_recode(region, "Asia/Oceania" = "Australia/New Zealand/Oceania",
"Asia/Oceania" = "Asia", 
"Americas" = "USA/Canada",
"Americas" = "Latin America/Caribbean"
))

#merge checks and merging 
#id names check
names(un)
names(twodata)
intersect(names(un),names(twodata))

#unique id check
unique.twodata <- unique(select(twodata, country))
nrow(unique.twodata); nrow(twodata)
unique.un <- unique(select(un, country))
nrow(unique.un); nrow(un)

#id values check
check1 <- anti_join(un, twodata, by="country")
check2 <- anti_join(twodata, un, by="country")
check1; check2
twodata <- mutate(twodata, country=fct_recode(country, 
"United States" = "USA",
"United Kingdom" = "UK",
"United Arab Emirates" = "UAE",
"Korea, South" = "South Korea"
))
tidy <- inner_join(twodata, un)

#new variables
tidy <- mutate(tidy, gdpcap = as.numeric(gdpcap),
GINI = as.numeric(GINI),
literacy = as.numeric(literacy), 
medianage = as.numeric(medianage), 
population = as.numeric(population), 
povertyindex = as.numeric(povertyindex))
tidy <- tidy %>%
mutate(povertyindex = povertyindex*100) %>%
mutate(population = population/1000000) %>%
mutate(happy = hscore > 4.5)

#Descriptive Statistics and `ggplot` graphics
#Quantiles
quantile(tidy$medianage, probs=c(.10, .30, .65, .91287), na.rm=TRUE)

#Cross-tabs
table <- table(tidy$oilstate, tidy$region)
kable(table)
kable(prop.table(table, margin=1))

#Difference in means test
tidy1 <- filter(tidy, !is.na(tidy$oilstate))
t.test(as.numeric(tidy1$oilstate), tidy1$hscore)

#Correlation
tidy1 <- select(tidy, "hscore", "literacy", "gdpcap", "povertyindex")
tidy1 <- na.omit(tidy1)
cor(tidy1)

#ggplots

#Scatterplot
g <- ggplot(tidy, aes(x=povertyindex, y=hscore, col=region, pch=region)) +
geom_point() +
xlab("Poverty Index") +
ylab("Happiness Score") +
ggtitle("Happiness Score based on Poverty Index and Region")
g

g <- ggplot(tidy, aes(x=povertyindex, y=hscore)) +
geom_point() +
geom_smooth(method="lm") +
xlab("Poverty Index") +
ylab("Happiness Score") +
ggtitle("Happiness Score vs. Poverty Index per Region") +
facet_wrap(~ region, scales="free")
g

#Bar plot
g <- ggplot(tidy, aes(x=region, fill=happy)) +
geom_bar(position="dodge") +
xlab("Region") +
guides(fill=guide_legend(title="Happy country")) +
ggtitle("Count of Happy Countries by Region")
g

#Grouped Density
tidy1 <- select(tidy, GINI, oilstate)
tidy1 <- na.omit(tidy1)
g <- ggplot(tidy1, aes(x=GINI, fill=oilstate)) +
geom_density(alpha=.3) + 
ggtitle("Economic Inequality in a Country")
g

#linear and logistic regression

reg <- lm(hscore ~ povertyindex + region + oilstate + literacy + medianage, data=tidy)
summary(reg)

tidy <- mutate(tidy, region=fct_relevel(region, "Americas", 
"Africa", 
"Europe",
"Asia/Oceania"))
reg <- lm(hscore ~ povertyindex + region + oilstate + 
literacy + medianage, data=tidy)
summary(reg)
confint(reg)

#Logistic regression
logit <- glm(happy ~ povertyindex + region + oilstate + literacy +
medianage, data=tidy, family=binomial(link="logit"))
summary(logit)
exp(coef(logit))

#marginal changes in probability
m <- margins(logit, type = "response")
summary(m)

#predicted probability plot

c <- cplot(logit, "region", what = "prediction")
g <- ggplot(c, aes(x = xvals, y = yvals, col=xvals)) +
geom_point() +
geom_linerange(aes(ymin=lower, ymax=upper)) +
xlab("Region") +
ylab("Probability of being Happy") +
ggtitle("Probability of being Happy by Region") +
guides(col=FALSE)
g


c <- cplot(logit, "oilstate", what = "prediction")
g <- ggplot(c, aes(x = xvals, y = yvals, col=xvals)) +
geom_point() +
geom_linerange(aes(ymin=lower, ymax=upper)) +
xlab("Oil State") +
ylab("Probability of being Happy") +
ggtitle("Probability of being Happy by Oil State Status") +
guides(col=FALSE)
g


#Predicted probability plot for the poverty index
c <- cplot(logit, "povertyindex", what="prediction")

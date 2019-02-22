library(lme4)
library(languageR)
library(psych)
library(multcomp)
library(LMERConvenienceFunctions)

data_file = 'x.txt'
data <- read.csv(data_file,header=TRUE,sep='\t') #to go that path "" go to properties to get actual path of particular file
str(data)

#defining variables as factors (independent variables)#change every "data" to "data" if doesn't work
data$Subject <- factor(data$Subject)
data$Passages <- factor(data$Passages)
data$Booklet <- factor(data$Booklet)
data$Com_Score_IDM <- factor(data$Com_Score_IDM)
data$RTNoOut_IDM <- factor(data$RTNoOut_IDM)


data<-subset(data, Passages_Sen_Num != "6_5" & Passages_Sen_Num != "6_3")

mean(as.numeric(as.character(subset(data, RTNoOut!="." & Condition=="Hard")[, "RTNoOut"])))

mean(as.numeric(as.character(subset(data,sum(MW)/Num_Probs & Condition=="Hard")[, "MW"])))



mean(as.numeric(as.character(subset(data, RTNoOut!="." & Condition=="Easy")[, "RTNoOut"])))

sd(as.numeric(as.character(subset(data, RTNoOut!="." & Condition=="Hard")[, "RTNoOut"])))

sd(as.numeric(as.character(subset(data, RTNoOut!="." & Condition=="Easy")[, "RTNoOut"])))


#tests for mind wandering

m0 = lmer(MW ~ (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', data)
m1 = lmer(MW ~ Condition + (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', data)
anova(m0, m1)  #Compare RANDOM only model to model with FIXED effects

#tests for response time
m1 = lmer(RTNoOut_Zub ~ Condition + (1 | Subject) + (1 | Passages_Sen_Num), data)
pamer.fnc(m1)

#This is an ANOVA to test significance of linear MER models
#It needs the library(LMERConvenienceFunctions)
pamer.fnc(m1)


#does MW predict response times
m1 = lmer(RTNoOut_Zub ~ as.factor(MW) + (1 | Subject) + (1 | Passages_Sen_Num), data)
pamer.fnc(m1)


#does MW interact with condition to predict RT
interaction.plot(data$MW, data$Condition , data$RT)
m1 = lmer(RTNoOut_Zub ~ as.factor(MW)*Condition + (1 | Subject) + (1 | Passages_Sen_Num), data)
pamer.fnc(m1)

m1 = lmer(Com_Score ~ as.factor(MW)*Condition + (1 | Subject) + (1 | Passages_Sen_Num), data)

##Predicting comp score as a function of condition
m0 = lmer(Com_Score ~ (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', data)
m1 = lmer(Com_Score ~ Condition + (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', data)
anova(m0, m1)

#Predicting comp score as a function of MW
m0 = lmer(Com_Score ~ (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', data)
m1 = lmer(Com_Score ~ as.factor(MW) + (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', data)
anova(m0, m1)


error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
stop("vectors must be same length")
arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)


interaction.plot(data$MW, data$Condition , data$Com_Score)
interaction.plot(data$MW, data$Condition , data$Com_Score, xlab = "Mind Wandering Frequency", ylab = "Mean Comprehension", main = "Mind Wandering and Comprehension by Text Difficulty", legend = TRUE, trace.label = "Text Diff.", xpd = FALSE, xaxt = par("xaxt"))

#Easy condition
m0 = lmer(Com_Score ~ (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', subset(data, Condition == "Easy"))
m1 = lmer(Com_Score ~ as.factor(MW) + (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', subset(data, Condition == "Easy"))
anova(m0, m1)

#Hard condition
m0 = lmer(Com_Score ~ (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', subset(data, Condition == "Hard"))
m1 = lmer(Com_Score ~ as.factor(MW) + (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', subset(data, Condition == "Hard"))
anova(m0, m1)
anova(m1, m0)

#this is old stuff - not needed

#data_lolo <- subset(data, Com_Score_IDM == "1" & RTNoOut_IDM == "1")
#m1 = lmer(MW ~ Condition + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', data_lolo)
#interaction.plot(data_lolo$MW, data_lolo$Condition , data_lolo$Com_Score)
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_lolo, Condition == "Easy"))
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_lolo, Condition == "Hard"))

#data_lohi <- subset(data, Com_Score_IDM == "1" & RTNoOut_IDM == "2")
#m1 = lmer(MW ~ Condition + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', data_lohi)
#interaction.plot(data_lohi $MW, data_lohi$Condition , data_lohi$Com_Score)
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_lohi, Condition == "Easy"))
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_lohi, Condition == "Hard"))


#data_hilo <- subset(data, Com_Score_IDM == "2" & RTNoOut_IDM == "1")
#m1 = lmer(MW ~ Condition + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', data_hilo)
#interaction.plot(data_hilo $MW, data_hilo $Condition , data_hilo$Com_Score)
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_hilo, Condition == "Easy"))
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_hilo, Condition == "Hard"))


#data_hihi <- subset(data, Com_Score_IDM == "2" & RTNoOut_IDM == "2")
#m1 = lmer(MW ~ Condition + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', data_hihi)
#interaction.plot(data_hihi$MW, data_hihi$Condition , data_hihi$Com_Score)
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_hihi, Condition == "Easy"))
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_hihi, Condition == "Hard"))


#data_comp_lo <- subset(data, Com_Score_IDM == "1")
#interaction.plot(data_comp_lo$MW, data_comp_lo$Condition , data_comp_lo$Com_Score)

#data_comp_hi <- subset(data, Com_Score_IDM == "2")
#interaction.plot(data_comp_hi$MW, data_comp_hi$Condition , data_comp_hi$Com_Score)


#p = pvals.fnc(m1, nsim = 1000)$fixed #make sure before you run this replace "m" to "m1, m2, m3" etc (depends on which function) if successfully, a R graphic should pop up

 #run this after every linear modeling, and put "p" in console for output
#estimate is estimated means from parameter, lower and upper bound, significance (no idea what MCMmean is...)

data$MW <- factor(data$MW)

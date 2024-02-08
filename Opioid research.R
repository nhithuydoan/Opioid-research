library(tidyverse)
library(ggplot2)
library(haven)
library(expss)
library(caret)
library(tibble)

# DATA MANIPULATION
#UPLOAD FILE
file <-  read_sav("C:/Users/ndoan1/Downloads/tedsd_puf_2020.sav")
file <- add_labelled_class(file)
file.age <- file %>% select(AGE,LOS,FRSTUSE1,GENDER,RACE, REASON, STFIPS, NOPRIOR, DAYWAIT, FREQ1,
                            SERVICES_D, EMPLOY_D, PRIMINC, FREQ1_D, ROUTE1)
#checkdata <- data.frame(table(file.age$FRSTUSE1, file.age$AGE,deparse.level = 2))
file.age  <- file.age %>% filter(!(FRSTUSE1 == -9 | AGE == -9 | LOS == -9 | FREQ1 == -9)) %>% 
  filter(DAYWAIT == 0) %>% filter(NOPRIOR == 0) %>% filter(REASON == 1) %>% 
  filter(AGE<=3)


I realize from the last work that there are some variables that I have been ignored
in the original big dataset. Specifically, there is a variable that explains the
reasons for discharge. Before, three assumptions I have for this model is that this model
works for people who have never been admitted to other opiod treatment programs
before, they get admitted/treated immediately (so there is no waiting time), and
this model is only true for programs in Connecticut. I realize that based on the length
of stay alone might not be a reliable predictor because people can drop out, which skews
the results. 

A series of filter cuts the data so much I decided to have to reset the parameters.
I still use the length in stay as a response variable. The implicit assumption I have
for choosing this variable is that people have to complete the program, and the longer
the stay the harder it it to treat opoid addiction. So the reason for finishing the
program has to be about the program completion. Assumptions include they do not
have to wait to get admitted, they have no prior treatment, and the age group is 
from 12 to 20 years old.

#CHECK DEPENDENT VARIABLE
file.age %>% ggplot(aes(x=factor(FRSTUSE1), y =LOS))+
  geom_boxplot()
hist(file.age$LOS)
qqnorm(file.age$LOS)
qqline(file.age$LOS)

# Inverse transformation --------------------------------------------------
# 
# file.age$LOS1 <- 1/file.age$LOS
# file.age.transform1 <- subset(file.age, LOS1 < -3)
# file.age.transform1 %>% ggplot(aes(x=factor(FRSTUSE1), y = LOS1))+
#   geom_boxplot()
# firstuse <- aov(LOS1 ~ factor(FRSTUSE1), data = file.age.transform1)
# summary(firstuse)
# res = residuals(firstuse)
# qqnorm(res)
# qqline(res)
# TukeyHSD(firstuse)

Can run the non parametric test. use either log and inverse

# Log transformation ------------------------------------------------------

file.age$LOS2 <- log(file.age$LOS)
hist(file.age$LOS2)
file.age %>% ggplot(aes(x= factor(FRSTUSE1), y =LOS2))+
  geom_boxplot()
file.age.transform2 <- subset(file.age, LOS2>0)
file.age.transform2 %>% ggplot(aes(x= factor(FRSTUSE1), y =LOS2))+
  geom_boxplot()
#table(file.age.transform2$FRSTUSE1)
frstuselog <- aov(LOS2 ~factor(FRSTUSE1) , data =file.age.transform2 )
summary(frstuselog)
res.log <- residuals(frstuselog)
qqnorm(res.log)
qqline(res.log)
hist(file.age.transform2$LOS2)
TukeyHSD(frstuselog)

# frstuselog <- aov(LOS ~factor(FRSTUSE1) , data = file.age)
# summary(frstuselog)
# res.log <- residuals(frstuselog)
# qqnorm(res.log)
# qqline(res.log)
# hist(file.age.transform2$LOS2)
# TukeyHSD(frstuselog)


# DATA ANALYSIS: IS THERE A DIFFERENCE AMONGST AGES THAT FIRST USE DRUG?
# Difference between different age groups ---------------------------------

kruskal.test(LOS2 ~factor(FRSTUSE1), data = file.age )

My main question is whether, for this underage age (from 12 to 20),
if there is a difference in length of stay, and I will try to answer what factors
that contribute it. For this first question, there seems to be a signficant 
difference amongst groups (H(3) = 332.17, p < 0.01). Since Kruskal-Wallis test
does not have a post up test, I will compare the groups manually by creating new variable.

#Difference between groups 11 and under and 12-14 years old
group11to14 <- file.age %>% filter(FRSTUSE1 == 1 |FRSTUSE1 == 2 )
group11to14 %>% ggplot(aes(x= factor(FRSTUSE1), y=LOS))+
  geom_boxplot()+labs(x= "Age at first use drug", y= "Length of stay (days)",
                      title = "Length of stay by first time using drugs")+
  theme_classic() 
kruskal.test(LOS2 ~factor(FRSTUSE1), data = group11to14)
There seems to be no difference in the length of stay between this group 
(H(1) = 1.49, p = 0.22)

#Difference between groups 12-14 year-old and 15-17 years old
group12to17 <- file.age %>% filter(FRSTUSE1 == 2 |FRSTUSE1 == 3 )
group12to17 %>% ggplot(aes(x= factor(FRSTUSE1), y=LOS))+
  geom_boxplot()+labs(x= "Age at first use drug", y= "Length of stay (days)",
                      title = "Length of stay by first time using drugs")+
  theme_classic() 
kruskal.test(LOS2 ~factor(FRSTUSE1), data = group12to17)
#wilcox.test(LOS ~factor(FRSTUSE1), data = group12to17)

Visually, there is a difference in median length of stay between the younger 
group (12 to 14) versus the older group (15 to 17) (H(1)=59.318, p < 0.01)

# Difference between 15-17 years old and 18-20 groups
group15to20 <- file.age %>% filter(FRSTUSE1 == 4 |FRSTUSE1 == 3 )
group15to20 %>% ggplot(aes(x= factor(FRSTUSE1), y=LOS))+
  geom_boxplot()+
  labs(x= "Age at first use drug", y= "Length of stay (days)",
       title = "Length of stay by first time using drugs")+
  theme_classic()

kruskal.test(LOS2 ~factor(FRSTUSE1), data = group15to20)

The same pattern is observed with this group. Specifically, the 15-17 group 
spent less time in the opioid treatment programs, compare to the 12-14 group
(H(1) = 155.58, p < 0.01).

OVerall, in all underage group, the group under 11 years old is the only one
we do not see a big difference between length of stay. Besides this group,
the older the person get, the shorter the treatment tend to be. 


# Create a dataset with relevant variables
under.age <- file.age %>% select(AGE,LOS,FRSTUSE1,GENDER,RACE, REASON, STFIPS, NOPRIOR, DAYWAIT, FREQ1,
                                 SERVICES_D, EMPLOY_D, PRIMINC, FREQ1_D, ROUTE1, LOS2)
under.age <- under.age %>% filter(!(FRSTUSE1 == -9 | AGE == -9 | LOS == -9 | FREQ1 == -9)) %>% 
  filter(DAYWAIT == 0) %>% filter(NOPRIOR == 0) %>% filter(REASON == 1) %>% 
  filter(FRSTUSE1 == 2|FRSTUSE1 == 3|FRSTUSE1 == 4)

under.age <- under.age[apply(under.age,1, function(x) all(x!= -9)),]

# DATA ANALYSIS: WHAT FACTORS INFLUENCE THE LENGTH OF STAY
# Analysis ----------------------------------------------------------------

As seen above, we establish that in this under age group (from 12 to 20), the younger the
user, the longer they tend to stay in opioid treatment programs. I did some visualization
and found out 5 factors that are signficant predictors in the length of stay.
I will evaluate each of these factors individually before I combine it with first
age of using drug


# in general
under.age %>% ggplot(aes(fill= factor(FRSTUSE1), y= LOS2))+geom_boxplot()+
  labs(x= "",y="Log of days", title = "Gender and length of stay", 
       subtitle = "Not to scale", fill = "")+theme_classic()+
  theme(plot.title = element_text(hjust =0.5, face="bold", size = 15))+
  scale_fill_manual(values = c("#9BA4B5","#F1F6F9","#394867"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
# Effect of gender on length of stay
under.age %>% ggplot(aes(x= factor(GENDER), y= LOS2))+
  geom_boxplot()+ ylim(3.25, 3.5)+
  labs(x= "",y="Log of days", title = "Gender and length of stay", 
       subtitle = "Not to scale")+theme_classic()+
  theme(plot.title = element_text(hjust =0.5, face="bold"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

kruskal.test(LOS2 ~ factor(GENDER), data = under.age)

Not much difference between men and women in length of stays (H(1)=0.33, p=0.56)

# Effect of  Service at discharge on length of stay

under.age$SERVICES_D[under.age$SERVICES_D == 1] <- "Detox, 24-hour, hospital inpatient"
under.age$SERVICES_D[under.age$SERVICES_D == 7] <- " Ambulatory, non-intensive outpatient"
under.age$SERVICES_D[under.age$SERVICES_D == 2] <- "Detox, 24-hour, free-standing residential"
under.age$SERVICES_D[under.age$SERVICES_D == 3] <- "Rehab/residential, hospital (non-detox) "
under.age$SERVICES_D[under.age$SERVICES_D == 4] <- "Rehab/residential, short term (30 days or fewer)"
under.age$SERVICES_D[under.age$SERVICES_D == 5] <- "Rehab/residential, long term (more than 30 days)"
under.age$SERVICES_D[under.age$SERVICES_D == 6] <- "Ambulatory, intensive outpatient "
under.age$SERVICES_D[under.age$SERVICES_D == 8] <- "Ambulatory, detoxification"

under.age %>% ggplot(aes(fill= factor(SERVICES_D), y= LOS2))+
  geom_boxplot()+ ylim(3.25, 3.5)+
  labs(x= "",y="Log of days", title = "Type of services and length of stay", 
       subtitle = "Not to scale", fill = "Type of treatment")+theme_classic()+
  theme(plot.title = element_text(hjust =0.5, face="bold"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_manual(values = 
                      c("#F1F6F9","#9BA4B5","#F1F6F9","#394867","#474E68","#50577A","#6B728E","#404258"))


kruskal.test(LOS2 ~ factor(SERVICES_D), data = under.age)
mod.ser <- aov(LOS2 ~ factor(SERVICES_D), data = under.age)
summary(mod.ser)
TukeyHSD(mod.ser)
plot(mod.ser,c(1,2))

Some services keep patients longer than other. Most services keep patients more than
30 days, besides the 24-hour treatment and ambulatory detoxification and rehab, hospital
(one-way ANOVA, F(7,3307)=826.1,p <0.01)

# Employment after discharge: unemployed tend to spend less time
under.age$EMPLOY_D[under.age$EMPLOY_D == 1] <- "Full-time"
under.age$EMPLOY_D[under.age$EMPLOY_D == 2] <- "Part-time"
under.age$EMPLOY_D[under.age$EMPLOY_D == 3] <- "Unemployed"
under.age$EMPLOY_D[under.age$EMPLOY_D == 4] <- "Not in labor force"

under.age %>% ggplot(aes(fill= factor(EMPLOY_D), y= LOS2))+
  geom_boxplot()+ ylim(1,3.5)+
  labs(x= "",y="Log of days", title = "Employment and length of stay", 
       subtitle = "Not to scale", fill = "Type of employment \n after discharge")+theme_classic()+
  theme(plot.title = element_text(hjust =0.5, face="bold"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_manual(values = 
                      c("#F1F6F9","#9BA4B5","#F1F6F9","#394867"))

mod.employ <- aov(LOS2 ~ factor(EMPLOY_D), data = under.age)
summary(mod.employ)
TukeyHSD(mod.employ)
plot(mod.employ,c(1,2))

There seems to be a significant different in length of stay in hospital amongst
different types of employments. Specifically, not in labor force tends to stay 
longest, whereas there is so much variability in unemployed people. There is
not much difference between full-time or part-time jobs. I wonder how 
this will act as a motivation/ discouragment for some people 
(one-way ANOVA,F(3,3311)=148.1,p <0.01)

# Frequency use at discharge: no use tend to stay in the program longer, more likely to relapse?
under.age %>% ggplot(aes(fill= factor(FREQ1_D), y= LOS2))+
  geom_boxplot()+ ylim(1,3.5)+
  labs(x= "",y="Log of days", title = "Frequency use and length of stay", 
       subtitle = "Not to scale", fill = "Frequency drug use \n at discharge")+theme_classic()+
  theme(plot.title = element_text(hjust =0.5, face="bold"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_manual(values = c("#F1F6F9","#9BA4B5","#394867"))



mod.freq <- aov(LOS2 ~ factor(FREQ1_D), data = under.age)
summary(mod.freq)
TukeyHSD(mod.freq)
plot(mod.freq,c(1,2))

Frequency of use seems to be a significant predictor of length of stay. Suprisingly,
daily usage tends to have lowest median length of stay, whereas people who do not
use drugs in the past month tends to stay longer. This may due to relapse in the system
where the are new molecules in the body and the body does not know what to do with it
(one-way ANOVA, F(2,3312)= 211.4,p <0.01)

#under.age$ROUTE1[under.age$ROUTE1 == "Injection (intravenous, intramuscular, intradermal, or subcutaneous)"] <- "Injection"
# Route of administration
under.age %>% ggplot(aes(fill= factor(ROUTE1), y= LOS2))+
  geom_boxplot()+ ylim(1,3.5)+
  labs(x= "",y="Log of days", title = "Route of administration and length of stay", 
       subtitle = "Not to scale", fill = "Route of administration")+theme_classic()+
  theme(plot.title = element_text(hjust =0.5, face="bold"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_manual(values = c("#F1F6F9","#9BA4B5","#F1F6F9","#394867","#474E68","#50577A","#6B728E","#404258"))


mod.route <- aov(LOS2 ~ factor(ROUTE1), data = under.age)
summary(mod.route)
TukeyHSD(mod.route)
plot(mod.route,c(1,2))

Each route is significant predictor of how long one person can stay in opioid 
treatment programs. Specifically, smoking tends to stay the longest, and
injection seems to have lowest median length of stay (one-way ANOVA, F(4,3310)= 71.06,p <0.01).
This is consistent with pharmacology study. Smoking is the shortest way to the brain,
so people tend to overdose and get shocked by the large amount of opioid. Injection
and oral routes tend to be safer, because they have to go to the bloodstream
and get broken down by enzyme and released slowly to the bloodstream. The body
has time to digest and excrete opioid, so it plays lighter toll on the body.

Now we know that types of service at the program, employment, frequency of drug
use, and route of adminsitration are significant predictors of the length of stay.
I will try to combine them with first time people use drugs to see what combination
of factors influence the length of stay.


# DATA ANALYSIS: ANOVA MODELS FOR EACH AGE GROUP (12-14, 15-17, AND 18-20)
# Sinking model
allmod <- aov(LOS2 ~ factor(FRSTUSE1) + factor(SERVICES_D) + factor(EMPLOY_D)+factor(ROUTE1)+factor(FREQ1_D),
              data = under.age)
summary(allmod)
library(writexl)

#Create a ANOVA table
final = anova(allmod)[,c(1,3,5)]
final
colnames(final) = c("DF", "MS", "P-value")
colnames(final)[2] = cols[2]
final = as.data.frame(round(final, digits = 2))
final$sign[final$`P-value` < 0.05] <- "*" 
final$sign[final$`P-value` < 0.01] <- "**"
final$sign[final$`P-value` < 0.001] <- "***"
final$sign[final$`P-value` > 0.05] <- "ns"
anova = writexl::write_xlsx(final, 'underage.xlsx')

plot(allmod, c(1,2))
This sinking model indicates all factors are significant in explaining the length
of stay of under age group (one-way ANOVA, F(16,3298)=380.0, p<0.01). This
model is also able to explain 65% of data.

# Stepwise model
step(allmod)
Stepwise model also chooses the sinking model.

# Check if we can remove any variables
mod1 <- lm(LOS2 ~ factor(FRSTUSE1) + factor(FREQ1_D),
           data = under.age)
summary(mod1)
as.table(mod1)

mod2 <- lm(LOS2 ~ factor(FRSTUSE1)+ factor(EMPLOY_D)+factor(ROUTE1)+factor(FREQ1_D),
           data = under.age)
summary(mod2)

mod3 <- lm(LOS2 ~ factor(FRSTUSE1) + factor(SERVICES_D) +factor(ROUTE1)+factor(FREQ1_D),
           data = under.age)
summary(mod3)

No have models have higher R-squared values than the sinking model, but the
model without employment type has a R-squared of 0.649, implying that maybe 
employment type is not very important in predicting the length of stay.

Now I want to use two way and three way ANOVA to understand this dataset. 

mod2 <- lm(LOS2 ~ factor(FRSTUSE1)+ factor(SERVICES_D)+factor(ROUTE1)+factor(FREQ1_D),
           data = under.age)
summary(mod2)


mod.interact <- aov(LOS2 ~ factor(FRSTUSE1)*factor(SERVICES_D)*factor(ROUTE1)*factor(FREQ1_D),
                    data = under.age)
summary(mod.interact )

Even though these models give us a lot of information and show that 4 factors 
are significant variables, it does not help explain for each group. I am interested
in understand how these variables influence each age group.


# Group 12 to 14 years old

# ANOVA 
group12.14 <- file.age %>% filter(FRSTUSE1 == 2)
mod12.14 <- aov(LOS2 ~ factor(SERVICES_D)+factor(ROUTE1)+factor(EMPLOY_D)+factor(FREQ1_D),
                data = group12.14)
summary(mod12.14)
#plot(mod12.14,c(1,2))

#Create a ANOVA table

final = anova(mod12.14)[,c(1,3,5)]
final
colnames(final) = c("DF", "MS", "P-value")
colnames(final)[2] = cols[2]
final = as.data.frame(round(final, digits = 2))
final$sign[final$`P-value` < 0.05] <- "*" 
final$sign[final$`P-value` < 0.01] <- "**"
final$sign[final$`P-value` < 0.001] <- "***"
final$sign[final$`P-value` > 0.05] <- "ns"
anova = writexl::write_xlsx(final, 'group12-14.xlsx')

# Interaction
file.age$SERVICES_D[file.age$SERVICES_D == 1] <- "Detox, 24-hour, hospital inpatient"
file.age$SERVICES_D[file.age$SERVICES_D == 7] <- " Ambulatory, non-intensive outpatient"
file.age$SERVICES_D[file.age$SERVICES_D == 2] <- "Detox, 24-hour, free-standing residential"
file.age$SERVICES_D[file.age$SERVICES_D == 3] <- "Rehab/residential, hospital (non-detox) "
file.age$SERVICES_D[file.age$SERVICES_D == 4] <- "Rehab/residential, short term (30 days or fewer)"
file.age$SERVICES_D[file.age$SERVICES_D == 5] <- "Rehab/residential, long term (more than 30 days)"
file.age$SERVICES_D[file.age$SERVICES_D == 6] <- "Ambulatory, intensive outpatient "
file.age$SERVICES_D[file.age$SERVICES_D == 8] <- "Ambulatory, detoxification"

file.age$EMPLOY_D[file.age$EMPLOY_D == 1] <- "Full-time"
file.age$EMPLOY_D[file.age$EMPLOY_D == 2] <- "Part-time"
file.age$EMPLOY_D[file.age$EMPLOY_D == 3] <- "Unemployed"
file.age$EMPLOY_D[file.age$EMPLOY_D == 4] <- "Not in labor force"

mod12.14.2 <- aov(LOS2 ~ factor(SERVICES_D)*factor(EMPLOY_D)*factor(FREQ1_D),
                  data = group12.14)
summary(mod12.14.2)

Very interesting!! Factors such as services at discharge, and frequency use 
are significant predictors for this group. Route of drug administration is normally 
very significant but it is not significant in predicting the length of stay for this 
group (F(18,2810)=142.8, p<0.01). Employment is also a significant predictor, but 
not as significant as services at discharge and frequency use.

# Group 15 to 17 years old
group15.17 <- file.age %>% filter(FRSTUSE1 == 3)
mod15.17 <- aov(LOS2 ~ factor(SERVICES_D)+factor(ROUTE1)+factor(EMPLOY_D)+factor(FREQ1_D),
                data = group15.17)
summary(mod15.17)
final = anova(mod15.17)[,c(1,3,5)]
final
colnames(final) = c("DF", "MS", "P-value")
colnames(final)[2] = cols[2]
final = as.data.frame(round(final, digits = 2))
final$sign[final$`P-value` < 0.05] <- "*" 
final$sign[final$`P-value` < 0.01] <- "**"
final$sign[final$`P-value` < 0.001] <- "***"
final$sign[final$`P-value` > 0.05] <- "ns"
anova = writexl::write_xlsx(final, 'group15.17.xlsx')

# Interaction
mod15.17.2 <- aov(LOS2 ~ factor(SERVICES_D)*factor(FREQ1_D)*factor(ROUTE1)*factor(EMPLOY_D),
                  data = group15.17)
summary(mod15.17.2)

Main effect of service at discharge and frequency of use were revealed in 
this analysis. The effect of employment and route of administration disappeared.
There is a significant interaction between these four variables.

# group 18 to 20 years old
group18.20 <- file.age %>% filter(FRSTUSE1 == 4)
mod18.20 <- aov(LOS2 ~ factor(SERVICES_D)+factor(ROUTE1)+factor(EMPLOY_D)+factor(FREQ1_D),
                data = group18.20 )
summary(mod18.20)

#Create a ANOVA table

final = anova(mod18.20)[,c(1,3,5)]
final
colnames(final) = c("DF", "MS", "P-value")
colnames(final)[2] = cols[2]
final = as.data.frame(round(final, digits = 2))
final$sign[final$`P-value` < 0.05] <- "*" 
final$sign[final$`P-value` < 0.01] <- "**"
final$sign[final$`P-value` < 0.001] <- "***"
final$sign[final$`P-value` > 0.05] <- "ns"
anova = writexl::write_xlsx(final, 'group18-20.xlsx')

plot(mod12.14,c(1,2))
# interaction
mod18.20.2 <- aov(LOS2 ~ factor(SERVICES_D)*factor(ROUTE1)*factor(EMPLOY_D)*factor(FREQ1_D),
                  data = group18.20)
summary(mod18.20.2)

Same patterns with the 15-17 group. Frequency of use and service at discharge 
seems to explain the length of stay very well. No interaction between two variables,
but the interaction of route and employment was revealed.

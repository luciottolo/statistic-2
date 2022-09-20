library(tidyverse) 
library(stargazer)
library(lmtest)
library(plm)
library(gtsummary) # descriptive statistics
library(modelsummary) # alternative to stargazer
library(estimatr) # regressions with robust SE
library(knitr) # data frames as latex tables
library(texreg)
library(kableExtra)
library(ggplot2)
library(gplots)
library(ggpubr)
library(dbplyr)

data <- read.csv("C:\\Users\\Lucio Tassone\\Downloads\\income_health_data.csv")
attach(data)
View(data)
str(data)
dim(data)
names(data)

data <- data %>% mutate(indiv_id = group_indices(., country_name, hh_id, childid))

data <- data %>% mutate(country_id =as.numeric(factor(country_name)),
                        region_id = as.numeric(factor(region)),
                        year_fct = as.factor(year)
)

df <- data %>% select (Y_his, log_y, country_name, year, stunting, dead5)


data[rowSums(is.na(data)) > 0,]
colSums(is.na(data))

dt <- data %>% filter(!is.na(stunting))
colSums(is.na(dt))
dim(dt)

dt %>%
  summarise(min = min(stunting, na.rm = T), max = max(stunting, na.rm = T),
            mean = mean(stunting, na.rm = T), sd = sd(stunting, na.rm = T)) 

data <-data %>% group_by(indiv_id, year, count(indiv_id))

qplot(stunting)
0.37* 157261 
summary(stunting)
hist(stunting)
boxplot(stunting)

summary(dead5)
0.04589*216213
hist(dead5)
boxplot(dead5)

summary(Y_his)
hist(Y_his)
boxplot(Y_his)

summary(log_y)
hist(log_y)
boxplot(log_y)

summary(mo_age)
summary(c_age)



factor(survey_id)
factor(data$indiv_id)

a <- lm(Y_his ~ child5)
summary(a)



dt %>% 
  group_by(year) %>%
  summarise(avg = mean(stunting), 
            min = min(stunting),
            max = max(stunting),
            total = n())
data %>% 
  group_by(year) %>%
  summarise(avg = mean(dead5), 
            min = min(dead5),
            max = max(dead5),
            total = n())

# Mean +/- standard deviation
ggerrorplot(data, x = "year", y = "dead5", 
            desc_stat = "mean_sd",
            error.plot = "errorbar",            # Change error plot type
            add = "mean"                        # Add mean points
)


ggerrorplot(data, x = "country_name", y = "dead5", 
            desc_stat = "mean_sd",
            error.plot = "errorbar",            # Change error plot type
            add = "mean"                        # Add mean points
)

ggerrorplot(dt, x = "year", y = "stunting", 
            desc_stat = "mean_sd",
            error.plot = "errorbar",            # Change error plot type
            add = "mean"                        # Add mean points
)


ggerrorplot(dt, x = "country_name", y = "stunting", 
            desc_stat = "mean_sd",
            error.plot = "errorbar",            # Change error plot type
            add = "mean"                        # Add mean points
)


lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

zoomaaa <- data %>% 
  group_by(country_name,year) %>%
  summarise(p_st= mean(stunting, na.rm=T)*100,
            sd.st = sd(stunting, na.rm = TRUE),
            p_de= mean(dead5, na.rm=T)*100,
            sd.de= sd(dead5, na.rm=T) ,
            count_st = sum(!is.na(stunting)),
            count_de = n(), 
            income_m=mean(Y_his),
            log_y_m=mean(log_y)) %>%
  mutate(se_st = sd.st / sqrt(count_st),
         se_de = sd.de / sqrt(count_de),
         lower.ci.s = p_st - qt(1 - (0.05 / 2), count_st - 1) * se_st,
         upper.ci.s = p_st + qt(1 - (0.05 / 2), count_st - 1) * se_st,
         lower.ci.d = p_de - qt(1 - (0.05 / 2), count_de - 1) * se_de,
         upper.ci.d = p_de + qt(1 - (0.05 / 2), count_de - 1) * se_de) %>%
  mutate(p_st= round(p_st,3),
         p_de= round(p_de,3),
         income_m= round(income_m,3),
         log_y_m= round(log_y_m,3),
         lower.ci.s= round(lower.ci.s,3),
         upper.ci.s= round(upper.ci.s,3),
         lower.ci.d= round(lower.ci.d,3),
         upper.ci.d= round(upper.ci.d,3))%>%
  subset(year > "2003" , ) %>%
  relocate(country_name, year, count_st, p_st, lower.ci.s, upper.ci.s,count_de, p_de, lower.ci.d, upper.ci.d)%>%
  select(-one_of("sd.st" ,"sd.de","se_st","se_de"))%>%
  unite(CIs, lower.ci.s, upper.ci.s, sep = " - " , remove = T, na.rm=T)%>%
  unite(CId, lower.ci.d, upper.ci.d, sep = " - " , remove = T, na.rm=T)%>%
  mutate(CIs = paste0("(", CIs, ")"))%>%
  mutate(CId = paste0("(", CId, ")"))%>%
  unite(PrS, p_st, CIs, sep = "%  " , remove = T, na.rm=T)%>%
  unite(Prd, p_de, CId, sep = "%  " , remove = T, na.rm=T)



zoomaaa %>%
  kbl(col.names = (c(" ",
                     " ",
                     "N",
                     "Proportion stunted
        (95% CI)",
                     "N",
                     "Proportion child mortality (95% CI)",
                     " ",
                     " ")), booktabs = T) %>%
  kable_material(c("striped", "hover"))%>%
  add_header_above(c(" ","Survey year", "Stunting sample" = 2, "Child mortality Sample" = 2,"Income " ,"log_income"))%>%
  add_footnote(c("Data are from surveys after 2003. N is the sample size. Income measured in USD."),
               notation = "symbol")






#this has been taken from david, please do not evaluate it, just saving it as I find it really cool #
zoom1 <- data%>%  group_by(country_name,year_fct)%>% summarise(count=n())

ggplot(zoom1,aes(year_fct,country_name))+
  geom_tile(aes(fill = count))+
  geom_text(aes(label =round(count, 1)))+
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.text.x =element_text(angle = 45, hjust = 1))+
  labs(title = "Number of observations over years and countries")+
  xlab("Year")
##
zooma <- data%>%  group_by(country_name)%>% summarise(count=n())
summary(zooma$count)

zoomb <- dt%>%  group_by(country_name, stunting)%>% summarise(count=n()) %>%mutate(prop=count[stunting==1]/(count[stunting==1]+count[stunting==0]))
summary(zoomb$prop)

zoomc <- data%>%  group_by(country_name, dead5)%>% summarise(count=n()) %>%mutate(prop=count[dead5==1]/(count[dead5==1]+count[dead5==0]))
summary(zoomc$prop)

zoom2 <- data%>% group_by(year_fct,country_name)%>% mutate(mean_Y=mean(Y_his),mean_stunt=mean(stunting,na.rm=T),mean_dead5=mean(dead5))%>% ungroup()%>% group_by(year_fct)

summary(zoom2$yr_stunt_na)

##also this, saving it but please do not evaluate it, not done by myself #
dfa <- df%>% group_by(year,country_name)%>% mutate(mean_Y=mean(Y_his),mean_stunt=mean(stunting,na.rm=T),mean_dead5=mean(dead5))%>% ungroup()%>% group_by(year)%>% mutate(yr_Y=mean(Y_his),yr_stunt_na=mean(is.na(stunting)),yr_dead5=mean(dead5))
ggplot(dfa,aes(year, mean_dead5, colour=country_name))+
  geom_line()+
  labs(title="Evolution of child mortality under 5 years")+
  xlab("Year")+
  ylab("mean child5")

ggplot(dfa,aes(year, mean_stunt, colour=country_name))+
  geom_line()+
  labs(title="Evolution of proportion of stunted children")+
  xlab("Year")+ ylab("mean stunted")


ggplot(dfa)+
  geom_line(aes(year, mean_Y, colour=country_name))+
  labs(title="Evolution of houshold income (simulated)")+
  xlab("Year")+
  ylab("mean Y_his")
####

####stunting####
library(tidyverse) 
library(stargazer)
library(lmtest)
library(plm)
library(gtsummary) # descriptive statistics
library(modelsummary) # alternative to stargazer
library(estimatr) # regressions with robust SE
library(knitr) # data frames as latex tables
library(texreg)
library(kableExtra)
library(ggplot2)

data <- read.csv("C:\\Users\\Lucio Tassone\\Downloads\\income_health_data.csv")
attach(data)
View(data)
str(data)
dim(data)
names(data)

data <- data %>% mutate(indiv_id = group_indices(., country_name, hh_id, childid))

data <- data %>% mutate(country_id =as.numeric(factor(country_name)),
                        region_id = as.numeric(factor(region)),
                        year_fct = as.factor(year)
)

dt <- data %>% filter(!is.na(stunting))
colSums(is.na(dt))
dim(dt)
#I start model for stunting

m1a <- lm(stunting ~ log_y, data=dt)
summary(m1a)
screenreg(m1a)

m_x <- lm_robust(stunting ~ log_y, data=dt, se_type = "HC1")
summary(m_x)


m1b <- lm(stunting ~ log_y + survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
          data=dt)
summary(m1b)
screenreg(m1b)

m_xx <- lm_robust(stunting ~ log_y, data=dt, se_type = "HC1")
summary(m_xx)

modelsummary(list(m1a, m1b), 
             stars = T) %>%
  kable_styling(latex_options = "hold_position")

#I add indiv_id and year FE

maa <- plm(stunting  ~ log_y ,
           data= dt,
           index = c("indiv_id"),
           model = "within") 
summary(maa)

pFtest(maa,m1a)
#no need use indiv_id FE

ma2 <- plm(stunting  ~ log_y ,
           data= dt,
           index = c("year"),
           model = "within"
) 
summary(ma2)

pFtest(ma2,m1a)

#cool to use year FE

ma3 <- plm(stunting  ~ log_y ,
           data= dt,
           index = c("indiv_id","year"),
           model = "within"
) 
summary(ma3)

pFtest(ma3,m1a)

#try to compare if used both, then I ll select the FE that make sense to be used
#I will use only year FE, definitely

ma4 <- plm(stunting  ~ log_y + as.factor(country_id) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
           data= dt,
           index = c("year"),
           model = "within") 
summary(ma4)

#adding every variables in the pmodel derives in singularity

ma5 <- lm(stunting  ~ log_y + as.factor(country_id) + as.factor(region_id)+ as.factor(year) + survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
          data = dt) 
summary(ma5)

#using ols I have a less precise regression, no problem of unicity 

#I will use binomial, probit and logit as stunting is binary to have an even more precise model

mybinomiala <- glm(stunting  ~ log_y ,family = binomial, 
                   data = dt)
summary(mybinomiala)

myprobita <- glm(stunting  ~ log_y , family = binomial(link = "probit"), 
                 data = dt)
summary(myprobita)

mylogita <- glm(stunting  ~ log_y , family = binomial(link = "logit"), 
                data = dt)
summary(mylogita)

modelsummary(list(mybinomiala, myprobita, mylogita), 
             stars = T) %>%
  kable_styling(latex_options = "hold_position")

mybinomial <- glm(stunting  ~ log_y + as.factor(country_id) + as.factor(region_id)+ as.factor(year)  + survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
                  family = binomial, 
                  data = dt)
summary(mybinomial)

myprobit <- glm(stunting  ~ log_y + as.factor(country_id) + as.factor(region_id) + as.factor(year) + survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
                family = binomial(link = "probit"), 
                data = dt)
summary(myprobit)

mylogit <- glm(stunting  ~ log_y + as.factor(country_id) + as.factor(region_id) + as.factor(year) + survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
               family = binomial(link = "logit"), 
               data = dt)
summary(mylogit)

modelsummary(list(mybinomial, myprobit, mylogit), 
             stars = T) %>%
  kable_styling(latex_options = "hold_position")

modelsummary(list(mybinomiala, mybinomial), 
             stars = T) %>%
  kable_styling(latex_options = "hold_position")

modelsummary(list(myprobita, myprobit), 
             stars = T) %>%
  kable_styling(latex_options = "hold_position")

modelsummary(list(mylogita, mylogit), 
             stars = T) %>%
  kable_styling(latex_options = "hold_position")


#same but for different countries

mac1 <- glm(stunting  ~ log_y + as.factor(region_id) + as.factor(year)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 1 ,
            family = binomial(link = "logit"),
            data= dt ) 
summary(mac1)

mac2 <- glm(stunting  ~ log_y + as.factor(region_id) + as.factor(year)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 2 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac2)

mac3 <- glm(stunting  ~ log_y + as.factor(year) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 3 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac3)

mac4 <- glm(stunting  ~ log_y + as.factor(year) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 4 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac4)

mac5 <- glm(stunting  ~ log_y + as.factor(year) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 5 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac5)

mac6 <- glm(stunting  ~ log_y + as.factor(year) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 6 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac6)

mac7 <- glm(stunting  ~ log_y + as.factor(year) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 7 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac7)

mac8 <- glm(stunting  ~ log_y + as.factor(year) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 8 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac8)

mac9 <- glm(stunting  ~ log_y + as.factor(year) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 9 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac9)

rob_se <- list(sqrt(diag(vcovHC(mac1, type = "HC1"))),
               sqrt(diag(vcovHC(mac2, type = "HC1"))),
               sqrt(diag(vcovHC(mac3, type = "HC1"))),
               sqrt(diag(vcovHC(mac4, type = "HC1"))),
               sqrt(diag(vcovHC(mac5, type = "HC1"))),
               sqrt(diag(vcovHC(mac6, type = "HC1"))),
               sqrt(diag(vcovHC(mac7, type = "HC1"))),
               sqrt(diag(vcovHC(mac8, type = "HC1"))),
               sqrt(diag(vcovHC(mac9, type = "HC1"))),
               sqrt(diag(vcovHC(mylogita, type = "HC1"))),
               sqrt(diag(vcovHC(mylogit, type = "HC1"))))

stargazer(m1b, mylogit, mac1, mac2, mac3, mac4,mac5,mac6,mac7,mac8,mac9,
          type = "latex",
          single.row = FALSE,
          digits = 3, 
          se = rob_se,
          header = FALSE, 
          font.size = "tiny", 
          column.sep.width = "0.025pt",
          omit = c("year", "region_id", "country_id", "survey_weight", "mo_age", "mo_totchild", "mo_totchild", "mo_5births", "mo_breastfeeding", "c_age", "c_sex","mo_age_birth", "c_first","hhs", "child5", "urban"),
          
          df = FALSE,
          no.space = TRUE)

#last findings to be added...
rob_se <- list(sqrt(diag(vcovHC(m1a, type = "HC1"))),
               sqrt(diag(vcovHC(m1b, type = "HC1"))),
               sqrt(diag(vcovHC(mylogita, type = "HC1"))),
               sqrt(diag(vcovHC(mylogit, type = "HC1"))))


stargazer(m1a, m1b, mylogita, mylogit, 
          type = "latex",
          single.row = FALSE,
          digits = 3, 
          se = rob_se,
          header = FALSE, 
          font.size = "tiny", 
          column.sep.width = "0.025pt",
          omit = c("year", "region_id", "country_id"),
          
          df = FALSE,
          no.space = TRUE)


#i wanna select variables that could be related to compare their effect on stunting/dead5 
#relatively to log_y

myprobit <- glm(stunting  ~ log_y + as.factor(country_id) + as.factor(region_id) + as.factor(year)  + mo_age + mo_totchild + as.factor(mo_5births) + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year  + c_first  + deadsibling + hhs + child5 + urban, 
                family = binomial(link = "logit"), 
                data = dt)
summary(myprobit)

mylogita <- glm(stunting  ~ log_y + as.factor(country_id) + as.factor(region_id) + as.factor(year) + mo_age + mo_totchild + as.factor(mo_5births)  + mo_breastfeeeding + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance     + hhs + child5 + urban, 
                family = binomial(link = "logit"), 
                data = dt)
summary(myprobit)

stargazer(m1a, m1b, mylogita, mylogit, 
          type = "latex",
          single.row = FALSE,
          digits = 3, 
          se = rob_se,
          header = FALSE, 
          font.size = "tiny", 
          column.sep.width = "0.025pt",
          omit = c("year", "region_id", "country_id", "survey_weight", "mo_age", "mo_totchild", "mo_totchild", "mo_5births", "mo_breastfeeding", "c_age", "c_sex","mo_age_birth", "c_first","hhs", "child5", "urban"),
          
          df = FALSE,
          no.space = TRUE)


mm <- lm(as.numeric(as.factor(mo_pregnant)) ~ mo_noedu + mo_primary + mo_secondary,
         data = data)
summary(mm)

#notes on interpretation of coefficients in logistic models
#Probability = 1 / (1 + exp(-x)) = 1 /(1 + exp(- -0.113)) = 1 /(1 + exp(1.94)) = 0.13 = 13%.
#1 /(1 + exp(- -0.113))

mo_diff_prim_onli <-   mo_primary-mo_secondary
educ_log_y <- lm(stunting ~ log_y + as.factor(country_id) + as.factor(region_id) + as.factor(year)+as.factor(mo_5births)+ mo_noedu + mo_diff_prim_onli + mo_secondary  + mo_noedu*log_y + mo_diff_prim_onli*log_y + mo_secondary*log_y,
                 data = dt)
screenreg(educ_log_y)


log(2)
####Interpretation####
#I wanna compare strenght of effect of income Y compared to
#educational and healthcare relate variables.
#my final equation 
#stunting =  -0.184*log_y -0.063*water_improved_total -0.112*sani_improved_total +0.079*mo_noedu -0.152*mo_primary -0.375*mo_secondary -0.144*mo_assistance + c(interceipt+ other variables)

####dead5####

library(tidyverse) 
library(stargazer)
library(lmtest)
library(plm)
library(gtsummary) # descriptive statistics
library(modelsummary) # alternative to stargazer
library(estimatr) # regressions with robust SE
library(knitr) # data frames as latex tables
library(texreg)
library(kableExtra)
library(ggplot2)

data <- read.csv("C:\\Users\\Lucio Tassone\\Downloads\\income_health_data.csv")
attach(data)
View(data)
str(data)
dim(data)
names(data)

data <- data %>% mutate(indiv_id = group_indices(., country_name, hh_id, childid))

data <- data %>% mutate(country_id =as.numeric(factor(country_name)),
                        region_id = as.numeric(factor(region)),
                        year_fct = as.factor(year)
)
dt <- data

m1a <- lm(dead5 ~ log_y, data=dt)
summary(m1a)
screenreg(m1a)

m_x <- lm_robust(dead5 ~ log_y, data=dt, se_type = "HC1")
summary(m_x)


m1b <- lm(dead5 ~ log_y + survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
          data=dt)
summary(m1b)
screenreg(m1b)

m_xx <- lm_robust(dead5 ~ log_y, data=dt, se_type = "HC1")
summary(m_xx)

modelsummary(list(m1a, m1b), 
             stars = T) %>%
  kable_styling(latex_options = "hold_position")

#I add indiv_id and year FE

maa <- plm(dead5  ~ log_y ,
           data= dt,
           index = c("indiv_id"),
           model = "within") 
summary(maa)

pFtest(maa,m1a)
#no need use indiv_id FE

ma2 <- plm(dead5  ~ log_y ,
           data= dt,
           index = c("year"),
           model = "within"
) 
summary(ma2)

pFtest(ma2,m1a)

#cool to use year FE

ma3 <- plm(dead5  ~ log_y ,
           data= dt,
           index = c("indiv_id","year"),
           model = "within"
) 
summary(ma3)

pFtest(ma3,m1a)

#try to compare if used both, then I ll select the FE that make sense to be used
#I will use only year FE, definitely

ma4 <- plm(dead5  ~ log_y + as.factor(country_id) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
           data= dt,
           index = c("year"),
           model = "within") 
summary(ma4)

#adding every variables in the pmodel derives in singularity

ma4 <- lm(dead5  ~ log_y + as.factor(country_id) + as.factor(region_id)+ as.factor(year) + survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
          data = dt) 
summary(ma4)

#using ols I have a more precise regression. 

#I will use binomial, probit and logit as stunting is binary to have an even more precise model

mybinomiala <- glm(dead5  ~ log_y ,family = binomial, 
                   data = dt)
summary(mybinomiala)

myprobita <- glm(dead5  ~ log_y , family = binomial(link = "probit"), 
                 data = dt)
summary(myprobita)

mylogita <- glm(dead5  ~ log_y , family = binomial(link = "logit"), 
                data = dt)
summary(mylogita)

modelsummary(list(mybinomiala, myprobita, mylogita), 
             stars = T) %>%
  kable_styling(latex_options = "hold_position")

mybinomial <- glm(dead5  ~ log_y + as.factor(country_id) + as.factor(region_id)+ as.factor(year)  + survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
                  family = binomial, 
                  data = dt)
summary(mybinomial)

myprobit <- glm(dead5  ~ log_y + as.factor(country_id) + as.factor(region_id) + as.factor(year) + survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
                family = binomial(link = "probit"), 
                data = dt)
summary(myprobit)

mylogit <- glm(dead5  ~ log_y + as.factor(country_id) + as.factor(region_id) + as.factor(year) + survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
               family = binomial(link = "logit"), 
               data = dt)
summary(mylogit)

modelsummary(list(mybinomial, myprobit, mylogit), 
             stars = T) %>%
  kable_styling(latex_options = "hold_position")

modelsummary(list(mybinomiala, mybinomial), 
             stars = T) %>%
  kable_styling(latex_options = "hold_position")

modelsummary(list(myprobita, myprobit), 
             stars = T) %>%
  kable_styling(latex_options = "hold_position")

modelsummary(list(mylogita, mylogit), 
             stars = T) %>%
  kable_styling(latex_options = "hold_position")


#same but for differen countries

mac1 <- glm(dead5  ~ log_y + as.factor(region_id) + as.factor(year)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 1 ,
            family = binomial(link = "logit"),
            data= dt ) 
summary(mac1)

mac2 <- glm(dead5  ~ log_y + as.factor(region_id) + as.factor(year)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 2 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac2)

mac3 <- glm(dead5  ~ log_y + as.factor(year) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 3 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac3)

mac4 <- glm(dead5  ~ log_y + as.factor(year) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 4 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac4)

mac5 <- glm(dead5  ~ log_y + as.factor(year) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 5 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac5)

mac6 <- glm(dead5  ~ log_y + as.factor(year) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 6 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac6)

mac7 <- glm(dead5  ~ log_y + as.factor(year) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 7 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac7)

mac8 <- glm(dead5  ~ log_y + as.factor(year) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 8 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac8)

mac9 <- glm(dead5  ~ log_y + as.factor(year) + as.factor(region_id)+ survey_weight + mo_age + mo_totchild + mo_5births + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year + mo_age_birth + c_first  + deadsibling + hhs + child5 + urban, 
            subset = country_id == 9 ,
            family = binomial(link = "logit"),
            data= dt) 
summary(mac9)


#final tabs and graphs

rob_se <- list(sqrt(diag(vcovHC(mac1, type = "HC1"))),
               sqrt(diag(vcovHC(mac2, type = "HC1"))),
               sqrt(diag(vcovHC(mac3, type = "HC1"))),
               sqrt(diag(vcovHC(mac4, type = "HC1"))),
               sqrt(diag(vcovHC(mac5, type = "HC1"))),
               sqrt(diag(vcovHC(mac6, type = "HC1"))),
               sqrt(diag(vcovHC(mac7, type = "HC1"))),
               sqrt(diag(vcovHC(mac8, type = "HC1"))),
               sqrt(diag(vcovHC(mac9, type = "HC1"))),
               sqrt(diag(vcovHC(mylogita, type = "HC1"))),
               sqrt(diag(vcovHC(mylogit, type = "HC1"))))

stargazer(m1b, mylogit, mac1, mac2, mac3, mac4,mac5,mac6,mac7,mac8,mac9,
          type = "latex",
          single.row = FALSE,
          digits = 3, 
          se = rob_se,
          header = FALSE, 
          font.size = "tiny", 
          column.sep.width = "0.025pt",
          omit = c("year", "region_id", "country_id", "survey_weight", "mo_age", "mo_totchild", "mo_totchild", "mo_5births", "mo_breastfeeding", "c_age", "c_sex","mo_age_birth", "c_first","hhs", "child5", "urban"),
          
          df = FALSE,
          no.space = TRUE)

#last findings to be added...
rob_se <- list(sqrt(diag(vcovHC(m1a, type = "HC1"))),
               sqrt(diag(vcovHC(m1b, type = "HC1"))),
               sqrt(diag(vcovHC(mylogita, type = "HC1"))),
               sqrt(diag(vcovHC(mylogit, type = "HC1"))))


stargazer(m1a, m1b, mylogita, mylogit, 
          type = "latex",
          single.row = FALSE,
          digits = 3, 
          se = rob_se,
          header = FALSE, 
          font.size = "tiny", 
          column.sep.width = "0.025pt",
          omit = c("year", "region_id", "country_id"),
          
          df = FALSE,
          no.space = TRUE)


#i wanna select variables that could be related to compare their effect on stunting/dead5 
#relatively to log_y

mylogit <- glm(dead5  ~ log_y + as.factor(country_id) + as.factor(region_id) + as.factor(year)  + mo_age + mo_totchild + as.factor(mo_5births) + mo_pregnant + mo_breastfeeeding + c_age + c_sex + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance + birth_year  + c_first  + deadsibling + hhs + child5 + urban, 
               family = binomial(link = "logit"), 
               data = data)
summary(mylogit)

mylogita <- glm(dead5 ~ log_y + as.factor(country_id) + as.factor(region_id) + as.factor(year) + mo_age + mo_totchild + as.factor(mo_5births)  + mo_breastfeeeding + water_improved_total + sani_improved_total + mo_noedu + mo_primary + mo_secondary + mo_tetanus + mo_care + mo_assistance     + hhs + child5 + urban, 
                family = binomial(link = "logit"), 
                data = data)
summary(mylogita)+++
  
  stargazer(m1a, m1b, mylogita, mylogit, 
            type = "latex",
            single.row = FALSE,
            digits = 3, 
            se = rob_se,
            header = FALSE, 
            font.size = "tiny", 
            column.sep.width = "0.025pt",
            omit = c("year", "region_id", "country_id", "survey_weight", "mo_age",  "mo_totchild", "mo_5births", "mo_breastfeeding", "c_age", "c_sex","mo_age_birth", "c_first","hhs", "child5", "urban"),
            
            df = FALSE,
            no.space = TRUE)


mm <- lm(as.numeric(as.factor(mo_pregnant)) ~ mo_noedu + mo_primary + mo_secondary,
         data = data)
summary(mm)


mo_diff_prim_onli <-   mo_primary-mo_secondary
educ_log_y <- lm(dead5 ~ log_y + as.factor(country_id) + as.factor(region_id) + as.factor(year)+as.factor(mo_5births)+ mo_noedu + mo_diff_prim_onli + mo_secondary  + mo_noedu*log_y + mo_diff_prim_onli*log_y + mo_secondary*log_y,
                 data = data)
screenreg(educ_log_y)


log(2)










#The following code is the application of Reduced Rank Regression model in the context of
#multiple predictors and multiple mixed response variables. The data analyzed are related to the 
#environment, resulting from a questionnaire. The objective of the analysis is to understand whether,
#and in what measure, is present a relationship between people "attitudes" and "behaviours" towards 
#the environment.


source("C:/Users/annag/Desktop/Papers/gmr4.R")

#load data and import the libraries
library(foreign)
library(HH)
library(tidyverse)
df= read.spss("C:\\Users\\annag\\Desktop\\Papers\\Dataset\\ZA5500_v3-0-0.sav\\ZA5500_v3-0-0.sav",to.data.frame=TRUE)

dim(df) #50437 rows and 360 columns
head(df) #we can see the first 3 columns are useless
#filter for only one country(Netherlands) row 30733 until row 32204
df_filtered=df[30733:32204,]
df_=df_filtered[,6:72] #after column 63 (labeled 68) there are all the questions related to the single individual
#(sex,age,income etc.)
#I included sex,year of birth, age and education years:they could influence the answers people give.

str(df_) #The variables are all factors with different levels. Some variables are encoded with numbers.
dim(df_) #1472 observations and 67 variables

#In the basic questionnaire there are 60 questions (+2 optional but they are all NA values),
#47 are variables with ordinal responses,4 are variables with binary responses, the rest are variables with nominal 
#responses but the gmr4 program cannot deal with them so we ignore them.
sum(is.na(df_))#There are 7066 NULL values.
#percent missing values per variable
apply(df_, 2, function(col)sum(is.na(col))/length(col)*100) #apart from predictor variables like Sex, Age and Educyrs
#which have no null values the other variables contain null values in lesser or greater extent 
#v65 is the one with the greatest percentage of null values (21.6%)[not used],then v54 (19.09%)[not used],
#v66(15.56%)[optional item in the questionnaire, not used],v57(14.81%) [Q20c: How often do you cut back on driving a car for enviromental reasons?]
#Since the variable v57 has too many missing values we delete it.
df_[duplicated(df_),] #there are no duplicated rows in this dataset


#define Yn, Yo and X, and set Yn as NULL
Yn=NULL

pred_var=c(12,26:29,35,45,64,66,67)#sex,age and years of educations are among predictor variables
#for X we select only the most predictive variables 

X=df_[, pred_var]
attach(X) #in this way objects(like columns) in the dataframe can be accessed by simply giving their names
names(X) #checking the names of the columns of X

unique(X) #there are some columns which contain useless sentences in addition to the appropriate information
#question 6 (concern about enviromental issues), 9 and 10(belief in science, worries about future of enviroment)
#question 11 (economic growth and enviroment), question 12 a,b,c (taxes, cut of standard living 
#to protect the enviroment), 13 from a to g, question 16(what your country is doing for enviroment)



Yb=df_[ ,58:61] #questions 21 and 22
attach(Yb)
names(Yb)
typeof(Yb)
unique(Yb) #checking with unique for string inconsistencies(typos,capitalization errors,misplaced punctuation ecc.)



Yo=df_[ ,c(52,53,55,56,57)] #consider only question 20(a,b,c,d,e,f)
attach(Yo)
names(Yo)
unique(Yo)
#Q20a:How often do you make a special effort to sort glass or tins or plastic or newspapers and so on for recycling?
#Q20b:How often do you make a special effort to buy fruit and vegetables grown without pesticides or chemicals?
#Q20d:How often do you reduce the energy or fuel you use at home for environmental reasons? 
#Q20e:And how often do you choose to save or re-use water for environmental reasons?
#Q20f:And how often do you avoid buying certain products for environmental reasons?

# transform ordinal to numbers
Yo2 = matrix(NA, 1472, 5); colnames(Yo2) = colnames(Yo); rownames(Yo2) = rownames(Yo)
for(r in 1:5){ 
  Yo2[ , r] = ifelse(Yo[ , r] == "Always", 4, #ifelse(test expression,yes,no)
                     ifelse(Yo[,r] == "Often", 3, 
                            ifelse(Yo[ , r] == "Sometimes", 2, 1)))

}  

names(Yo)[names(Yo) == "v55"] <- "Recycling"
names(Yo)[names(Yo) == "v56"] <- "Buy fruit and vegetables without pesticides"
names(Yo)[names(Yo) == "v58"] <- "Reduce energy or fuel"
names(Yo)[names(Yo) == "v59"] <- "Save or re-use water"
names(Yo)[names(Yo) == "v60"] <- "Avoid buying certain products"

names(Yo2)[names(Yo2) == "v55"] <- "Recycling"
names(Yo2)[names(Yo2) == "v56"] <- "Buy fruit and vegetables without pesticides"
names(Yo2)[names(Yo2) == "v58"] <- "Reduce energy or fuel"
names(Yo2)[names(Yo2) == "v59"] <- "Save or re-use water"
names(Yo2)[names(Yo2) == "v60"] <- "Avoid buying certain products"


mydt_1 <- Yo %>% 
  pivot_longer(
    values_drop_na = TRUE, # drop NA
    cols = c("Recycling","Buy fruit and vegetables without pesticides","Reduce energy or fuel","Save or re-use water","Avoid buying certain products")
  ) %>% 
  group_by(name) %>% 
  summarise('Always'  =  sum(value == "Always")/n(),
            'Often' = sum(value == "Often")/n(),
            'Sometimes' = sum(value == "Sometimes")/n(),
            'Never' = sum(value == "Never")/n()
  )
mydt_1
likert(name~.,mydt_1,xlab="Percentage",ylab = "Question",as.percent = TRUE,main=list("Effort for the environment",x=unit(.63, "npc")),
       rightAxis = FALSE)
# summary statistics of the answers
summary(mydt_1[,3],digits=2)


#the majority of people always make a special effort to sort glass or tins or plastic or newspapers and so on for recycling
#only sometimes people make a special effort to buy fruit and vegetables grown without pesticides or chemicals
#often people reduce the energy or fuel used at home for environmental reasons
#sometimes people choose to save or re-use water for environmental reasons
#sometimes people avoid buying certain products for environmental reasons
#thus, the majority of the effort for the environment is spent on recycling

#Yo2 <- Yo2 %>% as_tibble() %>% dplyr::filter(complete.cases(Yo2)) #filtering Yo2 for complete cases 1179 rows 6 columns



# transform binary to numbers (0/1)
Yb2 = matrix(NA, 1472, 4); colnames(Yb2) = colnames(Yb); rownames(Yb2) = rownames(Yb)
#Q21:Are you a member of any group whose main aim is to preserve or protect the environment?
Yb2[ , 1] = ifelse(Yb[ , 1] == "Yes", 1, 0)
member= table(Yb[ , 1]) 
#expressing this in percentage:
n= nrow(df_)
percent_member=((member)/n * 100) 
#the majority of the people  (86.34%) aren't members of an environmental group, so the percentage is quite unbalanced


#Q22a: In the last five years have you signed a petition about an environmental issue? 
#Q22b:In the last five years have you given money to an environmental group?
#Q22c:In the last five years have you taken part in a protest or demonstration about an environmental issue?
for(r in 2:4){
  Yb2[ , r] = ifelse(Yb[ , r] == "Yes, I have", 1, 0)
}
#typeof(Yb2) #double,the type of an R object of class numeric is double, a variable will be stored as double if the value is numeric
# also transform X to numbers

X2 = matrix(NA, 1472, 10); colnames(X2) = colnames(X); rownames(X2) = rownames(X)
#Q6:how concerned are you about environmental issues? #v15
X2[ ,1]= ifelse(X[ ,1]=="Very concerned",5,X[,1])
names(X)[names(X) == "v15"] <- "Concern about the enviroment"


mydt_2 <- X %>% 
  pivot_longer(
    values_drop_na = TRUE, # drop NA
    cols = "Concern about the enviroment"
  ) %>% 
  group_by(name) %>% 
  summarise('1 Not at all concerned'  =  sum(value == "1 Not at all concerned")/n(),
            '2' = sum(value == "2")/n(),
            '3' = sum(value == "3")/n(),
            '4' = sum(value == "4")/n(),
            '5 Very concerned' = sum(value == "5 Very concerned")/n()
  )
mydt_2
likert(name~.,mydt_2,xlab="Percentage",ylab = "Question",as.percent = TRUE,rightAxis=FALSE,main = list("How concerned are you about the enviroment?",x=unit(.58, "npc")))




#Q12a:How willing would you be to pay much higher prices in order to protect the environment? 
#Q12b:How willing would you be to pay much higher taxes in order to protect the environment?
#Q12c: How willing would you be to accept cuts in your standard of living in order to protect the environment? 
for(r in 2:4){ 
  X2[ , r] = ifelse(X[ , r] == "Very willing", 5, 
                     ifelse(X[,r] == "Fairly willing", 4,
                           ifelse(X[,r] == "Neither willing nor unwilling ", 3,
                                  ifelse(X[,r] == "Fairly unwilling ", 2,
                                         ifelse(X[ , r] == "Very unwilling", 1, 0)))))
}
table(X2[,r]) #the majority of people couldn't choose


names(X)[names(X) == "v29"] <- "Pay higher prices"
names(X)[names(X) == "v30"] <- "Pay higher taxes"
names(X)[names(X) == "v31"] <- "Accept cuts in your stardard of living"


mydt_3 <- X %>% 
  pivot_longer(
    values_drop_na = TRUE, # drop NA
    cols = c("Pay higher prices","Pay higher taxes","Accept cuts in your stardard of living")
  ) %>% 
  group_by(name) %>% 
  summarise('Very willing'  =  sum(value == "Very willing")/n(),
            'Fairly willing' = sum(value == "Fairly willing")/n(),
            'Fairly unwilling' = sum(value == "Fairly unwilling")/n(),
            'Very unwilling' = sum(value == "Very unwilling")/n()
  )
(mydt_3)*100
likert(name~.,mydt_3,xlab="Percentage",ylab = "Question",as.percent=TRUE,main =list( "Willing to pay more/accept cuts for the enviroment",x=unit(.62, "npc")),rightAxis=FALSE)

#people are fairly willing to pay much higher prices, taxes and accept cuts in their standard of living to protect the environment

#Q13a:It is just too difficult for someone like me to do much about the environment
#Q13g: Environmental problems have a direct effect on my everyday life

for(r in 5:6){
  X2[ ,r] = ifelse(X[ ,r]== "Agree strongly",5,
                     ifelse(X[ ,r]== "Agree",4,
                            ifelse(X[ ,r]== "Neither agree or disagree",3,
                                  ifelse(X[ ,r]== "Disagree",2,
                                         ifelse(X[ ,r]== "Disagree strongly",1,0)))))
  
}
table(X2[,r])
names(X)[names(X) == "v32"] <- "Individual action for the environment "

mydt_4 <- X %>% 
  pivot_longer(
    values_drop_na = TRUE, # drop NA
    cols = "Individual action for the environment "
  ) %>% 
  group_by(name) %>% 
  summarise('Agree strongly'  =  sum(value == "Agree Strongly")/n(),
            'Neither agree or disagree' = sum(value == "Neither agree or disagree")/n(),
            'Disagree' = sum(value == "Disagree")/n(),
            'Disagree strongly' = sum(value == "Disagree strongly")/n()
  )
mydt_4
likert(name~.,mydt_4,xlab="Percentage",ylab = "Question",as.percent=TRUE,main = list("It is too difficult to do much about the environment",x=unit(.62, "npc")),rightAxis=FALSE)
                                                   


names(X)[names(X) == "v38"] <- "Effect of enviromental problems on life "


mydt_5 <- X %>% 
  pivot_longer(
    values_drop_na = TRUE, # drop NA
    cols = "Effect of enviromental problems on life "
  ) %>% 
  group_by(name) %>% 
  summarise('Agree strongly'  =  sum(value == "Agree Strongly")/n(),
            'Neither agree or disagree' = sum(value == "Neither agree or disagree")/n(),
            'Disagree' = sum(value == "Disagree")/n(),
            'Disagree strongly' = sum(value == "Disagree strongly")/n()
  )
mydt_5
likert(name~.,mydt_5,xlab="Percentage",ylab = "Question",as.percent=TRUE,main = list("Environmental problems have an effect on my everyday life",x=unit(.62,"npc")),rightAxis=FALSE)
                                                  

#BARPLOT
effect_on_life= table(X[ , 6]) 
#expressing this in percentage:
n= nrow(df_)
percent_effect_on_life=((effect_on_life)/n * 100) 
barplot(percent_effect_on_life,ylim=c(0,50), ylab="Percent",main="Environmental problems have a direct effect on my life",col = grey.colors(5, start =1, end = 0))
#the majority of people (35%) disagree, they don't think that environmental problems have a direct effect on their everyday life



#Q16:Do you think that your country is doing ... to protect the environment?
X2[ , 7] = ifelse(X[ , 7] == "More than enough", 3,
                  ifelse(X[ ,7]=="About the right amount",2,
                         ifelse(X[ ,7]=="Or too little",1,0)))


names(X)[names(X) == "v48"] <- "Country for the environment"

mydt_6 <- X %>% 
  pivot_longer(
    values_drop_na = TRUE, # drop NA
    cols = "Country for the environment"
  ) %>% 
  group_by(name) %>% 
  summarise('More than enough'  =  sum(value == "More than enough")/n(),
            'About the right amount' = sum(value == "About the right amount")/n(),
            'Or too little' = sum(value == "Or too little")/n()
  )

likert(name~.,mydt_6,xlab="Percentage",ylab = "Question",as.percent=TRUE,main = list( "How much the country is doing for the environment?",x=unit(.6,"npc")),rightAxis = FALSE)
                                                  

table(X[,7])
n= nrow(df_)
((table(X[,7]))/n * 100) #almost 43% of people answer that the country is doing "about the right amount",23.7% believes
#the country the country is doing "More than enough",20.58% "Too little"
barplot(((table(X[,7]))/n * 100),ylim=c(0,60), ylab="Percent",main="The country does enough for the enviroment?",
        col="lightgreen",border=TRUE)
#according to them, the country is doing "about the right amount" for the environment

#All these are examples of Likert items (used to measure respondents’ attitudes to a particular question or statement)
#Likert-type data is indeed ordinal data. The best way to display the distribution of responses (% that agree, disagree etc) is
#to use a bar chart.

X2[ , 8] = ifelse(X[ , 8] == "Male", 1, 0) #1 Male, 0 Female

sex= table(X[ , 8]) #817 females and 655 males: the sample is quite balanced respect to sex
#expressing this in percentage:
n= nrow(df_)
percent_sex=((sex)/n * 100) #55.5% are females, 44.5% are males


#Column AGE, for NL: Calculated as DATEYR ‘year of interview’ minus BIRTH ‘year of birth’
#for Netherlands the minimum age of the respondent is 17, the maximum age is 97 years
summary(X[,9]) #there are some values below the minimum range and others over the maximum 
age <- X[,9]
levels(age)[1] <- "15"
levels(age)[75] <- "89"
levels(age)[85] <- "99"
X2[,9] <- as.numeric(levels(age)[age])
X2 <- X2 %>% as_tibble() %>% dplyr::filter(AGE >= 17, AGE <= 97)
hist(as.numeric(levels(age)[age]),main="Age")
boxplot.stats(as.numeric(levels(age)[age]))# median value is 55 years old
boxplot(as.numeric(levels(age)[age]),main="Age") #the data don't appear dispersed around the median


#NL: How many years of education have you attended after primary school? (If part time, recalculate to full-time.)
#Repeated years are not included
#EDUCYRS in the Netherlands: primary school=4-12 (lasts 8 years),secondary school=12-16/17/18(VMBO is 4 years,MBO is
#until 4 years,VWO is 6 years) (or HAVO is 5 years-->HBO bachelor of applied sciences lasts 4 years
#-->master will be until 2 years),university= bachelor is 3 years, master is until 3 years,PHD is 4 years

X2[,10]=ifelse(X[,10]=="8 years, NZ: primary",8,
                       ifelse(X[,10]=="16 years, NZ: university less than 3 years",16,
                              ifelse(X[,10]=="18 years, NZ: university more than 4 years",18,
                                            ifelse(X[,10]=="3 years, NZ: few years primary",3,
                                                   ifelse(X[,10]=="11 years, NZ: secondary school less than 3 years",11,
                                                          ifelse(X[,10]=="13 years, NZ: secondary school more han 4 years",13,
                                                                 ifelse(X[,10]=="Still at school Still at college, university, in vocational training",X2[,9]-12,X[,10])))))))

X2 <- X2 %>%  as_tibble() %>% mutate(row_nr = 1:n()) #creating a new object with all the rows
#subtract 12 from the age, 12 years is the number of years for compulsory school in NL(for still at school/at college,university)
X2 <- X2 %>% dplyr::filter(EDUCYRS >5, EDUCYRS <= 17, EDUCYRS + 11 <= AGE) #filtering out observations according to the codebook
##the maximum numbers of years of education in NL should be 25 years. Since primary school lasts 8 years the maximum should be 17,not taking into account people
#whose education years are higher than their actual age
summary(X2[,10])
row_nrs <- X2$row_nr #these are the rows we want to keep, in this way the rows kept in X2 will match with the rows in Yo2 and Yb2


#change names of the variables also for X2
names(X2)[names(X2) == "v15"] <- "Concern about the enviroment"
names(X2)[names(X2) == "v29"] <- "Pay higher prices"
names(X2)[names(X2) == "v30"] <- "Pay higher taxes"
names(X2)[names(X2) == "v31"] <- "Accept cuts in your stardard of living"
names(X2)[names(X2) == "v32"] <- "Individual action for the environment "
names(X2)[names(X2) == "v38"] <- "Effect of enviromental problems on life "
names(X2)[names(X2) == "v48"] <- "Country for the environment"


#plots of predictor + response variable
#Mosaic plots assume that every level of one grouping variable can be combined with every level of another grouping variable.
#In a mosaic plot both the heights and the widths of individual shaded areas vary (differently from a stacked bar plot)
#For two variables, the width of the columns is proportional to the number of observations in each level of the variable plotted 
#on the horizontal axis. The vertical length of the bars is proportional to the number of observations in the second variable within
#each level of the first variable.
n=nrow(df_)
table(X$SEX,Yb$v61)/n * 100

table1 <- table(X$`Country for the environment`, Yo$`Reduce energy or fuel`)
table1 #even the people who think that the Netherlands is doing more than enough for the environment often/sometimes tend to 
#reduce the use of energy or fuel. who thinks that the country is doing the right amount (the majority) also often reduce the energy/
#fuel consumption
summary(table1)#1265 cases
plot(X$`Country for the environment`,Yo$`Reduce energy or fuel`,xlab="How much is Netherlands doing for the environment?",
     ylab="I reduce the use of energy or fuel",
     main="Reduction of energy use based on belief about the effort of the country for the environment",col=c("#99ff66","#77DD77","#669900","#556B2F"))

table2 <- table(X$`Individual action for the environment `, Yo$`Avoid buying certain products`)
table2
summary(table2) #1401 cases
plot(X$`Individual action for the environment `,Yo$`Avoid buying certain products`,xlab="I can't do much for the environment",
     ylab="I avoid buying certain product",
     main="Avoid buying certain products based on belief I can/can't do much for the environment",col=c("#99ff66","#77DD77","#669900","#556B2F"))
#the majority of people disagrees. people who disagree with this statement tend sometimes or often to avoid to buy certain product for the sake of the environment
# people who instead agree strongly (the minority) are by the way quite balanced in the frequency in which they avoid buying certain products
#(often/sometimes/never)

table3 <- table(X$`Individual action for the environment `,Yo$Recycling) 
table3
#the majority of people that disagree with the statement always recycles but also the majority of people who agrees with this affirmation
#tends to recycle (always or often)
summary(table3) #1379 people
plot(X$`Individual action for the environment `,Yo$Recycling,xlab="I can't do much for the environment",
     ylab="I sort glass, paper, plastic for recycling",
     main="Sorting materials for recycling based on belief I can/can't do much for the environment",col=c("#99ff66","#77DD77","#669900","#556B2F"))
#in general the proportion of people who sorts the rubbish is quite high, there's no real effect of this opinion on the action
#of sorting the rubbish for recycling


table4<-table(X$`Individual action for the environment `,Yb$v64)
table4 #neither the people who disagree (they believe they can do something for the environment) have in general taken part to a protest
#or a demonstration for environmental reasons (615 haven't vs 14 who have taken part)
summary(table4)#1349 cases
plot(X$`Individual action for the environment `,Yb$v64,xlab="It is too difficult for me to do much about the environment",
     ylab="I took part in a protest/demonstration about an environmental issue",
     main="Taking part in a protest/demonstration based on belief of being able to do something for the environment",col=c("#669900","#99ff66"))
#people who took part to an environmental protest/demonstration are in general really few 

table5 <- table(X$`Effect of enviromental problems on life `,Yb$v61)
table5 #the vast majority of people that don't believe that the environmental issues have effect on their life are not members
#of an environmental group
summary(table5)#1344 cases
plot(X$`Effect of enviromental problems on life `,Yb$v61,xlab="Environmental problems have a direct effect on my everyday life",
     ylab="I'm a member of an environmental group ",
     main="Member of an environmental group since environment issues have an effect on my everyday life",col=c("#669900","#99ff66"))
#the more the person agrees with the fact that environmental issues have a direct effect on his/her life the more likely is they are
#members of an environmental group

table6<-table(X$`Pay higher prices`,Yb$v63)
table6 #almost all the people who are very unwilling to pay higher prices have never give money to an environmental group
summary(table6)#1352 cases 
plot(X$`Pay higher prices`,Yb$v63,xlab="Willing to pay higher prices to protect the environment",
     ylab="I have given money to an environmental group",
     main="Paid an environmental group based on willing to pay more to protect the environment",col=c("#669900","#99ff66"))
#people who are willing to pay higher prices to protect the environment tend to give money to environmental groups

table7<-table(X$`Pay higher taxes`,Yb$v63)
table7 #the majority of people who is very unwilling to pay higher taxes to protect the environment hasn't given any money to an 
#environmental group
summary(table7)#1365 cases
plot(X$`Pay higher taxes`,Yb$v63,xlab="Willing to pay higher taxes to protect the environment",
     ylab="I have given money to an environmental group",
     main="Paid an environmental group based on willing to pay higher taxes to protect the environment",col=c("#669900","#99ff66"))
#people who are willing to pay higher taxes to protect the environment tend to give money to environmental group


table8<- table(X$`Country for the environment`,Yb$v61)
table8 #the majority of respondents thinks that their country is doing about the right amount 
#the majority of people who believes that their country is doing more than enough is not a member of an environmental group
#but also the majority of those who think that Netherlands is doing too little for the environment is not a member
plot(X$`Country for the environment`,Yb$v61,xlab="How much is Netherlands doing for the environment?",
     ylab="I'm a member of an environmental group",
     main="Environmental group membership based on belief about the effort of the country for the environment",col=c("#669900","#99ff66"))
#people who think that their country is doing too little for the environment tend to be members of an environmental group but anyway,
#there are very few members of environmental groups

table9<-table(X$`Concern about the enviroment`,Yo$`Save or re-use water`)  
table9
summary(table9)#1393
plot(X$`Concern about the enviroment`,Yo$`Save or re-use water`,xlab="Level of concern about the environmental issues",
     ylab="I save or re-use water",
     main="Saving or re-using water based on the level of concern about the environment",
     col=c("#99ff66","#98FF98","#77DD77","#669900","#556B2F"))
#the majority of people is concerned about the environment and the proportion of people who saves or re-use water often is higher among 
#those who are concerned or very concerned about the environment

table10<-table(X$`Concern about the enviroment`,Yb$v62)
table10 #the quantity of those who signed a petition is higher when they are more concerned about the environment. However not many 
#people signed a petition among the respondents 
summary(table10)#1356 cases
plot(X$`Concern about the enviroment`,Yb$v62,xlab="Level of concern about the environmental issues",
     ylab="I signed a petition about an environmental issue ",
     main="Signing a petition for the environemnet based on the level of concern about the environmental issues",
     col=c("#669900","#556B2F"))


#We use complete.cases to handle missing values. The complete.cases function detects rows in a data.frame that do not contain any missing value.

Yo2_filtered <- Yo2[row_nrs,]
Yb2_filtered <- Yb2[row_nrs,]
X2 <- X2[,1:10] %>% as.matrix() #to apply complete.cases we need that X2 is a matrix
idx = complete.cases(cbind(X2, Yo2_filtered, Yb2_filtered)) 


#optimal scaling levels for predictors
#N will be for example for Sex (ex: Female=1, Male=0), O is for all the ordinal response variables
# "v15"     "v29"     "v30"     "v31"     "v32"     "v38"     "v48"     "SEX"     "AGE"     "EDUCYRS"
# "O"       "O"       "O"       "O"       "O".      "O"       "O".      "N"       "N"       "N"

###iterations: first number is the negative log likelihood of the previous iteration, the second number the negative log likelihood 
#of the next iteration, the third number the difference between he two,this difference will be lower proceeding with the iterations,
#the algorithm stops when it is lower than dcrit(convergence criterion)
#THE DIFFERENCE DOES NOT HAVE TO BE NEGATIVE

#FITTING OF THE MODEL
out_1= gmr4(Yb=Yb2_filtered[idx, ],Yo=Yo2_filtered[idx, ], X = X2[idx, ], 
          Xscale=list("O","O", "O", "O","O", "O", "O", "N", "N", "N"), penalties = c(0,0,0),S=1)
#loss(deviance) is 4591.094
#AIC=deviance + 2*K
#K=(ncol(X)+ncol(Y)-S)*S + number of elements in m(intercept), ncol(X)-->Predictors, ncol(Y)-->Responses
#for ordinal responses=5*3=15 Yo is 5 columns with 4 categories so there are 3 tresholds
#for binary responses=4*1=4 yb is 4 columns
#15+4=19

#trying first with rank 1
k_1=((ncol(X2)+ncol(Yb2)+ncol(Yo2)-1)*1) + 19
aic_1= out_1[["loss"]]+2*k_1
aic_1 #4665.094
#BIC=deviance +log(nrow(X))*k
bic_1=out_1[["loss"]]+ log(nrow(X2))*k_1
bic_1 #4845.82

#rank 2
out_2= gmr4(Yb=Yb2_filtered[idx, ],Yo=Yo2_filtered[idx, ], X = X2[idx, ], 
            Xscale=list("O","O", "O", "O","O", "O", "O", "N", "N", "N"), penalties = c(0,0,0),S=2)
#loss is 4539.792
k_2=((ncol(X2)+ncol(Yb2)+ncol(Yo2)-2)*2) + 19
aic_2=out_2[["loss"]] + 2*k_2
aic_2 #4645.792
#BIC=deviance +log(nrow(X))*k
bic_2=out_2[["loss"]]+ log(nrow(X2))*k_2
bic_2 #4904.67


#then we try with different values of S to see what model performs better in terms of AIC, BIC
out_3= gmr4(Yb=Yb2_filtered[idx, ],Yo=Yo2_filtered[idx, ], X = X2[idx, ], 
          Xscale=list("O","O", "O", "O","O", "O", "O", "N", "N", "N"), penalties = c(0,0,0),S=3)
#loss is 4523.796
k_3=((ncol(X2)+ncol(Yb2)+ncol(Yo2)-3)*3) + 19
aic_3=out_3[["loss"]] + 2*k_3
aic_3 #4657.796
bic_3=out_3[["loss"]]+ log(nrow(X2))*k_3
bic_3 #4985.056

out_4= gmr4(Yb=Yb2_filtered[idx, ],Yo=Yo2_filtered[idx, ], X = X2[idx, ], 
            Xscale=list("O","O", "O", "O","O", "O", "O", "N", "N", "N"), penalties = c(0,0,0),S=4)
#loss is 4515.317
k_4=((ncol(X2)+ncol(Yb2)+ncol(Yo2)-4)*4) + 19
aic_4=out_4[["loss"]] + 2*k_4
aic_4 #4673.317
bic_4=out_4[["loss"]]+ log(nrow(X2))*k_4
bic_4 # 5059.152
#ADD BIPLOTS only if rank is 2, otherwise plots comparing the different dimensions (more difficult to read)

#the model with the lowest loss is the one with rank 4 (as the rank increases the loss decreases)
#the model with the lowest aic is the one with rank 2 (aic has a connection with cross-validation)
#bic increases as the the value of the rank increases so the lowest bic is for rank 1 (since bic penalizes more complex models)

#BV = B %*% t(V)
A=out_2[["B"]]%*%t(out_2[["V"]])
A #estimated regression coefficients (P x R matrix --> 10 predictors, 9 responses)

out_2[["Xoriginal"]]-out_2[["PHI"]]


#scatterplot of X and scaled predictor variables
#PHI[, p] = G[[p]] %*% quantifications[[p]]  #standardized quantifications
#in paper ROS the plots are obtained as G_k*v_k(indicator matrix * super vector of quantifications )
plot(out_2[["Xoriginal"]],out_2[["PHI"]],xlab="X",ylab="PHI(X)")


#plots of original categories of single X predictor and scaled predictor
#stair step plots
#for ordinal (unstandardized) quantifications monotone regression function is applied
plot(out_2[["originalcategories"]][[1]],out_2[["quantifications"]][[1]],xlim = c(1,5),type = "s",lty=1,
     main="Concern about the environment",xlab="original categories",ylab=" quantifications ordinal")
#The first transformation shows a monotonically increasing function, is quite regular,
#for example, combined with positive estimated regression coefficient with the first response variable 
#(member or not of environmental group - binary response) on average people about the environment are members
#positive coefficients for all responses
#for a unit change in predictor there is a 0.6 effect in log-odds of the probability to be a member of the
#environmental group

#(ordinal response)the cumulative probability of buying fruits and vegetables without pesticides with higher frequency
#increases by 0.434 as the concern about the environment increases 


#don't compare ordinal response variables with binary ones


plot(out_2[["originalcategories"]][[2]],out_2[["quantifications"]][[2]],xlim = c(0,5),type="s",lty=1,
     main="Pay higher prices",xlab="original categories",ylab="quantifications ordinal")
#on fifth response (recycling) we have a negative coefficient 
#category 0,1,2,3 have the same quantifications, also 4 and 5

plot(out_2[["originalcategories"]][[3]],out_2[["quantifications"]][[3]],xlim = c(0,5),type="s",lty=1,
     main="Pay higher taxes",xlab="original categories",ylab="quantifications ordinal")


plot(out_2[["originalcategories"]][[4]],out_2[["quantifications"]][[4]],xlim = c(0,5),type="s",lty=1,
     main="Accepts cuts in the living standard",xlab="original categories",ylab="quantifications ordinal")
plot(out_2[["originalcategories"]][[5]],out_2[["quantifications"]][[5]],xlim = c(1,5),type="s",lty=1,
     main="Individual action for the environment",xlab="original categories",ylab="quantifications ordinal")
plot(out_2[["originalcategories"]][[6]],out_2[["quantifications"]][[6]],xlim = c(0,5),type="s",lty=1,
     main="Effect of environmental problems on life",xlab="original categories",ylab="quantifications ordinal")
plot(out_2[["originalcategories"]][[7]],out_2[["quantifications"]][[7]],xlim = c(1,5),type="s",lty=1,
     main="Country for the environment",xlab="original categories",ylab="quantifications ordinal")
#the other 3 predictors are treated as numeric so there is no optimal scaling(SEX,AGE,EDUCYRS)


#difference in the log odds
out_2[["originalcategories"]][[1]]-out_2[["quantifications"]][[1]]


library(tidyverse)
library(ggplot2)
library(patchwork)
library(lattice)
data <- read.csv("student performance.csv")
view(data)
print(data)
head(data)
#handling the missing data and removing unnecessary columns
#Changing variables' names
colnames(data) <- c("gender", "ethnicity","parentsLevelOfEducation", "lunch", "testPreparation", "mathScore", "readingScore","writingScore")

#Checking for missing values
sum(is.na(data))
mean(is.na(data))

#mean of three score
ndata<-mutate(data,mean=(`mathScore`+`readingScore`+`writingScore`)/3) 
ndata
#describe of data
summary(data)

#split data in Train and test set
split = sample.split(data$mathScore, SplitRatio = 0.8)

#Creating the training set and test set separately
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)
training_set
test_set
str(training_set)
str(test_set)

#to find the multiple regression
lm1<- lm(mathScore~ gender+readingScore, data=train)
summary(lm1)

lm2<- lm(mathScore~ gender+testPreparation+readingScore, data=train)
summary(lm2)


#(1)analysis by score
#scatter plot
girl_data<-data%>%filter(gender=='female')
boy_data<-data%>%filter(gender=='male')
ggplot()+
  geom_point(girl_data,mapping=aes(`mathScore`,`readingScore`,color='female'))+
  geom_point(boy_data,mapping=aes(`mathScore`,`readingScore`,color='male'))#math and reading plot
ggplot()+
  geom_point(girl_data,mapping=aes(`mathScore`,`writingScore`,color='female'))+
  geom_point(boy_data,mapping = aes(`mathScore`,`writingScore`,color='male'))#math and writing plot
#inference that in the same reading score,male's math is generally higher than females,on the contrary,at the same math score,
#female's score is generally higher than males->so i predict that in the same language level(writing,reading),females are generally higher at language,and male is generally better at math

ggplot()+
  geom_point(girl_data,mapping=aes(`readingScore`,`writingScore`,color='female'),alpha=1/2)+
  geom_point(boy_data,mapping = aes(`readingScore`,`writingScore`,color='male'),alpha=1/2)
#observed the plot that x=reading,y=writing,know that the density of higher score is female

#(2)analysis by spread the group,observe the factor that affect the score
#boxplot
groupa<-ndata%>%filter(`ethnicity`=='group A')
groupb<-ndata%>%filter(`ethnicity`=='group B')
groupc<-ndata%>%filter(`ethnicity`=='group C')
groupd<-ndata%>%filter(`ethnicity`=='group D')
groupe<-ndata%>%filter(`ethnicity`=='group E')
ggplot(groupa)+geom_boxplot(mapping=aes(x=reorder(`parentsLevelOfEducation`,mean,median),mean))+
  ggtitle('group A')+xlab('parentsLevelOfEducation')
ggplot(groupb)+geom_boxplot(mapping=aes(x=reorder(`parentsLevelOfEducation`,mean,median),mean))+
  ggtitle('group B')+xlab('parentsLevelOfEducation')
ggplot(groupc)+geom_boxplot(mapping=aes(x=reorder(`parentsLevelOfEducation`,mean,median),mean))+
  ggtitle('group C')+xlab('parentsLevelOfEducation')
ggplot(groupd)+geom_boxplot(mapping=aes(x=reorder(`parentsLevelOfEducation`,mean,median),mean))+
  ggtitle('group D')+xlab('parentsLevelOfEducation')
ggplot(groupe)+geom_boxplot(mapping=aes(x=reorder(`parentsLevelOfEducation`,mean,median),mean))+
  ggtitle('group E')+xlab('parentsLevelOfEducation')#observe the education of parent and the mean of the score
#observe the education of parent and the mean of the score


ggplot()+geom_freqpoly(groupa,mapping=aes(mean,color='A'))+geom_freqpoly(groupb,mapping=aes(mean,color='B'))+
  geom_freqpoly(groupc,mapping=aes(mean,color='C'))+geom_freqpoly(groupd,mapping=aes(mean,color='D'))+
  geom_freqpoly(groupe,mapping=aes(mean,color='E'))
#In the plot A~D,it have three high school education at the lowest mean,and the highest two education is master's and bachelor's,and it looks like E is different from A~D,and it's higher than others

group_A<-groupa%>%rename(mean_A=mean)%>%select(mean_A)%>%arrange(desc(`mean_A`))%>%head(89)%>%round(digits = 1)
group_B<-groupb%>%rename(mean_B=mean)%>%select(mean_B)%>%arrange(desc(`mean_B`))%>%head(89)%>%round(digits = 1)
group_C<-groupc%>%rename(mean_C=mean)%>%select(mean_C)%>%arrange(desc(`mean_C`))%>%head(89)%>%round(digits = 1)
group_D<-groupd%>%rename(mean_D=mean)%>%select(mean_D)%>%arrange(desc(`mean_D`))%>%head(89)%>%round(digits = 1)
group_E<-groupe%>%rename(mean_E=mean)%>%select(mean_E)%>%arrange(desc(`mean_E`))%>%head(89)%>%round(digits = 1)
cbi<-cbind(group_A,group_B,group_C,group_D,group_E)
#observe the combine table
#comprehensive of the plot and table,we can know that C,D are relatively concentrated,A have big drop that the highest is 96.3 and the lowest is 23.3
#but the amount of row will affect the analysis,A have only 89 rows and E have only 140 rows->look at the summary of the table 
summary(cbi)
#we know that E has the higher score but the mean isn't,so the boxplot above can't put together for comparison because of the different rows of amount


#(3)observe test preparation course and mean
#bar graph and box plot
uncomplete_prep<-ndata%>%filter(`testPreparation`=='none')
complete_prep<-ndata%>%filter(`testPreparation`=='completed')
prep1<-ggplot()+geom_bar(uncomplete_prep,mapping = aes(x=mean,fill='uncompleted'))+
  geom_bar(complete_prep,mapping=aes(x=mean,fill='completed'))
prep2<-ggplot()+geom_boxplot(ndata,mapping=aes(`testPreparation`,mean))
prep1/prep2
#conclusion:it is obvious that completed the test preparation is generally higher than uncompleted

#observe the parental.level.of.education and the lunch
#bar graph
stan_data<-ndata%>%filter(lunch=='standard')%>%group_by(`parentsLevelOfEducation`)%>%
  select(`parentsLevelOfEducation`)%>%count()
free_data<-ndata%>%filter(lunch=='free/reduced')%>%group_by(`parentsLevelOfEducation`)%>%
  select(`parentsLevelOfEducation`)%>%count()
percent_data<-stan_data%>%inner_join(free_data,by='parentsLevelOfEducation')%>%rename('stan'=n.x,'free'=n.y)%>%
  mutate(percent=free/(stan+free)*100)
ggplot(ndata)+geom_bar(mapping=aes(`parentsLevelOfEducation`,fill=lunch),position = 'dodge')
ggplot(percent_data)+geom_bar(mapping=aes(`parentsLevelOfEducation`,y=percent,fill=percent),stat='identity')


#spread the group and observe the parental.level.of.education and lunch
pp1<-ggplot(groupa)+geom_bar(mapping = aes(`parentsLevelOfEducation`,fill=lunch),position='dodge')+ggtitle('GROUP A')
pp2<-ggplot(groupb)+geom_bar(mapping = aes(`parentsLevelOfEducation`,fill=lunch),position='dodge')+ggtitle('GROUP B')
pp3<-ggplot(groupc)+geom_bar(mapping = aes(`parentsLevelOfEducation`,fill=lunch),position='dodge')+ggtitle('GROUP C')
pp4<-ggplot(groupd)+geom_bar(mapping = aes(`parentsLevelOfEducation`,fill=lunch),position='dodge')+ggtitle('GROUP D')
pp5<-ggplot(groupe)+geom_bar(mapping = aes(`parentsLevelOfEducation`,fill=lunch),position='dodge')+ggtitle('GROUP E')
pp1+pp2/pp3+pp4+pp5



#Loading Libraries

library(ggplot2)
library(dslabs)
library(dplyr)
library(tidyverse)
library(GGally)
library(MASS)
#Importing data and renaming the columns
naval_data=read.csv("naval_vessel.csv",header=FALSE)

colnames(naval_data)=c(1:16,"Compressor_decay_state_coefficient","Turbine_decay_state_coefficent")
head(naval_data)

# 1. On naval dataset plot a Kernel density of Compressor Decay State Coefficient, and then answer the following question
#What kind of distribution does Compressor Decay State Coefficient have?

naval_data%>%ggplot(aes(Compressor_decay_state_coefficient))+geom_density()+labs(title="Density plot") #Plot-1,"Density Plot"

# Answer: The density plot is close to a continous uniform distribution.

#2 Draw a scatterplot matrix on the naval dataset and watching the diagram name any two features which are linearly related to most of the other features. 

  
pairs(naval_data[,1:18]) #Plot-2 "Scatterplot Matrix"

#3 Apply Min-Max scaling on the naval dataset.

scaled_naval_data<-naval_data


for(i in  1:ncol(scaled_naval_data)){ 
  scaled_naval_data[,i]=(scaled_naval_data[,i]-min(scaled_naval_data[,i]))/(max(scaled_naval_data[,i])-min(scaled_naval_data[,i]))
  }

head(scaled_naval_data)

#4 From the naval dataset find out mean and median of both the outputs

naval_data%>%summarise(mean_Compressor_decay_state_coefficient=mean(Compressor_decay_state_coefficient),
                       median_Compressor_decay_state_coefficient=median(Compressor_decay_state_coefficient),
                       mean_Turbine_decay_state_coefficent=mean(Turbine_decay_state_coefficent),
                       median_Turbine_decay_state_coefficent=median(Turbine_decay_state_coefficent))

#5 From R- MASS library use the Boston dataset and then answer the following questions. Who has the lowest salary?.Who does work in HR? 

boston=Boston
head(boston)


#6 Load the Titanic dataset (inbuilt to R), then find out how many male child survived?

titanic=data.frame(Titanic)
head(titanic)
titanic%>%filter(Sex=="Male" & Age=="Child" & Survived=="Yes")%>%summarise(male_child_survived=sum(Freq))

##How many first class passengers were there? 

titanic%>%filter(Class=="1st")%>%summarise(total_1st_class=sum(Freq))

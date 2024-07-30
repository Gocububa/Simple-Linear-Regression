datam<-read.csv('C:/Users/ASUS/Documents/IceCream.csv',header=TRUE, sep=',')



# Load necessary libraries
library(DT)
library(corrplot)
library(ggplot2)

# Load the dataset
datam <- read.csv('C:/Users/ASUS/Documents/IceCream.csv', header = TRUE, sep = ',')

# Display the first 10 rows of the dataset in a readable format
datatable(head(datam, 10), options = list(pageLength = 10), caption = "First 10 Rows of IceCream Dataset")



x<-datam$Temperature
y<-datam$Ice.Cream.Profits

my_reg<-lm(y~x,datam)

#linearity
ggplot(datam,aes(x=x,y=y))+
  geom_point()+
  geom_smooth(method="lm", col="red") 

#independence of errors
#normality of errors
#equal variances
par(mfrow=c(2,2))
plot(my_reg)


# Select numeric columns only
numeric_columns <- datam[sapply(datam, is.numeric)]

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_columns, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Visualize the correlation matrix using corrplot
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")



#for r-squared and regression equation
summary(my_reg)

#for anova and hypothesis testing
anova(my_reg)






#for another way to illustrate the linearity in our simple linear regression project
library(tidyverse)
data %>% ggplot(aes(x,y)) +
  geom_point() +
  geom_smooth(method = "lm")


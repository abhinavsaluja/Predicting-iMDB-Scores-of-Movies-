
setwd("C:/Users/Abhinav/Desktop/R Project")

imdb<-read.csv("movie_metadata.csv")


#selecting only USA

imdb.usa<-imdb[which(imdb[,'country']=='USA'),]




#Putting imdb.usa in a data frame

imdb.df=data.frame(imdb.usa)

#Removing irrelevant column

imdb1<-imdb.df[, -which(names(imdb.df)=='movie_imdb_link')]

str(imdb1)

library(amelia)

#Number of missing values with respect to column

sapply(imdb1,function(x) sum(is.na(x))) 

#Omitting missing values

movie<- na.omit(imdb1)


#Double check for missing values

sapply(movie,function(x) sum(is.na(x))) 

#defining model1 with relevant independent and dependent variables

movie1<-movie[,c('imdb_score','num_voted_users','num_critic_for_reviews',
                 'num_user_for_reviews','duration','facenumber_in_poster',
                 'gross','movie_facebook_likes','director_facebook_likes',
                 'cast_total_facebook_likes','budget','title_year','genres')]

#Running multiple linear regression

model1<-lm(formula = movie1$imdb_score~movie1$num_voted_users+movie1$num_critic_for_reviews
           +movie1$num_user_for_reviews+movie1$duration+movie1$facenumber_in_poster
           +movie1$gross+movie1$movie_facebook_likes+movie1$director_facebook_likes
           +movie1$cast_total_facebook_likes+movie1$budget+movie1$title_year+factor(movie1$genres))

#reviewing coefficients and variables significant on the basis of p values

summary(model1)


x<-c(1:3005)



train_df <- sample(x, 0.7*nrow(x),  replace=FALSE)
train_df

# Converting the data set into training and validatioN sets

sample <- sample.int(n = nrow(x), size = floor(.70*nrow(x)), replace = F)
train <- x[sample,]
valid <- x[-sample,]
View(valid)

#Performing predictive analysis on the validation data set

pr<-predict.lm(model1,newdata = data.frame(valid),interval = 'confidence')

pred_t <- predict(model1, na.action=na.pass,interval='confidence')
pred_t
View(pred_t)

pred_t<-pred_t[,1]

movie1$fitted <- pred_t

movie1

library(caret)
library(lattice)

library(ggplot2)

table(pred_t,train$imdb_score)

nrow(pred_t)
pred_t
nrow(valid)


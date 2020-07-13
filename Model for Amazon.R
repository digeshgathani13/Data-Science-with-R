##set project directory##
setwd("/home/digesh_gathani_gmail/Project-1")
getwd()


MoviesRating=read.csv("Amazon - Movies and TV Ratings.csv", header=TRUE)
View(MoviesRating)
str(MoviesRating)
head(MoviesRating)
tail(MoviesRating)
names(MoviesRating)
nrow(MoviesRating)
summary(MoviesRating)

#apply NA to 0
MoviesRating[is.na(MoviesRating)]=0


#1 . find max and min rating column wise
max_df=apply(MoviesRating,2,max)
table(max_df)
min_df=apply(MoviesRating,2,min)
table(min_df)

#2. What is the average rating for each movie?
MoviesRatingWithoutUserID = subset(MoviesRating, select = -c(user_id))
names(MoviesRatingWithoutUserID)
avg_df = colMeans(MoviesRatingWithoutUserID, na.rm = TRUE)
avg_df[order(avg_df, decreasing = TRUE)[1:5]]
avg_df

#3 Define the top 5 movies with the maximum ratings.
order(colSums(MoviesRatingWithoutUserID != 0),decreasing = TRUE)[1:5]

#4 Define the top 5 movies with the least audience.
MoviesRatingWithoutUserID$audience = rowSums(MoviesRatingWithoutUserID != 0)
order(colSums(MoviesRatingWithoutUserID != 0),decreasing = FALSE)[1:5]

#5 Divide the data into training and test data
MoviesRatingWithoutUserID$audience = as.factor(MoviesRatingWithoutUserID$audience)
split = sample.split(MoviesRatingWithoutUserID, SplitRatio=.7)
train=subset(MoviesRatingWithoutUserID, split==T)
val=subset(MoviesRatingWithoutUserID, split==F)

nrow(MoviesRatingWithoutUserID)
nrow(train)
nrow(val)

# Build a recommendation model on training data
model=rpart(audience ~.,data=train)
rpart.plot(model)

val$pred_audience = predict(model, val, type='vector')
table(val$audience, val$pred_audience)


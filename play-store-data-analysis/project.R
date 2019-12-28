install.packages("UsingR")
install.packages("kohonen")
install.packages("dplyr")
install.packages("e1071")
install.packages("ggplot2")
library(kohonen)
library(dplyr)
library(ggplot2)
library(e1071)


# For categorical-numeric use boxplot, table, chisq
# For categorical-categorical comparison use t-test, boxplot, anova
# For numerical-numerical use dotplot, cov, scatter.smooth (linear regression)


# Display the numbers in non-scientific notation (without e10 for example)
options(scipen = 100)

# Choose the csv file with data
#fileName <- file.choose()
#store = read.csv(fileName)

# File on the local system
fileName <- "/home/luchev/Github/uni-statistics/play-store-data-analysis/googleplaystore.csv"
store = read.csv(fileName)

# Store is dataframe with the following columns
# App Category Rating Reviews Size Installs Type Price Content.Rating Genres Last.Updated Current.Ver Android.Ver

# Convert the data in store to make it usable
# Convert 1,000+ to 1000+ in Installs
store$Installs = gsub(',', '', store$Installs, fixed = TRUE)
# Convert 1000+ to 1000 in Installs
store$Installs = gsub('+', '', store$Installs, fixed = TRUE)

# Convert Installs, Reviews, Rating to numeric values
store$Installs = as.numeric(store$Installs)
store$Reviews = as.numeric(store$Reviews)
store$Rating = as.numeric(store$Rating)

# Remove invalid entries (where the conversion to numeric failed)
store = subset(store, ! is.na(Installs))
store = subset(store, Type != 'NaN')

store = subset(store, ! is.na(Reviews))
store = subset(store, Type != 'NaN')

store = subset(store, ! is.na(Rating))
store = subset(store, Type != 'NaN')

# Convert to string because it bugs otherwise
store$Type = as.character(store$Type)

# Generate list of categories
categories = c()
for (i in unique(store$Category)) {
  categories = c(categories, i) 
}

# Generate list of genres
genres = c()
for (l in unique(store$Genres))
{
  for (g in strsplit(l, split = ';')) {
    genres = c(genres, g)
  }
}
genres = unique(genres)

# Number of installs of all
par(mar=c(5,5,5,0))
barplot(prop.table(table(store$Installs)), xlab="Installs", ylab="Percentage of apps")
# There are very few applications with 1B+ installs so we probably
# want to try and develop an application targeting 1M to 100M users

# Summary to show the mean and the quantiles
summary(store$Installs)

# Paid/Free and number of Installs
par(mar=c(5,5,5,0))
barplot(prop.table(table(store$Type, store$Installs), 2), xlab="Installs", ylab="Percentage paid apps (grey = paid)")
# There are no paid applications with more than 1M users, so if we want many users
# we should develop an application that is Free

# Average installs per category
category_average_installs = data.frame()
for (category in categories) {
  average = mean(store$Installs[grep(category, store$Category)])
  nextRow = data.frame(average, row.names = category)
  category_average_installs = rbind(category_average_installs, nextRow)
}

# Total installs per category
category_total_installs = data.frame()
for (category in categories) {
  total = sum(store$Installs[grep(category, store$Category)])
  nextRow = data.frame(total, row.names = category)
  category_total_installs = rbind(category_total_installs, nextRow)
}

# Total installs by category chart
par(mar=c(12,8,5,0))
barplot(category_total_installs[,1], main = "Total Installs by category", names=row.names(category_total_installs), col=rainbow(length(row.names(category_total_installs))), las=2)
# The most installed apps are either for Communication or Games, so if we want to
# have many users we need to target one of these categories

# Average installs by category chart
par(mar=c(12,8,5,0))
barplot(category_average_installs[,1], main = "Average Installs by category", names=row.names(category_average_installs), col=rainbow(length(row.names(category_average_installs))), las=2)
# On average, however Communication apps have way more installs than Games
# That means that if we make a game our chance of success is less

# Rating per category
gameRating <- subset(store, Category == "GAME")$Rating
mapRating <- subset(store, Category == "MAPS_AND_NAVIGATION")$Rating
shoppingRating <- subset(store, Category == "SHOPPING")$Rating
educationRating <- subset(store, Category == "EDUCATION")$Rating
entertainRating <- subset(store, Category == "ENTERTAINMENT")$Rating
medicalRating <- subset(store, Category == "MEDICAL")$Rating

par(mar=c(5,5,5,2))
hist(gameRating, main = "Category Gaming", xlab = "Rating", ylab = "Reviews")
hist(mapRating, main = "Category Maps and Navigation", xlab = "Rating", ylab = "Reviews")
hist(shoppingRating, main = "Category Shopping", xlab = "Rating", ylab = "Reviews")
hist(educationRating, main = "Category Education", xlab = "Rating", ylab = "Reviews")
hist(entertainRating, main = "Category Entertainment", xlab = "Rating", ylab = "Reviews")
hist(medicalRating, main = "Category Medical", xlab = "Rating", ylab = "Reviews")
# Because there are not many 5-star apps in these categories it's a good idea to try
# to develop an app in one of these categories - Game/Maps/Shopping,

# Rating and Installs
qplot(store$Installs, store$Rating)
# Apps with many users have mainly 4+ rating, OBVIOUSLY

# Number of Reviews and Installs
qplot(store$Installs, store$Reviews)
# There's apps with many installs but very few reviews
# This means people are not so satisfied they would voluntarily go and
# rate the app they use

# Number of Reviews and Rating
qplot(store$Reviews, store$Rating, xlab = "Number of Reviews", ylab = "Rating")
# Most reviews are mainly for apps with 3.5+ rating
# This means that people are more likely to give a good review
# rather than negative feedback what they didn't like

# The same thing can be ovserved in the following density plot
par(mar=c(5,5,5,2))
plot(density(store$Rating), main = "Rating density")
polygon(density(store$Rating), col="green")


# Why rating is so important
qqplot(store$Rating, store$Installs, xlab = "Rating", ylab = "Installs")













# TODO self organized map

# Category Rating Reviews Installs

#data_train = store[, c("Rating", "Reviews", "Installs")]
#colnames(data_train) = c("Rating", "Reviews", "Installs")
#head(data_train)
#categories


#data_train <- store[, c("Category", "Rating", "Reviews", "Installs")]
#data_train = subset(data_train, Rating > 4.5)
#head(data_train)

#data_train_matrix <- as.matrix(scale(data_train))

#som_grid <- somgrid(xdim=15, ydim=15, topo="hexagonal")

#som_model <- som(data_train_matrix, grid=som_grid, rlen=500, alpha=c(0.05,0.01), keep.data=TRUE)

# plot(som_model, type="changes")
#plot(som_model, type="count", main="Node Counts")
#plot(som_model, type="dist.neighbours", main = "SOM neighbour distances")
#plot(som_model, type="codes")

#som_cluster <- cutree(hclust(dist(unlist(som_model$codes))), 6)
# plot these results:
#plot(som_model, type="mapping", main = "Clusters")
#add.cluster.boundaries(som_model, som_cluster)

# multidimetional scanning
# self organized map
# ANOVA за параметрична AI
# Kruskal за непараметрична AI

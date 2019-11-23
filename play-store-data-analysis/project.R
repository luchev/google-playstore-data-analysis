#install.packages("UsingR")

options(scipen = 100)

# App Category Rating Reviews Size Installs Type Price Content.Rating Genres Last.Updated Current.Ver Android.Ver
store = read.csv("googleplaystore.csv")
# Convert 1,000+ to 1000+
store$Installs = gsub(',', '', store$Installs, fixed = TRUE)
# Convert 1000+ to 1000
store$Installs = gsub('+', '', store$Installs, fixed = TRUE)
store$Installs = as.numeric(store$Installs)

# remove invalid entries
store = subset(store, ! is.na(Installs))
store = subset(store, Type != 'NaN')

store$Type = as.character(store$Type)

# head(store)

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
barplot(prop.table(table(store$Installs)))

# Paid/Free and number of Installs
barplot(prop.table(table(store$Type, store$Installs), 2))

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
barplot(category_total_installs[,1], names=row.names(category_total_installs), col=rainbow(length(row.names(category_total_installs))), las=2)

# Average installs by category chart
par(mar=c(12,8,5,0))
barplot(category_average_installs[,1], names=row.names(category_average_installs), col=rainbow(length(row.names(category_average_installs))), las=2)








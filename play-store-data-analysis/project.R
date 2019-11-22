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



prop.table(table(store$Type, store$Installs))
table(store$Type, store$Installs)
barplot(table(store$Type, store$Installs))
barplot(prop.table(table(store$Type, store$Installs)))
barplot(prop.table(table(store$Type, store$Installs), 2))

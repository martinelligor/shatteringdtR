require(rpart)

wine.fl <- "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
# reading wine data
wine <- read.csv(wine.fl,header = F)

# names of the variables
wine.names=c("Alcohol", "Malic acid", "Ash", "Alcalinity of ash", "Magnesium",
             "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins",
             "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")
colnames(wine)[2:14]=wine.names
colnames(wine)[1]="Class"

wine$Class <- as.factor(wine$Class)

# creating tree for wine dataset
tree <- rpart(Class ~ ., data=wine, method="class")

shattering <- compute_shattering(tree, 1000)
chernoff_bound(tree)

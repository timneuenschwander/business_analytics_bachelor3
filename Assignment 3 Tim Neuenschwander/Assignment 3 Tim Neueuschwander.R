#Tim Neuenschwander
#18-615-955
#Assignment 3 Business analytics and Data Science

#1.1
taxidata =read.csv("rides_assignment.csv")
str(taxidata)
View(taxidata)
total_amount2 = taxidata$total_amount
View(total_amount2)

#1.2 Hierarchical clustering

# "center" subtrahiert den Mittelwert einer Variable von jedem einzelnen Wert.
# "scale" dividiert jeden Wert durch die Standardabweichung der Variable.
# "zv" entfernt Variablen ohne Varianz (solche bei denen alle Observationen gleich sind)
# "BoxCox" ist ein Weg nicht-normalverteilte Variablen so zu transformieren, dass sie 
# normalverteilt sind.
library(caret)
preProcess(taxidata, method = c("center", "scale","zv","BoxCox"),)
summary(taxidata)
distances = dist(taxidata[1:18], method = "euclidean")
cluster = hclust(distances, method = "ward.D2") 
plot(cluster) 

# When looking at the cluster Dendogram it gets clear that over the hight of 50 there is a pretty clear separation in clusters possible!

threeclusters = cutree(cluster, k = 3) 
threeclusters 
fiveclusters = cutree(cluster, k = 5)
fiveclusters
eightclusters = cutree(cluster, k = 8)
eightclusters

tapply(taxidata$total_amount, threeclusters, mean)
tapply(taxidata$total_amount, fiveclusters, mean)
tapply(taxidata$total_amount, eightclusters, mean)

tapply(taxidata$trip_distance_miles, threeclusters, mean)
tapply(taxidata$trip_distance_miles, fiveclusters, mean)
tapply(taxidata$trip_distance_miles, eightclusters, mean)

#1.3 Kmeans Clustering

threekmeans = kmeans(taxidata[1:18],centers=3,iter.max=10000)
fivekmeans = kmeans(taxidata[1:18],centers=5,iter.max=10000)
eightkmeans = kmeans(taxidata[1:18],centers=8,iter.max=10000)

View(threekmeans)
str(fivekmeans)
str(eightkmeans)

threekmeans$tot.withinss
fivekmeans$tot.withinss
eightkmeans$tot.withinss

#Assess Quality of Clustering using Davies-Bouldin

library(clusterSim)

index.DB(taxidata[1:18],threekmeans$cluster)$DB
index.DB(taxidata[1:18],fivekmeans$cluster)$DB
index.DB(taxidata[1:18],eightkmeans$cluster)$DB #Best solution!!! Lowest Davies Boulden result!
index.DB(taxidata[1:18],threeclusters)$DB#Best solution when concidering explenation!
index.DB(taxidata[1:18],fiveclusters)$DB
index.DB(taxidata[1:18],eightclusters)$DB

#1.4 Interpretation of Clusters:

"Der Einfachheit halber habe ich die hierarchische Clustering-Methode mit 3 Clustern gewählt, die nach der eightkmeans Clustering-Methode die zweit beste Methode ist. 
Die geringere Anzahl von Clustern sollte es einfacher machen, Variabeln in diese Cluster zu differenzieren."

tapply(taxidata$total_amount, threeclusters, mean) #very good                  !
tapply(taxidata$trip_distance_miles, threeclusters, mean) #very good           !
tapply(taxidata$passenger_count, threeclusters, mean) #bad
tapply(taxidata$pickup_longitude, threeclusters, mean) #bad
tapply(taxidata$pickup_latitude, threeclusters, mean) #bad
tapply(taxidata$dropoff_longitude, threeclusters, mean) #bad
tapply(taxidata$dropoff_latitude, threeclusters, mean) #bad
tapply(taxidata$pickup_hour, threeclusters, mean) #bad
tapply(taxidata$pickup_timeOfDay, threeclusters, mean) #bad
tapply(taxidata$trip_duration, threeclusters, mean) #very good                 !
tapply(taxidata$speed, threeclusters, mean) #very good                         !
tapply(taxidata$temp, threeclusters, mean) #medium - good                 !
tapply(taxidata$fog, threeclusters, mean) #medium - good
tapply(taxidata$rain, threeclusters, mean) #medium - good (reversed)
tapply(taxidata$snow, threeclusters, mean) #bad
tapply(taxidata$conds, threeclusters, mean) #good                         !
tapply(taxidata$vis, threeclusters, mean) #good
tapply(taxidata$holiday, threeclusters, mean) #very good                       !

"Nach der Analyse der 3 verschiedenen Cluster kamen nun einige sehr spezifische Merkmale jedes Clusters zum Vorschein. 
Natürlich gehören die Werte, die bestimmte Merkmale räpresentieren, zu einer Variablen, die gut differenzierbar ist.

Folgende Erkentnisse wurden gemacht:
Das erste cluster beinhalted kurze und billige fahrten und das dritte cluster lange und teure.

Die Länge der Fahrt, die Fahrt Distanz,der Preis und die Schnelligkeit sind essentielle Variablen. Diese Variabeln besitzen das höchste Differenzirungspotenzial.
Die geographischen angaben, sowie die meteologischen Angaben sind weniger ausschlaggebend für das Clustering."

#2.1
is.data.frame(taxidata)
taxidata$total_amount = total_amount2
taxidata$cluster = eightkmeans$cluster

trainIndex <- createDataPartition(taxidata$total_amount, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)
taxiTrain <- taxidata[ trainIndex,]
taxiTest  <- taxidata[-trainIndex,]
View(taxiTrain)
View(taxiTest)

#2.2
library(rpart)
library(rattle)
library(rpart.plot)
library(randomForest)

treemodel <- train(total_amount~.,data = taxiTrain,method="rpart", trControl=trainControl("cv",number = 5),tuneLength=3)

"Eine ganze Zahl, die die Menge der Granularität im Tuning-Parameter-Gitter angibt. 
Standardmäßig ist dieses Argument die Anzahl der Ebenen für jeden Tuning-Parameter, die 
sollte mit dem Zug erzeugt werden. Wenn trainControl die Option search = &quot;random; hat, 
Dies ist die maximale Anzahl von Tuning-Parameter-Kombinationen, die generiert werden 
durch die zufällige Suche. ( HINWEIS: Wenn angegeben, muss dieses Argument benannt werden. )
Source:https://stackoverflow.com/questions/38859705/r-understanding-caret-traintunelength-and-svm-methods-from-kernlab"

plot(treemodel)

#2.3
predict.tree <- treemodel %>% predict(taxiTest)
head(predict.tree)

postResample(pred = predict.tree,obs = taxiTest$total_amount)

#Output:
#     RMSE  Rsquared       MAE 
#6.4534700 0.7397027 3.7498740 

#2.4

"Als Vergleichsmodel nehme ich die Taxidaten und benutze die Elastic Net Regression."

taxidata2 <- taxidata
taxidata2$total_amount <- taxidata$total_amount
taxidata2$cluster = eightkmeans$cluster

trainIndex2 <- createDataPartition(taxidata2$total_amount, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex2)
taxiTrain2 <- taxidata2[ trainIndex2,]
taxiTest2  <- taxidata2[-trainIndex2,]
View(taxiTrain2)
View(taxiTest2)

treemodel2 <- train(total_amount~.,data = taxiTrain2,method="enet", trControl=trainControl("cv",number = 5))

plot(treemodel2)

predict.tree2 <- treemodel2 %>% predict(taxiTest2)
head(predict.tree2)

postResample(pred = predict.tree2,obs = taxiTest2$total_amount)

#Output:
#     RMSE  Rsquared       MAE 
#3.5825505 0.9181416 1.7046681

"Das letztere Model weist eine sehr hohe Fhigkeit, die Varianz der Input variablen zu erklären, auf."

#2.5
install.packages("caretEnsemble")
library(caretEnsemble)

my_control <- trainControl(method="boot", savePredictions="final", classProbs=TRUE, index=createResample(taxiTrain2$total_amount, 25),summaryFunction=twoClassSummary,"cv")
modelense <- caretList(total_amount~., data=taxiTrain2, methodList = c("enet","rpart"), trControl = my_control)

#Small problem!!!

stackedensemble <- caretEnsemble(modelense,metric="RMSE")
summary(stackedensemble)

predict.ensemble <- stackedensemble %>% predict(taxiTest2)
postResample(pred = predict.ensemble, obs = taxiTest2$total_amount)

#RMSE  Rsquared       MAE 
#3.5922003 0.9116329 1.6013576 







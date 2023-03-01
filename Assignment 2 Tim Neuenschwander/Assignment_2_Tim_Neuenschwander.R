#Tim Neuenschwander
#18-615-955
#Assignment 2 Business analytics and Data Science

install.packages("rmarkdown")
install.packages("data.table")

library(rmarkdown)
library(knitr)
library(data.table)

#1.1

reddit <- read.csv("reddit.csv",stringsAsFactors = FALSE)

str(reddit)
reddit=setDT(reddit)
is.data.table(reddit)

View(reddit)

dowjones <- read.delim("dowjones.csv", header = TRUE, sep = ";")
str(dowjones)
dowjones=setDT(dowjones)
is.data.table(dowjones)

View(dowjones)

#1.2

reddit$Date = as.Date(reddit$Date, format = "%d.%m.%Y")
dowjones$Date = as.Date(dowjones$Date, format = "%d.%m.%Y")

#1.3

dowjones$UpDown = ifelse(dowjones$Close > dowjones$Open,"Up","Down")

#2.1

library(qdap)

reddit$Content = gsub("[^A-Za-z]", " ", reddit$Content)

sentiment = polarity(reddit$Content, reddit$Date)

View(sentiment$all)
View(sentiment$group) #per day --> average polarity score is calculated!

#2.2

sentimentdata <- sentiment$all
sentimentdata = setDT(sentimentdata)
is.data.table(sentimentdata)
sentimentdata$Date = as.Date(sentimentdata$Date, format = "%d.%m.%Y")

sentimentdata= sentimentdata[,.((WC = sum(wc)), (POLARITY = sum(polarity))), by=,(DATE = Date)]

colnames(sentimentdata) <- c("Date","wc Sum","polarity Sum")

View(sentimentdata)

#2.3

sentimentdata[order(Date)]
dowjones[order(Date)]
View(dowjones)

DT = merge(sentimentdata, dowjones, all=TRUE)
DT[order(Date)]
View(DT)

DT1 <- na.omit(DT)
View(DT1)

#2.4

Trainingdata = DT1[Date <= as.Date("0016-03-11")]
Testdata = DT1[Date > as.Date("0016-03-11")]

data.frame(Trainingdata)
Trainingdata = setDT(Trainingdata)
Trainingdata$UpDown = as.factor(Trainingdata$UpDown)

str(Trainingdata)
View(Trainingdata)

data.frame(Testdata)
Testdata = setDT(Testdata)
Testdata$UpDown = as.factor(Testdata$UpDown)
View(Testdata)

#2.5

#If UpDown not a factor:
#Trainingdata$UpDown = ifelse(Trainingdata$UpDown == "Up",1,0)
up = glm(Trainingdata$UpDown ~ Trainingdata$`wc Sum` + Trainingdata$`polarity Sum`, data=Trainingdata, family=binomial(link = "logit"))
summary(up)

up$coefficients
predict.train = predict(up, type="response")
table(predict.train > 0.5, Trainingdata$UpDown)
#TN|FN
#FP|TP
#accuracy TP + TN / TP + Tn + FP + FN
#sensitivity TP / TP + FN
#specificity TN / TN + FP
#precision = TP / TP + FP

(102+58)/(102+58+90+50) #accuracy 0.5333333
102/(102+50) # sensitivity / recall 0.6710526
58/(58+90) #specificity 0.3918919
102/(102+90) #precision 0.53125

#If UpDown not a factor:
#Testdata$UpDown = ifelse(Testdata$UpDown == "Up",1,0)
up2 = glm(Testdata$UpDown ~ Testdata$`wc Sum` + Testdata$`polarity Sum`, data=Testdata, family=binomial(link = "logit"))
summary(up)

predict.test = predict(up2, Testdata, type="response")
table(predict.test > 0.5, Testdata$UpDown)

(12+40)/(12+40+5+19) #accuracy 0.6842105
12/(12+19) # sensitivity / 0.3870968
40/(40+19) #specificity 0.6779661
12/(12+19) #precision 0.3870968

#2.6

data.frame(Trainingdata)
is.data.frame(Trainingdata)

library(ggplot2)

plot <- ggplot(Trainingdata, aes(x=`polarity Sum`, y=UpDown)) + 
  geom_point(color="blue3", alpha = 0.4) + 
  labs(x="Sediment value", y="1=Up, 0=Down", title="Changes in stockprices:") 
plot

data.frame(Testdata)
is.data.frame(Testdata)

library(ggplot2)

plot <- ggplot(Testdata, aes(x=`polarity Sum`, y=UpDown)) + 
  geom_point(color="blue3", alpha = 0.4) + 
  labs(x="Sediment value", y="1=Up, 0=Down", title="Changes in stockprices:") 
plot
#3.1

reddit <- read.csv("reddit.csv",stringsAsFactors = FALSE)
str(reddit)
is.data.frame(reddit)
# Zwischenschritt: reddit1 = reddit[1:50,]

is.data.table(reddit)
setDT(reddit)
reddit = reddit[,.(Content = paste(Content, collapse=" ")), by=,(Date = Date)]
reddit=setDT(reddit)

reddit$Date = as.Date(reddit$Date, format = "%d.%m.%Y")
View(reddit)

#3.2

reddit$Content = gsub("\\n", " ", reddit$Content)
reddit$Content = gsub("[^A-Za-z]", " ", reddit$Content)
View(reddit)

library(tm)
library(NLP)
library(SnowballC)

data_for_corpus = VectorSource(reddit$Content)
corpus = Corpus(data_for_corpus)
inspect(corpus)

corpus = tm_map(corpus, content_transformer(tolower))

corpus = tm_map(corpus, removeWords, stopwords("english"))

corpus = tm_map(corpus, stemDocument, language="english")

corpus = tm_map(corpus, stripWhitespace)
inspect(corpus[1:5])


dtm = DocumentTermMatrix(corpus, control=list(weighting = weightTf))
View(dtm)
dtm = removeSparseTerms(dtm, 0.99)
inspect(dtm)

dtm = as.matrix(dtm)
dtm = as.data.frame(dtm)

#3.3

dtm$UpDown = DT$UpDown
china = dtm$UpDown[dtm$china>0]
attack = dtm$UpDown[dtm$attack>0]
table(china)
table(attack)

dtm$Date = DT$Date

library(dplyr)
dtm <- dtm %>% select(Date, everything())
dtm <- dtm %>% select(UpDown, everything())
View(dtm)
table(dtm$UpDown)
dtm <- na.omit(dtm)


#3.4

library(naivebayes)
dtm = setDT(dtm)

Trainingcontent = dtm[Date <= as.Date("0016-03-11")]
is.data.frame(Trainingcontent)
Trainingcontent$Date <- NULL
View(Trainingcontent)

Testcontent = dtm[Date > as.Date("0016-03-11")]
is.data.frame(Testcontent)
Testcontent$Date <- NULL
View(Testcontent)

library(naivebayes)
detector = naive_bayes(UpDown~., data = Trainingcontent, laplace = 0)
prediction = predict(detector, Testcontent)

table(prediction, Testcontent$UpDown)

#TN|FN
#FP|TP
#accuracy TP + TN / TP + Tn + FP + FN
#sensitivity TP / TP + FN
#specificity TN / TN + FP

(28+12)/(28+12+19+17) #accuracy 0.5263158
(28)/(28+17) #sensitivity 0.6222222
(12)/(12+19) #specificity 0.3870968

#3.5

#Wie würde das Modell auf neue Inahlte in den Testdaten wie z.B. „Corona-Virus“ 
#reagieren? Die letzten Tage haben ja gezeigt, dass sich die Aktienmärkte davon 
#stark betroffen sind. Wie wäre dies beim Sentiment-Modell?

#3.6

#Wie würden Sie weiter vorgehen, um die Qualität des Modells zu verbessern?

"
In conclusion I must admit that the results (sediment analysis) are not very satisfactory. In my opinion there is only a weak connection between the news and the performance of the stockmarket, because "bad" news are more attractive and are therefore published irrespective of the world's situation. The degree of destruction of scandal or a catastrophe is not taken into consideration.
Additionally, the focus shall be shifted to "good" news which occur less often and only when something really good has happened. I suggest that when weighing the positive words or text document parts more, the result will be more promissing.
Unfortunately, also this model is far from predicting the future reality.
The reasons are the same as mentioned above.

The Corona Virus continously produces "bad" news, however, the stock market is slowly returning to its normal state. Thus the recurring production of "bad" news will lead to even worse performance of the models.

In my opinion these problems and the problems mentioned above can be solve by word detecting and not by generalizing stuff and suming up documents and numbers. Clear signs must be found and model elaborated according to those signs!
"

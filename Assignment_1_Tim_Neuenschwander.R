#Tim Neuenschwander
#18-615-955
#Assignment 1 Business analytics and Data Science

bike = read.csv("CapitalBikeshare.csv")
View(bike)
str(bike)

#1.1
bike$season_=factor(bike$season)
bike$month=factor(bike$mnth)
bike$weekday=factor(bike$weekday)

levels(bike$season_)=c("spring", "summer", "fall", "winter")
levels(bike$month)=c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")
levels(bike$weekday)=c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")

contrasts(bike$season)
contrasts(bike$moth)
contrasts(bike$weekday)


str(bike)

#1.2
total.wetterdaten=rbind(bike$temp, bike$atemp, bike$hum, bike$windspeed)
total.wetterdaten=data.frame(total.wetterdaten)
is.data.frame(total.wetterdaten)

View(total.wetterdaten)

new.wetterdaten <- data.frame(t(total.wetterdaten))
names(new.wetterdaten) <- c("temp","atemp","hum","windspeed")

View(new.wetterdaten)

cor(new.wetterdaten)

"Diese Untersuchung hat ergeben:
- temp und atemp korellieren stark positiv miteinander
- hum und die beiden temps korellieren schwach positiv miteinander
- windspeed und alle anderen variablen korelliren schwach negativ miteinander"

#1.3
head(bike)
n = nrow(bike)
bike.index = sample(1:n, size = round(0.75*n), replace=FALSE)
bike.index

bike.train = bike[bike.index,]
nrow(bike.train)
bike.test = bike[-bike.index,]
nrow(bike.test)

"Meiner meinung nach gibt es keine generelle Regel, sondern nur gängie Praktiken.
Das einzig wichtige ist, dass der Trainingsdatensatz verhältnismässig grösser ist 
und dass beide (Training und Test) Datensätze zufällig ausgewählt werden um eine 
gleichmässige Vertielung von Werten zu erhalten."

#2.1

baseline_model1 = lm(cnt ~ season + workingday + holiday + mnth + yr, data=bike.train)
summary(baseline_model1)

"Zusätzlich habe ich auch ein baseline Model erstellt, das alle Levels der Faktoren in Betracht zieht.
In diesem model besteht der Intercept aus dem Zusammenschluss aller 0-Dummy Variablen.
In der Fragestellung war es unklar für mich, welches Model gefragt ist.
Approach with levels of factors:"
baseline_model1_1 = lm(cnt ~ season_ + workingday + holiday + month + yr, data=bike.train)
summary(baseline_model1_1)

"season: Der Faktor Season hat einen stark positiven Einfluss auf die Anzahl Vermietungen. Zudem werden in den kälteren Seasons mehr bikes vermietet!
workingday: Diese Variable hat einen sehr schwachen positiven Einfluss auf die Anzahl Vermietungen und ist somit statistisch insignifikant.
holiday: Diese Variable hat einen sehr schwachen negativen Einfluss auf die Anzahl Vermietungen und ist somit statistisch insignifikant.
month: Dieser Faktor hat durchschnittlich einen sehr stark positiven Einfluss auf die Anzahl der Vermietungen. Speziell ist, dass die Sommer 
Monate einen höheren Regressionskoeffizienten aufweisen als die Wintermonate, was der Aussage des Season Faktors wiederspricht und somit auf eine fehlerhafte Beschreibung hinweist.
yr: Diese Variable hat einen sehr stark positiven Einfluss auf die Vermietungszahl. Im Jahr 2012 boomt das business."

RMSE.baseline_model1 = sqrt(mean(baseline_model1$residual^2))
RMSE.baseline_model1

RMSE.baseline_model1_1 = sqrt(mean(baseline_model1_1$residual^2))
RMSE.baseline_model1_1

"RMSE ist die Wurzel des durchschnittlichen quadrierten Fehlers eines Linearen Regression Model's und representiert die Totale Differenz
zwischen der linearen Regressionslinie und den einzelnen realen Werten."

#2.2

baseline_model2 = lm(cnt ~ season + workingday + holiday + mnth + yr + temp + atemp + hum + windspeed, data=bike.train)
summary(baseline_model2)

RMSE.baseline_model2 = sqrt(mean(baseline_model2$residual^2))
RMSE.baseline_model2

"Ja die Qualität des Modells verbessert sich."

predictions = predict(baseline_model2, newdata=bike.test)
predictions

RMSE.baseline_model2test = sqrt(mean((bike.test$cnt - predictions)^2))
RMSE.baseline_model2test

"Das Modell weist eine eher gute Qualität auf beim verwenden vom Testdatensatz."

#2.3

"Ich nehme an das mit Wetter-Modell die Wetterdaten gemient sind. Somit werde ich die Faktoren temp und atemp, welche statistisch insignifikant sind, aus dem Model ausschliessen."

baseline_model3 = lm(cnt ~ season + workingday + holiday + mnth + yr + hum + windspeed, data=bike.train)
summary(baseline_model3)

RMSE.baseline_model3 = sqrt(mean(baseline_model3$residual^2))
RMSE.baseline_model3

"Gegen meine Erwartung hat sich das Model nicht verbessert. Die Qualität ist gesunken."

baseline_model3_1 = lm(cnt ~ season + workingday + holiday + mnth + yr + temp + atemp, data=bike.train)
summary(baseline_model3_1)

RMSE.baseline_model3_1 = sqrt(mean(baseline_model3_1$residual^2))
RMSE.baseline_model3_1

"Und wenn ich die angeblich signifikanten Wetterfaktoren entferne erhöht sich die Qualität des Modells wieder.
Trotzdem, ist das Model, das alle Variablen umfasst, das beste."

#2.4

"Für die predictions wähle ich das beste Modell --> baseline_model2"
baseline_model4 = lm(cnt ~ season + workingday + holiday + mnth + yr + temp + atemp + hum + windspeed, data=bike.train)
summary(baseline_model4)

predictions = predict(baseline_model4, newdata=bike.test)
predictions
bike.test$cnt

plot(predictions, bike.test$cnt, pch=20, col="darkgreen", ylab="Actual Values", xlab="Predicted Values", main="Actual vs. Predicted Values", xlim=c(0,10000), ylim=c(0,10000))
reg <- lm(predictions ~ bike.test$cnt, data = bike.test)
abline(a=0,b=1, col="green4") 

#3.1

bike.train$überbuchung <- ifelse(bike.train$cnt>=5000,1,0)
View(bike.train)
bike.test$überbuchung <- ifelse(bike.test$cnt>=5000,1,0)
View(bike.test)

#3.2

log_model_überbuchung = glm(überbuchung ~ season + workingday + holiday + mnth + yr + temp + atemp + hum + windspeed, data = bike.train, family=binomial(link = "logit"))

summary(log_model_überbuchung)

log_model_überbuchung$coefficients
exp(log_model_überbuchung$coefficients["atemp"])
exp(log_model_überbuchung$coefficients["hum"])
exp(log_model_überbuchung$coefficients["windspeed"])
exp(log_model_überbuchung$coefficients["yr"])
exp(log_model_überbuchung$coefficients["temp"])

log_model_überbuchung_1 = glm(überbuchung ~ season + workingday + holiday + mnth + yr + temp + atemp + hum + windspeed, data = bike.test, family=binomial(link = "logit"))

summary(log_model_überbuchung_1)

"Research hat ergeben, dass man Variablen mit einem P-Wert unter 0.2 in das Model integrieren soll. Deshalb habe ich das falgende Model erstellt."

log_model_überbuchung_1_1 = glm(überbuchung ~ yr + windspeed + hum, data = bike.test, family=binomial(link = "logit"))

summary(log_model_überbuchung_1_1)

#3.3

predict.train = predict(log_model_überbuchung, type="response")

table(predict.train > 0.5, bike.train$überbuchung, dnn = c("Outcome", "Prediction"))

(310+179)/(310+179+30+29) #accuracy
179/(179+29) # sensitivity / recall
310/(310+30) #specificity
179/(179+30) #precision

predict.test = predict(log_model_überbuchung_1, type="response")

table(predict.test > 0.5, bike.test$überbuchung, dnn = c("Outcome", "Prediction"))

(99+69)/(99+69+8+7) #accuracy
69/(69+7) # sensitivity / recall
99/(97+8) #specificity
69/(69+8) #precision

#3.4

library(ROCR)
predict.train.ROC = prediction(predict.train, bike.train$überbuchung)
predict.train.ROC.perf = performance(predict.train.ROC, "tpr", "fpr")
plot(predict.train.ROC.perf, main="ROC Curve for Training Data", lwd=2)

plot(predict.train.ROC.perf, main="ROC Curve for Training Data", lwd=2, 
     colorize=TRUE, 
     print.cutoffs.at=seq(0,0.9, by=0.1), 
     text.adj=c(-0.2,1.5))
abline(a=0,b=1)

performance(predict.train.ROC, "auc")@y.values





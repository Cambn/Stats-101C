library(leaps)
library(tree)
library(dplyr)
library(gbm)
library(randomForest)
library(xgboost)

testing_without <- read.delim(file = "Kitu/College/Junior Year/Spring Quarter/Stats 101C/testing_without.txt", sep = ",")
training_lafd <- read.csv(file = "Kitu/College/Junior Year/Spring Quarter/Stats 101C/lafdtraining updated.csv")

# fit <- lm(elapsed_time~., data = training_lafd)

# change class types
training_lafd$First.in.District <- as.character(training_lafd$First.in.District)
training_lafd$year <- as.factor(training_lafd$year)
training_lafd$Dispatch.Sequence <- as.numeric(training_lafd$Dispatch.Sequence)

# create new variable that has hour of creation only
training_lafd$Incident.Creation.Time..GMT. <- as.character(training_lafd$Incident.Creation.Time..GMT.)
training_lafd$Creation.Hour <- substring(training_lafd$Incident.Creation.Time..GMT., 1, 2)
training_lafd$Creation.Hour <- as.factor(training_lafd$Creation.Hour)
training_lafd <- training_lafd[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 11)] # reorder columns

# transform elapsed time variable
training_lafd$transformed <- log(training_lafd$elapsed_time)

# tree
# tree <- tree(transformed~year+Dispatch.Status+PPE.Level+Creation.Hour, data = training_lafd)
# summary(tree)
# plot(tree)

# read in mo's stuff
load(file = "Kitu/College/Junior Year/Spring Quarter/Stats 101C/Mo_train.Rdata")
load(file = "Kitu/College/Junior Year/Spring Quarter/Stats 101C/num_incidentID.Rdata")

# fit linear model
# training_lafd_without <- training_lafd[, 1:11]
# training_lafd_subset <- training_lafd[sample(nrow(training_lafd), 1000000), ]
# fit <- lm(elapsed_time~factor(year)+factor(First.in.District)+factor(Dispatch.Status)
#           + factor(Unit.Type)+factor(PPE.Level)+factor(Creation.Hour), data = training_lafd)
# summary(fit)
# anova(fit)
# my_prediction <- predict(fit, training_lafd_without, type = "response")
# my_solution <- data.frame(id = training_lafd$row.id, prediction = my_prediction, actual = training_lafd$elapsed_time, 
#                          error = (my_solution$actual-my_solution$prediction)^2)
# my_solution <- my_solution[complete.cases(my_solution), ]
# MSE <- sum(my_solution$error)/nrow(my_solution)

# merge num distinct incident to dataset
training_lafd <- left_join(training_lafd, p, by = "incident.ID")

# merge district dataset to training
fdgroup_orig <- read.csv(file = "Kitu/College/Junior Year/Spring Quarter/Stats 101C/LAFD_First_In_Districts.csv", header = TRUE)
training_lafd <- merge(training_lafd, fdgroup_orig, by.x = c("First.in.District"), by.y = c("FIRSTIN_DI"))

# change types for new variables
training_lafd$BATTALION_ <- as.factor(training_lafd$BATTALION_)
training_lafd$DIVISION_N <- as.factor(training_lafd$DIVISION_N)

# make dispatch sequence into groups
for (i in nrow(training_lafd))
{
  training_lafd$DSequence[training_lafd$Dispatch.Sequence == 1] <- 1
  training_lafd$DSequence[training_lafd$Dispatch.Sequence == 2] <- 2
  training_lafd$DSequence[training_lafd$Dispatch.Sequence == 3] <- 3
  training_lafd$DSequence[training_lafd$Dispatch.Sequence >= 4 & training_lafd$Dispatch.Sequence <= 29] <- 4
  training_lafd$DSequence[training_lafd$Dispatch.Sequence > 29 & training_lafd$Dispatch.Sequence <= 43] <- 5
  training_lafd$DSequence[training_lafd$Dispatch.Sequence > 43] <- 6
}

training_lafd$DSequence <- as.factor(training_lafd$DSequence)

# group districts into categories
fdgroups <- read.csv(file = "Kitu/College/Junior Year/Spring Quarter/Stats 101C/LAFD_First_In_Districts.csv", header = TRUE)
fdgroups <- with(fdgroups, fdgroups[order(BUREAU), ])
fdgroups <- fdgroups[, c(4, 7)]
central <- fdgroups[1:22, 1]
south <- fdgroups[23:46 , 1]
valley <- fdgroups[47:83, 1]
west <- fdgroups[48:102, 1]

testing <- training_lafd[1:5000, ]
training_lafd$Bureau <- "0"
training_lafd$First.in.District <- as.character(training_lafd$First.in.District)
central <- as.character(central)
south <- as.character(south)
west <- as.character(west)
valley <- as.character(valley)

training_lafd$Bureau <- fdgroups$BUREAU[match(as.character(training_lafd$First.in.District), as.character(fdgroups$FIRSTIN_DI))]

# grouping by median of unit types
median <- aggregate(transformed~Unit.Type, training_lafd, median)

save(median, file = "Kitu/College/Junior Year/Spring Quarter/Stats 101C/median_unit_type.Rdata")

median$transformed[median$transformed < 4.6] <- "A"
median$transformed[median$transformed > 4.6 & median$transformed < 5.3] <- "B"
median$transformed[median$transformed > 5.4 & median$transformed < 6.4] <- "C"
median$transformed[median$transformed > 6.5 & median$transformed < 6.8] <- "D"
median$transformed[median$transformed > 6.9 & median$transformed < 7.6] <- "E"
median$transformed[median$transformed > 7.7 & median$transformed < 8.7] <- "F"
median$transformed[median$transformed > 8.8 & median$transformed < 9.7] <- "G"

training_lafd$Unit.Type_Letter <- median$transformed[match(as.character(training_lafd$Unit.Type), as.character(median$Unit.Type))]
training_lafd$Unit.Type_Letter <- as.character(training_lafd$Unit.Type_Letter)

# use only non-redundant variables
subset_training_lafd <- training_lafd[, c(2, 4, 6, 7, 9, 11, 13, 14, 15, 16, 18, 21, 23, 24, 27)]
subset_training_lafd <- subset_training_lafd[ , c(1, 2, 3, 15, 4, 5, 6, 8, 9, 11, 10, 12, 13, 14, 7)] # reorder columns
save(subset_training_lafd, file = "Kitu/College/Junior Year/Spring Quarter/Stats 101C/kitu_training_new2.Rdata") 

# take care of NAs in transformed
a <- median(subset_training_lafd$transformed[subset_training_lafd$Unit.Type_Letter == "A"], na.rm = TRUE)
b <- median(subset_training_lafd$transformed[subset_training_lafd$Unit.Type_Letter == "B"], na.rm = TRUE)
c <- median(subset_training_lafd$transformed[subset_training_lafd$Unit.Type_Letter == "C"], na.rm = TRUE)
d <- median(subset_training_lafd$transformed[subset_training_lafd$Unit.Type_Letter == "D"], na.rm = TRUE)
e <- median(subset_training_lafd$transformed[subset_training_lafd$Unit.Type_Letter == "E"], na.rm = TRUE)
f <- median(subset_training_lafd$transformed[subset_training_lafd$Unit.Type_Letter == "F"], na.rm = TRUE)
g <- median(subset_training_lafd$transformed[subset_training_lafd$Unit.Type_Letter == "G"], na.rm = TRUE)


for (i in nrow(training_lafd))
{
  if (is.na(training_lafd$transformed[i]))
  {
    training_lafd$transformed[training_lafd$Unit.Type_Letter == "A"] <- a
    training_lafd$transformed[training_lafd$Unit.Type_Letter == "B"] <- b
    training_lafd$transformed[training_lafd$Unit.Type_Letter == "C"] <- c
    training_lafd$transformed[training_lafd$Unit.Type_Letter == "D"] <- d
    training_lafd$transformed[training_lafd$Unit.Type_Letter == "E"] <- e
    training_lafd$transformed[training_lafd$Unit.Type_Letter == "F"] <- f
    training_lafd$transformed[training_lafd$Unit.Type_Letter == "G"] <- g
  }
}

subset_subset$Unit.Type_Letter <- as.factor(subset_subset$Unit.Type_Letter)
subset_subset <- subset_training_lafd[!is.na(subset_training_lafd$transformed), ]
#boost <- gbm(transformed~., data = subset_subset, distribution = "gaussian", n.trees = 1000)

# random forest
set.seed(123)
complete_subset_training_lafd <- subset_training_lafd[complete.cases(subset_training_lafd), ]
training_subset_training_lafd <- complete_subset_training_lafd[sample(nrow(complete_subset_training_lafd), 2491884), ] # training
testing_subset_training_lafd <- complete_subset_training_lafd[sample(nrow(complete_subset_training_lafd), 276876), ] # testing

subset_training_subset_training_lafd <- training_subset_training_lafd[sample(nrow(training_subset_training_lafd), 150000), ] # use smaller sample
subset_training_subset_training_lafd <- subset_training_subset_training_lafd[, -c(1)] # take out row.id
colnames(subset_training_subset_training_lafd)[7] <- "dispatches"
subset_training_subset_training_lafd$Unit.Type_Letter <- as.factor(subset_training_subset_training_lafd$Unit.Type_Letter)

rf <- randomForest(transformed~., data = subset_training_subset_training_lafd, mtry = 5, ntree = 100, importance = TRUE)
plot(rf)
varImpPlot(rf)
importance(rf)
RF_pred1 <- predict(rf, newdata = testing_subset_training_lafd[, -c(1)])
colnames(testing_subset_training_lafd)[8] <- "dispatches"
testing_subset_training_lafd$Unit.Type_Letter <- as.factor(testing_subset_training_lafd$Unit.Type_Letter)
mean((exp(RF_pred1) - exp(testing_subset_training_lafd$transformed))^2, na.rm = TRUE) # find MSE

rf_prediction <- predict(rf, subset_testing_without[, -c(1)], type = "response")
rf_solution <- data.frame(row.id = subset_testing_without$row.id, prediction = exp(rf_prediction)) 

# replace NAs in prediction w median 389
for (i in nrow(rf_solution))
{
  rf_solution$prediction[is.na(rf_solution$prediction)] <- 389
}

write.csv(rf_solution, file = "Kitu/College/Junior Year/Spring Quarter/Stats 101C/rf_solution.csv")

# forward model
# nona_training_lafd <- training_lafd[complete.cases(training_lafd), ] # only choose rows with non NA response
# sub_nona_training_lafd <- nona_training_lafd[sample(nrow(nona_training_lafd), 500), ]
# out.f <- regsubsets(x = sub_nona_training_lafd[, c(3, 4, 5, 6, 7, 8, 9, 11)], y = sub_nona_training_lafd[, 12], method = "forward")


# xgboost
xgb <- xgboost(data = subset_training_lafd[, -c(1)])

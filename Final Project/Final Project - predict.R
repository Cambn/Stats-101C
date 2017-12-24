library(leaps)
library(tree)
library(dplyr)
library(gbm)

testing_without <-read.table(file = "Kitu/College/Junior Year/Spring Quarter/Stats 101C/testing.without.response.txt", header = TRUE, sep =  ",")
training_lafd <- read.csv(file = "Kitu/College/Junior Year/Spring Quarter/Stats 101C/lafdtraining updated.csv")

# change class types
testing_without$First.in.District <- as.character(testing_without$First.in.District)
testing_without$year <- as.factor(testing_without$year)
testing_without$Dispatch.Sequence <- as.numeric(testing_without$Dispatch.Sequence)

# create new variable that has hour of creation only
testing_without$Incident.Creation.Time..GMT. <- as.character(testing_without$Incident.Creation.Time..GMT.)
testing_without$Creation.Hour <- substring(testing_without$Incident.Creation.Time..GMT., 1, 2)
testing_without$Creation.Hour <- as.factor(testing_without$Creation.Hour)
testing_without <- testing_without[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 11)] # reorder columns

# find num distinct
t <- testing_without %>% 
  group_by(.,incident.ID) %>% 
  summarise(.,dispatches = n_distinct(row.id))

# merge num distinct incident to dataset
testing_without <- left_join(testing_without, t, by = "incident.ID")

# group districts into categories
fdgroups <- read.csv(file = "Kitu/College/Junior Year/Spring Quarter/Stats 101C/LAFD_First_In_Districts.csv", header = TRUE)
fdgroups <- with(fdgroups, fdgroups[order(BUREAU), ])
fdgroups <- fdgroups[, c(4, 7)]
central <- fdgroups[1:22, 1]
south <- fdgroups[23:46 , 1]
valley <- fdgroups[47:83, 1]
west <- fdgroups[48:102, 1]

testing_without$Bureau <- "0"
testing_without$First.in.District <- as.character(testing_without$First.in.District)
central <- as.character(central)
south <- as.character(south)
west <- as.character(west)
valley <- as.character(valley)

testing_without$Bureau <- fdgroups$BUREAU[match(as.character(testing_without$First.in.District), as.character(fdgroups$FIRSTIN_DI))]

# grouping by median of unit types
load(file = "Kitu/College/Junior Year/Spring Quarter/Stats 101C/median_unit_type.Rdata")

testing_without$Unit.Type_Letter <- median$transformed[match(as.character(testing_without$Unit.Type), as.character(median$Unit.Type))]
testing_without$Unit.Type_Letter <- as.factor(testing_without$Unit.Type_Letter)

# make dispatch sequence into groups
for (i in nrow(testing_without))
{
  testing_without$DSequence[testing_without$Dispatch.Sequence == 1] <- 1
  testing_without$DSequence[testing_without$Dispatch.Sequence == 2] <- 2
  testing_without$DSequence[testing_without$Dispatch.Sequence == 3] <- 3
  testing_without$DSequence[testing_without$Dispatch.Sequence >= 4 & testing_without$Dispatch.Sequence <= 29] <- 4
  testing_without$DSequence[testing_without$Dispatch.Sequence > 29 & testing_without$Dispatch.Sequence <= 43] <- 5
  testing_without$DSequence[testing_without$Dispatch.Sequence > 43] <- 6
}

testing_without$DSequence <- as.factor(testing_without$DSequence)

# merge district dataset to training
fdgroup_orig <- read.csv(file = "Kitu/College/Junior Year/Spring Quarter/Stats 101C/LAFD_First_In_Districts.csv", header = TRUE)
testing_without <- merge(testing_without, fdgroup_orig, by.x = c("First.in.District"), by.y = c("FIRSTIN_DI"))

# change types for new variables
testing_without$BATTALION_ <- as.factor(testing_without$BATTALION_)
testing_without$DIVISION_N <- as.factor(testing_without$DIVISION_N)

# use only non-redundant variables
subset_testing_without <- testing_without[ , c(2, 4, 6, 7, 9, 11, 12, 13, 14, 15, 17, 20, 22, 23)] # reorder columns
save(subset_testing_without, file = "Kitu/College/Junior Year/Spring Quarter/Stats 101C/kitu_testing.Rdata") 

# take out NA from training and replace with median
#med <- median(subset_testing_without$elapsed_time[!is.na(subset_testing_without$elapsed_time)])
#subset_testing_without$transformed[is.na(subset_testing_without$transformed)]<- log(med)

# fit model on training
#boost.train <- gbm(transformed~. , data = subset_testing_without)







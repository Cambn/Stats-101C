for (i in 1:ncol(test_dummy)){
  test_dummy[,i] <- as.numeric(test_dummy[,i])
} # convert everything to numeric

row.names <- read.delim("/Volumes/SAVE2HERE/testing.without.response.txt", sep = ",", header = TRUE)

## load the dataset to get the row names

dtest1 <- xgb.DMatrix(data = as.matrix(test_dummy))

colnames(test_dummy) <- make.names(colnames(test_dummy), unique = TRUE)  ## fix the column names
preds <- predict(xgb5, as.matrix(test_dummy))

preds3 <- predict(xgb11, as.matrix(test_dummy)) 
for (i in 1:length(preds5)){
  if (preds5[i] < 0){
    preds5[i] <- 0
  }
} # make the minimum value 0 if there are negatives

preds4 <- predict(xgb9, as.matrix(test_dummy)) # this increased my MSE though
summary(preds4)

df4 <- data.frame(row.id = row.names$row.id, prediction = preds4)
df3 <- data.frame(row.id = row.names$row.id, prediction = preds3) # this one h
xgb_number_whatever_and_a_half <- data.frame(row.id = row.names$row.id, prediction = preds3)
xgb_number_whatever_and_a_half2 <- data.frame(row.id = row.names$row.id, prediction = preds3)
summary(exp(dummy_lafd$transformed))
xgb2_df_b <- data.frame(row.id = row.names$row.id, prediction = exp(preds)) 
# as is gives a better prediction

write.csv(xgb_number_whatever_and_a_half2, file = "/Volumes/SAVE2HERE/xgb_number_whatever_and_a_half.csv")

preds5 <- predict(xgb11, as.matrix(test_dummy)) 

summary(preds5)

xgb_number_whatever_and_a_half2 <- data.frame(row.id = row.names$row.id, prediction = preds5)
write.csv(xgb_number_whatever_and_a_half2, file = "/Volumes/SAVE2HERE/xgb_number_whatever_and_a_half3.csv")

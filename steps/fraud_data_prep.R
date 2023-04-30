fraud <- fraud %>%
  mutate(Month = case_when(
    Month == "Jan" ~ 1,
    Month == "Feb" ~ 2,
    Month == "Mar" ~ 3,
    Month == "Apr" ~ 4,
    Month == "May" ~ 5,
    Month == "Jun" ~ 6,
    Month == "Jul" ~ 7,
    Month == "Aug" ~ 8,
    Month == "Sep" ~ 9,
    Month == "Oct" ~ 10,
    Month == "Nov" ~ 11,
    Month == "Dec" ~ 12
  ),
  DayOfWeek = case_when(
    DayOfWeek == "Monday" ~ 1,
    DayOfWeek == "Tuesday" ~ 2,
    DayOfWeek == "Wednesday" ~ 3,
    DayOfWeek == "Thursday" ~ 4,
    DayOfWeek == "Friday" ~ 5,
    DayOfWeek == "Saturday" ~ 6,
    DayOfWeek == "Sunday" ~ 7
  ),
  DayOfWeekClaimed = case_when(
    DayOfWeekClaimed == "Monday" ~ 1,
    DayOfWeekClaimed == "Tuesday" ~ 2,
    DayOfWeekClaimed == "Wednesday" ~ 3,
    DayOfWeekClaimed == "Thursday" ~ 4,
    DayOfWeekClaimed == "Friday" ~ 5,
    DayOfWeekClaimed == "Saturday" ~ 6,
    DayOfWeekClaimed == "Sunday" ~ 7
  ))


# Create new data frame "nfraud" and convert non-numeric values to numeric
nfraud <- fraud %>%
  mutate_if(is.character, as.factor) %>% # convert character columns to factors
  mutate_all(as.numeric) %>% # convert all columns to numeric
  na.omit() # remove rows with missing values

# Verify the new data frame
head(nfraud)
str(nfraud)
summary(nfraud)
dim(nfraud)

# Create new data frames for each variable
for (col_name in names(fraud)) {
  if (!is.numeric(fraud[[col_name]])) {
    col_unique <- unique(fraud[[col_name]])
    col_df <- data.frame(
      variable = col_unique,
      index = rep(NA, length(col_unique))
    )
    for (i in 1:length(col_unique)) {
      # Get the corresponding index value from nfraud
      index_val <- nfraud[fraud[[col_name]] == col_unique[i], col_name]
      col_df[i, "index"] <- index_val[1]
    }
    colnames(col_df)[1] <- col_name
    assign(col_name, col_df)
  }
}



#Feature scaling:
# Normalize the features
nfraud_norm <- apply(nfraud, 2, function(x) (x - min(x)) / (max(x) - min(x)))


#Handling imbalanced classes:
#install.packages("ROSE")
library(ROSE)

# Oversample the minority class using ROSE
nfraud_oversampled <- ROSE(FraudFound_P ~ ., data = nfraud, seed = 123)$data



# Handling categorical variables:
library(caret)
fraud_encoded <- predict(dummyVars(FraudFound_P ~ ., data = fraud), newdata = fraud)


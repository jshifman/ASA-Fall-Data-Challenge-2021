ggplot(data= allStatesDataFrame, aes(x = PovertyRate, y = LA1and10)) + geom_point(alpha = 0.2) + geom_smooth(se=FALSE)

ggplot(data= allStatesDataFrame, aes(x = MedianFamilyIncome, y = PovertyRate)) + geom_point(alpha = 0.2) + geom_smooth(se=FALSE)

ggplot(data= allStatesDataFrame, aes(x = TractSNAP, y = TractHUNV)) + geom_point(alpha = 0.2) + geom_smooth(se=FALSE) + xlim(0,1250)

ggplot(data= allStatesDataFrame, aes(y = lasnap10, x = PovertyRate)) + geom_point(alpha = 0.2) + geom_smooth(se=FALSE) + xlim(0,75)

byCountyPop <- allStatesDataFrame %>% select(State, County, Pop2010, LA1and10) %>% group_by(County, State) %>% summarise(totalPop = sum(Pop2010), hasLA = sum(LA1and10))
byCountyPop$State <- state2abbr(byCountyPop$State)
byCountyPop$CS <- paste(byCountyPop$County, byCountyPop$State)

educationDataFrame <- read_xls("Education.xls")
educationDataFrame$CS <-paste(educationDataFrame$`Area name`, educationDataFrame$State)

educationAndPopulation <- merge(x=byCountyPop, y=educationDataFrame, by = "CS")


rest_rows <- as.vector(createDataPartition(educationAndPopulation$hasLA, p = 0.8, list = FALSE))
test <- educationAndPopulation[-rest_rows, ]
rest <- educationAndPopulation[rest_rows, ]

train_rows <- as.vector(createDataPartition(rest$hasLA, p = 0.75, list = FALSE))

train <- rest[train_rows, ]
validate <- rest[-train_rows, ]
#edModel <- lm(hasLA ~`Percent of adults with a bachelor's degree or higher, 2015-19` + `Percent of adults with a high school diploma only, 2015-19`, data = train)
#edModel <- lm(hasLA ~ `Percent of adults with less than a high school diploma, 2015-19` + `Percent of adults with a high school diploma only, 2015-19` + `Percent of adults completing some college or associate's degree, 2015-19` + `Percent of adults with a bachelor's degree or higher, 2015-19`, data = train)
edModel <- lm(hasLA ~ `Less than a high school diploma, 2015-19` + `High school diploma only, 2015-19` + `Some college or associate's degree, 2015-19` + `Bachelor's degree or higher, 2015-19`, data = train)
predictions <- add_predictions(validate, edModel)

ggplot(data = predictions, mapping = aes(x = hasLA, y = pred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

resids <- add_residuals(validate, edModel)


ggplot(data = resids, mapping = aes(x = `High school diploma only, 2015-19`, y = resid)) + 
  geom_ref_line(h = 0) +
  geom_point()

clean <- predictions %>%
  filter(!is.na(pred))

summary(edModel)
R2(clean$pred, clean$hasLA)
MAE(clean$pred, clean$hasLA)
RMSE(clean$pred, clean$hasLA)

predictions <- add_predictions(test, edModel)

ggplot(data = predictions, mapping = aes(x = hasLA, y = pred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

resids <- add_residuals(test, edModel)


ggplot(data = resids, mapping = aes(x = `Bachelor's degree or higher, 2015-19`, y = resid)) + 
  geom_ref_line(h = 0) +
  geom_point()

clean <- predictions %>%
  filter(!is.na(pred))

summary(edModel)
R2(clean$pred, clean$hasLA)
MAE(clean$pred, clean$hasLA)
RMSE(clean$pred, clean$hasLA)


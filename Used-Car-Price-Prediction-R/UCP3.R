data= read.csv("D:\\project 3\\used cars1.csv");data
head(data)
dim(data)
str(data)
names(data)
install.packages("psych")
library(psych)
data(ToothGrowth)
describe(ToothGrowth)
data1 = subset(data, select = -c(1));data1
data30=subset(data1,select=-c(1));data30









# Map levels to numeric values-fuel

fuel<- as.numeric(factor(data$Fuel_Type, levels = c("Diesel","Petrol","CNG")));fuel 
data1<- cbind(data, fuel);data1
data1 <- data1[, -which(names(data1) == "Fuel_Type")];data1

#

location<- as.numeric(factor(data$Location, levels =c("Mumbai","Chennai","Jaipur","Hyderabad","Pune","Kolkata","Ahmedabad","Kochi","Bangalore","Coimbatore","Delhi")));location
data1<- cbind(data1,location);data1
data1<- data1[, -which(names(data1) == "Location")];data1

#
transmission<- as.numeric(factor(data$Transmission, levels =c("Automatic","Manual")));transmission
data1<- cbind(data1,transmission);data1
data1<- data1[, -which(names(data1) == "Transmission")];data1

#
owner_type=as.numeric(factor(data$Owner_Type, levels =c("First","Second")));owner_type
data1<- cbind(data1,owner_type);data1
data1<- data1[, -which(names(data1) == "Owner_Type")];data1

#
data1<- data1[, -which(names(data1) == "Name")];data1

data1
str(data1)
as.numeric(data1$New_Price)
is.na(data1$New_Price)

# Sample data containing the column values
column_values <- c("2527000", "9,27,000", "14,95,000", "70,43,000", "1189000", "1102000", "894000", "578000", "940000", "630000", "641000", "4062000", "880000", "1559000", "4480000", "574000", "743000", "529600", "459000", "1148000", "1593000", "1432000", "399000", "1,58,00,000", "615000", "895000", "6207000", "3193000", "1588000", "4365000", "823000", "857000", "719000", "5698000", "1413000", "1148000", "1120000", "1413000", "641000", "912000", "391000", "2504000", "892000", "501000", "1901000", "945000", "3,75,00,000", "3516000", "678000", "624000", "5561000", "942000", "844000", "456000", "2990000", "712000", "994000", "644000", "767000", "1682000", "1559000", "1557000", "951000", "1175000", "6347000", "670000", "3410000", "531000", "1524000", "3650000", "3703000", "1086000", "885000", "5912000", "579000", "396000", "1055000", "1573000", "891000", "470000", "767000", "2756000", "1700000", "475000", "4073000", "2177000", "1147000", "708000", "7792000", "1486000", "2614000", "3241000", "4073000", "3336000", "663000", "1205000", "8697000", "638000", "589000", "655000", "779000", "787000", "1832000", "1065000", "1664000", "2100000", "954000", "926000", "633000", "3410000", "3500000", "466000", "2329000", "395000", "459000", "1308000", "571000", "402000", "6710000", "6671000", "1333000", "450000", "1511000", "4912000", "1051000", "865000", "579000", "504000", "5314000", "912000", "551000", "1505000", "1191000", "860000", "1555000", "3410000", "1036000", "2069000", "425000", "399000", "409000", "1,06,00,000", "9201000", "1094000", "828000", "1094000", "3241000", "2401000", "790000", "721000", "1694000", "1013000", "796000", "654000", "527000", "556000", "2074000", "1114000", "659000", "794000", "1102000", "2001000", "794000", "4459000", "3262000", "1159000", "646000", "3336000", "6207000", "1514000", "1972000", "409000", "6787000", "1246000", "911000", "1268000", "553000", "706000", "879000", "1816000", "1057000", "708000");column_values

# Identify rows where commas are present
rows_with_commas <- grepl(",", column_values);rows_with_commas

# Remove commas and convert to numeric for rows with commas
column_values[rows_with_commas] <- as.numeric(gsub(",", "", column_values[rows_with_commas]));column_values[rows_with_commas] 

# Convert the entire column to numeric
column_values <- as.numeric(column_values);column_values


# Add or replace the existing column in 'data1' with the modified values
data1$new_price <- column_values;data1$new_price

# Print the modified data frame
data1

data1<- data1[, -which(names(data1) == "New_Price")];data1 

data30$new_price=column_values;data30$new_price
data30<- data30[, -which(names(data30) == "New_Price")];data30 

###################





# Load required libraries
install.packages("dplyr")
library(dplyr)
attach(data1)


# Check for missing values in the dataset
missing_values <- sapply(data1, function(x) sum(is.na(x)));missing_values


summary(data1)

# Find duplicates
duplicate_rows=data[duplicated(data1) | duplicated(data1, fromLast = TRUE), ];duplicate_rows

#CORRELATION

install.packages("corrplot")
library(corrplot)
attach(data1)
datamatrix=cor(data1);datamatrix
corrplot(datamatrix, method = "number")

##DATA VISUAIZATION##

# Load the ggplot2 library
install.packages("ggplot2")
library(ggplot2)

#location
# Aggregating the data based on 'location'
location_counts <- table(data1$location);location_counts

# Converting the aggregated data to a data frame
location_df <- as.data.frame(location_counts);location_df
names(location_df) <- c('Location', 'Count');names(location_df)

# Creating the barplot
ggplot(location_df, aes(x=Location, y=Count)) +geom_bar(stat="identity", fill="skyblue") +labs(title="Count of Cars by Location", x="Location", y="Count") +theme_minimal()

#year
year_counts <- table(data1$Year);year_counts
year_df <- as.data.frame(year_counts);year_df
names(year_df) <- c('year', 'Count');names(year_df)
ggplot(year_df, aes(x=year, y=Count)) +geom_bar(stat="identity", fill="skyblue") +labs(title="Count of Cars by year", x="year", y="Count") +theme_minimal()

# Kilometers_Driven
Kilometers_Driven_counts <- table(data1$Kilometers_Driven);Kilometers_Driven_counts
Kilometers_Driven_df <- as.data.frame(Kilometers_Driven_counts);Kilometers_Driven_df
names(Kilometers_Driven_df) <- c('Kilometers_Driven', 'Count');names(Kilometers_Driven_df)
ggplot(Kilometers_Driven_df, aes(x=Kilometers_Driven, y=Count)) +geom_bar(stat="identity", fill="skyblue") +labs(title="Count of Cars by Kilometers_Driven", x="Kilometers_Driven", y="Count") +theme_minimal()

#Mileage.kmpl.
Mileage.kmpl._counts <- table(data1$Mileage.kmpl.);Mileage.kmpl._counts
Mileage.kmpl._df <- as.data.frame(Mileage.kmpl._counts);Mileage.kmpl._df
names(Mileage.kmpl._df) <- c('Mileage.kmpl.', 'Count');names(Mileage.kmpl._df)
ggplot(Mileage.kmpl._df, aes(x=Mileage.kmpl., y=Count)) +geom_bar(stat="identity", fill="skyblue") +labs(title="Count of Cars by Mileage.kmpl.", x="Mileage.kmpl.", y="Count") +theme_minimal()

#Engine.CC.
Engine.CC._counts <- table(data1$Engine.CC.);Engine.CC._counts
Engine.CC._df <- as.data.frame(Engine.CC._counts);Engine.CC._df
names(Engine.CC._df) <- c('Engine.CC.', 'Count');names(Engine.CC._df)
ggplot(Engine.CC._df, aes(x=Engine.CC., y=Count)) +geom_bar(stat="identity", fill="skyblue") +labs(title="Count of Cars by Engine.CC.", x="Engine.CC.", y="Count") +theme_minimal()

#Power.bhp. 
Power.bhp._counts <- table(data1$Power.bhp.);Power.bhp._counts
Power.bhp._df <- as.data.frame(Power.bhp._counts);Power.bhp._df
names(Power.bhp._df) <- c('Power.bhp.', 'Count');names(Power.bhp._df)
ggplot(Power.bhp._df, aes(x=Power.bhp., y=Count)) +geom_bar(stat="identity", fill="skyblue") +labs(title="Count of Cars by Power.bhp.", x="Power.bhp.", y="Count") +theme_minimal()

#Seats 
Seats_counts <- table(data1$Seats);Seats_counts
Seats_df <- as.data.frame(Seats_counts);Seats_df
names(Seats_df) <- c('Seats', 'Count');names(Seats_df)
ggplot(Seats_df, aes(x=Seats, y=Count)) +geom_bar(stat="identity", fill="skyblue") +labs(title="Count of Cars by Seats", x="Seats", y="Count") +theme_minimal()

#fuel 
fuel_counts <- table(data1$fuel );fuel_counts
fuel_df <- as.data.frame(fuel_counts);fuel_df
names(fuel_df) <- c('fuel', 'Count');names(fuel_df)
ggplot(fuel_df, aes(x=fuel, y=Count)) +geom_bar(stat="identity", fill="skyblue") +labs(title="Count of Cars by fuel", x="fuel", y="Count") +theme_minimal()

#transmission 
transmission_counts <- table(data1$transmission);transmission_counts
transmission_df <- as.data.frame(transmission_counts);transmission_df
names(transmission_df) <- c('transmission', 'Count');names(transmission_df)
ggplot(transmission_df, aes(x=transmission, y=Count)) +geom_bar(stat="identity", fill="skyblue") +labs(title="Count of Cars by transmission", x="transmission", y="Count") +theme_minimal()

#owner_type 
owner_type_counts <- table(data1$owner_type);owner_type_counts
owner_type_df <- as.data.frame(owner_type_counts);owner_type_df
names(owner_type_df) <- c('owner_type', 'Count');names(owner_type_df)
ggplot(owner_type_df, aes(x=owner_type, y=Count)) +geom_bar(stat="identity", fill="skyblue") +labs(title="Count of Cars by owner_type", x="owner_type", y="Count") +theme_minimal()

#new_price
new_price_counts <- table(data1$new_price);new_price_counts
new_price_df <- as.data.frame(new_price_counts);new_price_df
names(new_price_df) <- c('new_price', 'Count');nems(new_price_df)
ggplot(new_price_df, aes(x=new_price, y=Count)) +geom_bar(stat="identity", fill="skyblue") +labs(title="Count of Cars by new_price", x="new_price", y="Count") +theme_minimal()






##to compute partial correlation coefficients between variables in a dataset##
data1=data1[,-c(1)];data1

############
#HITOGRAM#

# Load necessary libraries
library(ggplot2)

ggplot(data1, aes(x = Year)) +geom_histogram(binwidth = 1, fill = 'blue', color = 'black', alpha = 0.7) +labs(title = "Histogram of Year", x = "Year", y = "Frequency") +theme_minimal()
ggplot(data1, aes(x =Mileage.kmpl.)) +geom_histogram(binwidth = 1, fill = 'blue', color = 'black', alpha = 0.7) +labs(title = "Histogram of Mileage.kmpl.", x = "Mileage.kmpl.", y = "Frequency") +theme_minimal()
ggplot(data1, aes(x =Seats)) +geom_histogram(binwidth = 1, fill = 'blue', color = 'black', alpha = 0.7) +labs(title = "Histogram of Seats", x = "Seats", y = "Frequency") +theme_minimal()





#SCATTERPLOT#
install.packages("GGally")
library(GGally)
# Load necessary libraries
library(GGally)

# Select only the variables of interest
data1_subset <- data1[, c("new_price","Year","Kilometers_Driven","Mileage.kmpl.","Engine.CC.","Power.bhp.","Seats","fuel","location","transmission","owner_type")];data1_subset

# Create scatter plot matrix
ggpairs(data1_subset,title = "Scatter Plot Matrix of Variables",lower = list(continuous = "points"),upper = list(continuous = "cor"),diag = list(continuous = "densityDiag"))













###################
install.packages("ppcor")
library(ppcor)
pcor(data1, method = "pearson")

###############################

#IDENTIFY AND VISUALIZE OUTLIERS#

boxplot(data1$Year,main="Year",col="lightblue",border="black")
outliers=boxplot.stats(data1$Year)$out;outliers

boxplot(data1$Kilometers_Driven,main="Kilometers_Driven",col="lightblue",border="black")
outliers=boxplot.stats(data1$Kilometers)$out;outliers

boxplot(data1$Mileage.kmpl,main="Mileage.kmpl",col="lightblue",border="black")
outliers=boxplot.stats(data1$Mileage.kmpl)$out;outliers

boxplot(data1$Engine.CC.,main="Engine.CC.",col="lightblue",border="black")
outliers=boxplot.stats(data1$Engine.CC)$out;outliers

boxplot(data1$Power.bhp.,main="Power.bhp.",col="lightblue",border="black")
outliers=boxplot.stats(data1$Power.bhp)$out;outliers

boxplot(data1$Seats,main="Seats",col="lightblue",border="black")
outliers=boxplot.stats(data1$Seats)$out;outliers

boxplot(data1$fuel,main="fuel",col="lightblue",border="black")
outliers=boxplot.stats(data1$fuel)$out;outliers

boxplot(data1$location,main="location",col="lightblue",border="black")
outliers=boxplot.stats(data1$location)$out;outliers

boxplot(data1$transmission,main="transmission",col="lightblue",border="black")
outliers=boxplot.stats(data1$transmission)$out;outliers

boxplot(data1$owner_type,main="owner_type",col="lightblue",border="black")
outliers=boxplot.stats(data1$owner_type)$out;outliers

boxplot(data1$new_price,main="new_price",col="lightblue",border="black")
outliers=boxplot.stats(data1$new_price)$out;outliers
#########################

model0=lm(new_price~.,data=data1);model0
summary(model0)

############

#SPLITTING DATA SET INTO TRAIN AND TEST DATA

set.seed(121)

trainindices=sample(1:nrow(data1), 0.7 * nrow(data1), replace = TRUE);trainindices
data1_train=data1[trainindices,];data1_train
data1_test=data1[-trainindices,];data1_test

install.packages("MASS")  
library(MASS)

model <- lm(new_price ~ ., data = data1_train);model
  
#model adequacy#
install.packages("car")  
library(car)
install.packages("lmtest")
library(lmtest)

# Check normality of residuals
shapiro.test(residuals(model))
# Perform the Durbin-Watson test(autocorrelation)
durbinWatsonTest(model)
# Perform the Breusch-Pagan test(homo)
bptest(model)
#multicollinearity
vif(model)

##################



#####STEPWISE####

library(MASS)

stepwise_model <- stepAIC(lm(new_price~1,data = data1_train),scope = list(lower = ~1, upper =model),direction = "forward");stepwise_model
summary(stepwise_model)

model2=lm(new_price~Power.bhp.+transmission+Year+Mileage.kmpl. ,data=data1_train);model2 
summary(model2)

#model adequacy#
install.packages("car")  
library(car)
install.packages("lmtest")
library(lmtest)

# Check normality of residuals
shapiro.test(residuals(model2))
# Perform the Durbin-Watson test(autocorrelation)
durbinWatsonTest(model2)
# Perform the Breusch-Pagan test(homo)
bptest(model2)
#multicollinearity
vif(model2)

############

#OUTLIERS REMOVAL#

#cooks distence method

cooks_dist <-cooks.distance(model2);cooks_dist
plot(cooks.distance(model2),pch=20,main="OUTLIERS BY COOKS DISTANCE METHOD")
abline(h=4/215,col='red',lty=2)
# Set a threshold for Cook's distance
cook_threshold <- 0.5 / nrow(data1_test);cook_threshold 
# Identify outliers based on Cook's distances
outliers <- which(cooks_dist > cook_threshold);outliers
# Remove outliers from the dataset
data1_train2<- data1_train[-outliers, ];data1_train2

################################

model20=lm(new_price~Power.bhp.+transmission+Year+Mileage.kmpl. ,data=data1_train2);model20
summary(model20)

# Check normality of residuals
shapiro.test(residuals(model20))
# Perform the Durbin-Watson test(autocorrelation)
durbinWatsonTest(model20)
# Perform the Breusch-Pagan test(homo)
bptest(model20)
#multicollinearity
vif(model20)

#checking whwther error terms have zero mean 
# Get residuals
residuals2 <- residuals(model20);residuals2

# Calculate mean of residuals
mean_residuals <- mean(residuals2);mean_residuals

##################

# Residual analysis
par(mfrow = c(2, 2))
# Plot Residuals vs Fitted Values
plot(model20, which = 1)

# Plot Normal Q-Q plot of residuals
qqPlot(model20$residuals)

# Plot Scale-Location plot (Square root of standardized residuals vs Fitted Values)
plot(model20, which = 3)

# Plot Residuals vs Leverage (Cook's distance)
plot(model20, which = 5)


################################


#PREDICTION ON TEST DATA#

price_test=data1_test$new_price;price_test		
actual=price_test;actual
predictions=predict(model20,newdata=data1_test,type="response");predictions
predicted <- ifelse(predictions >= 0.5, 1, 0)
result=data.frame(actual=price_test,predicted=predicted);result


##################
# Extract actual values
actuals <- data1_train2$new_price;actual

# Make predictions using the model
predictions2 <- predict(model20);predictions2

# Calculate correlation between actual and predicted values
correlation <- cor(actuals, predictions2);correlation

##############

model_summary =summary(model20)
model_summary$r.squared


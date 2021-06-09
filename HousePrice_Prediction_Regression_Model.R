#Regression Model

#Installing Required Packages and Reading Data
library(tidyverse)
library(stringr)
library(lubridate)
library(DT)
library(caret)
library(leaflet)
library(corrplot)
library(boot) #for diagnostic plots
library(naniar)
library(Amelia)




rm(list=ls())

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

#Importing Data
KCHouseData = read_csv("G:/NCI/Data Mining/Project/House Sales Prediction_Regression/kc_house_data.csv")
print(KCHouseData)
View(KCHouseData)

#Checking the Missing Values
is.na(KCHouseData)
vis_miss(KCHouseData)
missmap(KCHouseData, main = "Missing values vs observed")

#Bedrooms and Price comparision
KCHouseData %>%
  group_by(bedrooms) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bedrooms = reorder(bedrooms,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  
  ggplot(aes(x = bedrooms,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = bedrooms, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'bedrooms', 
       y = 'Median Price', 
       title = 'bedrooms and Median Price') +
  coord_flip() + 
  theme_bw()

# Bathrooms and Price comparision
KCHouseData %>%
  group_by(bathrooms) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bathrooms = reorder(bathrooms,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  head(10) %>%
  
  
  ggplot(aes(x = bathrooms,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = bathrooms, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'bathrooms', 
       y = 'Median Price', 
       title = 'bathrooms and Median Price') +
  coord_flip() + 
  theme_bw()

# Grade and Price comparision
KCHouseData %>%
  group_by(grade) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(grade = reorder(grade,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  
  ggplot(aes(x = grade,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = grade, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'grade', 
       y = 'Median Price', 
       title = 'grade and Median Price') +
  coord_flip() + 
  theme_bw()


# waterfront and Price comparision
KCHouseData %>%
  group_by(waterfront) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(waterfront = reorder(waterfront,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  
  ggplot(aes(x = waterfront,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  
  labs(x = 'waterfront', 
       y = 'Median Price', 
       title = 'waterfront and Median Price') +
  
  theme_bw()

# Sqft Living and Price comparision
KCHouseData %>% 
  filter(!is.na(price)) %>% 
  filter(!is.na(sqft_living)) %>% 
  
  ggplot(aes(x=sqft_living,y=price))+
  geom_point(color = "blue")+
  
  stat_smooth(aes(x=sqft_living,y=price),method="lm", color="red")+
  theme_bw()+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  xlab("(Sqft Living)")+
  ylab("Price")

KCHouseData %>% 
  filter(!is.na(price)) %>% 
  filter(!is.na(sqft_lot)) %>% 
  
  ggplot(aes(x=sqft_lot,y=price))+
  geom_point(color = "orange")+
  
  stat_smooth(aes(x=sqft_lot,y=price),method="lm", color="red")+
  theme_bw()+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  xlab("(Sqft Lot)")+
  ylab("Price")

# Year Built and Price comparision
KCHouseData %>%
  group_by(yr_built) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(yr_built = reorder(yr_built,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  head(10) %>%
  
  
  ggplot(aes(x = yr_built,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = yr_built, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'year built', 
       y = 'Median Price', 
       title = 'year built and Median Price') +
  coord_flip() + 
  theme_bw()

# Year Renovated and Price comparision
KCHouseData %>%
  group_by(yr_renovated) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(yr_renovated = reorder(yr_renovated,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  head(10) %>%
  
  
  ggplot(aes(x = yr_renovated,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = yr_renovated, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'year renovated', 
       y = 'Median Price', 
       title = 'year renovated and Median Price') +
  coord_flip() + 
  theme_bw()

# Price Plots
KCHouseData %>%
  
  ggplot(aes(x = price)) +    
  geom_histogram(alpha = 0.8,fill = fillColor2) +
  scale_x_continuous(limits=c(0,2e6)) +
  
  labs(x= 'Price',y = 'Count', title = paste("Distribution of", ' Price ')) +
  theme_bw()

KCHouseData %>%
  
  ggplot(aes(x = price)) +    
  geom_histogram(alpha = 0.8,fill = fillColor2) +
  scale_x_continuous(limits=c(0,1e6)) +
  
  labs(x= 'Price',y = 'Count', title = paste("Distribution of", ' Price ')) +
  theme_bw()

# Maps of Houses
KCHouseData$PriceBin<-cut(KCHouseData$price, c(0,250e3,500e3,750e3,1e6,2e6,999e6))

center_lon = median(KCHouseData$long,na.rm = TRUE)
center_lat = median(KCHouseData$lat,na.rm = TRUE)

factpal <- colorFactor(c("black","blue","yellow","orange","#0B5345","red"), 
                       KCHouseData$PriceBin)



leaflet(KCHouseData) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~long, lat = ~lat, 
             color = ~factpal(PriceBin))  %>%
  # controls
  setView(lng=center_lon, lat=center_lat,zoom = 12) %>%
  
  addLegend("bottomright", pal = factpal, values = ~PriceBin,
            title = "House Price Distribution",
            opacity = 1)

# Price Bins Count
KCHouseData %>%
  mutate(PriceBin = as.factor(PriceBin)) %>%
  group_by(PriceBin) %>%
  dplyr::summarise(Count = n()) %>%
  ungroup() %>%
  mutate(PriceBin = reorder(PriceBin,Count)) %>%
  arrange(desc(Count)) %>%
  
  ggplot(aes(x = PriceBin,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = PriceBin, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'PriceBin', 
       y = 'Count', 
       title = 'PriceBin and Count') +
  coord_flip() + 
  theme_bw()

# Correlation plot
KCHouseData = KCHouseData %>% select(-PriceBin)

KCHouseData3 = KCHouseData %>%
  select(-id,-date)

CorrelationResults = cor(KCHouseData3)

corrplot(CorrelationResults)

# single linear regression model to predict price using the sqft_plot* attribute
model <- glm(price ~ sqft_lot, data = KCHouseData, family = gaussian)
glm.diag.plots(model)

# Columns Name
colnames(KCHouseData)

# Removing id and date column as this columns are not required
KCHouseData2 = KCHouseData %>%
  select(-id,-date)
colnames(KCHouseData2)

# Traincontrol function
formula = price ~ .

fitControl <- trainControl(method="cv",number = 5)

KCHouseDataModel = train(formula, data = KCHouseData2,
                         method = "lm",trControl = fitControl,metric="RMSE")

# Variable Importance of Linear Regression
importance = varImp(KCHouseDataModel)


PlotImportance = function(importance)
{
  varImportance <- data.frame(Variables = row.names(importance[[1]]), 
                              Importance = round(importance[[1]]$Overall,2))
  # Create a rank variable based on importance
  rankImportance <- varImportance %>%
    mutate(Rank = paste0('#',dense_rank(desc(Importance))))
  
  rankImportancefull = rankImportance
  
  ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                             y = Importance)) +
    geom_bar(stat='identity',colour="white", fill = fillColor) +
    geom_text(aes(x = Variables, y = 1, label = Rank),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Variables', title = 'Relative Variable Importance') +
    coord_flip() + 
    theme_bw()
  
  
}

PlotImportance(importance)

# Model Accuracy
KCHouseDataModel


# glmnet
formula = price ~ .

fitControl <- trainControl(method="cv",number = 5)

KCHouseDataModel = train(formula, data = KCHouseData2,
                         method = "glmnet",trControl = fitControl,metric="RMSE")

# Variable importance
importance = varImp(KCHouseDataModel)


PlotImportance = function(importance)
{
  varImportance <- data.frame(Variables = row.names(importance[[1]]), 
                              Importance = round(importance[[1]]$Overall,2))
  
  # Create a rank variable based on importance
  rankImportance <- varImportance %>%
    mutate(Rank = paste0('#',dense_rank(desc(Importance))))
  
  rankImportancefull = rankImportance
  
  ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                             y = Importance)) +
    geom_bar(stat='identity',colour="white", fill = fillColor) +
    geom_text(aes(x = Variables, y = 1, label = Rank),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Variables', title = 'Relative Variable Importance') +
    coord_flip() + 
    theme_bw()
  
  
}

PlotImportance(importance)


# Accuracy of model
KCHouseDataModel

# PCA
PCAData = KCHouseData %>%
  select(lat,long)

pca = prcomp(PCAData, scale. = T)

KCHouseData_pca <- predict(pca, newdata = PCAData)

KCHouseData_pca = as.data.frame(KCHouseData_pca)

KCHouseData2 = cbind(KCHouseData2,KCHouseData_pca)

# Lat and Long
KCHouseData %>% 
  filter(!is.na(lat)) %>% 
  filter(!is.na(long)) %>% 
  
  ggplot(aes(x=lat,y=long))+
  geom_point(color = "blue")+
  
  theme_bw()+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  xlab("Latitude")+
  ylab("Longitude")

# PCA direction
pca = prcomp(PCAData, scale. = T)
biplot (pca , scale =0)

#Tuning Parameters
xgbGrid <- expand.grid(nrounds = 500,
                       max_depth = 4,
                       eta = .05,
                       gamma = 0,
                       colsample_bytree = .5,
                       min_child_weight = 1,
                       subsample = 1)


# PCA and XGBoost
set.seed(13)

KCHouseDataModelXGB = train(formula, data = KCHouseData2,
                            method = "xgbTree",trControl = fitControl,
                            tuneGrid = xgbGrid,na.action = na.pass,metric="RMSE")

importance = varImp(KCHouseDataModelXGB)

PlotImportance(importance)

#Accuracy
KCHouseDataModel
KCHouseDataModelXGB

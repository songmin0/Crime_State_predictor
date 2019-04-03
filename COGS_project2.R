
#source of data -> https://github.com/emorisse/FBI-Hate-Crime-Statistics/tree/master/2013 

library(tidyverse)
library(digest)
library(repr)
install.packages("caret")
library(caret)
install.packages("dplyr")
library(dplyr)
install.packages("gridExtra")
library(gridExtra)

#reading in the data
input <- read_csv("https://raw.githubusercontent.com/emorisse/FBI-Hate-Crime-Statistics/master/2013/table13.csv")
head(input)

#renaming columns
cleaning_data <- names(input) <- c("State","Agency_type","Agency_name", "Race", "Religion", "Sexual_orientation", 
                                   "Ethnicity", "Disability", 
                                     "Gender","Gender_identity", "first_quarter", "second_quarter", "third_quarter", "fourth_quarter", "Population")
head(cleaning_data)

#scaling the data
scaled_input <- input %>%
        na.omit() %>%
    mutate(scaled_race = scale(Race,center = FALSE), 
    scaled_religion = scale(Religion, center = FALSE), 
    scaled_ethnicity = scale(Ethnicity, center = FALSE),
    scaled_disability = scale(Disability, center = FALSE),
    scaled_gender = scale(Gender, center = FALSE),
    scaled_sexual_orientation = scale(Sexual_orientation, center = FALSE),
    scaled_gender_identity = scale(Gender_identity, center = FALSE),
    scaled_1st_quarter = scale(first_quarter, center = FALSE),
    scaled_2nd_quarter = scale(second_quarter, center = FALSE),
    scaled_3rd_quarter = scale(third_quarter, center = FALSE),
    scaled_4th_quarter = scale(fourth_quarter, center = FALSE)) %>%
    select(-Agency_name,-Race, -Religion, -Sexual_orientation, -Ethnicity, -Disability, -Gender,
           -Gender_identity, -first_quarter, -second_quarter, -third_quarter, -fourth_quarter, -Agency_type,-Population)
head(scaled_input)

#some visualisations of the data
visualization1<- scaled_input %>% 
  ggplot(aes(x =State, y= scaled_race)) +
  geom_point(alpha = 0.5)+
  labs(x = "state",
       y = "race crimes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

visualization1

visualization1<- scaled_input %>% 
  ggplot(aes(x =State, y= scaled_race)) +
  geom_point(alpha = 0.5)+
  labs(x = "state",
       y = "race crimes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

visualization1

visualization1<- scaled_input %>% 
  ggplot(aes(x =State, y= scaled_race)) +
  geom_point(alpha = 0.5)+
  labs(x = "state",
       y = "race crimes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

visualization1

visualization2<- scaled_input %>% 
  ggplot(aes(x =State, y= scaled_religion)) +
  geom_point(alpha = 0.5)+
  labs(x = "state",
       y = "religious crimes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

visualization2

visualization3<- scaled_input %>% 
  ggplot(aes(x =State, y= scaled_ethnicity)) +
  geom_point(alpha = 0.5)+
  labs(x = "state",
       y = "ethnicity crimes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

visualization3

visualization4<- scaled_input %>% 
  ggplot(aes(x =State, y= scaled_disability)) +
  geom_point(alpha = 0.5)+
  labs(x = "state",
       y = "disability crimes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

visualization4

visualization5<- scaled_input %>% 
  ggplot(aes(x =State, y= scaled_gender)) +
  geom_point(alpha = 0.5)+
  labs(x = "state",
       y = "gender crimes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

visualization5
visualization6<- scaled_input %>% 
  ggplot(aes(x =State, y= scaled_sexual_orientation)) +
  geom_point(alpha = 0.5)+
  labs(x = "state",
       y = "sexual_orient crimes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

visualization6

visualization7<- scaled_input %>% 
  ggplot(aes(x =State, y= scaled_gender_identity)) +
  geom_point(alpha = 0.5)+
  labs(x = "state",
       y = "gender_identity crimes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

visualization7


install.packages("e1071")

set.seed(151)

#removing all rows with just one input
cleaned_input <-scaled_input %>%
                group_by(State,scaled_race, scaled_religion, scaled_ethnicity,scaled_disability, scaled_gender, 
                          scaled_sexual_orientation, scaled_gender_identity) %>% 
                filter(n()>1)
head(cleaned_input)

cleaned_input2<- ungroup(cleaned_input)
cleaned_input2

#creating model
training_rows <- cleaned_input2 %>%
  select(State) %>%
    mutate(State = as.factor(State)) %>%
    unlist() %>%
    createDataPartition(p=0.80, list=FALSE)

X_train <- cleaned_input2 %>% 
    select(scaled_race, scaled_religion, scaled_ethnicity,scaled_disability, scaled_gender, 
            scaled_sexual_orientation, scaled_gender_identity) %>%
    slice(training_rows) %>% 
    data.frame()

Y_train <- cleaned_input2 %>% 
    select(State) %>% 
    slice(training_rows) %>% 
    unlist()

X_test <- cleaned_input2 %>% 
    select(scaled_race, scaled_religion, scaled_ethnicity,scaled_disability, scaled_gender, 
            scaled_sexual_orientation, scaled_gender_identity) %>% 
    slice(-training_rows) %>% 
    data.frame()

Y_test <- cleaned_input2 %>% 
    select(State) %>% 
    slice(-training_rows) %>%
    unlist()

ks <- data.frame(k=c(1:11))
train_control <- trainControl(method='cv', number=10)
choose_k <- train(x = X_train, y = Y_train, method = 'knn', tuneGrid = ks, trControl = train_control)
choose_k

 

final_k = data.frame(k = 1)
final_classifier_data <- train(x = X_train, y = Y_train, method = "knn", tuneGrid = final_k)
final_classifier_data


test_pred <- predict(final_classifier_data, X_test) 
head(test_pred)
modelvalues <- data.frame(obs = Y_test, pred = test_pred)
head(modelvalues)
knn_mult_test_results <- defaultSummary(modelvalues)
knn_mult_test_results

#just to check if its working
new_obs <- data.frame(scaled_race = 200, scaled_religion= 30, scaled_ethnicity= 40,
                                 scaled_disability= 20, scaled_gender = 0, 
                                 scaled_sexual_orientation= 0, scaled_gender_identity= 0)
                        
predicted_all_knn <- predict(object = final_classifier_data, new_obs)
predicted_all_knn

#defining the function
myprediction <- function(a,b,c,d,e,f,g) {
 data.frame(scaled_race = a, scaled_religion= b, scaled_ethnicity= c,
  scaled_disability= d, scaled_gender = e, 
  scaled_sexual_orientation= f, scaled_gender_identity= g) %>%
  predict(object =final_classifier_data) %>%
   as.data.frame ()}

#just to see if function call works
myprediction(200,30,40,20,0,0,0)


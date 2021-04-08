library(tidyverse)
hotels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv")

colnames(hotels)


head(hotels)


# Tidymodels --------------------------------------------------------------
# install.packages('tidymodels')
library(tidymodels)


# Data Exploration --------------------------------------------------------


hotel_stays <- hotels %>% filter(is_canceled==0) %>% 
     mutate(children = case_when(children+babies > 0 ~"children", 
                                  TRUE ~ "none"), 
            required_car_parking_spaces = case_when(required_car_parking_spaces > 0 ~ "parking", TRUE ~"none" )) %>% 
     select(-is_canceled, -reservation_status, -babies)

hotel_stays

hotel_stays %>% count(children)



# Skim through data using SkimR --------------------------------------------------------------

library(skimr)

skim(hotel_stays)


# Change String to a Factor -----------------------------------------------

hotel_stays %>%  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name)) %>% 
     count(arrival_date_month, children) %>% 
     group_by(children) %>% 
     mutate(proportion = n/sum(n),
            percentage = proportion*100) %>% 
     ggplot(aes(arrival_date_month, proportion, fill = children))+
     geom_col(position = 'dodge') + 
     scale_y_continuous(labels = scales::percent_format())


hotel_stays %>%  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name)) %>% 
     count(hotel, arrival_date_month, children) %>% 
     group_by(hotel, children) %>% 
     mutate(proportion = n/sum(n),
            percentage = proportion*100) %>% 
     ggplot(aes(arrival_date_month, proportion, fill = children))+
     geom_col(position = 'dodge') + 
     scale_y_continuous(labels = scales::percent_format())+
     facet_wrap(~hotel, nrow=2)




hotel_stays %>% count(hotel, required_car_parking_spaces, children) %>% 
     group_by(hotel, children) %>% 
     mutate(proportion = n/sum(n),
            percentage = proportion*100) %>% 
     ggplot(aes(required_car_parking_spaces, proportion, fill = children))+
     geom_col(position = 'dodge') + 
     scale_y_continuous(labels = scales::percent_format())+
     facet_wrap(~hotel, nrow=2)



# GGally ------------------------------------------------------------------

library(GGally)

hotel_stays %>% select(children, adr, required_car_parking_spaces, total_of_special_requests) %>% 
     ggpairs(mapping=aes(color=children))


hotels_df <- hotel_stays %>% select(children, hotel, arrival_date_month, meal, adr, adults, total_of_special_requests,
                       required_car_parking_spaces, stays_in_week_nights, stays_in_weekend_nights) %>% 
     mutate_if(is.character, factor)



# TidyModels --------------------------------------------------------------


set.seed(111)


hotel_split <- initial_split(hotels_df)

hotel_train <- hotel_split %>%  training()     
hotels_test <- hotel_split %>%  testing()


hotel_rec <- recipe(children~., data = hotel_train) %>% 
     step_downsample(children) %>% 
     step_dummy(all_nominal(), -all_outcomes()) %>% 
     step_zv(all_numeric()) %>% 
     step_normalize(all_numeric()) %>% 
     prep()

hotel_rec

bake(hotel_rec, new_data = hotels_test)

juice(hotel_rec) %>% 
     count(children)



# Nearest Neighbour  ------------------------------------------------------

knn_spec <- nearest_neighbor() %>% 
     set_engine("kknn") %>% 
     set_mode("classification")

knn_fit <- knn_spec %>% 
     fit(children~., 
         data = juice(hotel_rec))


# Decistion Tree ----------------------------------------------------------

tree_spec <- decision_tree() %>% 
     set_engine("rpart") %>% 
     set_mode("classification")


tree_fit <- tree_spec %>% 
     fit(children ~.,
         data = juice(hotel_rec))

tree_fit




#####################################################



# Evaluation --------------------------------------------------------------

set.seed(1234)
library(rsample)

validation_split <- mc_cv(juice(hotel_rec), prop=0.9, strata = children)
validation_split


knn_res <- fit_resamples(children ~., knn_spec, validation_split, control = control_resamples(save_pred = TRUE))

knn_result %>%  
     collect_metrics()

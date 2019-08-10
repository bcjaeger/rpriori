## code to prepare `titanic` dataset goes here

library(titanic)
library(tidyverse)

titanic <- titanic_train %>% 
  as_tibble() %>% 
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>% 
  mutate(
    Survived = factor(
      Survived, 
      labels = c("No","Yes")
    ),
    Sex = factor(
      Sex, 
      levels = c("male","female"), 
      labels = c("Male", "Female")
    ),
    Pclass = factor(
      Pclass, 
      labels = c("First","Second","Third")
    )
  ) %>% 
  set_names(tolower(names(.)))

usethis::use_data(titanic)

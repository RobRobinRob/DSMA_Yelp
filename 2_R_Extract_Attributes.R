#This Code for extracting attributes is mostly taken from: https://www.kaggle.com/ucaby40/analysis-of-business-attributes


library(tidyverse) #  data manipulation and graphs
library(stringr) #  string manipulation
library(lubridate) #  date manipulation
library(DataExplorer)

library(purrr)
library(furrr)
library(future)
library(data.table)


# Number of digits (here 2 digits)
options(digits = 9)
# Avoid scientific notation
options(scipen=999)

# Set the working directory to the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

business <- read.csv("files/businesstable.csv")

glimpse(business)

#Filter to only businesses that contain the category "Restaurant"
business_restaurants <- business %>% filter(grepl("Restaurants",business$categories,fixed=T))

#Extract the attributes
#

business$attributes <- business$attributes %>% 
  gsub("['{}]","",x=.) %>% 
  gsub('["]',"",x=.) %>% 
  gsub(" ","",x=.)

business$attributes[2]

#Three functions for extracting and dealing with attributes:
step1 <- function(x){
  logi <- word(x,-1,sep=":")
  title <- gsub(str_c(":",logi),"",x)
  return(list(title,logi))
}

step2 <- function(x){
  x <- x %>% str_split(",") %>% unlist() %>% 
    map(step1) %>% unlist() %>% matrix(nrow=2,byrow=F) %>% 
    as.data.frame()
  
  x <- data.table::transpose(x)
}

step3 <- function(x){
  x <- x[attributes_rel,on="V1"]
  return(x)
}

#Create an individual dataframe for each business entry
list_entire_att <- future_map(business$attributes,step2)
#Take a look at attributes
atttribute_table <- list_entire_att %>% do.call(rbind,args=.) %>% 
  count(V1,sort=T)
atttribute_table

#A count of 1000 seems like a good cutoff point; the lower ones are probably too few to be used
attributes_rel <- list_entire_att %>% 
  do.call(rbind,args=.) %>% 
  count(V1,sort=T) %>% 
  filter(n > 1000) %>% 
  select(V1)
attributes_rel

#Our function needs a data.table
list_entire_att <- future_map(list_entire_att,data.table)
#
list_selected_att <- map(list_entire_att,step3)
#Create one column for each attribute
logi <- lapply(list_selected_att, function(x){x[,2]})
result <- do.call(cbind,logi) %>% data.table::transpose()
#Add the column names from the attribute_rel
colnames(result) <- attributes_rel$V1

result <- result %>% add_column(business_id=business$business_id,.before = colnames(result)[1])
head(result)
write.csv(result, "~/Master/Data Science MArketing Analytics/attributes", row.names= FALSE)

#Clean some of the attributes with misleading results
result$WiFi <- result$WiFi %>% 
  gsub("ufree","free",x=.) %>% 
  gsub("uno","no",x=.) %>% 
  gsub("upaid","paid",x=.) %>% 
  gsub("no","None",x=.) %>% 
  gsub("free","True",x=.) %>% 
  gsub("None","False",x=.) 
result %>% count(WiFi)

result$RestaurantsAttire <- result$RestaurantsAttire %>% 
  gsub("ucasual","casual",x=.) %>% 
  gsub("udressy","dressy",x=.) %>% 
  gsub("uformal","formal",x=.) 
result %>% count(RestaurantsAttire)

result$NoiseLevel <- result$NoiseLevel %>% 
  gsub("uaverage","average",x=.) %>% 
  gsub("uquiet","quiet",x=.) %>% 
  gsub("uvery_loud","very_loud",x=.) %>% 
  gsub("uloud","loud",x=.)
result %>% count(NoiseLevel)

result$Alcohol <- result$Alcohol %>% 
  gsub("ubeer_and_wine","beer_and_wine",x=.) %>% 
  gsub("none|unone","None",x=.) %>% 
  gsub("ufull_bar","full_bar",x=.)
result %>% count(Alcohol)


#Write again cleaned
write.csv(result, "files/attributes_cleaned_2.csv", row.names= FALSE)

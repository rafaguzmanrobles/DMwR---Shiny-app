library(shiny)
library(shinydashboard)

#NOTE: the code is splitted into three parts:
#      1. All code required to load and prepare the data that the app will use, including the FOMO FIX functions
#      2. UI: where we define the interface of the shiny app
#      3. server: all background computation required while using the app to generate the outputs


#################################################
##          LOAD DATASET                       ##
#################################################

#install.packages("recommerderlab")
#install.packages("readr")
library(readr)
library(dplyr)
library(plyr)
library(recommenderlab)
library(magrittr)
library(readxl)
library(DMwR2)
library(RJSONIO)
library(leaflet)
library(tidyr)
library(readxl)
#setwd("~/NYU/Courses/Capstone Project/FOMO Fix/Yelp/Data")
state_list <- read_csv("C:/Users/Usuario/Documents/MSBA/Capstone/Data/Capstone_R/Recommender_test/Albastaki_Guzman_Moss_Neoley_Shah/List_of_States.csv")
full_set <- read_csv("C:/Users/Usuario/Documents/MSBA/Capstone/Data/filtered_data/full set without text.csv")
reviews_full <- as.data.frame(full_set)
users <-unique(reviews_full$user_id) #create list of user_ids
businesses <- unique(reviews_full$business_id) #create list of business_ids
which(businesses == "" | is.na(businesses), arr.ind = TRUE) #check if there are any blank or NA records in the business and user id lists
which(users == "" | is.na(users), arr.ind = TRUE)
users<- na.omit(users) #clean out NA records (if there are any)
businesses<- na.omit(businesses)

##### Create a column in reviews_full to use the naming ###################
bus_names <- c(1:length(businesses)) #create list of user friendly business ids
for(i in seq_len(length(bus_names))) {bus_names[i] <- paste0("b_",i)}
bus_data <- data.frame(Businesses = businesses, Bus_names = bus_names)
n_bus_id <- c()
reviews_full$n_business_id <- for(i in seq_len(nrow(reviews_full))) {
  for(y in seq_len(nrow(bus_data))){
    if(reviews_full$business_id[i] == businesses[y]) {
      n_bus_id[i] = bus_names[y]
      break
    }
  }
} 
reviews_full$n_business_id <- n_bus_id
full_set$n_business_id <- n_bus_id
bus_data <- subset(full_set,!duplicated(full_set$id)) %>% 
  select(id, name, address, city, state, postal_code, longitude, latitude,stars, n_business_id)
##########################################################################

library(data.table) #use this library to change row and column names below
rec_table<-data.frame(matrix(NA, nrow = length(users), ncol = 1)) #create recommender table
(setattr(rec_table, "row.names", users)) #label rows as user ids
rec_table[,bus_names] <- NA #label column names as business ids
rec_table <- rec_table[,-1] #remove first dummy column

for (i in seq_len(nrow(full_set))){  #populate rec_table
  rec_table[full_set$user_id[i],full_set$n_business_id[i]] <- full_set$stars_1[i]
}


#################################################
##          DATA RESHAPE                       ##
#################################################

##Changing Users and Restaurants IDs by names in rec_table
u<-0
for (i in seq_len(nrow(rec_table))){ #create list of friendly user names
  u[i] <- paste0("u_",i)
}

#rename users to friendly names
(setattr(rec_table, "row.names", u)) #label rows as user ids


###########################################################################
##  NADER: THIS IS FOR YOU
##  Add here some lines of code to add random ratings to rec_table
##  The objective is to populate rec_table so that all users have at least
##  ratings for two restaurants
##  .................
###########################################################################

# Randomly assign values to rec_table (all unique users and businesses)
###########################################################################
## This is to be removed later in the project
###########################################################################
values <- c(NA,1,2,3,4,5)
for (i in 1:length(rec_table)) {
  rec_table[i] <- sample(values, nrow(rec_table), replace = TRUE, prob = c(.5,.05,.05,.1,.15,.15))
}
###########################################################################

#install.packages("openxlsx")
#library(openxlsx) # may need to install this package
#create excel file of rec table and save it to folder:
#write.xlsx(rec_table, "C:\Users\nader.albastaki\Documents\NYU\Courses\Capstone Project\FOMO Fix\Yelp\Data", row.names = TRUE)
#rownames(rec_table)[1] # get row name
#colnames(rec_table)[1] # get col name
#install.packages("psych")
library(psych)
#library(ggplot2)
#table(full_set$user_id)
#table(full_set$business_id)

############## REMOVING rec_table_sub with rec_table in below, ########################
############## as rec_table has assigned random variables      ########################

#rec_table_sub <- subset(rec_table, length(rec_table) - apply(is.na(rec_table),1,sum) >2)


#################################################
##      SPLIT DATA INTO TRAINING/TEST          ##
#################################################

#### NADER: MODIFY NEXT THREE LINES TO SUBSTITUTE rec_table_sub BY rec_table
which_train <- sample(x = c(TRUE, FALSE), size = nrow(rec_table), replace = TRUE, prob = c(0.8, 0.2))
rec_table_train <- rec_table[which_train, ]
rec_table_test <- rec_table[!which_train, ]


#################################################
##   ADAPT DATASET TO RECOMMENDERLAB FORMAT    ##
#################################################
recc_data_train <- as(as.matrix(rec_table_train), "realRatingMatrix")
recc_data_test  <- as(as.matrix(rec_table_test),  "realRatingMatrix")


################################################
####   FOMO FIX RECOMMENDER FUNCTIONS    #######
################################################

create_diff_table <- function(user, recommendation_table){
  #Description: given a user and a recommendation table, it returns a dataframe containing the absolute difference
  #between the user reviews and the rest of the users reviews for each business. 
  
  #An additional column is added ("Total_reviews_diff") with the sum of all differences. 
  #Note that users having a sum of zero had the same reviews than the user passed to the function. 
  #"Neares neighbours" will be those with lowest values (ideally zero)
  
  #Another column has added ("Common_reviews") to take into consideration the number of reviews compared: 
  #we could have two users with sum of differences = 0. The first one having 5 exact reviews 
  #that the user passed to the function, and the second one having only 1 review but exact to the 
  #user passed to the function. Both will have differneces = 0 but actually the first one is "closer" 
  #to the user passed to the function as they had 5 matches while the second one had only 1
  
  # Example: 
  # REC_TABLE:
  #
  #     B1  B2  B3 
  # U1  4   NA  NA
  # U2  1   3   NA
  # U3  2   4   NA
  #
  # DIFF_TABLE for user U1:
  #
  #     B1  B2  B3  Total_reviews_diff  Common_reviews
  # U1  0   NA  NA         0                  1
  # U2  3   NA  NA         3                  1
  # U3  2   NA  NA         2                  1
  #
  # DIFF_TABLE for user U2:
  #
  #     B1  B2  B3  Total_reviews_diff  Common_reviews 
  # U1  3   NA  NA         3                  1
  # U2  0   0   NA         0                  1
  # U3  1   1   NA         2                  2
  #
  
  #extract reviews from the user to be recommended
  user_rating <- recommendation_table[user,]
  
  #create an empty dataframe with same dimensions and same row and column names
  diff_table <- data.frame(matrix(ncol = ncol(recommendation_table), nrow = nrow(recommendation_table)))
  colnames(diff_table) <- colnames(recommendation_table)
  rownames(diff_table) <- rownames(recommendation_table)
  
  #review_counter counts the number of common reviews both users have 
  review_counter <- vector(mode="numeric", length=nrow(recommendation_table))
  
  for (i in 1:(nrow(recommendation_table))) {
    review_counter[i] <- 0
    for(j in 1:(ncol(recommendation_table))){
      if(!is.na(user_rating[j]) & !is.na(recommendation_table[i,j])){
        diff_table[i,j] <- abs(user_rating[j] - recommendation_table[i,j])
        review_counter[i] <- review_counter[i] + 1 
      }
    }
  }
  
  #Add a new column with the sum of all business review for that user
  diff_table <- cbind(diff_table, 'Total_reviews_diff'=rowSums(diff_table,na.rm = TRUE))
  #Add a new column with the number of common reviews between both users
  diff_table <- cbind(diff_table, 'Common_reviews'= review_counter)
  
  return (diff_table) 
}

find_kNN <- function(user, diff_table, k){
  #Description: this function receives an user, the differece_table created by the create_diff_table
  #function and the "k" nearest neighbours expected and returns the name of the users
  
  #first approach: sort descendent the diff_table by "Total_reviews_diff" and get the k first elements
  
  # Example: 
  #
  # DIFF_TABLE for user U2:
  #
  #     B1  B2  B3  Total_reviews_diff  Common_reviews 
  # U1  3   NA  NA         3                  1
  # U2  0   0   NA         0                  1
  # U3  1   1   NA         2                  2
  #
  #
  # DIFF_TABLE after removing U2:
  #
  # B1  B2  B3  Total_reviews_diff  Common_reviews 
  # U1  3   NA  NA         3                  1
  # U3  1   1   NA         2                  2  
  #
  #
  # NN = U1, U3 (as all have at least one review in common with U2)
  #
  # sortedNN = U3, U1 (sorted by total review difference) 
  #
  # kNN = U3 (in case k=1) or U3, U1 (in case k=2)
  
  #We first exclude the reference user as it will always have Total_reviews_diff = 0 as it was
  #compare with himself
  diff_table <- diff_table[-which(rownames(diff_table)==user),]
  
  #we get only those rows from diff_table with at least one review in common
  NN <- diff_table[diff_table$Common_reviews >= 1, ]
  
  #we sort it by Total_reviews_diff to start with lowest values of difference (and therefore "closer") neighbours
  sortedNN <- NN[order(NN$Total_reviews_diff),]
  
  #we only take the firs "k" elements from the list
  kNN <- rownames(head(sortedNN, k))
  
  return (kNN) 
}


make_recommendation <- function(kNN, recommendation_table, user){
  #Description: this funtion receives the kNN list and the table with all reviews, and returns
  #the list of Business that the kNN also reviewed, with at least 4 stars
  
  # Example: 
  # REC_TABLE:
  #
  #     B1  B2  B3 
  # U1  4   NA  NA
  # U2  1   3   NA
  # U3  2   4   NA
  #
  #
  # kNN = U3, U1 (k=2)
  #
  # minimun_rating_reco = 0 (NOTE: ideally it would be at least 4 stars)
  #
  # recommended_businesses = B1 (coming from U1,U3), B2 (coming from U3) 
  #
  # NOTE: as U2 has already reviewed B1, it is deleted from the results. Therefore:
  # recommended_businesses = B2
  
  minimun_rating_reco <- 0
  # this variable can be adjusted if we want to recommend restaurants with higher/lower rating
  # we set it up by default to 0 stars but should be at least 3
  
  #we initiate the variable with an empty string
  recommended_businesses <- character(0)
  
  #we go thorugh the list of kNN
  for (i in 1:length(kNN)){
    #we get the row number of the nearest neighbor
    user_row <- which(rownames(recommendation_table)==kNN[i])
    
    #we go through the list of businesses
    for (j in 1:ncol(recommendation_table)) 
      if(!is.na(recommendation_table[user_row,j])){
        if (recommendation_table[user_row,j] >= minimun_rating_reco){
          recommended_businesses <- append(recommended_businesses,colnames(recommendation_table)[j])
        }
      }
  }
  
  #we delete duplicates
  recommended_businesses <- unique(recommended_businesses)
  
  #we get the list of businesses that the reference user has already reviewed and we delete those
  #business from the output of this function to avoid recommending already known businesses
  user_already_reviewed_businesses <- character(0)
  
  for (j in 1:ncol(recommendation_table)){
    if(!is.na(recommendation_table[user,j])){
      user_already_reviewed_businesses <- append(user_already_reviewed_businesses,colnames(recommendation_table[j]))
    }
  }
  
  recommended_businesses <- recommended_businesses[!recommended_businesses %in% user_already_reviewed_businesses]
  return (recommended_businesses)
}

######################################################################################################
#### This section creates the documentation required for the mapping tool that will will plot on  ####
######################################################################################################
#---------------this functions get the list of business ids from the predictor and generates the restaurant dataframe----#
get_restaurants<-function(b_name){
  #b_name<- c("b_1","b_7","b_8")
  index <- match(b_name, bus_data$n_business_id)
  res_df<- bus_data[index,]
  return(res_df)
}

#----------------function to plot restaurants----------------#

res_plot <- function(res_df){ #this function will take a subset of restaurants as an input and generate a map of those restuarants
  
  #create the FOMO icon
  FOMO_icon <- makeIcon(
    iconUrl = "FOMO.ico",
    iconWidth = 38, iconHeight = 50,
    iconAnchorX = 22, iconAnchorY = 94
  )
  
  #this creates the content to populate the popups over the restaurant name
  for (i in 1:nrow(res_df)){
    if (i==1) content<-NULL # need this line to initialise the content variable
    content[i]<- paste(sep = "<br/>",
                       res_df[i,"name"],
                       res_df[i,"address"],
                       paste("<b> rating: ",res_df[i,"stars"]," <b>")
    )
  }
  
  #full_set <- read_excel("bus_data_for_jason_trying_to_make_this_name_long.xlsx") ######*********this needs to be changed
  restaurants <- bus_data %>% as.data.frame()
  restaurants$longitude %<>% as.double()
  restaurants$latitude %<>% as.double()
  restaurants_nv <- subset(restaurants,restaurants$state =="NV") #Update to read from Shiny
  m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=res_df$longitude, lat=res_df$latitude, 100, res_df$name, icon = FOMO_icon) %>%
    addPopups(res_df$longitude, res_df$latitude, content, options = popupOptions(closeButton = TRUE)) %>%
    setView(-115.1400, 36.1719, zoom = 18)
    #setView(-115.2873, 36.1434, zoom = 18)
  return(m %>% fitBounds(-115.2946, 36.1196, -115.0422, 36.2206))
}


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Recommender Selection Criteria"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(selectizeInput('User', 'Select User', 
                                  choices = rownames(rec_table_test),
                                  selected = rownames(rec_table_test)[1])
                   ,
                   #selectInput('State', label = 'Select State', state_list, selected = 'NV')
                   #,
         radioButtons("model", label ="Recommender Model",
                     choices = list("FOMO Fix" = 1,
                                    "UBCF" = 2, 
                                    "IBCF" = 3),
                     selected = 1)
         ,
         sliderInput("kn",
                     "K-nearest neighbours:",
                     min = 1,
                     max = 20,
                     value = 5)
        ,
        radioButtons("distance", label ="Distance method (only applicable to IBCF and UBCF)",
                   choices = list("Cosine Similarity" = "cosine", 
                                  "Euclidean Distance" = "euclidean", 
                                  "Pearson Correlation" = "pearson"), 
                   selected = "cosine")
        ,
        sliderInput("n_recommendations",
                    "Number of recommendations:",
                    min = 1,
                    max = 5,
                    value = 3)
        , p("    MODELS DETAILS:")
        , p("FOMO Fix: Capstone recommender based on  user-based collaborative filtering.")
        , p("UBCF: Recommender based on user-based collaborative filtering")
        , p("IBCF: Recommender based on item-based collaborative filtering")
        )
      ,
      # Show a plot of the generated distribution
      mainPanel(
        #helpText('Recommended restaurants based on the selected criteria'),
        #verbatimTextOutput('recommendations')
        leafletOutput("mymap",height = 800)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #Support Functions:
  #function that given an user and a recommendation matrix, it returns recommended restaurants for that user
  recommendations_for_user <- function(u, rec_mat){
    result <- rec_mat[rec_mat$.id==u,-1]
    result <- result[!is.na(result)]
    #in case no recommendation is provided, we return NA
    if(length(result)==0) {
      result <- NA
    }
    result
  }
  
  #Output
    #output$recommendations <- renderPrint({
    output$mymap <- renderLeaflet({  
      ## model == 1 : FOMO Fix
      if (input$model == 1){
        user <- input$User
        kn <- as.numeric(input$kn)
        
#### NADER: MODIFY NEXT LINE TO SUBSTITUTE rec_table_sub BY rec_table
        diff_table <- create_diff_table(user, rec_table_test) 
        kNN <- find_kNN(user, diff_table, kn)
#### NADER: MODIFY NEXT LINE TO SUBSTITUTE rec_table_sub BY rec_table
        recommended_businesses <- make_recommendation(kNN, rec_table_test, user)
        #recommended_businesses[1:input$n_recommendations]
#### JASON: when FOMO Fix model is selected, in the previous line of code you can find
####        the list of recommendations you will have to use as input for your map
        prediction_output <- as.vector(recommended_businesses[1:input$n_recommendations]) ######*********this needs to be changed
        res_df<-get_restaurants(prediction_output) #this is the dataframe required to plot the graph
        res_plot(res_df) #call the res_plot (restaurant plot) function
      }
      
      ## model == 2 : UBCF
      else{ 
        if(input$model == 2){
          recc_model <- Recommender(data = recc_data_train, method = "UBCF", parameter = list(method = input$distance, nn = input$kn))
        }
        
        ## model == 3 : IBCF
        else if (input$model == 3){
          recc_model <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(method = input$distance, k = input$kn))
        }
      
        recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = input$n_recommendations)
        recc_list <- lapply(recc_predicted@items, function(x){colnames(recc_data_test)[x]})
        recc_matrix <- ldply(recc_list, rbind)
        recc_matrix_not_empty <- recc_matrix[!is.na(recc_matrix[2]),]
        
        #return only the recommendations for the user selected in the app
        #as.vector(recommendations_for_user(input$User, recc_matrix_not_empty))
        
#### JASON: when one of the  recommenderLab models is selected, in the previous line of code you can find
####        the list of recommendations you will have to use as input for your map        
        
        prediction_output <- as.vector(recommendations_for_user(input$User, recc_matrix_not_empty)) ######*********this needs to be changed
        res_df<-get_restaurants(prediction_output) #this is the dataframe required to plot the graph
        res_plot(res_df) #call the res_plot (restaurant plot) function
      }
    }) 
}

# Run the application 
shinyApp(ui = ui, server = server)


#' creaate mobility graph
#' 
#' This function combines the mobility data and the userid data together and creates a directed graph
#' 
#' @param mobility_path a character string. The path containing the the mobility data
#' @param user_data a dataframe. The data containing user information
#' @param pop_estimates_df a dataframe. A dataframe of ONS population estimates
#' @param MSOA_vect a chatacter vector. The MSOA for each observation
#' 
#' @details The ordering this is done, doesn't make total sense. For example the mobility data needs to be loaded to create the MSOA_vect.
#' Which means the data is loaded twice. This needs to be improved
#' 
#' 
#' @export
#' 


create_mobility_graph <- function(mobility_path, user_data, pop_estimates_df, MSOA_vect){

  
  mobility_data <- fread(mobility_path)
  
  #remove lat long from mobility data and create a data.table which includes the MSOA vector
  test <-as.data.table(mobility_data %>% 
                         select(userid, date, hour)%>% 
                         mutate(userid = as.character(userid)))[,
                                                                msoa.code_trace := MSOA_vect]
  
  test <- unique(test, by=c('userid', 'msoa.code_trace', 'date'))
  
  #find users not present in the user dataset
  is_present_vect <- as.character(test$userid) %in% user_data$userid 
  
  #join on home MSOA 
  #remove users that are not residents of london or do not have a known address.
  #This removal makes joining easier
  test <- test[is_present_vect,][user_data %>% select(userid, msoa.code_home = msoa.code) %>% as.data.table(), on ="userid"]
  #aggregate by home and destination MSOA across whole time period
  #can we use some time metric?
  test <- test[,.N, by =.(msoa.code_home, msoa.code_trace) ]%>%
    #add in the population for each msoa for normalisation
    left_join(pop_estimates_df %>% select(msoa.code_home = msoa.code, pop_home = pop), 
              by ="msoa.code_home") %>%
    left_join(pop_estimates_df %>% select(msoa.code_trace = msoa.code, pop_trace = pop), 
              by ="msoa.code_trace") %>%
    ###
    ### I am removing na's as not all the msoa are present. THe whole shebang needs to be redon with all msoa
    ###
    filter(!is.na(pop_trace) ) %>%
    #divide all edge weights by the number of days and the number of people in the respective MSOA
    #for directed the edge is divided by the from MSOA
    #for undirected the edge is divided by the sum of the 'from' and the 'to' MSOA
    mutate(pop_tot = pop_home+pop_trace,
           N_home = N/pop_home) 
  
  #create assymetric/directed adjacency matrix where each node is an MSOA
  #direction is travelled from home TO another location
  directed_g <- test %>%
    select(from = msoa.code_home, to = msoa.code_trace, weight = N_home) %>% 
    graph_from_data_frame()
  
  
  return(directed_g)
  
  
}
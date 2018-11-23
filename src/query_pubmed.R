library(XML)
library(httr)
library(dplyr)
library(ggplot2)

########################
# Download PubMed Data #
########################

query_pubmed <- function(query, yrStart, yrMax) {
  
  ### Some error checking ####
  if (is.numeric(yrStart) == FALSE || is.numeric(yrMax) == FALSE){
    stop("One of the year values is not numeric")
  }
  
  if (yrStart < 1947) {
    stop("No data available before year ", yrStart)
  }
  
  # Get year
  this.year <- Sys.time()
  this.year <- as.integer(format(this.year, "%Y"))
  
  
  if (yrMax > this.year) {
    
    stop(paste("Are you from the future? Please check your year interval; yrMax =",yrMax,"\n"))  
  }

  if (yrMax < yrStart) {
    stop("yrMax is smaller than yrMin!")
  }
  
  # Source main query function if it doesn't exist
  if(!exists('getCount', mode='function')){
  devtools::source_url("https://raw.githubusercontent.com/matiasandina/pubmed_query/master/src/getCount.R")
    
  }
  
  # Run getCount() for all query terms
  ## df <- plyr::ldply(query, getCount) how to do with multiple arguments?

  # Store the results of
  result_list <- lapply(query, function(x) getCount(x, yrStart, yrMax))
  
  # Give names to list
  names(result_list) <- query
  
  # Bind stuff
  
  df <- bind_rows(result_list, .id="query_term")
  
  
  ### Calculate relative frequencies ####
  ## Give a very basic plot
  
  if(!exists("total_table_updated")){
    
    # Let's source from GitHub to make sure the function works
    
    devtools::source_url("https://raw.githubusercontent.com/matiasandina/pubmed_query/master/src/get_totals.R")
    
    # Try reading it from data
    # If that fails, Call function to retrieve it
   
    total_table_updated <- tryCatch(
      {
        read.csv('./data/total_table_updated.csv')
      },
      error = function(e){
        get_totals(yrStart,yrMax)
      }
    )
    
    
    
    
  }
  
  df <- df %>% left_join(total_table_updated, by="year") %>%
    mutate(freq = count/total_count * 100) 
  
  
  lineplot <- df %>%
    ggplot(aes(year, freq, group=query_term, color=query_term)) +
    geom_line() +
    geom_point()+
    ylab("Frequency")+
    ggtitle('PubMed search hits by year', 
            subtitle = "Relative to total publication number.")
  
  print(lineplot)
  
  message("\nAll done!")
  return(df)
}



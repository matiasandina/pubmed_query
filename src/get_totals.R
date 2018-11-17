# create total_table ####
## This table appears to be counting less and/or Pubmed API counts more than once
## The difference for recent years is BIG!
## It could be because of lag to register or because electronic and print have 2 dates

# # Query
# tabs <- GET("http://www.nlm.nih.gov/bsd/medline_cit_counts_yr_pub.html")
# tabs <- readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F)
# 
# # Get first table (second element is garbage)
# total_table <- tabs[[1]]
# # change variable names
# names(total_table) <- c("year","total_count","us_counts","us_percent")
# 
# # remove asterisk
# total_table$year <- gsub("\\*","",total_table$year)
# # Replace commas
# # Not the best way but for quick fix
# total_table$total_count <- gsub(",","",total_table$total_count)
# total_table$us_counts <- gsub(",","", total_table$us_count)
# 
# # Transform everything into numeric (will introduce NAs)
# total_table <- total_table %>%
#   mutate_all(as.numeric) %>%
#   # fix us_percent column
#   mutate(us_percent = us_counts/total_count*100) %>%
#   filter(year>=1947)
# 
# 
# write.csv(total_table, "./data/total_table.csv", row.names = FALSE)


# Get updated total values by quering PubMed API --------------------------------------------------

get_totals <- function(yrStart, yrMax) {
  # add progressbar
  pb <- txtProgressBar(min = yrStart, max = yrMax, style = 3)
  # create empty data frame
  df <- data.frame(NULL)
  message("Getting updated yearly totals\n")
  
  # Start retrieval loop
  for(i in yrStart:yrMax) {
    # tell progressbar how it's going
    setTxtProgressBar(pb, i)
    # create query
    query.parsed <- paste(i, "%5Bppdat%5D", sep="")
    # Get XML
    pub.esearch <- GET(paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&rettype=count&term=", 
                                query.parsed, sep = ""))
    # Parse XML
    pub.esearch <- xmlTreeParse(pub.esearch, asText = TRUE)
    # Get number of hits from XML
    pub.count <- as.numeric(xmlValue(pub.esearch[["doc"]][["eSearchResult"]][["Count"]]))
    # Don't add anything if count is 0
    if (pub.count != 0) df <- rbind(df, data.frame("year" = i, "total_count" = pub.count))
    # Wait 0.5 sec
    Sys.sleep(0.5)
  }
  # close progressbar
  close(pb)
  # We want year as numeric. 
  df$year <- as.numeric(df$year)
  return(df)
}


# total_table_updated <- get_totals(1947,2017)

# write.csv(total_table_updated,
#          file="./data/total_table_updated.csv",
#          row.names=FALSE)

# compare counts from table with counts from my script -----------------------------------------
# # load(file="total_table")
# # load(file="total_table_updated")
# 
# df <- rbind(total_table[, 1:2], total_table_updated)
# df$id <- gl(2,nrow(total_table), labels=c("original", "updated"))
# df$year <- as.numeric(df$year)
# 
# # plot comparison
# ggplot(df, aes(year, total_count, group=id, color=id)) + 
#   geom_line() +
#   geom_point() + 
#   labs(title = "Pubmed total count (yearly)",
#        x = "Publication year",
#        y = "Total hits") +
#   scale_x_continuous(breaks=seq(1900,2017, 5))

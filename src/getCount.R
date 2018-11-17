### main search function #######
# --------------------------------------------------#

## curl = getCurlHandle() # reuse curl-connection

getCount <- function(query.term){
  
  # convert spaces to '+'
  query.gsub <- gsub(" ", "+", query.term)
  
  # convert some characters to brower friendly text (better to be safe than sorry)
  query.gsub <- gsub('"', "%22", query.gsub)
  query.gsub <- gsub("\\[", "%5B", query.gsub)
  query.gsub <- gsub("\\]", "%5D", query.gsub)
  
  # add progressbar
  pb <- txtProgressBar(min = yrStart, max = yrMax, style = 3)
  
  # create empty data frame
  df <- data.frame(NULL)
  message("Searching for: ", query.term,"\n")
  
  # Start retrieval loop
  for(i in yrStart:yrMax) {
    
    # tell progressbar how it's going
    setTxtProgressBar(pb, i)
    
    # add publication date [dp] to query
    query.parsed <- paste0(query.gsub, "+AND+",i, "%5Bppdat%5D")
    
    # Get XML with number of hits for query.parsed
    pub.esearch <- GET(paste0("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&rettype=count&term=", 
                              query.parsed))
                       # Parse XML
                       pub.esearch <- xmlTreeParse(pub.esearch, asText = TRUE)
                       # Get number of hits from XML
                       pub.count <- as.numeric(xmlValue(pub.esearch[["doc"]][["eSearchResult"]][["Count"]]))
                       
                       # Rbind results to df
                       df <- rbind(df, data.frame("year" = i, "count" = pub.count))
                       
                       # Wait 0.5 sec, you can safely query twice a second
                       Sys.sleep(0.5)
  }
  # close progressbar
  close(pb)
  return(df)
}


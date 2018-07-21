indeed <- function(title = NULL, loc = NULL, expLvl = NULL, type = NULL){
  url <- "https://www.indeed.com/jobs"
  if (is.null(loc))
    stop("Please Enter a valid Location for results")
  
  library(data.table)
  library(rvest)
  
  link <- createURL(loc = loc, jobTitle = title, type = type, expLvl = expLvl, site = url)
  print(link)
  html <- read_html(link)
  text <- html_text(html_nodes(html,"#searchCount"))
  numbers <- as.numeric(trimws(gsub("\\,","",(gsub("[[:alpha:]]+","",
                                                   gsub(".*of ","",text))))))
  paiginate <- round(numbers/10)
  data <- rbindlist(lapply(seq(1,paiginate,1), function(x){
    tryCatch({
      html <- read_html(paste0(gsub("\"","",link),"&start=",x*10))
      if (x == 0) html <- read_html(gsub("\"","",url))
      # company <- html_text(html_nodes(html,".company"))
      position <- html_text(html_nodes(html,".sjCapt .date , #sja5 , #sja4 , #sja3 , #sja1 , #sja2 , .sponsoredGray"))
      data <- html_text(html_nodes(html,".date , .sponsoredGray , .company, .jobtitle"))
      cleanData <- trimws(gsub("[[:cntrl:]]","",data))
      spn <- grep("sponsored",cleanData,ignore.case = T)
      number <- grep("[[:digit:]]",cleanData)
      cleanData[-c(spn,number)]
      
      
      d <- lapply(1:length(cleanData), function(x){
        if (grepl("sponsored",cleanData[x],ignore.case = T)) {
          if (grepl("[[:digit:]]",cleanData[x + 1])) {
            paste(cleanData[x - 1], cleanData[x], cleanData[x + 1], collapse = " ")
          }
        }else if (grepl("[[:digit:]]",cleanData[x]) & 
                  grepl("sponsored",cleanData[x-1], ignore.case = T)) {
          paste(cleanData[x - 1], cleanData[x], collapse = " ")
        }
        })
      
      
      
      results <- data.table(company,position)
      results <- cbind(results, daysAgo)
      results <- data.table(sapply(results,function(y){
        gsub("\\+","",trimws(gsub("[[:cntrl:]]","",y)))
      }))
    }, err = function(err){
      cat(conditionMessage(err))
    }
    )
    # results <- results[daysAgo != "Sponsored"]
    # results[,daysAgo := as.numeric(stringr::str_extract(daysAgo,"[[:digit:]]+"))]
    print(paste0(x," of ", max(paiginate)))
    if (x %% 2 == 1) Sys.sleep(0.2)
    results
  }))
}



createURL <- function(loc = NULL, jobTitle = NULL, type = NULL, expLvl = NULL, site = NULL){
  if (is.null(site)) stop("Site url Missing")
  if (is.null(loc)) stop("Location for Search Missing")
  if (is.null(jobTitle)) cat("Job Title Missing")
  
  location <- NULL
  if (grepl("indeed", site, ignore.case = T)) {
    
    if (grepl("\\,", loc)) location <- paste0(gsub(",.*", "", loc), "%2C+",
                                              gsub(".*,", "", loc))
    
    if (grepl(" ", loc)) location <- gsub(" ", "+", loc)
    if (is.null(location)) location <- loc
    
    location <- paste0("&l=", location)
    
    if (grepl(" ", jobTitle)) job <- gsub(" ", "+", jobTitle) else job <- jobTitle
    job <- paste0("?q=", job)
  }
  
  # if (grepl("monster",row$websiteList, ignore.case = T)){
  #   if (grepl(" ",loc)) location <- gsub(" ","-",loc)
  #   if (grepl("\\,",loc)) location <- gsub(",","__2C",loc)
  #   if (grepl("\\, ",loc)) location <- gsub(", ","__2C-",loc)
  #   
  #   location <- paste0("&where=",location,"&intcid=skr_navigation_nhpso_searchMain")
  #   
  #   if(grepl(" ",jobTitle)) job <- gsub(" ","-",jobTitle) else job <- jobTitle
  #   job <- paste0("?q=",job)
  # }
  
  link <- paste0(site,job,location)
  link
}

getStatesInfo_url <- "http://www.mastersindatascience.org/schools/new-york/"
get_US_States <- read_html(getStatesInfo_url) 

US_states <- get_US_States %>% html_nodes("#menu-states , .menu-item-350 , .menu-item-322 , a , .schoolinfo~ .schoolinfo .schoolname") %>% html_nodes("li a") %>% html_text()

df <- data.frame(SCHOOL=character(),
                 STATE=character(),
                 CITY=character(),
                 NOC=integer(),
                 PROGRAM=character(),
                 TYPE=character(),
                 DEPARTMENT=character(),
                 DELIVERY=character(),
                 DURATION=character(),
                 PREREQ=character(),
                 LINK=character(),
                 LOC_LAT=character(),
                 LOC_LONG=character(),
                 stringsAsFactors=FALSE)

for(states in US_states){

	ds_master_url=paste0("http://www.mastersindatascience.org/schools/",states)
	getDataSciUnivs <- read_html(ds_master_url) 
	getUnivStates1 <- getDataSciUnivs %>% html_nodes(".schoolinfo")
	NumSchools <- getUnivStates1 %>% html_name() %>% length()

	for(i in 1:NumSchools){
		
		schoolName <- getDataSciUnivs %>% html_nodes(".schoolinfo") %>% .[[i]] %>% html_nodes(".schoolname") %>% html_text()
		cityState <- getDataSciUnivs %>% html_nodes(".schoolinfo") %>% .[[i]] %>% html_nodes(".citystate") %>% html_text()
		tmp1 <- str_split(cityState,", ")
		city <- tmp1[[1]][1]
		state <- tmp1[[1]][2]
		loc_lat <- getDataSciUnivs %>% html_nodes(".schoolinfo") %>% .[[i]] %>% html_attr("data-attr-lat") 
		loc_long <- getDataSciUnivs %>% html_nodes(".schoolinfo") %>% .[[i]] %>% html_attr("data-attr-lon")
		NumOfCourses <- getDataSciUnivs %>% html_nodes(".schoolinfo") %>% .[[i]] %>% html_nodes(".programs") %>% html_children() %>% length()
  
		for(y in 1:NumOfCourses){
			
			program <- getDataSciUnivs %>% html_nodes(".schoolinfo") %>% .[[i]] %>% html_nodes("h4") %>% .[[y]] %>% html_text()
			link <- getDataSciUnivs %>% html_nodes(".schoolinfo") %>% .[[i]] %>% html_nodes("a") %>% .[[y]] %>% html_attr("href")
			
			offeredBy <- "Not Available"
			delivery <- "Not Available"
			duration <- "Not Available"
			
			progDetails <- getDataSciUnivs %>% html_nodes(".schoolinfo") %>% .[[i]] %>% html_nodes(".programs") %>% html_nodes(".schoolprogram") %>% .[[y]]%>% html_nodes(".programdetails")%>%html_nodes("div")%>%html_text()
			for(indProgDetail in progDetails){
			  if(grepl("OFFERED BY*",indProgDetail)){
			    offeredBy <- indProgDetail
			  }else if(grepl("DELIVERY*",indProgDetail)){
			    delivery <- indProgDetail
			  }else if(grepl("LENGTH*",indProgDetail)){
			    duration <- indProgDetail
			  }else if(grepl("PRE-REQUISITE TECHNICAL*",indProgDetail)){
			    preReq <- indProgDetail
			  }
			}
    
			if(grepl("*CERTIFICA*",toupper(program))){
				type <- "C"
			}else{
				type <-"M"
			}
    
			df <- rbind(df,
						data.frame( SCHOOL=schoolName,
									STATE=state,
									CITY=city,
									NOC=NumOfCourses,
									PROGRAM=program,
									TYPE=type,
									DEPARTMENT=offeredBy,
									DELIVERY=delivery,
									DURATION=duration,
									PREREQ=preReq,
									LINK=link,
									LOC_LAT=loc_lat,
									LOC_LONG=loc_long,
									stringsAsFactors=FALSE))
		}
	}
}

  
  
  

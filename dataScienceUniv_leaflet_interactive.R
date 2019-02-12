library(leaflet)
library(plyr)
library(dplyr)
setwd("D:/Studies MS/Datascience Shiny Proj/DataScienceUnivsShiny")
DataScience_Univs <- read.csv(file="data/timesMergedData.csv",stringsAsFactors = FALSE)
#DataScience_Univs[is.na(DataScience_Univs)] <- 0
tab1 <- DataScience_Univs %>% filter(YEAR==2016)
tab2 <- DataScience_Univs[is.na(DataScience_Univs$YEAR),]
DataScience_Univs2016 <- rbind(tab1,tab2)
DSUnivs_unique2016 <- unique(DataScience_Univs2016[,c("SCHOOL",
                                                      "STATE",
                                                      "LOC_LAT",
                                                      "LOC_LONG",
                                                      "TEACHING",
                                                      "RESEARCH",
                                                      "INTERNATIONAL",
                                                      "INCOME",
                                                      "CITATIONS",
                                                      "WORLD_RANK")])

# We are trying to display locations of  Universities across United States that offer Graduate Data Science
#Program both online and in-school. The data is collected in a csv format. The data was  webscraped from the 
# from the website "www.mastersindatascience.org",this data is merged with University ranking data obtained
#from the website "Times Higher Education".The data used for current demonstration is of year 2015.
#The data consists of columns like School,state, etc. We have customised the marker popup which indicates
#University world rankings based on the attributes like Teaching, Research, Citation etc, Also it displays the links to data science programs. The markers 
#are positioned based on the latitude and longitude coordinates obtained from scraped data. 


leaflet(DSUnivs_unique2016) %>% 
  addTiles() %>% 
  addMarkers(lng = ~LOC_LONG, lat = ~LOC_LAT,
             popup = ~paste0("<table>
                                       <tr>
                             <td>
                             <svg width='130' height='60'>
                             <rect x='5' y='5' rx='20' ry='20' width='100' height='50' style='fill:black;stroke:gray;stroke-width:5'/>
                             <text x='53' y='37'
                             font-family='arial black'
                             font-size='20px'
                             text-anchor='middle'
                             fill='white'>",WORLD_RANK,"</text>
                             </svg>
                             </td>
                             <td align='left' style='width:100px'><strong>",SCHOOL,"</strong></td> 
                             <tr>
                             <td>Teaching:</td>
                             <td align='left'><meter value='",TEACHING,"' min='0' max='100'></meter>&nbsp",TEACHING,"</td>
                             </tr>
                             <tr>
                             <td>Internarional Outlook:</td>
                             <td align='left'><meter value='",INTERNATIONAL,"' min='0' max='100'></meter>&nbsp",INTERNATIONAL,"</td>    
                             </tr>
                             <tr>
                             <td>Industry Income</td>
                             <td align='left'><meter value='",INCOME,"' min='0' max='100'></meter>&nbsp",INCOME,"</td>
                             </tr>
                             <tr>
                             <td>Research</td>
                             <td align='left'><meter value='",RESEARCH,"' min='0' max='100'></meter>&nbsp",RESEARCH,"</td>
                             </tr>
                             <tr>
                             <td>citations</td>
                             <td align='left'><meter value='",CITATIONS,"' min='0' max='100'></meter>&nbsp",CITATIONS,"</td>
                             </tr>
                             </table>"
                             ),
                       clusterOptions = markerClusterOptions())

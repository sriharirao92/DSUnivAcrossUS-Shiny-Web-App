library(shiny)
library(leaflet)
library(plyr)
library(dplyr)
library(shinythemes)
library(maps)
library(DT)
library(googleVis)

DataScience_Univs <- read.csv("data/timesMergedData.csv",stringsAsFactors = FALSE)
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


############################################################################ ui

ui <-   fluidPage(theme = shinytheme("spacelab"),
                    
          navbarPage("DataScience Universities across US",
             
            tabPanel("Overview",
                      br(),br(),
                      leafletOutput("clusterMap"),
                      br(),br(),
                      tags$div(style="color:black;",
                      tags$li("Above leaflet map shows interactive cluster of universities accross US"),
                      tags$li("Zoom in to see more details and click on markers to see university name, state"))
            ),
      
            tabPanel("By State",
              sidebarLayout(
                sidebarPanel(
                  selectInput("state", "Select state:",DSUnivs_unique2016 %>% distinct(STATE)),
                  uiOutput("listShow")
                ),
                mainPanel(
                  leafletOutput("StateWise")
                )
              )
            ),
      
            tabPanel("By University",
              column(5,
                     selectInput("university", "Select university:",width='100%',DataScience_Univs %>% distinct(SCHOOL)),
                     p("Click on the below marker to see world ranking and 2016 details:",style="font-weight: bold;background-color:powderblue;text-align:center;"),
                     leafletOutput("univWise")),
              column(7,
                     uiOutput("univDetails"),
                     p(),
                     p("Total Score over years:",style="font-weight: bold;font-size: 150%;"),
                     plotOutput("scoreChart"),
                     p(),
                     p("Stats for nerds:",style="font-weight: bold;font-size: 150%;"),
                     p(),
                     dataTableOutput("table2"))
            ),
      
            tabPanel("Data Explorer",
                     p(align="center",style="font-size:20px;font-weight:bold","Universities offering DataScience programs with world rankings [2016 Data]"),
                     br(),
                     dataTableOutput("table1")
            )
          )
        )


############################################################################### server

server <- function(input,output,session){
            
            output$clusterMap <- renderLeaflet({
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
            })
            
            output$StateWise <- renderLeaflet({
            stateDetails <- map("state",input$state)
            stateDetails <- data.frame(stateDetails[1:2])
            DSUnivs_stateFilter <- DSUnivs_unique2016 %>% filter(STATE==input$state)
            leaflet(DSUnivs_stateFilter) %>% 
            addTiles() %>% 
            addPolylines(stateDetails$x,stateDetails$y) %>% 
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
                       ))
            })
            
            
            output$univWise <- renderLeaflet({
              DSUnivs_univFilter <- DSUnivs_unique2016 %>% filter(SCHOOL==input$university)
              leaflet(DSUnivs_univFilter) %>% 
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
                           ))
            })
            
            output$table1 = renderDataTable(arrange(DataScience_Univs2016[,c(14,1:10,16:19:26)],WORLD_RANK),
              options = list(pageLength=35,
                             scrollX = TRUE,
                             rowCallback = DT::JS('function(row) {
                                                  $("td", row).css("color","#111111");
                                                  $("label").css("color","#111111");
                                                  $("select").css("color","#111111");}')))
            
            output$listShow <- renderUI({
              DSUnivs_stateList <- DataScience_Univs2016 %>% filter(STATE==input$state)
              l <- unique(DSUnivs_stateList[,c("SCHOOL")])
              content <- ""
              for(j in l){
                j_filtered <- DSUnivs_stateList %>% filter(SCHOOL==j)
                content <- paste0(content,"<h2>",j,"</h2><ul>")
                n <- NROW(j_filtered)
                for(i1 in 1:n){
                  k <- j_filtered[i1,]
                  content <- paste0(content,"<li><a href='",k$LINK,"'>",k$PROGRAM,"</a></li>")
                }
                content<-paste0(content,"</ul>")
              }
              HTML(content)
            })
            
            output$scoreChart <- renderPlot({
              univFilter <- DataScience_Univs %>% filter(SCHOOL==input$university)
              totalScore <- unique(univFilter[,c("YEAR","TOTAL_SCORE")])
              totalScore <- transform(totalScore,TOTAL_SCORE=as.numeric(TOTAL_SCORE)) %>% arrange(YEAR)
              plot(totalScore,type="o")
            })
            
            output$table2 = renderDataTable(
              arrange(unique((DataScience_Univs %>% filter(SCHOOL==input$university))[,c(26,16:25)]),YEAR),
              options = list(pageLength=35,
                             scrollX = TRUE,
                             rowCallback = DT::JS('function(row) {
                                                  $("td", row).css("color","#111111");
                                                  $("label").css("color","#111111");
                                                  $("select").css("color","#111111");}'))
            )
            
            output$univDetails <- renderUI({
              DSUnivs_univDetails <- DataScience_Univs2016 %>% filter(SCHOOL==input$university)
              content1 <- ""
              content1 <- paste0(content1,"<h2 align='center'>",unique(DSUnivs_univDetails$SCHOOL),"</h2>")
              content1 <- paste0(content1,"<p style='font-weight: bold;font-size: 150%;'>Programs Offered:</p>")
              n1 <- NROW(DSUnivs_univDetails)
              for(i2 in 1:n1){
                  k1 <- DSUnivs_univDetails[i2,]
                  content1 <- paste0(content1,"<li><a href='",k1$LINK,"'>",k1$PROGRAM,"</a></li>")
                }
                content1 <- paste0(content1,"</ul>")
              
              HTML(content1)
            })
            
          }

#################################################################################

shinyApp(ui = ui,server = server)
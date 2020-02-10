#Author: Joshua Rowan
#UIN:667041601 UID: jrowan4
#Date: 2-10-2020
#Desc: Shiny/R program to pull in data from litterati site and display interactive graphs of said data-
#      designed for large format screen of 11,520 pixels by 3160 pixels
library(shiny)
library(lubridate)
library(ggplot2)
library(leaflet)
library(rsconnect)
library(shinyalert)
library(scales)
# rsconnect::setAccountInfo(name='jrowan4',
#                           token='035896C80F5449829720C0060A45253B',
#                           secret='wkSb/BagexLSoMKxBDwndMk8p4L2vjcQAlGbc/Ph')




#pulls in the data file
mydata <- read.csv(file = "litterati challenge-65.csv", stringsAsFactors=FALSE)

#removes excessive tags
mydata$tags<-gsub(",.*","",mydata$tags)
#sets empty tags to "untagged"
mydata$tags<- sub("^$", "untagged",mydata$tags)

mydata$username<-sub("litterati-","Pick",mydata$username)



#eliminates out of area lines
mydata<- mydata[mydata$lat > 41.85,]
mydata<- mydata[mydata$lat < 41.88,]
mydata<- mydata[mydata$lon > -87.82,]
mydata<- mydata[mydata$lon < -87.75,]

#generates tag dataframe, sorts by amount and takes top ten
tags<-mydata$tags
tagSet <- as.data.frame(sort(table(tags),decreasing=T))
topTenTags <- head(tagSet,10)
### used only for initial lists

#makes list of names to used for selection
names <-subset(mydata, select = "username")

#makes 2 new columns: Date and Time and pulls in data as time and date
mydata$Date <- as.Date(mydata$litterTimestamp)
mydata$Time <- format(as.POSIXct(mydata$litterTimestamp),format = "%H:%M:%S", tz = "America/Chicago")


#data frame of all litter picked up by day
allDays<- as.data.frame(table(subset(mydata, select = "Date")))

#r_colors<-rgb(t(col2rgb(colors())/255))#### code from other mapping function.
#names(r_colors) <- colors()            ##### still in just in case

##size for layout for graphs and tables
winSize<-"950px"
loSize<- "1000px"
tableSize<-"300px"

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    
    
    # Application title
    titlePanel("Litterati users"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            textOutput("litterTotal"),
            selectInput("selectUsers",
                        "Names of users:",
                        c("All",names), width = "200px"), 
            selectInput("selectTags",
                        "Types of Litter:",
                        c("All",tags),
                        width = "200px"),
            useShinyalert(),
            actionButton("aboutPage", "About"),
            width=3),
            
        
        # main panel with all relevant data to be plotted and displayed
        mainPanel(
          splitLayout(cellWidths = c(loSize,loSize,loSize,loSize,loSize,loSize,tableSize,tableSize,tableSize,tableSize,tableSize),
            leafletOutput("mymap", width =winSize , height = winSize),        
            plotOutput("userPlot",width = winSize,height=winSize),
            plotOutput("timePlot", width = winSize, height =winSize),
            plotOutput("weekPlot", width = winSize, height = winSize),
            plotOutput("tagPlot", width = winSize, height = winSize),
            plotOutput("allDaysPlot", width = winSize, height=winSize),
            tableOutput("UsersTable"),
            tableOutput("TimeTable"),
            tableOutput("WeekTable"),
            tableOutput("TagTable")
          )
        )
    
                      
          
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  #####Top Ten Users Bar Chart
    TopTenUsersData <- reactive({
      
      if (input$selectTags != ""&& input$selectTags !="All"){
        newData<- mydata[which(mydata$tags == input$selectTags),] #filters dataframe when a tag is selected
      }else{
        newData<-mydata
      }
        #creates topten user data frame
      names <-subset(newData, select = "username")
      nameSet <- as.data.frame(sort(table(names),decreasing=T))
      TopTen <- head(nameSet,10)
        #checking for empty frame in case user is at 0 and generating empty frame
      if (nrow(TopTen) ==0){
        TopTen<-data.frame(names = c("No Data"), Freq=c(0))
      }
          #updates data frame for selected user from list
      if ((input$selectUsers != "" && input$selectUsers !="All")){
        selectedName<-nameSet[which(nameSet$names == input$selectUsers),]
        if(is.null(selectedName)){#checking for empty data frame
          
          TopTen
        }else{
        rbind(TopTen,selectedName)
        }  
      }else{
        TopTen
      }
    })
    
    output$userPlot <- renderPlot({
        ggplot(TopTenUsersData(), aes(x=names,y=Freq)) + geom_bar(stat="identity",fill = "#72B000") + labs(x = "Top ten pickers", y = "Litter picked up by picker")
    })
    output$UsersTable<- renderTable({data =TopTenUsersData()})
    
    
    #####Hourly Bar Chart
    HourlyData <- reactive({
      
      if ((input$selectTags != ""&& input$selectTags !="All")){
        newData<- mydata[which(mydata$tags == input$selectTags),]
      }else{
        newData<-mydata
      }
      
      
      if ((input$selectUsers != ""&& input$selectUsers !="All")){
        newData<-newData[which(newData$username == input$selectUsers),]
      }
      
      h<-hms(newData$Time)
      f<-data.frame( Hours = c(0:23))
      tableOfHours<- as.data.frame(table(Hours = hour(h)))
        if (nrow(tableOfHours)==0){
          tableOfHours<-data.frame(Hours=c(0:23),Freq=c(0))
        }else{
        v<-merge(x=f,y=tableOfHours,by="Hours",all=TRUE)
        v[is.na(v)]<-0
        tableOfHours<-v
        
        }
        tableOfHours
    })
    
    output$timePlot <-renderPlot({
        ggplot(HourlyData(), aes(x=Hours, y=Freq)) + geom_bar(stat="identity",fill="steelblue") + labs(x = "Hour of day (24 hr time)", y ="Litter Amount")  
    })
    output$TimeTable<- renderTable({data =HourlyData()})
    
    
    ####Weekly Bar Chart
    WeeklyData<- reactive({
      if ((input$selectTags != ""&& input$selectTags !="All")){
        newData<- mydata[which(mydata$tags == input$selectTags),]
      }else{
        newData<-mydata
      }
      
      if ((input$selectUsers != ""&& input$selectUsers !="All")){
        newData<-newData[which(newData$username == input$selectUsers),]
      }
      
      f<- data.frame(DoW=c(1:7)) ## creates  definite 7 days a week, regardless of how many
      d<-data.frame(table(DoW=wday(ymd(newData$Date))))
      
      if(nrow(d)==0){
        d<-data.frame(DoW = c(1:7), Freq= c(0))
      }
      
      v<-merge(x=f,y=d,by="DoW",all=TRUE)
      v[is.na(v)]<-0
      w<-data.frame(days=c("Mon","Tue","Wed","Thur","Fri","Sat","Sun"))
      z<-cbind(w,v)
      z$days<-factor(z$days, levels=c("Mon","Tue","Wed","Thur","Fri","Sat","Sun"))
      
      z
      
    })
    
    output$weekPlot<-renderPlot({
        ggplot(WeeklyData(), aes(x=days, y=Freq)) + geom_bar(stat="identity",fill ="#880000") + labs(x="Days of the Week", y= "Amount picked up on day")
    })
    output$WeekTable<- renderTable({data =WeeklyData()})
    
    #######Tags outputand plot
    TagData<-reactive({
      if ((input$selectUsers != ""&& input$selectUsers !="All")){
        newData<- mydata[which(mydata$username == input$selectUsers),]
        
      }else{
        newData<-mydata
      }
      
      tagList<-as.data.frame(subset(newData, select ="tags"))
      setoftags <- as.data.frame(sort(table(tagList),decreasing=T))
      Listoftags <- head(setoftags,10)
      
      if ((input$selectTags != "" && input$selectTags !="All")){
        selectedTag<-setoftags[which(setoftags$tagList == input$selectTags),]

        if(nrow(selectedTag)==0){
          Listoftags
        }else{
          Listoftags<-rbind(Listoftags,selectedTag)
        }
      }
        Listoftags
      })
    output$tagPlot <-renderPlot({
        ggplot(TagData(), aes(x=tagList, y=Freq)) + geom_bar(stat= "identity", fill ="#000066") + labs (x="Tags", y="Number of items with tag")
    })
    output$TagTable<- renderTable({data =TagData()})
    
    ########All days plotted
    AllDaysData<-reactive({
      
      if ((input$selectTags != ""&& input$selectTags !="All")){
        newData<- mydata[which(mydata$tags == input$selectTags),]
      }else{
        newData<-mydata
      }
      
      if ((input$selectUsers != ""&& input$selectUsers !="All")){
        newData<-newData[which(newData$username == input$selectUsers),]
      }
      
      if(nrow(newData)==0){
        a<-as.Date(c("2018-4-4","2020-1-7"),"%Y-%m-%d")
        allDays<-data.frame(Var1=a,Freq=c(0,0))
        test<-head(allDays,10)
      print(test)
      }else{
      allDays<- as.data.frame(table(subset(newData, select = "Date")))
      }
      allDays$Var1<-as.Date(allDays$Var1, "%Y-%m-%d")
      allDays
      
    })
    output$allDaysPlot<-renderPlot({
        ggplot(AllDaysData(), aes(x=Var1, y=Freq)) + geom_bar(stat= "identity", fill="#004400") + labs (x="Days in file", y="Litter picked up per day") +
        theme(axis.text.x = element_text(angle=45, hjust=1)) + scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y"))
    })
    
    
    #####total litter
    output$litterTotal<-renderText({
      
      paste("All litter picked: ", nrow(mydata))
    })
    
    MapData<-reactive({
      if ((input$selectTags != ""&& input$selectTags !="All")){
        newData<- mydata[which(mydata$tags == input$selectTags),]
      }else{
        newData<-mydata
      }
      
      if ((input$selectUsers != ""&& input$selectUsers !="All")){
        newData<-newData[which(newData$username == input$selectUsers),]
      }
      posOut<-data.frame(lat = newData$lat, lng=newData$lon)
      
    })
        #output for map at begining of set.
    output$mymap <- renderLeaflet({
      leaflet()%>% addTiles() %>% 
        addMarkers(clusterOptions = markerClusterOptions(),
                   data=MapData())
    })
    
    observeEvent(input$aboutPage,{
      
      shinyalert("About Page","Written by Joshua Rowan.\n Libraries used: ggplot, lubridate, shiny, shinyAlert, leaflet and scales\n
                 Data from https://www.evl.uic.edu/aej/424/litterati challenge-65.csv\n Written in Rstudio, 2-10-2020")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
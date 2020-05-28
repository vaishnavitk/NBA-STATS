#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(scales)

# Defining UI for application 
ui <- fluidPage(
    dashboardPage(
        dashboardHeader(title="NBA Visualizations"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Player Info",tabName="playerinfo",icon=icon("basketball-ball")),
                menuItem("Player Attributes",tabName="playerattributes",icon=icon("user")),
                menuItem("Player Efficiency",tabName="playerefficiency",icon=icon("user-shield")),
                menuItem("Team Stats",tabName="teamstats",icon=icon("steam-symbol")),
                menuItem("Game Trend",tabName="gametrend",icon=icon("chart-line"))
                                
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "playerinfo",
                        fluidRow(
                            box(width=12,status = "warning", column(6,align="left",selectInput("PlayerId","player",choices=sort(unique(playerinfo$player_name))))),
                            box(width=6,status="warning",plotOutput("Playerplot1")),
                            box(width=6,status="warning",plotOutput("Playerplot2"))
                        )
                        ),
                tabItem(tabName = "teamstats",
                        fluidRow(
                            box(width=12,status = "warning", column(6,align="left",selectInput("TeamId","Team",choices=sort(unique(teamstats$TEAM))))),
                            box(width=6,status="warning",plotOutput("Teamplot1")),
                            box(width=6,status="warning",plotOutput("Teamplot2"))
                        )
                ),
                tabItem(tabName = "gametrend",
                        fluidRow(
                            box(width=4,status = "warning", column(8,align="left",selectInput("TeamId1","Team1",choices=sort(unique(gametrend$TEAM))),
                            uiOutput("new_block"))),
                            box(width=3,status = "warning", column(8,align="left",selectInput("PeriodId","Quarter",choices=c(1,2,3,4)))),
                            box(width=12,status="warning",plotOutput("Gameplot1")),
                            box(width=12,status="warning",plotOutput("Gameplot2"))
                            )               
            ),
            tabItem(tabName = "playerattributes",
                    fluidRow(
                        box(width=12,status="warning",plotlyOutput("playerattributes"))
                    ) 
        ),
        tabItem(tabName = "playerefficiency",
                fluidRow(box(width=12,status = "warning", column(6,align="left",selectInput("TeamId1","Team",choices=sort(unique(playerefficiency$TEAM))),
                         radioButtons("result", "Game Result",
                                      c("Won" = "Won",
                                        "Lost" = "Lost")))),
                         
                         box(width=12,status="warning",plotOutput("playerefficiency"))
                )
        
    )
)
))
)



# Defining server logic required for all the plots
server <- function(input, output) {
    #To calculate the points scored by each played when played against each team
    output$new_block<-renderUI({
        data <-gametrend %>% filter(OPPOSITION_TEAM != input$TeamId1) %>% select(OPPOSITION_TEAM)
        selectInput("TeamId2","Team2",choices=sort(unique(data$OPPOSITION_TEAM)))
    })
    output$Playerplot1<-renderPlot({
        data<-playerinfo %>% filter(player_name == input$PlayerId) %>% select(OPPOSITION_TEAM,PTS)
        data<-summarize(group_by(data,OPPOSITION_TEAM),Points = sum(PTS,na.rm=TRUE))
        ggplot(data,aes(reorder(OPPOSITION_TEAM,Points),Points,fill=Points))+geom_bar(stat="identity")+coord_flip()+ggtitle("Points scored when played against each team")+theme(text=element_text(size=7.99))+xlab("Points")+ylab("Teams")+scale_fill_gradient(low="#FF7F50",high="#FF4500")+theme(plot.title = element_text(hjust = 0.5))
    })
    #Percentage of 2-point and 3-point shots hit by the player
    output$Playerplot2<-renderPlot({
        data<-shooting_made %>% filter(player_name == input$PlayerId) %>% select(PTS,all_points,lab.ypos)
        ggplot(data,aes(x="",y=all_points,fill=factor(PTS)))+geom_bar(width=1,stat="identity",color="white")+ggtitle("                              Percentage of 2-point/3-point shots")+xlab("")+ylab("")+labs(fill="Shot Type")+coord_polar("y",start = 0)+geom_text(aes(y=lab.ypos,label=percent_format()(round(all_points,2)/100)),color="white")+scale_fill_manual(values = c("#4B0082","#A52A2A")) +
            theme_void()
    })
    #Final margin of the team when played against other teams
    output$Teamplot1<-renderPlot({
        data<-teamstats %>% filter(TEAM == input$TeamId) %>% select(OPPOSITION_TEAM,FINAL_MARGIN)
        data<-summarize(group_by(data,OPPOSITION_TEAM),margin = sum(FINAL_MARGIN,na.rm=TRUE))
        ggplot(data,aes(reorder(x=OPPOSITION_TEAM,margin),y=margin,fill=ifelse(data$margin<0,"Negative Margin","Positive Margin")))+geom_bar(stat="identity")+coord_flip()+theme(text=element_text(size=7.99))+ggtitle("Final Margins of the team when played against other teams ")+theme(plot.title = element_text(hjust = 0.5))+ylab("Final Margin")+xlab("Teams")+labs(fill="Points")+scale_fill_manual(values = c("firebrick","blue4"))+guides(fill=guide_legend(reverse = T))
    
    })
    #Number of wins in home and away from home
    output$Teamplot2<-renderPlot({
        data<-teamstats %>% filter(TEAM == input$TeamId) %>% count(LOCATION,W)
        ggplot(data,aes(reorder(x=LOCATION,n),y=n,fill=ifelse(LOCATION=="Out-of-State","Out-of-State","Home")))+geom_bar(stat="identity")+ggtitle("Number of matches won per location")+theme(text=element_text(size=7.99))+xlab("Location")+ylab("Number of matches won")+theme(plot.title = element_text(hjust = 0.5))+labs(fill="Location")+scale_fill_manual(values = c("springgreen4","#D2691E"))
    })
    #To analyse the type of shots hit during every interval of time
    output$Gameplot1<-renderPlot({
        data<-gametrend %>% filter(TEAM == input$TeamId1 & OPPOSITION_TEAM==input$TeamId2 & PERIOD==input$PeriodId) %>% select(GAME_CLOCK,PTS,PERIOD)
        ggplot(data,aes(GAME_CLOCK,PTS))+scale_x_continuous(breaks=c(0,2,4,6,8,10,12))+scale_y_continuous(breaks=c(0,2,3))+geom_line(color="#8B4513")+geom_point(color="#8B4513",size=2)+theme(text=element_text(size=7.99)) +theme(plot.title = element_text(hjust = 0.5))+xlab("Game Clock")+ylab("Type of Shot scored")+ggtitle("Type of Shots scored through out the quarter")
    })
    #Range of shot distances
    output$Gameplot2<-renderPlot({
        data<-shotrange %>% filter(TEAM == input$TeamId1 & OPPOSITION_TEAM==input$TeamId2 & PERIOD==input$PeriodId) %>% select(SHOT_CLOCK,SHOT_DIST)
        ggplot(data,aes(SHOT_CLOCK,SHOT_DIST))+scale_x_continuous(limits=c(0,24))+scale_y_continuous(limits=c(0,30))+geom_line(color="#8B4513")+theme(text=element_text(size=7.99)) +theme(plot.title = element_text(hjust = 0.5))+xlab("Shot Clock")+ylab("Shot Distance")+ggtitle("Range of shot distance for every time interval")
    })
    #Best Two-way players (To check how a player scores a goal and defends a goal)
    output$playerattributes<-renderPlotly({
            data<-ggplot(playerattributes,aes(x=pts_allowed, y=pts_scored,text=paste(player_name),color=ifelse((pts_allowed<mean(pts_allowed)&pts_scored>mean(pts_scored)),"Efficient","Others"))) +geom_point()+coord_flip()+theme(text=element_text(size=7.99)) +theme(plot.title = element_text(hjust = 0.5))+xlab("Points Allowed")+ylab("Points Scored")+ggtitle("Best Two-Way Players")+labs(color="")+guides(colour = guide_legend(reverse=T)) 
        ggplotly(data,tooltip = c("text"))
        
            })
    #percentage of points scored by the player from the total score
    output$playerefficiency<-renderPlot({
        data<-playerefficiency %>% filter(TEAM == input$TeamId1 & W==input$result) %>% select(player_name,usage_rate)
        ggplot(data,aes(reorder(x=player_name,-usage_rate),y=usage_rate,color=usage_rate))+geom_point(size=2.5)+theme(text=element_text(size=7.99))+ggtitle("Contribution of each player towards the score")+theme(plot.title = element_text(hjust = 0.5))+ylab("Contribution rate")+xlab("Players")+scale_color_gradient(low="#9370DB",high="#8A2BE2")+labs(color="Contribution Rate")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

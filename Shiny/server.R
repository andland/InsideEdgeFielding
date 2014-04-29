# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output,session) {  
#     output$playsPlot <- renderPlot({
#       song.per.day=sqldf(paste0("Select Day, Song, Count(Song) as Num
#                           From playlist
#                           Where Artist='",toupper(input$artist),"'
#                           Group By Day, Song
#                           Order by Day, Song"))
#       dspd=dcast(song.per.day,Day~Song,sum,value.var="Num")
#       
#       song.per.day=merge(plays.per.day[,1,drop=FALSE],dspd,all.x=TRUE)
#       song.per.day[is.na(song.per.day)]=0
#       # which(apply(song.per.day[,-1],2,max)>=2)
#       # cbind(1:23,rank(-colSums(song.per.day[,-1])),colSums(song.per.day[,-1]))
#       if (ncol(song.per.day)>2) {
#         song.per.day=song.per.day[,c(1,which(rank(-colSums(song.per.day[,-1]))<=input$topSongs)+1)]
#       }
#       
#       song.per.day=melt(song.per.day,1,variable.name="Song",value.name="Num")
#       song.per.day$Alpha=ifelse(song.per.day$Num>0,1,0)
#       
#       p<-ggplot(song.per.day,aes(Day,Num,colour=Song))+geom_point(aes(alpha=Alpha))+
#         geom_smooth(method="gam",family=poisson,formula=y~s(x),se=F,size=1)+
#         labs(x="Date",y="Plays Per Day",title=input$artist,colour=NULL)+
#         scale_alpha_continuous(guide=FALSE,range=c(0,.5))
#       
#       print(p+theme_bw())
#     },width=650,height=400)
  
  output$leaderboard = renderDataTable({
    dat=subset(full.dat,Innings>=input$innings[1] & Innings<=input$innings[2])
    if (input$pos != "All") {
      dat=subset(dat,Position==input$pos)
    }
    if (input$team != "All") {
      dat=subset(dat,Team==input$team)
    }
    
    if (input$type=="rate") {
      mult=ifelse(dat$Position=="P",30*6,150*8.5)
    } else {
      mult=dat$Innings
    }
    dat$PlaysAboveAvg=round(dat$PlayProbDiff*dat$PPI*mult,2)
    dat$PlaysUpper=round(dat$PlayProbDiffUpper*dat$PPI*mult,2)
    dat$PlaysLower=round(dat$PlayProbDiffLower*dat$PPI*mult,2)
    
    dat[order(-dat$PlaysAboveAvg),c("Name","Position","Team","Innings","PlaysAboveAvg","PlaysLower","PlaysUpper")]
  }, 
  options = list(bSortClasses = TRUE, aLengthMenu = c(15, 30, 50), iDisplayLength = 30))
  
})

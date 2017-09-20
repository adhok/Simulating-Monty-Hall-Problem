


win <<-0
lose <<-0
n <- 0
data_win_lose <- data.frame(win=as.numeric(),lose=as.numeric())


while(n==0 | !is.numeric(n)){
  
  n <- readline(prompt = "How many doors do you want?")
  n <- ifelse(grepl("\\D",n),-1,as.integer(n))
  
  
  
  
}
N <<- as.numeric(n)
number_of_simulations <- 10000
for(i in 1:number_of_simulations){
  
  source("monty_hall_sim.R",local=T)
  
}

#cat("\n")

#cat("The win rate is ",win*100/(win+lose),"%")
plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 10,colour = "black",hjust=0.5),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=8),
    axis.title = element_text(size=5),
    axis.text = element_text(size=5),
    axis.title.x = element_text(hjust=1),
    axis.title.y = element_text(hjust=1),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "bold"),
    legend.text = element_text(colour = "black", face = "bold")
    )
}
print(names(data_win_lose))

# data_win_lose %>% mutate(index=1:n(),X0=cumsum(X0),X1=cumsum(X1))
#   rename(win=X1,lose=X0) %>% tidyr::gather(type,value,1:2) %>%
#   ggplot(aes(x=index,y=value,color=type))+geom_line()+geom_point()+
#   plotTheme()+labs(title=paste("Cumulative Wins and Losses in Monty Hall Game of",N,"doors and 1000 trials"))

library(tweenr)

tw_df<- data_win_lose %>% mutate(index=1:n(),win=cumsum(win),lose=cumsum(lose))%>% mutate(per_win=win/index,per_loss=lose/index) %>% select(per_win,per_loss,index) %>%
  split(.$index) %>% tween_states(tweenlength=2,statelength=3,ease=rep('cubic-in-out',3),nframes=150)

tw_df <- tw_df %>% tidyr::gather(type,per,1:2)
library(ggplot2)

library(animation)
#con <- Sys.which("magick")
oopt = ani.options(interval = 0.15)
saveGIF({for (i in 1:max(tw_df$.frame)) {
  temp <- subset(tw_df,.frame==i)
  g_bar <- ggplot(data=temp ,aes(x=type,y=per,fill=type))+
    geom_bar(stat="identity",alpha=0.4)+labs(title=paste("Percentage Wins and Losses in trial",temp[1,]$index))+geom_text(aes(label=paste(round(per*100,2),"%")), vjust=1.5, colour="black",size=2.5)+plotTheme()+
    scale_fill_manual(values=c("#24576D", "#A113E2"))+theme(panel.grid.major = element_blank(),
                                                                     panel.grid.minor = element_blank(),axis.title.y=element_blank(),
                                                                     axis.text.y=element_blank(),
                                                                     axis.ticks.y=element_blank())+scale_y_continuous(limits=c(0,1.0))

  print(g_bar)
  print(paste(i,"out of",max(tw_df$.frame)))
  ani.pause()}
},movie.name="letsee.gif",ani.width = 650, ani.height = 600)

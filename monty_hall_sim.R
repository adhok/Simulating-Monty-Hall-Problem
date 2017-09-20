library(dplyr)


#set.seed(100)

#n <-6
#N <- 100
car_door <- sample(1:N,size=1,replace=F,prob = seq(1/N,N))


seq_door <- sample(1:N,size=N,replace=F,prob = seq(1/N,N))
choose_door <- sample(1:N,size = 1,replace=F,prob = seq(1/N,N))




choose_door <- as.numeric(choose_door)


if(choose_door==car_door){
  
  
  data_monty <- data.frame(seq_door)
  t <- data_monty %>% filter(seq_door!=choose_door) %>% select(seq_door)
  seq_remaining <- as.numeric(t$seq_door)
  monty_choose <- sample(seq_remaining,size=N-2,replace=F)
  
  
    

  
  
  
}  else {
  
  data_monty <- data.frame(seq_door)
  t <- data_monty %>% filter(seq_door!=choose_door&seq_door!=car_door) %>% select(seq_door)
  seq_remaining <- as.numeric(t$seq_door)
  
  monty_choose <- sample(seq_remaining,size=N-2)
  


  
  
}



data_monty <- data.frame(seq_door)

contestent_choice <- data_monty %>% filter(!seq_door %in% monty_choose) %>% filter(seq_door!=choose_door)
contestent_choice <- contestent_choice$seq_door

if(contestent_choice==car_door){
  data_win_lose <- rbind(data_win_lose,c(1,0))
  names(data_win_lose) <- c("win","lose")
  
  
}else{
  data_win_lose <- rbind(data_win_lose,c(0,1))
  names(data_win_lose) <- c("win","lose")
}





######### Helpers with Date ##########
monnb <- function(d){
  lt <- as.POSIXlt(as.Date(d, origin = "1900-01-01"))
  lt$year*12 + lt$mon
}

mondf <- function(d1, d2){
  monnb(d2) - monnb(d1)
}

add.months <- function(date, n){
  if(day(date) > 28){
    day(date) <- day(date) - 7
  }
  format(seq(date, by = paste(1, "months"), length = (n+1)), "%Y-%m")
}

# mon_df <- mondf(dat$StartDate[1], dat$EndDate[1])
# add.months(dat$StartDate[1], mon_df)
# date <- dat$StartDate[3]
# add.months(date, mon_df)


##### Long ES Data Set #####
long_format_es <- function(dat){
  long <- NULL
  for(i in 1:nrow(dat)){
    mon_df <- mondf(dat$StartDate[i], dat$EndDate[i])
    time_vec <- add.months(dat$StartDatep[i], mon_df)
    
    temp_nodes <- unlist(strsplit(dat$LineName[i], ","))
    
    if(length(temp_nodes) == 1){
      # patientid = rep(dat$PatientID[i], length(time_vec))
      linehead = rep(temp_nodes, length(time_vec))
      linetail = rep(temp_nodes, length(time_vec))
    }
    else{
      node_combo <- combn(temp_nodes, 2)
      
      if(ncol(node_combo) == 1){
        # patientid = rep(dat$PatientID[i], length(time_vec))
        linehead = rep(temp_nodes[1], length(time_vec))
        linetail = rep(temp_nodes[2], length(time_vec))
      }
      else{
        linehead = rep(temp_nodes[1, ], length(time_vec))
        linetail = rep(temp_nodes[2, ], length(time_vec))
        time_vec = rep(time_vec, each = ncol(node_combo))
        # patientid = rep(dat$PatientID[i], length(time_vec))
      }
    }
    
    temp_long <- data.frame(
      # PatientID = patientid,
      LineHead = linehead,
      LineTail = linetail,
      TrtTime = time_vec
    )
    long <- rbind(long, temp_long)
  }
  
  # long$PatientID <- as.factor(long$PatientID)
  long$LineHead <- as.factor(long$LineHead)
  long$LineTail <- as.factor(long$LineTail)
  long$LineTail <- factor(long$LineTail, 
                          levels = c(setdiff(levels(long$LineHead), levels(long$LineTail)),
                                     levels(long$LineTail)))
  
  return(long)
}

##### Long VS Data Set #####
long_format_vs <- function(dat){
  long <- NULL
  for(i in 1:nrow(dat)){
    mon_df <- mondf(dat$StartDate[i], dat$EndDate[i])
    time_vec <- add.months(dat$StartDate[i], mon_df)
    
    temp_long <- data.frame(
      # PatientID = rep(dat$patientID[i], length(time_vec)),
      nodename = rep(dat$LineName[i], length(time_vec)),
      TrtTime = time_vec
    )
    long <- rbind(long, temp_long)
  }
  return(long)
}


################ Final ES VS Generation ################
es_generation <- function(dat, scaling_es){
  
  long_es <- long_format_es(dat)
  long_es$TrtTime <- as.character(long_es$TrtTime)
  
  long_es <- long_es[which(as.character(long_es$LineHead) != as.character(long_es$LineTail)), ]

  es <- long_es %>%
    group_by(.dots=c("TrtTime", "LineHead", "LineTail")) %>%
    summarize(Weight=n()/scaling_es)
  
  es$TrtTime <- match(es$TrtTime, levels(as.factor(es$TrtTime)))
  
  links <- es %>%
    mutate(TrtTime.1 = TrtTime) %>%
    select(LineHead, LineTail, Weight, TrtTime, TrtTime.1)
  
  colnames(links) <- c("from", "to", "Weight", "TrtTime", "TrtTime.1")
  return(links)
}

#c("id", "nodename", "Size", "TrtTime", "TrtTime.1")
vs_generation <- function(dat, scaling_vs, links){
  long_vs <- long_format_vs(dat)
  long_vs$TrtTime <- as.character(long_vs$TrtTime)
  
  vs <- NULL
  nodes <- union(levels(links$from), levels(links$to)) # dependent on es to see how many types of nodes
  for(i in 1:length(nodes)){
    temp <- long_vs %>%
      group_by(TrtTime) %>%
      summarize(Size = length(which(grepl(nodes[i], nodename))))
    
    temp$nodename <- rep(nodes[i], nrow(temp))
    
    vs <- rbind(vs, temp)
  }
  
  vs <- vs %>%
    arrange(TrtTime) %>%
    mutate(Size = Size/scaling_vs)
  
  vs$TrtTime <- match(vs$TrtTime, levels(as.factor(vs$TrtTime)))
  
  nodes <- vs %>%
    mutate(TrtTime.1 = TrtTime) %>%
    select(nodename, Size, TrtTime, TrtTime.1)
  
  return(nodes)
}

links <- es_generation(dat, scaling_es = 10)
nodes <- vs_generation(dat, scaling_vs = 100, links)


########## Some Basic Stats ##########
# range(es$Weight)
# #0.1 48.6
# range(vs$Size)
# #0.01 6.86
# hist(vs$Size)
# hist(vs$Size[vs$Cate == "A"])
# hist(vs$Size[vs$Cate == "B"])
# hist(vs$Size[vs$Cate == "C"])
# hist(vs$Size[vs$Cate == "D"])








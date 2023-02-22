library(lubridate)
library(dplyr)
library(readr)

spendpoints= function(total, csvname){
  data = read_csv(csvname, show_col_types = FALSE)
  datachange = data%>%mutate(timestamp=as.POSIXct( timestamp, format = "%y/%m/%d %H:%M:%S",tz = "GMT"), finalpoints = 0)%>%arrange(timestamp)


  currbank=total
  orderedpointsvector =as.vector(datachange$points)

  for(x in 1:length(orderedpointsvector)){
    currspender=as.character(datachange[x, "payer"])
    #print(currspender)
    totalforperson=0
    for(y in (x-1):1){
      #print(y)
      if(as.character(datachange[y,"payer"])==currspender){
        totalforperson=totalforperson+datachange[y, "finalpoints"]
        break
      }
    }


    if(currbank>orderedpointsvector[x]){
      datachange[x,"finalpoints"]=totalforperson
      currbank=currbank-orderedpointsvector[x]
    }
    else{
      datachange[x,"finalpoints"]=totalforperson+(orderedpointsvector[x]-currbank)
      currbank=0
    }

  }
  datachange=datachange%>%group_by(payer)%>%filter(timestamp==max(timestamp))%>%distinct
  datachange=datachange%>%select(payer, finalpoints)
  print(datachange)

}

args = commandArgs(trailingOnly=TRUE)
first=as.numeric(args[1])
second=as.character(args[2])
spendpoints(first, second)

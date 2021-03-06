library(xlsx)

#Load Data 
satalite.data <- read.xlsx("DataForR.xlsx", sheetIndex = 1)

#Sort data by min, year, day, and hour 
sorted.data <- satalite.data[ order( satalite.data$Minute, satalite.data$Year, 
                                     satalite.data$Day, satalite.data$Hour), ]
#remove 40 hour data 
reduced.data <- sorted.data[ !sorted.data$Minute == 40,  ]

#Extract 40 hour data into a its own table called "blank" 
blank.data <- satalite.data[ satalite.data$Minute == 40, 1:5 ]

#reduce data so that it only consists days of the month.
#This line needs to be modified according to which month's data is being processed.
#Modify by changing the days of the year in the data frame.
monthly.data <- reduced.data[ reduced.data$Day %in% 152:181, ]

#splitting the monthly data into days   
monthly.by.day <- split(monthly.data, monthly.data$Day)

#cleaning data: adding missing hours and deleting duplicates 
cleaned.monthly <- lapply(monthly.by.day, function(day){
  
    if(!identical(day$Hour, 0:23)){
    
        #find missing hours 
        missing.hours <- setdiff(0:23, day$Hour)
        
        # create a new row for each missing hour
        new.rows <- t(mapply( function(hour){
            list(Year = day$Year[1], Day = day$Day[1], Hour = hour, Minute = day$Minute[1], 
            WS = NA, WD = NA, WD.Sig = NA, Amb.Temp = NA, RH = NA, Pres = NA, Panel.Temp =NA, 
            O3 = NA, X..O3.Valid = NA, Battery = NA, O3.Temp = NA, O3.Sig = NA, Flow = NA,
            O3.Pres = NA)
        }, missing.hours ))
        
        #insert the new row in day
        day <- rbind( day, new.rows )
    
    }
  
    #remove duplicates
    day <- unique(day)
    
    #make day a data frame and sort it by hour
    day <- as.data.frame( lapply( day, unlist ))
    day <- day[order(day$Hour), ]
    
    #return day
    day
    
})

#combining all cleaned data into one table for the month 
unsplit.monthly <- do.call("rbind", cleaned.v)

#write data to an excel file
write.xlsx(unsplit.monthly, "sortedMData.xlsx", showNA = FALSE)

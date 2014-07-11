library(xlsx)
satalite.data <- read.xlsx("DataForR.xlsx", sheetIndex = 1)

sorted.data <- satalite.data[ order( satalite.data$Minute, satalite.data$Year, 
                                         satalite.data$Day, satalite.data$Hour), ]

reduced.data <- sorted.data[ !sorted.data$Minute == 40,  ]

blank.data <- satalite.data[ satalite.data$Minute == 40, 1:5 ]

june.data <- reduced.data[ reduced.data$Day %in% 152:181, ]

june.by.day <- split(june.data, june.data$Day)



cleaned.june <- lapply(june.by.day, function(day){
  
  if(!identical(day$Hour, 0:23)){

     missing.hours <- setdiff(0:23, day$Hour)
     new.rows <- t(mapply( function(hour){
         list(Year = day$Year[1], Day = day$Day[1], Hour = hour, Minute = day$Minute[1], WS = NA, 
              WD = NA, WD.Sig = NA, Amb.Temp = NA, RH = NA, Pres = NA, Panel.Temp =NA, 
              O3 = NA, X..O3.Valid = NA, Battery = NA, O3.Temp = NA, O3.Sig = NA, Flow = NA,
              O3.Pres = NA)
     }, missing.hours ))
     day <- rbind( day, new.rows )
     
   }
  
  day <- unique(day) 
  #day <- day[ order( day$Hour ), ]
  day
})

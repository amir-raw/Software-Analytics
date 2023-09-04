> x<--10:10
> y <- x^3 - 2*x + 1
> plot(x, y, type = "l", col = "blue", xlab = "x", ylab = "y", main = "Plot of y = x^3 - 2x + 1")
> euclidean_distance <- sqrt(sum((x - y)^2))

   dot_product <- sum(x * y)
  
    > # Load a CSV file
    data <- read.csv("C:/Users/faiya/OneDrive - Texas Tech University/Texas tech course/fall 23/software analytics/auto.csv")
    > 
    > View(data)
    > #Q3
    > highmpg<-max(data$mpg)
    > names(highmpg)
    NULL
    > name(highmpg)
    Error in name(highmpg) : could not find function "name"
    > highmpgname<-which.max(data$mpg)
    > namecar<- data$name[highmpgname]
    > View(data)
    > namecar
    [1] mazda glc
    301 Levels: amc ambassador brougham amc ambassador dpl amc ambassador sst amc concord amc concord d/l amc concord dl 6 amc gremlin ... vw rabbit custom
    > # Calculate the average mpg for US cars
    > us <- mean(data[data$origin == "1", "mpg"], na.rm = TRUE)
    > # Calculate the average mpg for eu cars
    > eu <- mean(data[data$origin == "2", "mpg"], na.rm = TRUE)
    > # Calculate the average mpg for asia cars
    > asia <- mean(data[data$origin == "3", "mpg"], na.rm = TRUE)
    > cat("Average mpg for US cars:", us, "\n")
    Average mpg for US cars: 20.03347 
    > cat("Average mpg for EU cars:", eu, "\n")
    Average mpg for EU cars: 27.60294 
    > cat("Average mpg for Asian cars:", asia, "\n")
    Average mpg for Asian cars: 30.45063 
    > #q5
    > highhp<-which.max(data$horsepower)
    > cat(highhp)
    317
    > namehp<-data$name[highhp]
    > cat(namehp)
    243
    > namehp
    [1] pontiac grand prix
    301 Levels: amc ambassador brougham amc ambassador dpl amc ambassador sst amc concord amc concord d/l amc concord dl 6 amc gremlin ... vw rabbit custom
    > 
    > 
    > 
    > 
    > #q6
    > modelpy<-table(data$model_year)
    > mostmpc<- names(modelpy[which.max(modelpy)])
    > print(mostmpc)
    [1] "73"
    > cat("The year with the most car models is:", mostmpc, "\n")
    The year with the most car models is: 73 
    > print(modelpy)
    
    70 71 72 73 74 75 76 77 78 79 80 81 82 
    29 27 28 40 26 30 34 28 36 29 27 28 30 
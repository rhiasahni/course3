cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
tempPath = '/Users/rhiasahni/Desktop/Rcodes/coursera/Rprog/week2/specdata'
setwd(tempPath)

a=c('003','005','007','009','099','230')
tmpFiles <- paste0(a, c(rep(".csv")))

for (i in 1:length(tmpFiles)) {
  
  tmp=read.csv(file=tmpFiles[i])
  colRead = tmp[,2]
  meanDt = mean(colRead, na.rm=TRUE)
}


tempPath = '/Users/rhiasahni/Desktop/Rcodes/coursera/rprog-data-ProgAssignment3-data'

rprog-data-ProgAssignment3-data

 
 

}


pollutantmean <- function(directory, pollutant, id = 1:332) {
  filename <- vector(mode="character", length=length(id))
  for(i in seq_along(id)) {
    x <- id[i]
    id_string <- toString(x)
    if (x >= 1 && x <= 9) {  
      monitor <- paste("00", id_string, ".csv", sep="")
    }
    else if (x >= 10 && x <= 99) {
      id_string <- toString(x)
      monitor <- paste("0", id_string, ".csv", sep="")
    }
    else {
      id_string <- toString(x)
      monitor <- paste(id_string, ".csv", sep="") 
    }
    filename[i] <- monitor      
  }
  airquality <- read.csv(i)
  accumulator <- 0
  total <- 0  
  for(i in filename) {
    airquality <- read.csv(i)
    good <- complete.cases(airquality[pollutant])
    total <- total + nrow(airquality)
    accumulator <- accumulator + sum(airquality[[pollutant]], na.rm = TRUE)
  }
  accumulator/total
}
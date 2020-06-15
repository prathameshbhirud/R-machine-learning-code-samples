#name we write next to #* is the api endpoint name when called from front end

#* @get /normaldistribution
normaldistribution = function(n = 10) {
    #rnorm(n)
    print(n)
}


#* @post /addTwo
addTwo = function(a, b) {
    as.numeric(a) + as.numeric(b)
}


#to run api on port 9090, run following in sequence
#install.packages("plumber")
library(plumber)
api = plumb("C:/Prathamesh/Sample Codes/R/RTVS-docs-master/examples/APIs/normalDistribution.R")
api$run(port = 9090)

#simulation
source("patient.R")
source("firstAvailableScheduling.R")
source("makeSchedule.R")
source("horizon.R") #returns the number of days in the appt type
source("genericFirstAvailable.R")
source("firstAvailBeta.R")
types <- c("acute","routine","well")
numproviders <- 2
numteams <- 2
POP <- 0
# Start the clock!
#Rprof()
#make random list of patients
#PatientList <- lapply (1 : 10000, function(x) patient(sample(types,1),sample(numproviders,1),sample(numteams,1)))

schedule <- makeSchedule(200,numteams,numproviders,2,8)

# for(i in 1:length(PatientList)) {
#       a <- firstAvailableScheduling(schedule,PatientList[[i]],1)
#       schedule <- a[[1]]
# }

for(j in 1:100) {
      PatientList <- lapply (1 : rpois(1,40), function(x) patient(sample(types,1),sample(numproviders,1),sample(numteams,1)))
      for(i in 1:length(PatientList)) {
            a <- firstAvailBeta(schedule,PatientList[[i]],j,TRUE)
            if(a[[1]][1]!=0) {
                  schedule[[ a[[1]][1] ]][[a[[1]][2]]][a[[1]][3],a[[1]][4]] <- schedule[[a[[1]][1]]][[a[[1]][2]]][a[[1]][3],a[[1]][4]]-1
            }
            POP <- as.numeric(a[[2]]) + POP
            providerPOP <- as.numeric(a[[3]]) + providerPOP
      }    
}
providerPOP
POP
# Stop the clock
#summaryRprof()
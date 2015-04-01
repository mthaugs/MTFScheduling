#' makeSchedule Function
#'
#' This function allows you to create a new schedule to populate with act and
#' non-act appts
#' @param days defines the number of days in the schedule. Defaults to 100 days
#' @param teams defines the number of teams in the clinic. Defaults to 1 team.
#' @param provPerTeam defines the number of providers on each team. Defaults to 1.
#' @param act defines the number of appts to reserve for acute patient requests. Defaults to 0.
#' @param nonact defines the number of appts in the general appt pool to book. Defaults to 0.
#' @param avail defines the percentage of time providers are available in the schedule. Defaults to 1
#' @keywords schedule
#' @export
#' @examples
#' makeSchedule(days=100, teams=1, provPerTeam=1, act=5, nonact=13, avail=1)

#Creates a new schedule with the number of days. Each day is populated with a list
#of provider teams and each provider team is populated with a matrix of providers
#and the number of available appointments.
#Schedule[[day]][[Team]][Provider][ApptType]
makeSchedule <- function(days=100, teams=1, provPerTeam=1, act=0, nonact=0, avail=1, seed=5) {
      
      #Create a matrix that represents a provider team
      createTeam <- function(act,nonact) {
            #set.seed <- seed 
            acute <- c(rep(act,provPerTeam))
            nonacute <- c(rep(nonact,provPerTeam))
            for(i in 1:provPerTeam) {
                  available <- sample(0:1,1,prob=c(1-avail,avail))
                  acute[i] <- acute[i]*available
                  nonacute[i] <- nonacute[i]*available
            }
            Team <- cbind(acute, nonacute)
      }
      
      #Create a full day to add to the schedule
      createDay <- function(teams) {
            Day <- vector("list",teams)
            #cycle to create all providers teams
            for(i in 1:teams) { Day[[i]] <- createTeam(act,nonact) }
            Day
      }
      
      #create the schedule
      schedule <- vector("list",days)
      for(i in 1:days) {
            schedule[[i]] <- createDay(teams)
      }
      schedule
}


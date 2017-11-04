## Kendall Ernst
## World Series Predictor
## October 23, 2017

library(dplyr)
library(plyr)

## Read in data
mlbdata = read.csv("/Users/kernst/Documents/Loaner/DALI/worldseries/mlb-2017-regular-season-all-games.csv", 
                   header = TRUE, stringsAsFactors = FALSE, sep = ";")

## Store unique teams 
unique(mlbdata$team1) %in% unique(mlbdata$team2)
team = sort(unique(mlbdata$team1))

## Keep track of which teams won
mlbdata["winner"] <- NA
mlbdata["winner"] = ifelse(mlbdata$runs1 > mlbdata$runs2,
                           mlbdata$team1, mlbdata$team2)

## Tabulate wins and home wins for each team
total_wins = as.data.frame(team)

for (i in c(1:30)) {
  total_wins[i, "wins"] = length(mlbdata$winner[mlbdata$winner == total_wins[i, "team"]])
  total_wins[i, "home_wins"] = length(mlbdata$winner[mlbdata$team2 == total_wins[i, "team"] & mlbdata$winner == total_wins[i, "team"]])
}
total_wins["win_pct"] = total_wins["wins"]/162
total_wins["home_win_pct"] = total_wins["home_wins"]/81

avgwinpct = mean(total_wins$win_pct)

## Find number of games between each pair of teams
games_played <- matrix (
  rep(0,30*30),
  nrow = 30,
  ncol = 30,
  byrow = TRUE
)

dimnames(games_played) <- list (team, team)
games_played

for (i in 0:length(mlbdata$team1)) {
  games_played[mlbdata$team1[i],
               mlbdata$team2[i]] = games_played[mlbdata$team1[i],mlbdata$team2[i]] + 1;
  games_played[mlbdata$team2[i],
               mlbdata$team1[i]] = games_played[mlbdata$team2[i],mlbdata$team1[i]] + 1;
}
games_played

## Find number of runs for each pair of teams
runs_scored <- matrix (
  rep(0,30*30),
  nrow = 30,
  ncol = 30,
  byrow = TRUE
)

dimnames(runs_scored) <- list (team, team)

for (i in 1:length(mlbdata$team1)) {
  runs_scored[mlbdata$team1[i],
              mlbdata$team2[i]] = runs_scored[mlbdata$team1[i],mlbdata$team2[i]] + mlbdata$runs1[i]
  runs_scored[mlbdata$team2[i],
              mlbdata$team1[i]] = runs_scored[mlbdata$team2[i],mlbdata$team1[i]] + mlbdata$runs2[i]
}

## Find percent of total runs between teams scored by team 1
runs_per_total_transition <- matrix (
  rep(0,30*30),
  nrow = 30,
  ncol = 30,
  byrow = TRUE
)

dimnames(runs_per_total_transition) <- list (team, team)

for (i in 1:length(mlbdata$team1)) {
  runs_per_total_transition[mlbdata$team1[i],
                            mlbdata$team2[i]] = runs_scored[mlbdata$team1[i],mlbdata$team2[i]] / (runs_scored[mlbdata$team1[i], mlbdata$team2[i]] + runs_scored[mlbdata$team2[i],mlbdata$team1[i]])
  runs_per_total_transition[mlbdata$team2[i],
                            mlbdata$team1[i]] = runs_scored[mlbdata$team2[i],mlbdata$team1[i]] / (runs_scored[mlbdata$team1[i], mlbdata$team2[i]] + runs_scored[mlbdata$team2[i],mlbdata$team1[i]])
}

## Teams in different leagues do not play each other.
## Calculate average percent of runs scored for each team.
avg_per_total <- vector(mode = "integer", length = 30)

for (i in 1:30) {
  runsFor <- 0
  runsAgainst <- 0
  for (j in 1:30) {
    runsFor <- runsFor + runs_scored[mlbdata$team1[i],
                                     mlbdata$team2[i]]
    runsAgainst <- runsAgainst + runs_scored[mlbdata$team2[i],
                                             mlbdata$team1[i]]
  }
  avg_per_total[i] <- runsFor / (runsFor + runsAgainst)
}
avg_per_total

## Fill in entries for teams who don't play each other with the average.
for (i in 1:30) {
  for (j in 1:30) {
    runs_per_total_transition[i,j] = ifelse(runs_per_total_transition[i,j] == 0,
                                            runs_per_total_transition[i,j] <- avg_per_total[i],
                                            runs_per_total_transition[i,j])
  }
}

## Find row sums and normalize each row to 1
row_sums <- rowSums(runs_per_total_transition)
row_sums

for (i in 1:30) {
  for (j in 1:30) {
    runs_per_total_transition[i,j] = runs_per_total_transition[i,j] /
      row_sums[i]
  }
}

row_sums_1 <- rowSums(runs_per_total_transition)
row_sums_1


## Calculate the probability of the Dodgers winning a 7-game series against the Astros
p_win = runs_per_total_transition["Dodgers", "Astros"] /
  (runs_per_total_transition["Astros", "Dodgers"] + 
     runs_per_total_transition["Dodgers", "Astros"])
p_win^4 + choose(4, 1) * p_win^4 * (1 - p_win) + 
  choose(5, 2) * p_win^4 * (1 - p_win)^2 + 
  choose(6, 3) * p_win^4 * (1 - p_win)^3

## The probability that the Dodgers beat the Astros in a 7-game series, given that this
## series is between the Astros and Dodgers is 0.412.

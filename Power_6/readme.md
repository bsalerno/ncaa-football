---
title: "'Power 6'?: Analyzing Concentration of Power in FBS"
author: "Brian Salerno"
date: "January 19, 2019"
output: html_document
---

### Idea
The idea behind this short project was a graphic that is displayed during *ESPN*'s bowl games. The graphic showed the records of each FBS conference in out of conference games throughout the season as well as the conferences' bowl records. This was interesting at the time because the American Athletic Conference had started their "Power 6" slogan, asserting that they should be considered with the top 5 conferences, instead of the lower "Group of 5" conferences. Indeed, in this particular year, the American had outperformed the Big 12 in out-of-conference win percentage. 

### Methodology

##### Data
The data for this project came from the [collegefootballdata.com API](https://api.collegefootballdata.com/). I wanted to take a wider look at the question of interest, so I took the regular season games from the 2014-2018 seasons. 2014 was chosen as the start point because it was the first season under the current conference alignment.

I used a Ruby script to pull from the API, that script is included in this repo.

##### Procedure
The methodology was fairly simple: take all of the out-of-conference games, and compute the win percentage for each conference. Later on, I decided to create a matrix so that each conference's performance against other conferences could be measured at one time.

### Analysis

```r
setwd("~/GitHub/ncaa-football/Power_6")
## load data
games18<- read.csv("2018_games.csv", stringsAsFactors = F)
games17<- read.csv("2017_games.csv", stringsAsFactors = F)
games16<- read.csv("2016_games.csv", stringsAsFactors = F)
games15<- read.csv("2015_games.csv", stringsAsFactors = F)
games14<- read.csv("2014_games.csv", stringsAsFactors = F)
games<- rbind(games18, games17, games16, games15, games14)
rm(games18);rm(games17);rm(games16);rm(games15);rm(games14)
## clean data
#### remove FCS teams
games<- subset(games, home_conference != "" & away_conference != "")
#### select columns of interest
games<- games[,c("id", "season", "week", "home_team", "home_conference", 
                 "home_points", "away_team", "away_conference", "away_points")]
#### selecting just interconference games
interconference<- subset(games, home_conference != away_conference)
head(interconference)
```

```
##           id season week        home_team   home_conference home_points
## 13 401013086   2018    1           Tulane American Athletic          17
## 17 401020673   2018    1 Western Michigan      Mid-American          42
## 19 401013089   2018    1             Duke               ACC          34
## 20 401012880   2018    1   Michigan State           Big Ten          38
## 21 401022512   2018    1         Stanford            Pac-12          31
## 22 401013354   2018    1        Wisconsin           Big Ten          34
##           away_team  away_conference away_points
## 13      Wake Forest              ACC          23
## 17         Syracuse              ACC          55
## 19             Army FBS Independents          14
## 20       Utah State    Mountain West          31
## 21  San Diego State    Mountain West          10
## 22 Western Kentucky   Conference USA           3
```

Above, you can see what the data for analysis looked like. The columns of interest are the conferences and the scores, and in order to move further, columns had to be added to the data frame.


```r
## convert points to numeric
interconference$home_points<- as.numeric(interconference$home_points)
interconference$away_points<- as.numeric(interconference$away_points)
```

```r
## verify there are no NA values in the scores, results of "0" are hidden for readability
sum(is.na(interconference$home_points))
sum(is.na(interconference$away_points))
```

```r
## add columns for analysis
interconference$winner<- character(nrow(interconference))
interconference$loser<- character(nrow(interconference))
interconference$mov<- numeric(nrow(interconference))
```



Other than adding the columns, I wanted to vefity that there were no NA values in the team scores. This would prevent the creation of a margin of victory column (called "mov" above) for further analysis. Next, values had to be assigned to each of these new columns for each row through iteration.


```r
for (i in seq(nrow(interconference))){
  if (interconference$home_points[i] > interconference$away_points[i]){
    interconference$winner[i]<- interconference$home_conference[i]
    interconference$loser[i]<- interconference$away_conference[i]
    interconference$mov[i]<- interconference$home_points[i] - interconference$away_points[i]
  } else {
    interconference$winner[i]<- interconference$away_conference[i]
    interconference$loser[i]<- interconference$home_conference[i]
    interconference$mov[i]<- interconference$away_points[i] - interconference$home_points[i]
  }
}
head(interconference)
```

```
##           id season week        home_team   home_conference home_points
## 13 401013086   2018    1           Tulane American Athletic          17
## 17 401020673   2018    1 Western Michigan      Mid-American          42
## 19 401013089   2018    1             Duke               ACC          34
## 20 401012880   2018    1   Michigan State           Big Ten          38
## 21 401022512   2018    1         Stanford            Pac-12          31
## 22 401013354   2018    1        Wisconsin           Big Ten          34
##           away_team  away_conference away_points  winner             loser
## 13      Wake Forest              ACC          23     ACC American Athletic
## 17         Syracuse              ACC          55     ACC      Mid-American
## 19             Army FBS Independents          14     ACC  FBS Independents
## 20       Utah State    Mountain West          31 Big Ten     Mountain West
## 21  San Diego State    Mountain West          10  Pac-12     Mountain West
## 22 Western Kentucky   Conference USA           3 Big Ten    Conference USA
##    mov
## 13   6
## 17  13
## 19  20
## 20   7
## 21  21
## 22  31
```

Now that we have the winning and losing conference for each game, the next step is to find the win percentage for each conference and creating the matrix.


```r
matrix<- matrix(nrow=11, ncol=11)
colnames(matrix)<- unique(interconference$home_conference)
rownames(matrix)<- unique(interconference$home_conference)
diag(matrix)<- 0

for (conf in unique(interconference$home_conference)){
  # cat("Results for", conf, ":\n")
  other_conf<- unique(interconference$home_conference)[-match(conf, unique(interconference$home_conference))]
  matrix_i<- match(conf, unique(interconference$home_conference))
  for (o_conf in other_conf){
    of_interest<- rbind(subset(interconference, winner == conf & loser == o_conf), subset(interconference, winner == o_conf & loser == conf))
    winpct<- round(nrow(subset(interconference, winner == conf & loser == o_conf))/nrow(of_interest),3)
    matrix_j<- match(o_conf, unique(interconference$home_conference))
    #cat("Win pct for", conf, "vs", o_conf, ":", winpct, "(", nrow(of_interest), "games )", "\n")
    matrix[matrix_i, matrix_j]<- winpct
  }
  no_games<- nrow(subset(interconference, winner == conf | loser == conf))
  ooc_winpct<- round(nrow(subset(interconference, winner == conf))/no_games,3)
  cat("Out of conference win percentage for", conf, ":", ooc_winpct, paste0("(", no_games, " games)"), "\n")
}
```

```
## Out of conference win percentage for American Athletic : 0.45 (191 games) 
## Out of conference win percentage for Mid-American : 0.303 (195 games) 
## Out of conference win percentage for ACC : 0.618 (207 games) 
## Out of conference win percentage for Big Ten : 0.712 (205 games) 
## Out of conference win percentage for Pac-12 : 0.696 (138 games) 
## Out of conference win percentage for Mountain West : 0.346 (185 games) 
## Out of conference win percentage for SEC : 0.785 (205 games) 
## Out of conference win percentage for Big 12 : 0.652 (112 games) 
## Out of conference win percentage for Conference USA : 0.29 (210 games) 
## Out of conference win percentage for Sun Belt : 0.287 (167 games) 
## Out of conference win percentage for FBS Independents : 0.444 (261 games)
```


```r
matrix
```

```
##                   American Athletic Mid-American   ACC Big Ten Pac-12
## American Athletic             0.000        0.833 0.406   0.273  0.667
## Mid-American                  0.167        0.000 0.050   0.245  0.000
## ACC                           0.594        0.950 0.000   0.435  0.167
## Big Ten                       0.727        0.755 0.565   0.000  0.450
## Pac-12                        0.333        1.000 0.833   0.550  0.000
## Mountain West                 0.500        0.467 0.500   0.000  0.250
## SEC                           0.800        0.933 0.486   0.429  0.667
## Big 12                        0.842        0.615 0.286   0.438  0.333
## Conference USA                0.391        0.667 0.095   0.056  0.000
## Sun Belt                      0.143        0.500 0.059   0.091  0.000
## FBS Independents              0.594        0.500 0.553   0.500  0.320
##                   Mountain West   SEC Big 12 Conference USA Sun Belt
## American Athletic         0.500 0.200  0.158          0.609    0.857
## Mid-American              0.533 0.067  0.385          0.333    0.500
## ACC                       0.500 0.514  0.714          0.905    0.941
## Big Ten                   1.000 0.571  0.562          0.944    0.909
## Pac-12                    0.750 0.333  0.667          1.000    1.000
## Mountain West             0.000 0.077  0.000          0.667    0.667
## SEC                       0.923 0.000  0.538          0.911    0.943
## Big 12                    1.000 0.462  0.000          0.952    1.000
## Conference USA            0.333 0.089  0.048          0.000    0.632
## Sun Belt                  0.333 0.057  0.000          0.368    0.000
## FBS Independents          0.515 0.105  0.429          0.435    0.333
##                   FBS Independents
## American Athletic            0.406
## Mid-American                 0.500
## ACC                          0.447
## Big Ten                      0.500
## Pac-12                       0.680
## Mountain West                0.485
## SEC                          0.895
## Big 12                       0.571
## Conference USA               0.565
## Sun Belt                     0.667
## FBS Independents             0.000
```

The results are as expected: the only conferences with win percentages over 50% are the Power 5 conferences. There does, however, appear to be a separation between the American and the other "Group of 5" conferences. This does lend some credence to the "Power 6" mantra adopted by the AAC. Further analysis shows that the only Power 5 conference that the American had a win percentage over 50% for was the Pac-12.

```r
for (conf in c("ACC", "Big Ten", "Pac-12", "SEC", "Big 12")){
  of_interest<- subset(interconference, (winner == conf | winner == "American Athletic") 
                       & (loser == conf | loser == "American Athletic"))
  winpct<- round(nrow(subset(of_interest, winner == "American Athletic"))/nrow(of_interest), 3)
  cat("Win percentage for AAC against", paste0(conf, ":"), winpct, 
      paste0("(", nrow(of_interest), " games)"), "\n")
}
```

```
## Win percentage for AAC against ACC: 0.406 (32 games) 
## Win percentage for AAC against Big Ten: 0.273 (22 games) 
## Win percentage for AAC against Pac-12: 0.667 (6 games) 
## Win percentage for AAC against SEC: 0.2 (15 games) 
## Win percentage for AAC against Big 12: 0.158 (19 games)
```

We can see that there were only six games against the Pac-12, and therefore the sample size may be insufficient to conclude that the AAC is superior to the Pac-12. Therefore, while we can say that the American is a head above the other "Group of 5" conferences, they are not yet on par with the "Power 5."

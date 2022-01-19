World Series Simulation for Feedback
================

getmode() function taken from [tutorial
point](https://www.tutorialspoint.com/r/r_mean_median_mode.htm)

``` r
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}
```

Data pulled from
[Baseball-Reference.com](https://www.baseball-reference.com)

``` r
team <- c("ATL", "BOS", "CHC", "CHC", "HOU", "KCR", "LAD", "LAD", "LAD", "SFG", "SFG", "CHW")
Year <- c(2021, 2004, 1903, 2014, 2021, 2015, 1966, 2020, 2021, 2014, 2021, 1994)
WR <- c(0.5465839, 0.6049383, 0.5942029, 0.4506173, 0.5308642, 0.4567901, 0.5864198, 0.7166667
        , 0.654321, 0.5432099, 0.6604938, 0.5929204)

mlb.dat <- data.frame(team, Year, WR)
```

The probability of success will be determined by the win/loss rate the
team had during the regular season for that particular year.

How this method works:

-   Two teams and associated years are passed through the function and
    put into a vector “teamsPlaying”.
    -   “nsim” was added to change how many times samples will be taken
        to make the results easier to read here. It was previously fixed
        at 100001.
-   Using the teams associated years as indexes the win rate is pulled
    from the data frame and put in a probability vector “p” that is used
    to weight the sampling probability.
-   `nsim` samples of the two teams are taken and put into a data frame
    called seriesResults.
    -   This is repeated 7 times to mimic a series of 7 games.
-   Using the [getmode()
    function](https://www.tutorialspoint.com/r/r_mean_median_mode.htm),
    the team that appears the most in the each of the column vectors is
    declared the winner of that “game” and assigned to the seriesResults
    vector.
-   The team that returned the most frequently in the seriesResults
    vector is declared the winner of the simulated World Series
    (“WSwinner”)

``` r
WS.simulation <- function(team1, year1, team2, year2, nsim){
  set.seed(Sys.time())
  mlb <- mlb.dat
   
   seriesResults <- vector()
   teamsPlaying <- c(paste(year1, team1, sep = "_"), paste(year2, team2, sep = "_"))
   
 # Pulling win rate from dataset
   team1WR <- mlb$WR[which(mlb$team == team1 & mlb$Year == year1)] 
   team2WR <- mlb$WR[which(mlb$team == team2 & mlb$Year == year2)]
   
   # List to hold the sampling results for each "game"
   gameNames <- c("Game1", "Game2", "Game3", "Game4", "Game5", "Game6", "Game7") 
   games <- list()
 
   # creating a vector of win rate as a psuedo probability
   p <- c(team1WR, team2WR)
   
   samplingResults<- replicate(7, sample(teamsPlaying, nsim, replace = TRUE, prob = p))
   
   for (i in 1:7) {
     
     # Putting most sampled team into results vector 
     seriesResults[i] <- getmode(samplingResults[,i])
     
     # Putting sampling results into previously made lists
     games[[i]] <- data.frame(Year = c(year1, year2)
                              , teams = c(team1, team2)
                              , nSampled = tabulate(match(samplingResults[,i], teamsPlaying))
                              , perSampled = c(tabulate(match(samplingResults[,i], teamsPlaying))[1]/nsim
                                               , tabulate(match(samplingResults[,i], teamsPlaying))[2]/nsim)
                              , winRate = p)
   }
   
   names(games) <- gameNames
    
    sim <- list(WSwinner = getmode(seriesResults)
                , gameResults = seriesResults
                , winRate = p
                , gameStats = games
                , samples = samplingResults
      )
    
    class(sim) <- "WS.sim"
    return(sim)
}
```

``` r
hou21.atl21 <- WS.simulation("HOU", 2021, "ATL", 2021, 5)
hou21.atl21
```

    ## $WSwinner
    ## [1] "2021_HOU"
    ## 
    ## $gameResults
    ## [1] "2021_HOU" "2021_HOU" "2021_HOU" "2021_HOU" "2021_HOU" "2021_HOU" "2021_ATL"
    ## 
    ## $winRate
    ## [1] 0.5308642 0.5465839
    ## 
    ## $gameStats
    ## $gameStats$Game1
    ##   Year teams nSampled perSampled   winRate
    ## 1 2021   HOU        4        0.8 0.5308642
    ## 2 2021   ATL        1        0.2 0.5465839
    ## 
    ## $gameStats$Game2
    ##   Year teams nSampled perSampled   winRate
    ## 1 2021   HOU        4        0.8 0.5308642
    ## 2 2021   ATL        1        0.2 0.5465839
    ## 
    ## $gameStats$Game3
    ##   Year teams nSampled perSampled   winRate
    ## 1 2021   HOU        3        0.6 0.5308642
    ## 2 2021   ATL        2        0.4 0.5465839
    ## 
    ## $gameStats$Game4
    ##   Year teams nSampled perSampled   winRate
    ## 1 2021   HOU        3        0.6 0.5308642
    ## 2 2021   ATL        2        0.4 0.5465839
    ## 
    ## $gameStats$Game5
    ##   Year teams nSampled perSampled   winRate
    ## 1 2021   HOU        5          1 0.5308642
    ## 2 2021   ATL        5         NA 0.5465839
    ## 
    ## $gameStats$Game6
    ##   Year teams nSampled perSampled   winRate
    ## 1 2021   HOU        3        0.6 0.5308642
    ## 2 2021   ATL        2        0.4 0.5465839
    ## 
    ## $gameStats$Game7
    ##   Year teams nSampled perSampled   winRate
    ## 1 2021   HOU        1        0.2 0.5308642
    ## 2 2021   ATL        4        0.8 0.5465839
    ## 
    ## 
    ## $samples
    ##      [,1]       [,2]       [,3]       [,4]       [,5]       [,6]      
    ## [1,] "2021_HOU" "2021_HOU" "2021_HOU" "2021_ATL" "2021_HOU" "2021_ATL"
    ## [2,] "2021_HOU" "2021_HOU" "2021_ATL" "2021_HOU" "2021_HOU" "2021_HOU"
    ## [3,] "2021_HOU" "2021_HOU" "2021_HOU" "2021_HOU" "2021_HOU" "2021_HOU"
    ## [4,] "2021_ATL" "2021_ATL" "2021_HOU" "2021_HOU" "2021_HOU" "2021_ATL"
    ## [5,] "2021_HOU" "2021_HOU" "2021_ATL" "2021_ATL" "2021_HOU" "2021_HOU"
    ##      [,7]      
    ## [1,] "2021_HOU"
    ## [2,] "2021_ATL"
    ## [3,] "2021_ATL"
    ## [4,] "2021_ATL"
    ## [5,] "2021_ATL"
    ## 
    ## attr(,"class")
    ## [1] "WS.sim"

``` r
lad66.lad2020 <- WS.simulation("LAD", 1966, "LAD", 2020, 100001)
lad66.lad2020$WSwinner
```

    ## [1] "2020_LAD"

``` r
lad66.lad2020$gameStats$Game1
```

    ##   Year teams nSampled perSampled   winRate
    ## 1 1966   LAD    44841  0.4484055 0.5864198
    ## 2 2020   LAD    55160  0.5515945 0.7166667

``` r
WS.simulation("SFG", 2014, "KCR", 2015, 7)
```

    ## $WSwinner
    ## [1] "2015_KCR"
    ## 
    ## $gameResults
    ## [1] "2015_KCR" "2015_KCR" "2015_KCR" "2015_KCR" "2014_SFG" "2014_SFG" "2015_KCR"
    ## 
    ## $winRate
    ## [1] 0.5432099 0.4567901
    ## 
    ## $gameStats
    ## $gameStats$Game1
    ##   Year teams nSampled perSampled   winRate
    ## 1 2014   SFG        1  0.1428571 0.5432099
    ## 2 2015   KCR        6  0.8571429 0.4567901
    ## 
    ## $gameStats$Game2
    ##   Year teams nSampled perSampled   winRate
    ## 1 2014   SFG        2  0.2857143 0.5432099
    ## 2 2015   KCR        5  0.7142857 0.4567901
    ## 
    ## $gameStats$Game3
    ##   Year teams nSampled perSampled   winRate
    ## 1 2014   SFG        3  0.4285714 0.5432099
    ## 2 2015   KCR        4  0.5714286 0.4567901
    ## 
    ## $gameStats$Game4
    ##   Year teams nSampled perSampled   winRate
    ## 1 2014   SFG        1  0.1428571 0.5432099
    ## 2 2015   KCR        6  0.8571429 0.4567901
    ## 
    ## $gameStats$Game5
    ##   Year teams nSampled perSampled   winRate
    ## 1 2014   SFG        5  0.7142857 0.5432099
    ## 2 2015   KCR        2  0.2857143 0.4567901
    ## 
    ## $gameStats$Game6
    ##   Year teams nSampled perSampled   winRate
    ## 1 2014   SFG        4  0.5714286 0.5432099
    ## 2 2015   KCR        3  0.4285714 0.4567901
    ## 
    ## $gameStats$Game7
    ##   Year teams nSampled perSampled   winRate
    ## 1 2014   SFG        2  0.2857143 0.5432099
    ## 2 2015   KCR        5  0.7142857 0.4567901
    ## 
    ## 
    ## $samples
    ##      [,1]       [,2]       [,3]       [,4]       [,5]       [,6]      
    ## [1,] "2015_KCR" "2015_KCR" "2014_SFG" "2015_KCR" "2014_SFG" "2015_KCR"
    ## [2,] "2015_KCR" "2014_SFG" "2014_SFG" "2015_KCR" "2015_KCR" "2014_SFG"
    ## [3,] "2015_KCR" "2015_KCR" "2015_KCR" "2015_KCR" "2015_KCR" "2015_KCR"
    ## [4,] "2014_SFG" "2015_KCR" "2015_KCR" "2015_KCR" "2014_SFG" "2014_SFG"
    ## [5,] "2015_KCR" "2014_SFG" "2015_KCR" "2014_SFG" "2014_SFG" "2014_SFG"
    ## [6,] "2015_KCR" "2015_KCR" "2014_SFG" "2015_KCR" "2014_SFG" "2015_KCR"
    ## [7,] "2015_KCR" "2015_KCR" "2015_KCR" "2015_KCR" "2014_SFG" "2014_SFG"
    ##      [,7]      
    ## [1,] "2015_KCR"
    ## [2,] "2015_KCR"
    ## [3,] "2015_KCR"
    ## [4,] "2015_KCR"
    ## [5,] "2014_SFG"
    ## [6,] "2015_KCR"
    ## [7,] "2014_SFG"
    ## 
    ## attr(,"class")
    ## [1] "WS.sim"

``` r
WS.simulation("CHC", 1903, "CHC", 2014, 11)
```

    ## $WSwinner
    ## [1] "2014_CHC"
    ## 
    ## $gameResults
    ## [1] "2014_CHC" "2014_CHC" "2014_CHC" "1903_CHC" "2014_CHC" "1903_CHC" "1903_CHC"
    ## 
    ## $winRate
    ## [1] 0.5942029 0.4506173
    ## 
    ## $gameStats
    ## $gameStats$Game1
    ##   Year teams nSampled perSampled   winRate
    ## 1 1903   CHC        2  0.1818182 0.5942029
    ## 2 2014   CHC        9  0.8181818 0.4506173
    ## 
    ## $gameStats$Game2
    ##   Year teams nSampled perSampled   winRate
    ## 1 1903   CHC        4  0.3636364 0.5942029
    ## 2 2014   CHC        7  0.6363636 0.4506173
    ## 
    ## $gameStats$Game3
    ##   Year teams nSampled perSampled   winRate
    ## 1 1903   CHC        5  0.4545455 0.5942029
    ## 2 2014   CHC        6  0.5454545 0.4506173
    ## 
    ## $gameStats$Game4
    ##   Year teams nSampled perSampled   winRate
    ## 1 1903   CHC        6  0.5454545 0.5942029
    ## 2 2014   CHC        5  0.4545455 0.4506173
    ## 
    ## $gameStats$Game5
    ##   Year teams nSampled perSampled   winRate
    ## 1 1903   CHC        4  0.3636364 0.5942029
    ## 2 2014   CHC        7  0.6363636 0.4506173
    ## 
    ## $gameStats$Game6
    ##   Year teams nSampled perSampled   winRate
    ## 1 1903   CHC        6  0.5454545 0.5942029
    ## 2 2014   CHC        5  0.4545455 0.4506173
    ## 
    ## $gameStats$Game7
    ##   Year teams nSampled perSampled   winRate
    ## 1 1903   CHC        6  0.5454545 0.5942029
    ## 2 2014   CHC        5  0.4545455 0.4506173
    ## 
    ## 
    ## $samples
    ##       [,1]       [,2]       [,3]       [,4]       [,5]       [,6]      
    ##  [1,] "2014_CHC" "1903_CHC" "2014_CHC" "1903_CHC" "2014_CHC" "2014_CHC"
    ##  [2,] "2014_CHC" "2014_CHC" "2014_CHC" "1903_CHC" "2014_CHC" "1903_CHC"
    ##  [3,] "2014_CHC" "2014_CHC" "1903_CHC" "2014_CHC" "1903_CHC" "2014_CHC"
    ##  [4,] "1903_CHC" "1903_CHC" "1903_CHC" "1903_CHC" "2014_CHC" "1903_CHC"
    ##  [5,] "2014_CHC" "1903_CHC" "2014_CHC" "2014_CHC" "1903_CHC" "2014_CHC"
    ##  [6,] "2014_CHC" "2014_CHC" "2014_CHC" "1903_CHC" "2014_CHC" "1903_CHC"
    ##  [7,] "2014_CHC" "2014_CHC" "1903_CHC" "1903_CHC" "1903_CHC" "1903_CHC"
    ##  [8,] "2014_CHC" "2014_CHC" "2014_CHC" "2014_CHC" "2014_CHC" "2014_CHC"
    ##  [9,] "1903_CHC" "1903_CHC" "2014_CHC" "1903_CHC" "2014_CHC" "2014_CHC"
    ## [10,] "2014_CHC" "2014_CHC" "1903_CHC" "2014_CHC" "2014_CHC" "1903_CHC"
    ## [11,] "2014_CHC" "2014_CHC" "1903_CHC" "2014_CHC" "1903_CHC" "1903_CHC"
    ##       [,7]      
    ##  [1,] "1903_CHC"
    ##  [2,] "2014_CHC"
    ##  [3,] "1903_CHC"
    ##  [4,] "1903_CHC"
    ##  [5,] "2014_CHC"
    ##  [6,] "2014_CHC"
    ##  [7,] "1903_CHC"
    ##  [8,] "2014_CHC"
    ##  [9,] "2014_CHC"
    ## [10,] "1903_CHC"
    ## [11,] "1903_CHC"
    ## 
    ## attr(,"class")
    ## [1] "WS.sim"

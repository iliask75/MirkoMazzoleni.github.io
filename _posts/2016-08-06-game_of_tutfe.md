---
layout: post
title: "GoT Battles - A Games of Tufte - Part I"
date: 2016-08-06
---


This report concerns an exploratory data analysis based on the Games of Thrones dataset hosted on [Kaggle](https://www.kaggle.com/mylesoneill/game-of-thrones). The aim of this work is to familiarize with the data for subsequent analysis, and using the Tufte design rules to represent the plots. During the process, personal domain knowledge (acquired from books and not the tv series) is used to motivate hypothesis and decisions. Since there aren't motivations or questions that brought me to collect data, in order to answer to them, we let the Exploratory Data Analysis phase to generate questions for us. A sound answer to those questions would require at least another dataset, so we let to fix in mind the fact that we are simply describing the dataset at hand, without the temptation to make inferences or other types of final statements.


# Exploratory Data Analysis
Let's load the required libraries and load the dataset which contains information about the main battles in the reign of Westeros, during the [War of the Five Kings](ttp://awoiaf.westeros.org/index.php/War_of_the_Five_Kings).

```r
library(ggplot2)
library(ggthemes)
library(readr)
library(stringr)
library(igraph)
library(dplyr)
library(magrittr)
library(tidyr)
battles <- read.csv("data/battles.csv", header=TRUE, stringsAsFactors=TRUE)
```

We first take an overview of the dataset at hand by checking the variables at our disposal. We can see 38 observations for each of the 25 variables. The next step will be to gain confidence with the features and the values they can take.

```r
str(battles)
```

```
## 'data.frame':	38 obs. of  25 variables:
##  $ name              : Factor w/ 38 levels "Battle at the Mummer's Ford",..: 13 1 7 14 18 10 25 5 3 17 ...
##  $ year              : int  298 298 298 298 298 298 298 299 299 299 ...
##  $ battle_number     : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ attacker_king     : Factor w/ 5 levels "","Balon/Euron Greyjoy",..: 3 3 3 4 4 4 3 2 2 2 ...
##  $ defender_king     : Factor w/ 7 levels "","Balon/Euron Greyjoy",..: 6 6 6 3 3 3 6 6 6 6 ...
##  $ attacker_1        : Factor w/ 11 levels "Baratheon","Bolton",..: 10 10 10 11 11 11 10 9 9 9 ...
##  $ attacker_2        : Factor w/ 8 levels "","Bolton","Frey",..: 1 1 1 1 8 8 1 1 1 1 ...
##  $ attacker_3        : Factor w/ 3 levels "","Giants","Mormont": 1 1 1 1 1 1 1 1 1 1 ...
##  $ attacker_4        : Factor w/ 2 levels "","Glover": 1 1 1 1 1 1 1 1 1 1 ...
##  $ defender_1        : Factor w/ 13 levels "","Baratheon",..: 12 2 12 8 8 8 6 11 11 11 ...
##  $ defender_2        : Factor w/ 3 levels "","Baratheon",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ defender_3        : logi  NA NA NA NA NA NA ...
##  $ defender_4        : logi  NA NA NA NA NA NA ...
##  $ attacker_outcome  : Factor w/ 3 levels "","loss","win": 3 3 3 2 3 3 3 3 3 3 ...
##  $ battle_type       : Factor w/ 5 levels "","ambush","pitched battle",..: 3 2 3 3 2 2 3 3 5 2 ...
##  $ major_death       : int  1 1 0 1 1 0 0 0 0 0 ...
##  $ major_capture     : int  0 0 1 1 1 0 0 0 0 0 ...
##  $ attacker_size     : int  15000 NA 15000 18000 1875 6000 NA NA 1000 264 ...
##  $ defender_size     : int  4000 120 10000 20000 6000 12625 NA NA NA NA ...
##  $ attacker_commander: Factor w/ 32 levels "","Asha Greyjoy",..: 8 6 9 22 16 18 6 30 2 28 ...
##  $ defender_commander: Factor w/ 29 levels "","Amory Lorch",..: 7 4 10 28 12 14 15 1 1 1 ...
##  $ summer            : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ location          : Factor w/ 28 levels "","Castle Black",..: 8 13 17 9 27 17 4 12 5 23 ...
##  $ region            : Factor w/ 7 levels "Beyond the Wall",..: 7 5 5 5 5 5 5 3 3 3 ...
##  $ note              : Factor w/ 6 levels "","Greyjoy's troop number based on the Battle of Deepwood Motte, in which Asha had 1000 soldier on 30 longships. That comes out to"| __truncated__,..: 1 1 1 1 1 1 1 1 1 2 ...
```

## Data cleaning and questions generation 
In this section, we will the following logical framework: 

1. Letting the exploration to generate the questions
2. Setting an expectation for what the data will tell us
3. Revise or confirm the expectation in the light of analysed data

Being the dataset composed most by categorical variables, we want to know first the discrete values that these variables can take, and see if any question arises.

### Categorical Data

This section deals with the understanding and cleaning of categorical variables in the dataset.

#### Attacker King
The variables represents the attacker's king. A slash indicators that the king charges over the course of the war.

```r
levels(battles$attacker_king)
```

```
## [1] ""                         "Balon/Euron Greyjoy"     
## [3] "Joffrey/Tommen Baratheon" "Robb Stark"              
## [5] "Stannis Baratheon"
```
The fact, in some circumstances, there isn't an attacking king is not an error: simply can be that there is no attacking king commanding the troops. 
In this case throwing away missing data can be detrimental, as they can be source of information. For example, a value of "" can mean "unknown" or "not applicable", so it should be encoded in that way. Usually I search for trend in missing data to see if they miss for a reason.

**Question Q1**: Does the " " level mean something? **Expectation E1**: Yes, simply there's no king. **Answer A1**: The " " stands for "NoKing".

- Check the battle names where there is no attacker king and possible notes on that battles

```r
battles[which(battles$attacker_king==""),c("name","note")]
```

```
##                            name note
## 23 Battle of the Burning Septry     
## 30             Sack of Saltpans
```
- Is the missing king due to the battle type? ==> NO, usually pitched battles have a king which guides the army, and troops without a king are expected to act as bandits, preferring a razing or ambush strategy

```r
table(battles$battle_type, battles$attacker_king=="")
```

```
##                 
##                  FALSE TRUE
##                      1    0
##   ambush            10    0
##   pitched battle    13    1
##   razing             1    1
##   siege             11    0
```
- Is the missing king due to the attacker? ==> YES, the Brave Companions and the Brotherhood don't have a king

```r
table(battles$attacker_1, battles$attacker_king=="")
```

```
##                              
##                               FALSE TRUE
##   Baratheon                       6    0
##   Bolton                          2    0
##   Bracken                         1    0
##   Brave Companions                0    1
##   Brotherhood without Banners     0    1
##   Darry                           1    0
##   Free folk                       1    0
##   Frey                            2    0
##   Greyjoy                         7    0
##   Lannister                       8    0
##   Stark                           8    0
```
- Is the previous conclusion the right one? Let's Check for other associations. Seems that the Riverlands are land of no-one, and bandits or sellswords prefer to fight in that area!

```r
table(battles$region, battles$attacker_king=="")
```

```
##                  
##                   FALSE TRUE
##   Beyond the Wall     1    0
##   The Crownlands      2    0
##   The North          10    0
##   The Reach           2    0
##   The Riverlands     15    2
##   The Stormlands      3    0
##   The Westerlands     3    0
```
The most plausible answer is that the missing value is related to fighters which don't have a king. We can then fill the missing value with a new one.

```r
levels(battles$attacker_king)[match("",levels(battles$attacker_king))]="NoKing"
```


**Question Q2**: Who is the king who attacked more? **Expectation E2**: Joffrey/Tommen Baratheon, being Joffrey the most sadistic character. **Answer A2**: Joffrey/Tommen Baratheon

![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-9-1.svg)<!-- -->




#### Defender King
This variable represents the defender's king.

```r
levels(battles$defender_king)
```

```
## [1] ""                         "Balon/Euron Greyjoy"     
## [3] "Joffrey/Tommen Baratheon" "Mance Rayder"            
## [5] "Renly Baratheon"          "Robb Stark"              
## [7] "Stannis Baratheon"
```

**Question Q3**: Does the " " level mean something? **Expectation E3**: Yes, simply there's no king. **Answer A3**: No king.

From the data we can see that there are 3 battles without a defending king. Digging a little deeper shows that two of them were against the brave Companion which don't have a king, and the remaining one is a razing, that is, an attack against an undefended position. 

```r
battles[which(battles$defender_king==""),c("name","note")]
```

```
##                            name note
## 23 Battle of the Burning Septry     
## 25        Retaking of Harrenhal     
## 30             Sack of Saltpans
```

```r
table(battles$defender_1, battles$battle_type,battles$defender_king=="")[,,"TRUE"]
```

```
##                   
##                      ambush pitched battle razing siege
##                    0      0              0      1     0
##   Baratheon        0      0              0      0     0
##   Blackwood        0      0              0      0     0
##   Bolton           0      0              0      0     0
##   Brave Companions 0      0              2      0     0
##   Darry            0      0              0      0     0
##   Greyjoy          0      0              0      0     0
##   Lannister        0      0              0      0     0
##   Mallister        0      0              0      0     0
##   Night's Watch    0      0              0      0     0
##   Stark            0      0              0      0     0
##   Tully            0      0              0      0     0
##   Tyrell           0      0              0      0     0
```
I feel confident to label the " " value to the "NoKing" one.

```r
levels(battles$defender_king)[match("",levels(battles$defender_king))]="NoKing"
```

**Question Q4**: Whose king undergone more attacks? **Expectation E4**: Robb Stark, it seems that everyone wants the North. **Answer A4**: Robb Stark.

![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-13-1.svg)<!-- -->

**Observation O1**: From the previous plots, it seems that both Renly Baratheon and Mance Rayder did not have the time, or the will, to perform any attack: they only defended their position.


#### Attackers
These variables indicates the major houses attacking.

```r
levels(battles$attacker_1)
```

```
##  [1] "Baratheon"                   "Bolton"                     
##  [3] "Bracken"                     "Brave Companions"           
##  [5] "Brotherhood without Banners" "Darry"                      
##  [7] "Free folk"                   "Frey"                       
##  [9] "Greyjoy"                     "Lannister"                  
## [11] "Stark"
```

```r
levels(battles$attacker_2)
```

```
## [1] ""          "Bolton"    "Frey"      "Greyjoy"   "Karstark"  "Lannister"
## [7] "Thenns"    "Tully"
```

```r
levels(battles$attacker_3)
```

```
## [1] ""        "Giants"  "Mormont"
```

```r
levels(battles$attacker_4)
```

```
## [1] ""       "Glover"
```

It can be seen that even in the attackers levels, except for the variable *attacker_1*, there are missing information represented as an empty string. Intuitively, this represent the fact that there aren't the same number of allies for every battles. We can encode the missing values as "NonPresent", indicating that an attacker is not present for that battle.

```r
levels(battles$attacker_2)[match("",levels(battles$attacker_2))]="NotPresent"
levels(battles$attacker_3)[match("",levels(battles$attacker_3))]="NotPresent"
levels(battles$attacker_4)[match("",levels(battles$attacker_4))]="NotPresent"
```

Another observation is that the House Glover is present all the times that 4 attackers participated in a battle.

**Question Q5**: What are the battle with the maximum number of attackers? **Expectation E5**: Probably the battles to conquer the North. **Answer A5**: The battles were those carried on by Stannis Baratheon to free the North from the Greyjoy's and Winterfell from Bolton's. 

```r
battles[which(battles$attacker_4=="Glover"),c("name","attacker_king","defender_king","attacker_outcome","region")]
```

```
##                          name     attacker_king            defender_king
## 31 Retaking of Deepwood Motte Stannis Baratheon      Balon/Euron Greyjoy
## 38        Siege of Winterfell Stannis Baratheon Joffrey/Tommen Baratheon
##    attacker_outcome    region
## 31              win The North
## 38                  The North
```

**Observation O2**:This last battle does not have an outcome, because in the book we only know a letter send to Jon Snow by Ramsey Bolton which tells him that Stannis died, but we are not sure of the letter trustfulness. 

**Question Q6**: There are other cases with missing *attacker_outcome*? **Expectation E6**: Probably not, since that is the last battle of the books until now. **Answer A6**: No, that battle is the only one. We can set the missing value to "unknown".

```r
battles[which(battles$attacker_outcome==""),c("name","attacker_king","defender_king","region")]
```

```
##                   name     attacker_king            defender_king
## 38 Siege of Winterfell Stannis Baratheon Joffrey/Tommen Baratheon
##       region
## 38 The North
```

```r
levels(battles$attacker_outcome)[match("",levels(battles$attacker_outcome))]="unknown"
```



#### Defenders
These variables indicates the major houses defending.

```r
levels(battles$defender_1)
```

```
##  [1] ""                 "Baratheon"        "Blackwood"       
##  [4] "Bolton"           "Brave Companions" "Darry"           
##  [7] "Greyjoy"          "Lannister"        "Mallister"       
## [10] "Night's Watch"    "Stark"            "Tully"           
## [13] "Tyrell"
```

```r
levels(battles$defender_2)
```

```
## [1] ""          "Baratheon" "Frey"
```

```r
levels(battles$defender_3)
```

```
## NULL
```

```r
levels(battles$defender_4)
```

```
## NULL
```

It can be seen that even in the defenders levels there are missing information represented as an empty string. As with the attackers, we can encode the missing values as "NonPresent", indicating that a defender is not present for that battle. 

**Observation O3**: It can be further noticed that nobody defended with more than one ally, and the *defender_3* and *defender_4* columns can be removed from the dataset. 

```r
levels(battles$defender_2)[match("",levels(battles$defender_2))]="NotPresent"
battles$defender_3=NULL
battles$defender_4=NULL
```


**Question Q7**: What does it mean a " " value on the variable *defender_1*? **Expectation E7**: A battle was fought without defenders, and probably was a razing. **Answer A7**: The battle was indeed a razing and there were nor attackers neither defender kings. We can set the missing value to the "NotPresent" one.

```r
battles[which(battles$defender_1==""),c("name","attacker_king","attacker_1","defender_king","battle_type")]
```

```
##                name attacker_king       attacker_1 defender_king
## 30 Sack of Saltpans        NoKing Brave Companions        NoKing
##    battle_type
## 30      razing
```

```r
levels(battles$defender_1)[match("",levels(battles$defender_1))]="NotPresent"
```


#### Attacker outcomes
This variable indicates the outcome from the perspective of the attacker. Categories: win, loss, draw.

**Question Q8**: What are the possible outcomes? **Expectation E8**: From the codebook, the possible values are "draw", "win", "loss".**Answer A8**: The values are under the expectations but no battle ended with a "draw".

```r
levels(battles$attacker_outcome)
```

```
## [1] "unknown" "loss"    "win"
```


#### Battle types
A classification of the battle's primary type. Categories: 
- Pitched_battle: armies meet in a location and fight. 
- Ambush: a battle where stealth or subterfuge was the primary means of attack. 
- Siege: a prolonged of a forties position. 
- Razing: an attack against an undefended position


```r
levels(battles$battle_type)
```

```
## [1] ""               "ambush"         "pitched battle" "razing"        
## [5] "siege"
```

**Question Q9**: What does it mean the value " " on the variable *battle_type*? **Expectation E9**: Probably an unknown battle type. **Answer A9**: The value is not indicated because is unknown how the battle went and its outcome, being the battle the Siege of Winterfell by Stannis Baratheon

```r
battles[which(battles$battle_type==""),c("name","attacker_king","attacker_outcome","defender_king","battle_type")]
```

```
##                   name     attacker_king attacker_outcome
## 38 Siege of Winterfell Stannis Baratheon          unknown
##               defender_king battle_type
## 38 Joffrey/Tommen Baratheon
```

```r
levels(battles$battle_type)[match("",levels(battles$battle_type))]="unknown"
```


#### Attacker commander
Major commanders of the attackers. Commander's names are included without honorific titles and commanders are separated by commas. Since there are many commanders, show only the first ones.

```r
head(levels(battles$attacker_commander))
```

```
## [1] ""                                           
## [2] "Asha Greyjoy"                               
## [3] "Dagmer Cleftjaw"                            
## [4] "Daven Lannister, Ryman Fey, Jaime Lannister"
## [5] "Euron Greyjoy, Victarion Greyjoy"           
## [6] "Gregor Clegane"
```
**Question Q10**: What does it mean the value " " on the variable *attacker_commander*? **Expectation E10**: Probably a missing or unknown commander. **Answer A10**: The value is not indicated because there wasn't a commander, being a battle led by the Brotherhood without Banners. We can set the missing value to a "NotPresent" one.

```r
battles[which(battles$attacker_commander==""),c("name","attacker_king","attacker_1","defender_king","battle_type")]
```

```
##                            name attacker_king                  attacker_1
## 23 Battle of the Burning Septry        NoKing Brotherhood without Banners
##    defender_king    battle_type
## 23        NoKing pitched battle
```

```r
levels(battles$attacker_commander)[match("",levels(battles$attacker_commander))]="NotPresent"
```


#### Defender commander
Major commanders of the defenders. Commander's names are included without honoric titles and commanders are separated by commas. Since there are many commanders, show only the first ones.

```r
head(levels(battles$defender_commander))
```

```
## [1] ""                 "Amory Lorch"      "Asha Greyjoy"    
## [4] "Beric Dondarrion" "Bran Stark"       "Brynden Tully"
```
**Question Q11**: What does it mean the value " " on the variable *defender_commander*? **Expectation E11**: Probably a missing or unknown commander. **Answer A11**: The value is not indicated because there wasn't a commander, or it was unknown. In the battles where there is "NoKing" as *defender_king*, we can assume that the a *defender_commander* was not present. In the rest of the battles, which most of them are led by the Greyjoy, probably there was a *defender_commander* but is not indicated, and thus is unknown.

```r
battles[which(battles$defender_commander==""),c("name","attacker_king","defender_king","battle_type")]
```

```
##                                                    name
## 8                                 Battle of Moat Cailin
## 9                              Battle of Deepwood Motte
## 10                            Battle of the Stony Shore
## 13                             Sack of Torrhen's Square
## 21                                       Siege of Darry
## 23                         Battle of the Burning Septry
## 29                                  Fall of Moat Cailin
## 30                                     Sack of Saltpans
## 32                         Battle of the Shield Islands
## 33 Invasion of Ryamsport, Vinetown, and Starfish Harbor
##               attacker_king            defender_king    battle_type
## 8       Balon/Euron Greyjoy               Robb Stark pitched battle
## 9       Balon/Euron Greyjoy               Robb Stark          siege
## 10      Balon/Euron Greyjoy               Robb Stark         ambush
## 13      Balon/Euron Greyjoy      Balon/Euron Greyjoy          siege
## 21               Robb Stark Joffrey/Tommen Baratheon          siege
## 23                   NoKing                   NoKing pitched battle
## 29 Joffrey/Tommen Baratheon      Balon/Euron Greyjoy          siege
## 30                   NoKing                   NoKing         razing
## 32      Balon/Euron Greyjoy Joffrey/Tommen Baratheon pitched battle
## 33      Balon/Euron Greyjoy Joffrey/Tommen Baratheon         razing
```

```r
levels(battles$defender_commander) = c(levels(battles$defender_commander), "NotPresent","unknown") 
battles[battles$defender_king=="NoKing" & battles$defender_commander=="","defender_commander"]="NotPresent"
battles[battles$defender_commander=="","defender_commander"]="unknown"
battles$defender_commander = droplevels(battles$defender_commander)
```


#### Battles locations
This variable represent the battle location.

```r
head(levels(battles$location))
```

```
## [1] ""               "Castle Black"   "Crag"           "Darry"         
## [5] "Deepwood Motte" "Dragonstone"
```

**Question Q12**: What does it mean the value " " on the variable *location*? **Expectation E12**: Probably a missing or unknown location **Answer A12**: The location [is not known](http://awoiaf.westeros.org/index.php/Battle_at_the_burning_septry).

```r
battles[which(battles$location==""),c("name","attacker_king","defender_king")]
```

```
##                            name attacker_king defender_king
## 23 Battle of the Burning Septry        NoKing        NoKing
```

```r
levels(battles$location)[match("",levels(battles$location))]="unknown"
```

#### Battle regions
The region where the battle takes place. Categories: Beyond the Wall, The North, The Iron Islands, The Riverlands, The Vale of Arryn, The Westerlands, The Crownlands, The Reach, The Stormlands, Dorne

**Question Q13**: What are the values assume med by the variable? **Expectation E13**: The values assumed by the variable are those described in the codebook. **Answer A13**: The answer meets the expectation, except for the regions "The Iron Islands", "The Vale of Arryn" and "Dorne", probably because no battle were fought in those regions.

```r
levels(battles$region)
```

```
## [1] "Beyond the Wall" "The Crownlands"  "The North"       "The Reach"      
## [5] "The Riverlands"  "The Stormlands"  "The Westerlands"
```

### Numerical Data

This section deals with the understanding and cleaning of numerical variables in the dataset.


#### Year

The year of the battle. We convert it to a factor variable for convenience and representation, since it assumes only $3$ different values.

```r
summary(battles$year)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   298.0   299.0   299.0   299.1   300.0   300.0
```

```r
battles$year=as.factor(battles$year)
```

#### Attacker size
The size of the attacker's force. No distinction is made between the types of soldiers such as cavalry and footmen.

```r
summary(battles$attacker_size)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      20    1375    4000    9943    8250  100000      14
```
From the summary we can see that the distribution of the attacker army has a mean of about $10000$ soldiers , but is very scattered with many missing numbers. Particularly impressing is maximum number of $100000$ men.

**Question Q14**: Which is the battle with $100000$ men? **Expectation E14**: A battle in the North with the wildlings. **Answer A14**: The battle was the assault of Castle Black by the wildlings and free folk, when Jon Snow loses Igritte. We can see that there is an error in the data, because we know that Stannis Baratheon was on the Night's side, defending the Nigth's Watch and seizing Mance Rayder. Furthermore, Stannis won the battle and Mance Rayder lost it, thus the *attacker_king* and *defender_king* variables should be swapped. The number of $100000$ is more meaningful now if we think of it as the army of all the freefolks, as it is also reported [here](http://awoiaf.westeros.org/index.php/Battle_of_Castle_Black).

```r
battles[which(battles$attacker_size==100000),c("name","attacker_king","defender_king","attacker_1","defender_1","attacker_outcome")]
```

```
##                      name     attacker_king defender_king attacker_1
## 28 Battle of Castle Black Stannis Baratheon  Mance Rayder  Free folk
##       defender_1 attacker_outcome
## 28 Night's Watch             loss
```

```r
tmp=battles[28,"attacker_king"]
battles$attacker_king=as.character(battles$attacker_king)
battles$defender_king=as.character(battles$defender_king)
battles[28,"attacker_king"]=as.character(battles[28,"defender_king"])
battles[28,"defender_king"]=as.character(tmp)
battles$attacker_king=as.factor(battles$attacker_king)
battles$defender_king=as.factor(battles$defender_king)
```

**Question Q15**: When attacking, which battle type required more men? **Expectation E15**: Probably the *pitched battle* type, since it requires more men than ambush or a siege, which require more discretion and tools (trebuchets, rams) capability respectively. **Answer 15**: The *pitched battle* has the higher median (about $10000$ troops) and it is very skewed around this value, and only $25%$ of values are lower than $3000$ troops. Perhaps surprising, the median of the *ambush* distribution is similar to that of *siege*, being the latter more concentrated, indicating that there some standard number of troops to do a siege. Here we have isolated cases of ambushes with less than $30$ men, and a siege with $100000$ men (the Mance Rayder attack to Castle Black).
We Do not consider "unknown" o "razing" battles since they have few or none observations.
![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-35-1.svg)<!-- -->

**Question Q16**: When attacking, which king had the most numerous army?  **Expectation E16**: We already know that Mance Rayder commanded $100000$ men. **Answer 16**: Mance had the most numerous attacking army, but he attacked only one time, so it is more interesting to considered the other kings. I made the choice to exclude from the comparison also the "NoKing" category, since it has few observations. We can see from the plot that the Greyjoy's had the smallest army, ranging from $10$ to $1000$ men. The Lannister's and the Stark's show a high median value army, but that also had great variation in its forces, mainly for Robb Stark. This can be probably due to his attitude to perform ambush attacks with few men. Stannis Baratheon forces undergo few losses, having a quite concentrated distribution.
![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-36-1.svg)<!-- -->


#### Defender size
The size of the defender's force. No distinction is made between the types of soldiers such as cavalry and footmen.

```r
summary(battles$defender_size)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     100    1070    6000    6428   10000   20000      19
```

**Question Q17**: When attacking, which king had the most numerous army?  **Expectation E17**:Probably the Lannister's or Stannis Baratheon. **Answer 17**: Apart from the "Noking" and "Renly Baratheon", which have few observation, the plots show that the Lannister's defended their position with more troops than the other, and Stannis Baratheon, despite attacking with an high number of troops, defended with very few ones.
![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-38-1.svg)<!-- -->


## More questions

This section highlights and depicts more question that arise from the data.

### Major deaths and captures rate

The following plots shows how the major deaths and captures are spread across the battle succession across the years. The major number of them happen in the year $299$, were almost each battle generates a major death. In the year $300$, it seems that no more major characters die in battle, with only one of them captured. It can bee seen that the majority of deaths and captures happen in the *Riverlands* and in the *North*. From the graph emerges that the cumulated number of deaths is always higher than the cumulated number of captures, symptom that the soldiers prefer to kill rather than take prisoners.
![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-39-1.svg)<!-- -->



### Number of battles won by each commander, when attacking

The plots shows the number of won battles by each king, thanks a specific commander, with a different color for each region where the battle was fought. It can be noticed that the Greyjoy's fought almost in the North, while the Lannister's and the Stark's fought all across Westeros. Gregor Clegane (the Mountain) is the commander who won more battles. Folks with no king fought only in the Riverlands.
![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-40-1.svg)<!-- -->


### Type of battles won  by each king, when attacking

The plots shows the number of won battles by each king, thanks a specific tactic, with a different color for each year in which the battle was fought. We can see that Robb Stark preferred to fight with ambushes, while the Lannister's with pitched battles since the year $299$, and  with sieges from the year $300$.

![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-41-1.svg)<!-- -->


### Number of battles fought in summer and winter

The plots shows the number of won battles by each king fought in summer (1) or winter (0), with a different color for each year in which the battle was fought. The poor Robb Stark died during the Red Wedding in year $299$, so he did not fight any battle during year $300$.We can see that winter has finally come in year $300$. Brace yourself!! 

![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-42-1.svg)<!-- -->

### King vs. King

This plots shows the preferred king versus whose each king fought his battles.

![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-43-1.svg)<!-- -->

By inspecting the plot, it seems strange that *Balon/Euron Greyjoy* attack himself. The battle considered is the *Sack of Torrhen's Square*.  In fact, the defender king would be *Bran Stark*, since Robb is dead. I make the decision to substitute the defender king with *Robb Stark* indicating that the defenders are the Stark's. 

```r
idx_battle=battles[battles$attacker_king=="Balon/Euron Greyjoy" & battles$defender_king=="Balon/Euron Greyjoy","battle_number"]
battles[idx_battle,c("name","attacker_king","defender_king")]
```

```
##                        name       attacker_king       defender_king
## 13 Sack of Torrhen's Square Balon/Euron Greyjoy Balon/Euron Greyjoy
```

```r
battles[idx_battle,"defender_king"]="Robb Stark"
```

The correct plot is the following. We can see that Robb Stark fought mainly versus Joffrey Baratheon and viceversa. The troops without a king fought each other. Stannis fought first versus his brother Renly and then versus the Lannister's.
![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-45-1.svg)<!-- -->


The previous plots do not change except for those of  **Question Q4**, for which the answer remain unchanged. The correct plots are reported here.

![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-46-1.svg)<!-- -->


### Battle success given army forces

The plot seems to suggest that there is a linear relationship between the size of the attacking army and the size of the defending one, maybe due to information that each rival part has about the other. It seems that after a certain attacker size threshold, the loss is more probable. Maybe is difficult to coordinate such many men, versus a numerous defending army. Having a bigger army, therefore, does not mean to win for sure.
![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-47-1.svg)<!-- -->




## Graph of Thrones

The following section (which owes a lot to the Kaggle's user [ColinFraser](https://www.kaggle.com/colinfraser/d/mylesoneill/game-of-thrones/battles-investigation), deals with the use of graph algorithms to analyse the social relations between houses, during the battles. By using the variables *attacker_i* and *defender_i*, with $i=1...4$, is possible to build a graph which vertices are the house names, and an edge is present between two of them if a battle has been fought between the two houses. 


```r
house_figths <- battles %>% 
  gather(attacker, attacker_house, matches('attacker_\\d')) %>%
  gather(defender, defender_house, matches('defender_\\d')) %>% 
  mutate(winner_house = ifelse(attacker_outcome == 'win', attacker_house, defender_house),
         loser_house = ifelse(attacker_outcome == 'win', defender_house, attacker_house)) %>% 
  select(loser_house, winner_house, name,battle_type,region,location,summer) %>% 
  na.omit

house_figths=filter(house_figths,winner_house!="NotPresent" & loser_house!="NotPresent")
house_figths$region=as.character(house_figths$region)
house_figths$name=as.character(house_figths$name)
house_figths$location=as.character(house_figths$location)

house_graph <- house_figths %>% 
  #this order determines the direction edge
  select( loser_house,winner_house) %>% 
  as.matrix %>%  
  graph_from_edgelist

V(house_graph)$wins <- degree(house_graph, mode='in')
V(house_graph)$losses <- degree(house_graph, mode='out')
V(house_graph)$battles <- degree(house_graph, mode='all')
E(house_graph)$weight <- 1
E(house_graph)$battle_name <- house_figths$name
E(house_graph)$region <- house_figths$region
E(house_graph)$location <- house_figths$location

simple_house_graph <- house_graph %>% simplify(remove.loops = F,
                    edge.attr.comb = list(weight="sum",battle_name="concat",location="concat",region="concat"))
```


The direction of the edge is from a loser to a winner, and a darker edge color show how many battles are present with that edge direction (that is, with that battle outcome): the darker, the higher. It can be seen that the Frey's fought with many houses and won many battles, and how the Nigth's Watch are isolated from the rest of the world. Interestingly, even the Tyrell's fought only versus the Greyjoy's. Both the Tully's and the Stark's won more battles versus the Lannister's with respect to how many they lost versus them, but for the latter house, they are behind the Greyjoy as direct matches won. Interesting is the loop around house Baratheon, which represent the figths of Stannis versus his brother Renly.

![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-49-1.svg)<!-- -->


**It is possible to label the edges with battle information, but the graph became too cluttered:**

- Information regarding battles names:
![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-50-1.svg)<!-- -->

- Information regarding battles locations:
![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-51-1.svg)<!-- -->


### Houses which won most

By counting the in-degree (number of battles won) and the total degee of a node (number of battle fought), is possible to compute how the house performed in terms of efficiency. Since the number of battles for each house is small, we employ the *Laplace correction* to correct for the small sample size:
$$ p(win)=\frac{n+1}{n+m+2} $$
where $n$ is the number of won battles, $m$ is the number of lost battles and $p(win)$ is the probability to win a battle.


![]({{ site.baseurl }}/assets/games_of_tufte/unnamed-chunk-53-1.svg)<!-- -->

### Most powerful houses

By using the **PageRank** algorithm, it is possible to assign a value to each node in the graph. In the case of Google, each node is a document, a web page, and the edges are links between pages. If a page has an incoming link from an important page, that link carries more value. In our context, we suppose this mean that win against the Lannister's, for example, carries more value than to win against the Glover's, and the algorithm is able to exploit this based on the graph structure of won and lost battles (again, incoming and outcoming edges).
From Wikipedia:

> The PageRank algorithm outputs a probability distribution used to represent the likelihood that a person randomly clicking on links will arrive at any particular page

Adapting the reasoning to our case, we can think of it as in terms of houses and battle outcomes. By walking through the Markov network following edge direction (which is from loser to winner), we discover the stationary probability distribution of the Markov chain. This represent the probability that, starting from a house at random and moving in the direction of battles outcome, after a while we expect to celebrate the victory of a house for that invariant amount of time.

The results show that the house Frey is the most powerful, and the $11.7% of times we expect to come to House Frey having a party for their victory.


```r
pr <- house_graph %>% page_rank
sort(pr$vector, decreasing=T)
```

```
##                        Frey                   Lannister 
##                  0.11706463                  0.10476410 
##                      Bolton                   Baratheon 
##                  0.09134930                  0.08013144 
##               Night's Watch                     Greyjoy 
##                  0.06422775                  0.04357728 
##                       Stark Brotherhood without Banners 
##                  0.04331037                  0.04023057 
##                     Bracken                       Tully 
##                  0.04023057                  0.03713693 
##                       Darry                    Karstark 
##                  0.03713693                  0.03440543 
##                     Mormont                      Glover 
##                  0.03440543                  0.03440543 
##            Brave Companions                   Mallister 
##                  0.02823198                  0.02823198 
##                   Free folk                      Tyrell 
##                  0.02823198                  0.02823198 
##                   Blackwood                      Thenns 
##                  0.02823198                  0.02823198 
##                      Giants 
##                  0.02823198
```

```r
# It has to output 1, being a probability distribution
sum(pr$vector)
```

```
## [1] 1
```


# Acknowledgements

  * [Some analysis of Game of Throne data](https://www.kaggle.com/sergeycherkasov/d/mylesoneill/game-of-thrones/test-test) - Sergey Cherkasov
  * [Exploratory Analysis and Predictions](https://www.kaggle.com/shaildeliwala/d/mylesoneill/game-of-thrones/exploratory-analysis-and-predictions) - Shail Deliwala
  * [Systematic Analysis on GoT Battles](https://www.kaggle.com/gowrishankarin/d/mylesoneill/game-of-thrones/analysis-on-battles) - Gowri Shankar
  * [Battles investigation](https://www.kaggle.com/colinfraser/d/mylesoneill/game-of-thrones/battles-investigation) - ColinFraser
  * [Tufte in R](http://motioninsocial.com/tufte/) - Lukasz Piwek
  



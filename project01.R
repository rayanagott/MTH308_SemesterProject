---
title: 'MTH 308: Linear Algebra'
output:
  html_document:
    df_print: paged
---

## Project 1: Sports Ranking

MTH 308: Applied Linear Algebra

Project parts: 

- Part 1: Colley method
- Part 2: Massey method
- Part 3: Application to real data
- Part 4: Cutting edge
- Part 5: Group member contributions


### Instructions

Complete each part as instructed. You are encouraged to work in groups, though you may work independently if you choose.

<u>Submission:</u> Complete the following notebook in order. Once done, save the notebook and upload the produced HTML file to Canvas.

<u>Rubric:</u> 40 total points, 10 points for each part.

### Project description
In sports, ranking teams is done in a number of ways. Win percentage is a very simple approach, though a drawback is you only know how a team compares to the other teams it played. For large groups of teams it is not possible for each team to play every other team. Here you will apply linear algebra ideas to overcome this challenge in two ways.

For this project, you will read a paper ([PAPER LINK](https://www.researchgate.net/publication/228435078_Bracketology_How_can_math_help)), explain approaches, and reproduce findings for a toy dataset.

### Toy dataset

#### Sample college basketball scores

![](scores.png){height=100}

#### Colley's method rating results to reproduce

![](colley.png){height=100}

#### Massey's method rating results to reproduce

![](massey.png){height=100}

### Part 1: Colley's method

Carefully read section 2 of the sports ranking paper and complete the following:

- Give a complete explanation of Colley's method. Reference ideas from this course where appropriate. You should draw on explanations from the paper, though your writing should be of your own thoughts. You should not take screenshots of the paper here.
- Using a difference reference, explain Laplace's rule of succession and how this applies to sports ranking. Cite the reference you use here.
- Write R code to reproduce the above 5 team ratings for Colley's method.
```{R}
# Teams and their corresponding r values (ratings)
teams <- c("Duke", "Miami", "UNC", "UVA", "VT")
r_values <- c(0.21, 0.79, 0.50, 0.36, 0.65)

# Total games played (W + L) for each team
games <- c(5, 5, 5, 5, 5)

# Wins for each team
wins <- c(1, 4, 3, 2, 3)

# Losses for each team
losses <- games - wins

# Create a data frame
df <- data.frame(Team = teams, Wins = wins, Losses = losses, r = r_values)
print(df)

# Initialize Colley's matrix
n <- length(teams)
colley_matrix <- matrix(0, n, n)
b <- rep(0, n)

# Define matchups (who played against whom)
matchups <- matrix(c(1, 0,   # Miami vs Duke (1 means Miami beat Duke)
                     2, 1,   # VT vs Miami
                     3, 2,   # UNC vs VT
                     4, 3),  # UVA vs UNC
                     ncol = 2, byrow = TRUE)

# Fill Colley's matrix
for (i in 1:n) {
  colley_matrix[i, i] <- 2 + wins[i] + losses[i]
  b[i] <- 1 + (wins[i] - losses[i])
}

for (m in 1:nrow(matchups)) {
  i <- matchups[m, 1]
  j <- matchups[m, 2]
  colley_matrix[i, j] <- -1
  colley_matrix[j, i] <- -1
}

# Solve
r <- solve(colley_matrix, b)


```

### Part 2: Massey's method

Carefully read section 3 of the sports ranking paper and complete the following:

- Give a complete explanation of Colley's method. Reference ideas from this course where appropriate. You should draw on explanations from the paper, though your writing should be of your own thoughts. You should not take screenshots of the paper here.
- Using a different reference, explain the method of least squares and how this applies to sports ranking. Our text has a section on this topic.
- Write R code to reproduce the above 5 team ratings for Massey's method.
```{R}
# Make the massey function
massey <- function(data) {
  # Get the list of teams
  teams <- unique(c(data$Team1, data$Team2))
  n <- length(teams)
  
  # Set the matrix and the vector
  M <- matrix(0, n, n)
  b <- numeric(n)
  
  # Loop through data to fill the matrix and vector
  for (i in 1:nrow(data)) {
    # Find the position of teams 
    t1 <- match(data$Team1[i], teams)
    t2 <- match(data$Team2[i], teams)
    
    # Update the matrix based on game results
    M[t1, t1] <- M[t1, t1] + 1
    M[t2, t2] <- M[t2, t2] + 1
    M[t1, t2] <- M[t1, t2] - 1
    M[t2, t1] <- M[t2, t1] - 1
    
    # Update the vector with differences in score
    b[t1] <- b[t1] + (data$Score1[i] - data$Score2[i])
    b[t2] <- b[t2] - (data$Score1[i] - data$Score2[i])
  }
  
  M[n, ] <- 1
  b[n] <- 0
  
  # Solve and print out results
  ratings <- solve(M, b)
  data.frame(Team = teams, MasseyRating = ratings)
}

# Test data
data <- data.frame(
  Team1 = c("Duke", "Miami", "Miami", "UVA", "Miami"),
  Team2 = c("Miami", "VT", "UNC", "Duke", "UNC"),
  Score1 = c(24, 8, 9, 7, 16),
  Score2 = c(14, 4, 6, 4, 12)
)

massey(data)



```

### Part 3: Application to real data

Apply the Colley's and Massey's methods to real sports data using the website [masseyratings.com](https://masseyratings.com/data)

- Explain the key findings of your code.
- Compare your findings to another source.
```{R}
# Colley method with 2018-2019 season nba data
# New dataset
Team1 <- c("Orlando", "Milwaukee", "New York", "Miami", "Charlotte", "Memphis", "Milwaukee", "Miami")
Team2 <- c("Miami", "Charlotte", "Atlanta", "Washington", "Orlando", "Atlanta", "Orlando", "Memphis")
Score1 <- c(104, 113, 126, 113, 120, 131, 113, 100)
Score2 <- c(101, 112, 107, 112, 88, 117, 91, 97)

# Teams
teams <- unique(c(Team1, Team2))
n <- length(teams)

# Initialize wins and losses
wins <- numeric(n)
losses <- numeric(n)

# Loop through the games to calculate wins and losses
for (i in 1:length(Team1)) {
  t1 <- match(Team1[i], teams)
  t2 <- match(Team2[i], teams)
  
  if (Score1[i] > Score2[i]) {
    wins[t1] <- wins[t1] + 1
    losses[t2] <- losses[t2] + 1
  } else {
    wins[t2] <- wins[t2] + 1
    losses[t1] <- losses[t1] + 1
  }
}

# Initialize Colley's matrix and b vector
colley_matrix <- matrix(0, n, n)
b <- numeric(n)

# Fill the diagonal of Colley's matrix and b vector
for (i in 1:n) {
  colley_matrix[i, i] <- 2 + wins[i] + losses[i]
  b[i] <- 1 + (wins[i] - losses[i])
}

# Fill the off-diagonal of Colley's matrix for matchups
for (i in 1:length(Team1)) {
  t1 <- match(Team1[i], teams)
  t2 <- match(Team2[i], teams)
  colley_matrix[t1, t2] <- colley_matrix[t1, t2] - 1
  colley_matrix[t2, t1] <- colley_matrix[t2, t1] - 1
}

# Solve the linear system
r <- solve(colley_matrix, b)

# Output the results
df <- data.frame(Team = teams, Wins = wins, Losses = losses, Rating = round(r, 4))
print(df)


#Massey method with 2018-2019 season nba data
data <- data.frame(
  Team1 = c("Orlando", "Milwaukee", "New York", "Miami", "Charlotte", "Memphis", "Milwaukee", "Miami"),
  Team2 = c("Miami", "Charlotte", "Atlanta", "Washington", "Orlando", "Atlanta", "Orlando", "Memphis"),
  Score1 = c(104, 113, 126, 113, 120, 131, 113, 100),
  Score2 = c(101, 112, 107, 112, 88, 117, 91, 97)
)
massey(data)


```

### Part 4: Cutting edge

Research cutting-edge methods for ranking sports teams.

- Summarize at least 3 different approaches. 
- Give a detailed explanation of one approach. Include math explanations, graphics, and examples where possible.
- Cite all references.

```{R}
# Bayesian Analysis of Formula One
# Load required package
library(rstan)

# Prepare data
race_data <- data.frame(
  driver_id = c(1, 2, 3, 1, 2, 3),  # IDs of drivers
  constructor_id = c(10, 11, 12, 10, 11, 12),  # IDs of constructors
  year = c(2020, 2020, 2020, 2021, 2021, 2021),
  race_number = c(1, 1, 1, 2, 2, 2),  # Race numbers in a season
  position = c(1, 2, 3, 2, 1, 3),  # Finish positions
  wet_race = c(0, 0, 0, 1, 1, 1),  # 0 for dry, 1 for wet
  permanent_circuit = c(1, 1, 1, 0, 0, 0)  # 1 for permanent, 0 for street
)

# Stan requires data to be in a list format
stan_data <- list(
  N = nrow(race_data),                   # Number of observations
  K = max(race_data$position),           # Number of categories in outcome
  position = race_data$position,         # Outcome variable
  driver_id = race_data$driver_id,       # Grouping by driver
  constructor_id = race_data$constructor_id, # Grouping by constructor
  year = race_data$year,                 # Grouping by year
  wet_race = race_data$wet_race,         # Predictor
  permanent_circuit = race_data$permanent_circuit # Predictor
)
# Fit model using rstan
fit <- stan(
  file = 'cumulative_logit.stan',
  data = stan_data,
  chains = 4,
  iter = 2000,
  cores = parallel::detectCores()
)

print(summary(fit))  # See model summary




```

### Part 5: Group member contributions

Give a summary of each group members contribution to this project. This summary should include:

- Approximate total time each member spent on the project.
- Which parts each member contributed to on the project.


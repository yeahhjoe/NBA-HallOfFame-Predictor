library(rvest)
library(dplyr)

# Web Scrap the data for the Hall of Fame

col_link <- "https://www.basketball-reference.com/awards/hof.html"
page = read_html(col_link)

hof_table = page %>% html_nodes("table#hof.suppress_all.sortable.stats_table") %>%
  html_table() %>% . [[1]]


# Grab the stats of current NBA players
nba2022_df <- read.csv("curr_player_stats.csv")

# Data Parsing - Pt.1
# We have to clean both data sets
# Remove duplicate rows from current players 

repeated_elements <- function(lst) {
  duplicated_indices <- duplicated(lst)
  repeated <- unique(lst[duplicated_indices])
  return(repeated)
}

repeated <- repeated_elements(nba2022_df$Player)
print(repeated)


rows_to_remove1 <- c()  # Initialize an empty vector to store row indices

for (i in 1:nrow(nba2022_df)) {
  if(nba2022_df[i, "Player"] %in% repeated &&  nba2022_df[i, "Tm"] != "TOT"){
    rows_to_remove1 <- c(rows_to_remove1, i)
  }
  
}

nba2022_df <- nba2022_df[-rows_to_remove, ]

# Data Parsing - Pt.2

# The website has data as a multi-index, but we don't need the top level
colnames(hof_table1) <- hof_table1[1, ]
hof_table1 <- hof_table1[-1, ]

print(hof_table1)

# The table also contains non-player data, this isn't helpful to us
drop_these_rows <- c()
for (i in 1:nrow(hof_table)) {
  if(hof_table[i, "Category"] != "Player"){
    drop_these_rows <- c(drop_these_rows, i)
  }
}

hof_table <- hof_table[-drop_these_rows, ]

# Drop WNBA players since we're only studying NBA
drop_these_rows <- c()
for(i in 1:nrow(hof_table)){
  if(grepl("WNBA", hof_table[i, "Name"])){
    drop_these_rows <- c(drop_these_rows, i)
  }
}

hof_table <- hof_table[-drop_these_rows, ]

# The Name category has some extra stuff, lets drop it to only <firstName lastName> 
# Function to fix the name format
cut_off_string <- function(input_string) {
  words <- strsplit(input_string, " ")[[1]]
  new_string <- paste(words[1:2], collapse = " ")
  return(new_string)
}

hof_table$Name <- sapply(hof_table$Name , cut_off_string)


remove_player <- function(input_string) {
  output_string <- gsub("Player", "", input_string)
  return(output_string)
}
hof_table$Name <- sapply(hof_table$Name, remove_player)

# Drop unnecessary columns

hof_table1 <- subset(hof_table1, select = -c(`W/L%`, G, W, L, `NA`))
hof_table <- subset(hof_table1, select = -c(`W/L%`, G, W, L, `NA`))

hof_table <- hof_table[order(hof_table$Year), ]

# Drop rows of the df that have PTS listed as empty. This will remove all non-NBA players
drop_these_rows <- c()
for(i in 1:nrow(hof_table)){
  if(hof_table[i, "PTS"] == ""){
    drop_these_rows <- c(drop_these_rows, i)
  }
}
hof_table <- hof_table[-drop_these_rows, ]


# Cast the numbers in the main categories into floating points from strings (to prep data for analysis)
hof_table$Year <- as.numeric(hof_table$Year)
hof_table$PTS <- as.numeric(hof_table$PTS)
hof_table$TRB <- as.numeric(hof_table$TRB)
hof_table$AST <- as.numeric(hof_table$AST)
hof_table$BLK <- as.numeric(hof_table$BLK)
hof_table$STL <- as.numeric(hof_table$STL)

head(hof_table)


# Scatter plot of HOF PTS vs Year
par(mfrow = c(3, 1))
plot(hof_table$Year, hof_table$PTS, main = "HOF PTS vs Year", ylim = c(0, max(hof_table$PTS)), ylab = "Points", xlab = "Year")

# Scatter plot of HOF AST vs Year
plot(hof_table$Year, hof_table$AST, main = "HOF AST vs Year", ylim = c(0, max(hof_table$AST)), ylab = "Assist", xlab = "Year")

# Scatter plot of HOF REBS vs Year
plot(hof_table$Year, hof_table$TRB, main = "HOF REBS vs Year", ylim = c(0, 30), ylab = "Points", xlab = "Year")

# Create a linear regression model to see overall trend in HOF PTS across the years.

hof_df <- hof_table

# Scatter plot of HOF PTS vs Year
plot(hof_df$Year, hof_df$PTS, main = "HOF PTS vs Year", ylim = c(0, 30), xlab = "Year", ylab = "Points")
fit <- lm(hof_df$PTS ~ hof_df$Year)
abline(fit)

# Note: The "m" and "b" variables are not necessary in R because the linear regression is directly fitted using the lm() function.

# If you want to extract the slope and intercept:
slope <- coef(fit)[2]
intercept <- coef(fit)[1]

# To display the equation of the regression line:
equation <- paste("y =", round(slope, 2), "x +", round(intercept, 2))
text(x = max(hof_df$Year), y = max(hof_df$PTS), labels = equation, pos = 4)


# Calculate the mean values for the "average NBA player today"
curr_points <- mean(nba2022_df$PTS)
curr_assists <- mean(nba2022_df$AST)
curr_rebounds <- mean(nba2022_df$TRB)
curr_blocks <- mean(nba2022_df$BLK)
curr_steals <- mean(nba2022_df$STL)

# Calculate the mean values for the "average Hall of Famer"
hof_points <- mean(hof_df$PTS, na.rm = T)
hof_assists <- mean(hof_df$AST, na.rm = T)
hof_rebounds <- mean(hof_df$TRB, na.rm = T)
hof_blocks <- mean(hof_df$BLK, na.rm = T)
hof_steals <- mean(hof_df$STL, na.rm = T)

# Compare the average player today with the average hall of famer across all categories
# Create a new figure with a specific size
par(mfrow = c(1, 5), mar = c(5, 4, 3, 2))

colors <- c('red', 'blue')

# Bar plot for "Points"
barplot(c(hof_points, curr_points), names.arg = c("Average Hall of Famer", "Average NBA Player"),
        col = colors, main = "Points")

# Bar plot for "Assists"
barplot(c(hof_assists, curr_assists), names.arg = c("Average Hall of Famer", "Average NBA Player"),
        col = colors, main = "Assists")

# Bar plot for "Rebounds"
barplot(c(hof_rebounds, curr_rebounds), names.arg = c("Average Hall of Famer", "Average NBA Player"),
        col = colors, main = "Rebounds")

# Bar plot for "Blocks"
barplot(c(hof_blocks, curr_blocks), names.arg = c("Average Hall of Famer", "Average NBA Player"),
        col = colors, main = "Blocks")

# Bar plot for "Steals"
barplot(c(hof_steals, curr_steals), names.arg = c("Average Hall of Famer", "Average NBA Player"),
        col = colors, main = "Steals")

# Method to calculate average of a category given a data frame and category name
calc_category_average <- function(hof_df, category) {
  player_category_stat <- as.numeric(hof_df[[category]])
  player_category_stat <- player_category_stat[!is.na(player_category_stat)]
  avg_stat <- sum(player_category_stat) / length(player_category_stat)
  return(avg_stat)
}
# Store results in a variable
avg_pts <- calc_category_average(hof_df, 'PTS')
avg_trb <- calc_category_average(hof_df, 'TRB')
avg_ast <- calc_category_average(hof_df, 'AST')
avg_stl <- calc_category_average(hof_df, 'STL')
avg_blk <- calc_category_average(hof_df, 'BLK')

avg_stats <- c(avg_pts, avg_trb, avg_ast, avg_stl, avg_blk)

# Create a function that returns the desired player stats
get_player_stats <- function(hof_df, player_name) {
  for (i in 1:nrow(hof_df)) {
    if (hof_df[i, "Name"] == player_name) {
      return(c(as.numeric(hof_df[i, "PTS"]), as.numeric(hof_df[i, "TRB"]), as.numeric(hof_df[i, "AST"]),
               as.numeric(hof_df[i, "STL"]), as.numeric(hof_df[i, "BLK"])))
    }
  }
  return(NULL)  # If player_name is not found
}

# Lets compare 3 point shooting for current players
avg_3pt_hof <- calc_category_average(hof_df, '3P%')
avg_3pt_curr <- calc_category_average(nba2022_df, 'X3P.') + .07 # add .07 for the players that have an average of 0.00

bar_data <- c(avg_3pt_hof, avg_3pt_curr)
bar_labels <- c("Average HOF", "Average Modern Player")
bar_colors <- c("red", "blue")

barplot(bar_data, names.arg = bar_labels, col = bar_colors, main = "Avg. HOF Player vs. Avg. Modern Player (3P%)",
        xlab = "Player Type", ylab = "3P Percentage", width = 0.9)

# Lets compare blocks for current players
avg_block_hof <- calc_category_average(hof_df, 'BLK')
avg_block_curr <- calc_category_average(nba2022_df, 'BLK')

bar_data <- c(avg_block_hof, avg_block_curr)
bar_labels <- c("Average HOF", "Average Modern Player")
bar_colors <- c("red", "blue")

barplot(bar_data, names.arg = bar_labels, col = bar_colors, main = "Avg. HOF Player vs. Avg. Modern Player (BLK)",
        xlab = "Player Type", ylab = "Block", width = 0.9)

# Below we'll examine the relationship between assists and points between Hall of Famers and today's players. As the number of points a player scores increases, will the assists also increase?

pts <- as.matrix(hof_df$PTS)
ast <- as.matrix(hof_df$AST)
model <- lm(ast ~ pts)
expected_ast <- predict(model)

par(mfrow = c(1, 1))

plot(pts, ast, main = "PPG and APG Relationship among HOF and 2022 players",
     xlab = "Points Per Game", ylab = "Assists Per Game", col = "red",
     pch = 16, xlim = range(pts), ylim = range(ast))
lines(pts, expected_ast, col = "maroon")

# Correlation of point and assist for HOF players
r1 <- cor(hof_df$PTS, hof_df$AST)

pts <- c()
ast <- c()
for (i in 1:nrow(nba2022_df)) {
  if (is.na(nba2022_df$AST[i]) || is.na(nba2022_df$PTS[i]) || nba2022_df$AST[i] < 0 || nba2022_df$PTS[i] < 10) {
    next
  }
  pts <- c(pts, nba2022_df$PTS[i])
  ast <- c(ast, nba2022_df$AST[i])
}

# Correlation of point and assist for current players
r2 <- cor(pts, ast)

model <- lm(ast ~ pts)
expected_ast <- predict(model)

points(pts, ast, col = "navy", pch = 16)
lines(pts, expected_ast, col = "navy")

legend("topleft", legend = c("HOF stats", "HOF player", "2022 stats", "2022 Player"),
       col = c("red", "maroon", "navy", "navy"), pch = c(16, NA, 16, NA), lty = c(0,1,0,1))

# Examine Michael Jordan and how he compares to other HOF players
jordan_stats <- get_player_stats(hof_df, "Michael Jordan")

par(mar = c(2, 4, 1, 1))
par(mfrow = c(5, 1))

categories <- c('PTS', 'TRB', 'AST', 'STL', 'BLK')

par(mfrow = c(5, 1))

for (i in 1:length(categories)) {
  bar_heights <- c(avg_stats[i], jordan_stats[i])
  bar_names <- c('Average', 'Jordan')
  bar_colors <- c('#fdb927', 'red')
  
  barplot(bar_heights, names.arg = bar_names, col = bar_colors, main = paste("Average HOF Player vs. Michael Jordan in", categories[i]), xlab = "Player", ylab = "Value", width = 0.3)
}

# Machine Learning aspect of Project: HOF vs. non HOF
nba_df <- read.csv("all_seasons.csv")

stats <- c('gp', 'pts', 'reb', 'ast', 'net_rating', 'oreb_pct', 'dreb_pct', 'usg_pct', 'ts_pct', 'ast_pct')
players <- nba_df %>% group_by(player_name)

mult_seasons <- character()
first_season <- list()

# Go through every player, mark whoever plays >1 season
for (player in unique(players$player_name)) {
  player_df <- subset(nba_df, player_name == player)
  if (nrow(player_df) > 1) {
    mult_seasons <- c(mult_seasons, player)
  }
  first_season[[player]] <- player_df$season[1]
}

# Go through main data frame now and keep only one row per duplicative player to ensure uniqueness + averaging of career stats
for (idx in 1:nrow(nba_df)) {
  player <- nba_df$player_name[idx]
  season <- nba_df$season[idx]
  if (player %in% mult_seasons) {
    if (first_season[[player]] == season) {
      player_df <- subset(nba_df, player_name == player)
      n <- nrow(player_df)
      for (stat in stats) {
        nba_df[idx, stat] <- sum(player_df[[stat]]) / n
      }
    } else {
      nba_df <- nba_df[-idx, ]
    }
  }
}

nba_df <- nba_df[, !names(nba_df) %in% "season"]

head(nba_df)

# now that it's only unique players, lets see if theyre in the HOF or not
nba_df$hof <- sapply(nba_df$player_name, function(x) {
  if (sum(hof_df$Name == x) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

non_hof_df1 <- subset(nba_df, hof == FALSE)
colnames(non_hof_df)[colnames(non_hof_df) == "player_name"] <- "Name"
colnames(non_hof_df)[colnames(non_hof_df) == "pts"] <- "PTS"
colnames(non_hof_df)[colnames(non_hof_df) == "reb"] <- "TRB"
colnames(non_hof_df)[colnames(non_hof_df) == "ast"] <- "AST"

head(non_hof_df)

# Remove un-drafted players from data set
non_hof_df <- non_hof_df[non_hof_df$draft_year != "Undrafted", ]

# Convert draft year values to numeric
non_hof_df$draft_year <- as.numeric(as.character(non_hof_df$draft_year))

# Create a separate dataset for players drafted after 1998
recent_players_df <- non_hof_df[non_hof_df$draft_year >= 1998, ]
non_hof_df <- non_hof_df[non_hof_df$draft_year < 1998, ]


# Modify the original method to only return points, rebounds, assists. This is because our non hof dataset doeesn't contain data for steals or blocks
get_player_stats_v2 <- function(hof_df, player_name) {
  player_stats <- c()
  for (i in 1:nrow(hof_df)) {
    if (hof_df[i, "Name"] == player_name) {
      player_stats <- c(player_stats, as.numeric(hof_df[i, c("PTS", "TRB", "AST")]))
      break
    }
  }
  return(player_stats)
}

# Input: The DF, name of player, and a threshold representing the number of categories a player must be better than the average hall of famer at.
# Output: Returns True if player_name is adequate enough to be inducted into the hall of fame
hofOrNah <- function(player_name, threshold, df) {
  playerStats <- get_player_stats_v2(df, player_name)
  if (is.null(playerStats)) {
    return(FALSE)
  }
  
  score <- 0
  if (playerStats[1] > avg_pts) {
    score <- score + 1
  }
  if (!is.na(playerStats[2]) && playerStats[2] > avg_trb) {
    score <- score + 1
  }
  if (playerStats[3] > avg_ast) {
    score <- score + 1
  }
  
  if (score >= threshold) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#Runs the training model on the hof and non-hof datasets and returns an accuracy percentagee for both classifications
#Input: The number of categories for the threshold (num_categories). Setting print_hof_results to true will display the output.
#Output: A tuple containing two floating point values representing the accuracies of hof and non_hof, respectively.

train_model <- function(num_categories, print_hof_results = FALSE, print_non_hof_results = FALSE) {
  hof_count <- 0
  non_hof_names <- non_hof_df$Name
  hof_names <- hof_df$Name
  
  if (print_non_hof_results) {
    cat("\nHall of Fame Classifications for non Hall of Famers:\n")
  }
  
  for (i in seq_along(non_hof_names)) {
    name <- non_hof_names[i]
    if (hofOrNah(name, num_categories, non_hof_df)) {
      hof_count <- hof_count + 1
      if (print_non_hof_results) {
        cat("YES", name, "should be a hall of famer\n")
      }
    } else {
      if (print_non_hof_results) {
        cat("NO", name, "should not be a hall of famer\n")
      }
    }
  }
  
  total <- nrow(non_hof_df)
  accuracy <- 100 - (hof_count / total * 100)
  cat("Accuracy of classifying non hall of famers:", accuracy, "%\n")
  non_hof_accuracy <- accuracy
  
  hof_count <- 0
  if (print_hof_results) {
    cat("\nHall of Fame Classifications for Actual Hall of Famers:\n")
  }
  
  for (i in seq_along(hof_names)) {
    name <- hof_names[i]
    if (hofOrNah(name, num_categories, hof_df)) {
      hof_count <- hof_count + 1
      if (print_hof_results) {
        cat("YES", name, "should be a hall of famer\n")
      }
    } else {
      if (print_hof_results) {
        cat("NO", name, "should not be a hall of famer\n")
      }
    }
  }
  
  total <- nrow(hof_df)
  accuracy <- hof_count / total * 100
  cat("Accuracy of classifying hall of famers:", accuracy, "%\n")
  hof_accuracy <- accuracy
  
  return(c(hof_accuracy, non_hof_accuracy))
}

nhf_accuracy = vector()
hf_accuracy = vector()

# Threshold of 1
result <- train_model(1)
nhf_accuracy <- c(nhf_accuracy, result[2])
hf_accuracy <- c(hf_accuracy, result[1])

# Threshold of 2
result <- train_model(2)
nhf_accuracy <- c(nhf_accuracy, result[2])
hf_accuracy <- c(hf_accuracy, result[1])

# Threshold 3
result <- train_model(3)
nhf_accuracy <- c(nhf_accuracy, result[2])
hf_accuracy <- c(hf_accuracy, result[1])


# Lets plot our findings
library(ggplot2)
library(gridExtra)

# Create separate plots for non hall of famer and hall of famer classifications
plot1 <- ggplot(data.frame(Threshold = 1:3, Accuracy = nhf_accuracy), aes(x = Threshold, y = Accuracy)) +
  geom_bar(stat = "identity") +
  ylim(0, 100) +
  labs(title = "Accuracy of non hall of famer classifications as threshold increases",
       x = "Threshold required",
       y = "Accuracy Percentage") +
  theme_bw()

plot2 <- ggplot(data.frame(Threshold = 1:3, Accuracy = hf_accuracy), aes(x = Threshold, y = Accuracy)) +
  geom_bar(stat = "identity") +
  ylim(0, 100) +
  labs(title = "Accuracy of hall of famer classifications as threshold increases",
       x = "Threshold required",
       y = "Accuracy Percentage") +
  theme_bw()

# Arrange the subplots into one column
combined_plot <- grid.arrange(plot1, plot2, ncol = 1)

# Display the combined plot
print(combined_plot)

train_model(1, print_hof_results = T)

#Let's run our classifier on today's players and see who will make into the hall of fame.

curr_player_names = c(nba2022_df$Player)
predicted_hof = c()

for(i in 1:length(curr_player_names)){
  name = curr_player_names[i]
  if(hofOrNah(name, 1, recent_players_df)){
    print(paste("YES based on his career averages so far,", name, "will be a hall of famer"))
    predicted_hof <- c(predicted_hof, name)
  }
}

#Now let's try running it with a parameter of 2 (below)
for(i in 1:length(curr_player_names)){
  name = curr_player_names[i]
  if(hofOrNah(name, 2, recent_players_df)){
    print(paste("YES based on his career averages so far,", name, "will be a hall of famer"))
    predicted_hof <- c(predicted_hof, name)
  }
}



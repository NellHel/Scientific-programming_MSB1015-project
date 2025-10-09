setwd("C:/Users/Nellie/Nellie/Interregnum nach Abi/Maastricht/Scientific programming/Project")

tf <- read.csv("TDF_Stages_History.csv")
# Woman TdF: df <- read.csv("TDFF_Stages_History.csv")

gc <- read.csv("TDF_Riders_History.csv")

# Extra column with team mates:

library(dplyr)
library(tidyr)
library(questionr)

# Split column "Winner.of.Stage" in "name of rider" and "team name"
tf1 <- tf %>%
  separate(Winner.of.stage, into = c("name", "team"), sep = " \\(", remove = FALSE) %>%
  mutate(team = sub("\\)$", "", team))

# One line included the also maiden name of the cyclist (TdF1908) so it thought the program was the cyclist
tf1[tf1$Winner.of.stage == "Henri Cornet (Geboren Jardy)  (Tdf 1908 ***)", "team"] <- "Tdf 1908 ***"

# 84 out of 465 stages are 'NA'. Often the name of the rider was missing 
# so when R split the data it puts the name of the team as name of the rider
rows_na <- tf1[is.na(tf1$team), ] # Manually checked the 84 samples: If "name" column was filled then it was in all cases a team name. And actually the winner rider name was missing (which I don't need for the analysis)
rows_team <- rows_na[!rows_na$Winner.of.stage == "",] # The 50 samples where only the team rider name is missing.
# Make the "name" column the "team column"
rows_team$team <- rows_team$name
# Make the "name" column NA
rows_team$name <- NA
# Save the row names: rownames(rows_team)

# Rename missing team names that accidentally ended up in the name column because the rider'S name was missing
tf1[rownames(rows_team), "team"] <- tf1[rownames(rows_team), "Winner.of.stage"]
tf1[rownames(rows_team), "name"] <- NA

# The name of the winner and "team" was missing for the 9th stage in 1907. 1907, there were no teams, so the team name is Tdf 1907***.
tf1[tf1$Year == "1907" & tf1$Stages == "9", "team"]<- "Tdf 1907 ***"
# The name of the winner of the 5th stage in 1909 was missing
tf1[tf1$Year == "1909" & tf1$Stages == "5", "name"]<- "François Faber"
tf1[tf1$Year == "1909" & tf1$Stages == "5", "team"]<- "Alcyon"
tf1[tf1$Year == "1921" & tf1$Stages == "12", "name"]<- "Émile Masson senior"
tf1[tf1$Year == "1971" & tf1$Stages == "0", "name"]<- "Eddy Merckx"
tf1[tf1$Year == "1971" & tf1$Stages == "0", "team"]<- "Molteni"
tf1[tf1$Year == "1974" & tf1$Stages == "6.1", "name"]<- "Jean-Luc Molineris"
tf1[tf1$Year == "1974" & tf1$Stages == "6.1", "team"]<- "Bic"
# Here maybe download kaggle data and take the missing values from there

# Check for duplicates
tapply(tf1$Stages, tf1$Year, anyDuplicated) #1924 (tie), 1934 (two half stages), 1937 and 1938 (both ties)
# Remove duplicates (for one stage, multiple rows)
tf1 <- tf1[-c(65,66,67),] #2022



# Count which team won more
total_wins <- tf1 %>%
  count(team, sort = TRUE)

# Count per year per team
counts_years <- tf1 %>%
  group_by(Year, team) %>%
  summarise(wins = n(), .groups = "drop") %>%
  arrange(Year, wins)

# GC wins top 10
gc_top10 <- gc[gc$Rank >=1 & gc$Rank <=10, ]
gc_top10 <- gc_top10[, c("Year", "Team", "Rank")]

# Merge gc_top10 and counts_years
counts_years$Team <- toupper(counts_years$team)
gc_stage_wins <- merge(gc_top10, counts_years, by = c("Year", "Team") , all = TRUE)
colnames(gc_stage_wins) <- c("Year", "Team", "Rank_GC", "team", "wins_stages")

# How many stages per year
stages_no <- freq(tf1$Year)
stages_no$Year <- rownames(stages_no)
stages_no <- stages_no[,c("Year", "n")]
gc_stage_wins <- merge(gc_stage_wins, stages_no, by = "Year", all = TRUE)
# Build data frame with points
gc_points <- data.frame(
  Rank_GC = c(1,2,3,4,5,6,7,8,9,10),
  Points_GC = c(25,18,15,12,10,8,6,4,2,1))
# Merge points to gc_stage_wins data frame
gc_stage_wins <- merge(gc_stage_wins, gc_points, by = "Rank_GC", all = TRUE)

# Stage_win share + GC share
gc_stage_wins$Win_Share <- gc_stage_wins$wins_stages / gc_stage_wins$n # Normalize by the total amount of possible stage wins for the respective year
gc_stage_wins$GC_Share <- gc_stage_wins$Points_GC / 101 # Normalize by the total amount of points

# Adding for each team each year the points together
# GC_Share can be added for every rider. Win_Share is only once the points for the team, so it's double in the table -> don't add
gc_shares <- aggregate(gc_stage_wins["GC_Share"], gc_stage_wins[c("Year", "Team")], sum, na.rm = TRUE)
gc_shares <- gc_shares[!duplicated(gc_shares[c("Year", "Team")]), ]

 
# Win_Share should not be added
win_shares <- gc_stage_wins[,c("Year","Team","Win_Share")]
win_shares <- unique(win_shares)

# Merge
tf_shares <- merge(win_shares, gc_shares, by = c("Year", "Team"), all = TRUE)
tf_shares[is.na(tf_shares$GC_Share), "GC_Share"] <- 0
tf_shares[is.na(tf_shares$Win_Share), "Win_Share"] <- 0

# Calculate hybrid strength
tf_shares$Strength <- 0.5 * tf_shares$Win_Share + 0.5 * tf_shares$GC_Share
tf_shares_max <- tf_shares %>%
  group_by(Year) %>%
  slice_max(Strength)

# Change TDF19xxx in "no teams"
tf_shares_max[grepl("TDF", tf_shares_max$Team), "Team"] <- "No Teams"
tf_shares_max$Color <- match(tf_shares_max$Team, unique(tf_shares_max$Team))
# Plot
# Assign unique colors for each team
team_colors <- setNames(hcl.colors(57, "Dark 3"), unique(tf_shares_max$Team))
tf_shares_max$Color <- team_colors[tf_shares_max$Team]
# unique_colors <- hcl.colors(length(unique(tf_shares_max$Color)), palette = "Dark 3")[tf_shares_max$Color]
plot(tf_shares_max$Year, tf_shares_max$Strength, type = "h", col = tf_shares_max$Color ,  xlab = "Year", ylab= "Hybrid strength index", title = "Strength of most dominant team per year")
legend("topright", legend = unique(tf_shares_max$Team), color = unique(tf_shares_max$Color))

# Calculate Dominance Index (HHI)
tf_shares$strength_square <- tf_shares$Strength * tf_shares$Strength
HHI <-rowsum(tf_shares$Strength_square, group = tf_shares$Year, na.rm = TRUE)
colnames(HHI) <- "HHI"
plot(rownames(HHI), HHI, type = "h")

# Average kilometer per year
km_avg <- tf1[ ,c("Year", "TotalTDFDistance")]
km_avg <- km_avg %>%
  group_by(Year) %>%
  slice_head(n = 1)
km_avg <- merge(km_avg, stages_no, by = "Year")
km_avg$KM_average <- km_avg$TotalTDFDistance / km_avg$n
plot(km_avg$Year, km_avg$KM_average, type = "l")
plot(km_avg$Year, km_avg$n, type = "h")

# Amount of teams per year
teams_no <- tf1[!duplicated(tf1[c("Year", "team")]), c("Year", "team")]
teams_number <- teams_no %>%
  group_by(Year) %>%
  summarise(teamnumber = n())
plot(teams_number$Year, teams_number$teamnumber, type = "h")


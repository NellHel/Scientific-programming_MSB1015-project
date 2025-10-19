setwd("C:/Users/Nellie/Nellie/Interregnum nach Abi/Maastricht/Scientific programming/Project")

tf <- read.csv("TDF_Stages_History.csv")
# Woman TdF: df <- read.csv("TDFF_Stages_History.csv")

gc <- read.csv("TDF_Riders_History.csv")

# Extra column with team mates:

library(dplyr)
library(tidyr)
library(questionr)#
library(readxl)
library(ggplot2)

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

# Upload Kaggle data
kaggle <- read.csv("stage_data.csv")
kaggle_clean <- kaggle %>%
  group_by(year, stage_results_id) %>%
  slice_head(n=1)
rm(kaggle)
kaggle_clean <- separate(kaggle_clean, stage_results_id, into = c("StageText", "Stages"), sep = "-", convert = TRUE)
kaggle_clean <- kaggle_clean[ ,c("year", "Stages", "rider", "team")]
colnames(kaggle_clean) <- c("Year", "Stages", "rider", "team")

----------------------------------------------------------------------------------
# rows_missing
rows_missing <- merged[is.na(merged$team), ]
-----------------------------------------------------------------------------------

merged <- merge(tf1, kaggle_clean, by = c("Year", "Stages"), all.x = TRUE, suffixes = c("", "_ans"))

# Fill missing values from _ans columns
merged$team[is.na(tf1$team)] <- merged$team_ans[is.na(tf1$team)]
merged$name[is.na(merged$name)] <- merged$rider[is.na(merged$name)]

# Drop the helper columns
merged <- merged[c("Year", "Stages", "TotalTDFDistance", "Start", "End", "Winner.of.stage", "name", "team", "Yellow.Jersey", "Green.jersey", "Polka.dot.jersey", "White.jersey", "Leader")]             
rm(tf1)
tf1 <- merged

--------------------------------------------------------------------------------------
# The name of the winner and "team" was missing for the 9th stage in 1907. 1907, there were no teams, so the team name is Tdf 1907***.
# tf1[tf1$Year == "1907" & tf1$Stages == "9", "team"]<- "Tdf 1907 ***"
# The name of the winner of the 5th stage in 1909 was missing
# tf1[tf1$Year == "1909" & tf1$Stages == "5", "name"]<- "François Faber"
# tf1[tf1$Year == "1909" & tf1$Stages == "5", "team"]<- "Alcyon"
# tf1[tf1$Year == "1921" & tf1$Stages == "12", "name"]<- "Émile Masson senior"
# tf1[tf1$Year == "1971" & tf1$Stages == "0", "name"]<- "Eddy Merckx"
# tf1[tf1$Year == "1971" & tf1$Stages == "0", "team"]<- "Molteni"
# tf1[tf1$Year == "1974" & tf1$Stages == "6.1", "name"]<- "Jean-Luc Molineris"
# tf1[tf1$Year == "1974" & tf1$Stages == "6.1", "team"]<- "Bic"
# Here maybe download kaggle data and take the missing values from there
---------------------------------------------------------------------------------

# Check for duplicates
tapply(tf1$Stages, tf1$Year, anyDuplicated) #1924 (tie), 1934 (two half stages), 1937 and 1938 (both ties)
# Remove duplicates (for one stage, multiple rows)
tf1 <- tf1[-c(65,66,67),] #2022

----------------------------------------------------------------------------------------
# Count which team won more
total_wins <- tf1 %>%
  count(team, sort = TRUE)
-----------------------------------------------------------------------------------------

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

----------------------------------------------------------------------------------------------
# Plot
# Assign unique colors for each team
team_colors <- setNames(hcl.colors(57, "Dark 3"), unique(tf_shares_max$Team))
tf_shares_max$Color <- team_colors[tf_shares_max$Team]
# unique_colors <- hcl.colors(length(unique(tf_shares_max$Color)), palette = "Dark 3")[tf_shares_max$Color]
plot(tf_shares_max$Year, tf_shares_max$Strength, type = "h", col = tf_shares_max$Color ,  xlab = "Year", ylab= "Hybrid strength index", title = "Strength of most dominant team per year")
legend("topright", legend = unique(tf_shares_max$Team), color = unique(tf_shares_max$Color))
plot(tf_shares_max$Win_Share, tf_shares_max$GC_Share)
# ggplot2
pal <- setNames(hcl.colors(length(tf_shares_max$Team), "Dark 3"), length(tf_shares_max$Team))

ggplot(tf_shares_max, aes(x = Year, y = Strength, color = Team)) +
  geom_segment(aes(xend = Year, y = 0, yend = Strength), linewidth = 1.2) +
  scale_color_manual(values = hcl.colors(length(unique(tf_shares_max$Team)), "Dark 3"),
                     name = "Dominant Team") +
  labs(title = "Most dominant team by year",
       x = "Year",
       y = "Strength score") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "right")
----------------------------------------------------------------------------------------

# Calculate Dominance Index (HHI)
tf_shares$strength_square <- tf_shares$Strength * tf_shares$Strength
HHI <- rowsum(tf_shares$strength_square, group = tf_shares$Year, na.rm = TRUE)
colnames(HHI) <- "HHI"
plot(rownames(HHI), HHI, type = "h", xlab = "Year", ylab = "HHI index")


# Average kilometer per year
km_avg <- tf1[ ,c("Year", "TotalTDFDistance")]
km_avg <- km_avg %>%
  group_by(Year) %>%
  slice_head(n = 1)
km_avg <- merge(km_avg, stages_no, by = "Year")
km_avg$KM_average <- km_avg$TotalTDFDistance / km_avg$n
----------------------------------------------------------------------------------------------
plot(km_avg$Year, km_avg$KM_average, type = "h", xlab = "Year", ylab = "Kilometer", title = "Average length of stage over the years")
plot(km_avg$Year, km_avg$n, type = "h",  xlab = "Year", ylab= "Number of stages", title = "Average number of stages over the years")
------------------------------------------------------------------------------------------

# Amount of teams per year
teams_no <- tf1[!duplicated(tf1[c("Year", "team")]), c("Year", "team")]
teams_number <- teams_no %>%
  group_by(Year) %>%
  summarise(teamnumber = n())
-------------------------------------------------------------------------------------
plot(teams_number$Year, teams_number$teamnumber, type = "h", xlab = "Year", ylab = "Number of teams", title = "Number of teams over the years")
-------------------------------------------------------------------------------------

# Proportion of doping per year (maybe normalize to total number of riders per year)
doping <- read_excel("Doping.xlsx")
doping <- doping[, c("Rider", "Year")]
doping_no <- doping %>%
  group_by(Year) %>%
  summarise(Doping_Cases = n())
----------------------------------------------------------------------------------
plot(doping_no$Year, doping_no$Doping_Cases, type = "h", xlab = "Year", ylab = "Number of doping cases", title = "Doping cases over the years")
-----------------------------------------------------------------------------------

# Budget from 2002 - 2024 adjusted for inflation
budget <- read_excel("Budget.xlsx")
budget$`WorldTour average budget per team` <- as.numeric(budget$`WorldTour average budget per team`)
------------------------------------------------------------------------------------
plot(budget$Year, budget$`WorldTour average budget per team`, type = "l", xlab = "Year", ylab = "Inflation adjusted team budget (Euro)")
----------------------------------------------------------------------------------------

# Correlation between HHI index and average kilometer per year, amount of teams, doping per year, budget
Corr_HHI <- data.frame(
  Year = rownames(HHI),
  HHI = HHI,
  Strength = tf_shares_max$Strength,
  KM_average = km_avg$KM_average,
  Teams_number = teams_number$teamnumber
)
Corr_HHI <- merge(Corr_HHI, doping_no, by = "Year", all = TRUE)
Corr_HHI[is.na(Corr_HHI$Doping_Cases), "Doping_Cases"] <- 0
Corr_HHI <- merge(Corr_HHI, budget, by = "Year", all = TRUE)
Corr_HHI <- Corr_HHI[ ,c("HHI", "Strength", "KM_average" ,"Teams_number", "Doping_Cases", "WorldTour average budget per team")]
corr_matrix <- cor(Corr_HHI, Corr_HHI, use = "pairwise.complete.obs", method = "spearman")
# p values for correlation values
library(Hmisc)
res <- rcorr(as.matrix(Corr_HHI), type = "spearman")

heatmap(corr_matrix, 
        Colv = NA, Rowv = NA, 
        scale = "none",
        xlab = c("HHI", "Strength", "KM_average" ,"Teams_number", "Doping_Cases", "WorldTour average budget per team"),
        col = colorRampPalette(c("blue", "white", "red"))(100))

library(gplots)

heatmap.2(
  as.matrix(corr_matrix),
  Colv = NA, Rowv = NA,
  scale = "none",
  trace = "none",
  col = colorRampPalette(c("blue", "white", "red"))(100),
  xlab = "Variables",
  key = TRUE,           # ✅ adds color scale
  key.title = "Correlation",
  key.xlab = "r",
  margins = c(6, 6)
)
--------------------------------------------------------------------------------- 
library(ggplot2)
library(reshape2)

# Melt for ggplot2
r_melted <- melt(res$r, varnames = c("Var1","Var2"), value.name = "r")
p_melted <- melt(res$P, varnames = c("Var1","Var2"), value.name = "p")
# Join r_melted and p_melted and assign p value scores 
plot_df <- left_join(r_melted, p_melted, by = c("Var1","Var2")) %>%
  mutate(stars = case_when(
    p < .001 ~ "***",
    p < .01  ~ "**",
    p < .05  ~ "*",
    TRUE     ~ ""
  ),
  label = ifelse(is.na(r), "", sprintf("%.2f%s", r, stars)))

ggplot(plot_df, aes(Var1, Var2, fill = r)) +
  geom_tile() +
  geom_text(aes(label = label), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Correlation") +
  labs(
    title = "Correlation Matrix with Significance",
    subtitle = "Spearman correlations between team variables",
    caption = "Stars indicate significance levels (* p < 0.05, ** p < 0.01, *** p < 0.001)",
    x = NULL,   # or use "Variable 1"
    y = NULL    # or use "Variable 2"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
----------------------------------------------------------------------------------

melted <- melt(corr_matrix)
ggplot(melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

---------------------------------------------------------------------------------
# Team budget for 2024 - 2025
budgets_team <- read_excel("Team_Budgets_2024_2025.xlsx")
budgets_team$Team_Year <- paste(budgets_team$Team, budgets_team$Year, sep = "_")
budgets_team <- budgets_team[ , c("Team_Year", "Budget (Millionen in Euro)")]
---------------------------------------------------------------------------------

# Unsupervised random forest, similarity matrix -> distance matrix, PCOA

# Install if needed
#install.packages("randomForest")
library(randomForest)

# Select data
data <- tf_shares[ , c("Year", "Team", "Win_Share", "GC_Share")]
data$Team_Year <- paste(data$Team, data$Year, sep = "_")
rownames(data) <- data$Team_Year
# Remove the year where there were no teams
data <- data[!data$Year %in%  c(1903:1908, 1919:1923), ]
# Create colors for each unique year (needed later for visualization)
years <- factor(data$Year) 
# Select only features 
data <- data[ , c("Win_Share", "GC_Share")]

# Step 1: Create synthetic data by permuting each column
synthetic <- apply(data, 2, sample)

# Step 2: Combine real + synthetic
combined <- rbind(data, synthetic)
labels <- factor(c(rep(1, nrow(data)), rep(0, nrow(synthetic))))

# Step 3: Fit Random Forest
rf_unsup <- randomForest(x = combined, y = labels, proximity = TRUE)

# Step 4: Extract proximity matrix (for the real data only)
prox <- rf_unsup$proximity[1:nrow(data), 1:nrow(data)]

# Step 5: Convert proximity in distance matrix
diss <- 1 - prox


# Principle Coordinate Analysis

# PCoA using ape package
# install.packages("ape")
library(ape)

pcoa_res <- pcoa(as.dist(diss))

# Visualize
# Assign
palette <- rainbow(length(levels(years)))
# To interpret the importance of each variable in the orthogonal space
library(vegan)
envfit_res <- envfit(pcoa_res$vectors ~ ., data = data[, c("Win_Share", "GC_Share")])
# Plot the first two axes
plot(pcoa_res$vectors[,1], pcoa_res$vectors[,2],
     xlab = paste0("PCoA1 (", round(pcoa_res$values$Relative_eig[1]*100, 1), "%)"),
     ylab = paste0("PCoA2 (", round(pcoa_res$values$Relative_eig[2]*100, 1), "%)"),
     main = "PCoA based on Random Forest dissimilarities",
     pch = 19)
plot(envfit_res, p.max = 0.05) # arrows show variables significantly aligned with axes

legend("topright", legend = levels(years), col = palette, pch = 19, title = "Year")


library(ggplot2)
data$Team_Year <- rownames(data)
merged_data <- merge(data, budgets_team, by = "Team_Year", all.x = TRUE)
merged_data <- separate(merged_data, Team_Year, into = c("Team_1", "Year"), sep = "_", convert = TRUE)
merged_data <- merged_data[order(merged_data$Year), ]

-----------------------------------------------------------------------------------
PCoA1_2 <- data.frame(PCoA1 = pcoa_res$vectors[,1],
                 PCoA2 = pcoa_res$vectors[,2],
                 Year = years,
                 GC_Share = data$GC_Share,
                 Win_Share = data$Win_Share,
                 Budget = merged_data$`Budget (Millionen in Euro)`
                 )

ggplot(PCoA1_2, aes(PCoA1, PCoA2, color = Budget, size = Win_Share)) +
  geom_point() +
  theme_minimal() +
  labs(title = "PCoA colored by Year",
       x = paste0("PCoA1 (", round(pcoa_res$values$Relative_eig[1]*100, 1), "%)"),
       y = paste0("PCoA2 (", round(pcoa_res$values$Relative_eig[2]*100, 1), "%)"),
       color = "Budget (Mill. EURO)",
       size = "Win Share"
       ) #+
  #scale_color_gradient(low = "green", high = "blue")

plot(data$Win_Share, data$GC_Share)

h<- read.delim("Doping_per_team.txt")

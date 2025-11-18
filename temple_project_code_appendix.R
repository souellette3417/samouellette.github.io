##-------------LOGISTIC REGRESSION MODEL--------------##


library(tidyverse)
library(dplyr)
setwd('/Users/samouellette/Desktop')
#import data set
df=read_csv("temple_football_data.csv")






#create binary EPA varaible
#EPA>0 means variable=1 (EPA>0 defines success)
df <- df %>%
  mutate(
    success = EXPECTED_POINTS_ADDED > 0   
  )

#handle missing data
df$RUN_PASS[is.na(df$RUN_PASS)] <- "X"

df <- df %>%
  mutate(
    # convert run/pass to factor
    RUN_PASS = as.factor(RUN_PASS),
    
    # hash to factor
    HASH = as.factor(HASH),
    
    # quarter to factor
    QUARTER = as.factor(QUARTER),
    
    #create binary variable for side of field
    FIELD_SIDE = ifelse(FIELD_POSITION < 0, "OWN", "OPP"),
    FIELD_SIDE = as.factor(FIELD_SIDE),
    
    # create yards-to-endzone
    yards_to_endzone = case_when(
      FIELD_POSITION < 0 ~ 100 - abs(FIELD_POSITION),
      FIELD_POSITION > 0 ~ abs(FIELD_POSITION),
      TRUE ~ 50
    ),
    
    # score differential
    SCORE_DIFF = OFF_SCORE - DEF_SCORE,
    
    # extract minutes and seconds
    minutes = as.numeric(sub(":.*", "", CLOCK)),
    seconds = as.numeric(sub(".*:", "", CLOCK)),
    
    # seconds left in quarter
    seconds_left_in_quarter = minutes * 60 + seconds,
    
    # total time remaining in the game
    time_remaining = (4 - as.numeric(QUARTER)) * 900 + seconds_left_in_quarter
  )


#create variable for EPA of the previous play using lag function
#how does momentum impact success?
df <- df %>%
  group_by(DEF_TEAM, DRIVE) %>%
  mutate(
    prev_epa = lag(EXPECTED_POINTS_ADDED)
  ) %>%
  ungroup() %>%
  mutate(
    prev_epa = ifelse(is.na(prev_epa), 0, prev_epa)
  )


#create variable that looks at previous play type(run or pass)
#using th lag fucntion to see how it affects the next play's EPA
df <- df %>%
  group_by(DEF_TEAM, DRIVE) %>%
  mutate(
    prev_run_pass = lag(RUN_PASS)
  ) %>%
  ungroup() %>%
  mutate(
    prev_run_pass = ifelse(is.na(prev_run_pass), "NONE", prev_run_pass),
    prev_run_pass = as.factor(prev_run_pass)
  )



df$RUN_PASS[is.na(df$RUN_PASS)] <- "NONE"
df$RUN_PASS <- as.factor(df$RUN_PASS)

df$DRIVE_PLAY <- as.numeric(df$DRIVE_PLAY)
df$DRIVE_PLAY[is.na(df$DRIVE_PLAY)] <- 1   # or 1 if that makes more sense


#logistic regression equation to see what factors affect success the most
model <- glm(
  success ~ DOWN + DISTANCE + RUN_PASS + yards_to_endzone + HASH + SCORE_DIFF + QUARTER + time_remaining + FIELD_SIDE + prev_epa + prev_run_pass + DRIVE_PLAY,
  data = df,
  family = binomial()
)

summary(model)


#check model accuracy
library(pROC)

df$pred_prob <- predict(model, type = "response")

roc_obj <- roc(df$success, df$pred_prob)
auc(roc_obj)



#build ROC plot
df$pred_prob <- predict(model, type = "response")
roc_obj <- roc(df$success, df$pred_prob)
plot(roc_obj,
     col = "#1c61b6",
     lwd = 3,
     main = "ROC Curve for Offensive Success Model")
text(0.6, 0.2,
     labels = paste("AUC =", round(auc(roc_obj), 3)),
     cex = 1.3)

#plot for predicted probability of success vs run
# create distance buckets 
df <- df %>%
  mutate(
    distance_bucket = case_when(
      DISTANCE <= 3 ~ "Short (1–3)",
      DISTANCE <= 7 ~ "Medium (4–7)",
      DISTANCE <= 12 ~ "Long (8–12)",
      TRUE ~ "Very Long (13+)"
    ),
    distance_bucket = factor(distance_bucket,
                             levels = c("Short (1–3)", "Medium (4–7)",
                                        "Long (8–12)", "Very Long (13+)"))
  )

# rename legend
df_no_x <- df %>%
  filter(RUN_PASS %in% c("P", "R")) %>%
  mutate(RUN_PASS = recode(RUN_PASS,
                           "P" = "Pass",
                           "R" = "Run"))

# recalculate average predicted probabilities
plot_df <- df_no_x %>%
  group_by(distance_bucket, RUN_PASS) %>%
  summarize(mean_prob = mean(pred_prob), .groups = "drop")

# plot
ggplot(plot_df, aes(x = distance_bucket, y = mean_prob,
                    color = RUN_PASS, group = RUN_PASS)) +
  geom_line(size = 1.4) +
  geom_point(size = 3) +
  labs(
    title = "Predicted Probability of Success: Run vs Pass",
    x = "Distance to First Down",
    y = "Predicted Probability of Success",
    color = "Play Type"
  ) +
  scale_color_manual(values = c("Pass" = "red", "Run" = "green")) +
  theme_minimal(base_size = 16)





##---------------FORMATION PERFORMANCE MODEL--------------------##


library(ggplot2)
library(lme4)


data = read_csv("temple_football_data.csv")

#filtering offensive plays
data_offense <- data %>%
  filter(OFF_TEAM == "Temple") %>%
  select(GAIN_LOSS_NET, BALL_CARRIER, QB, PASS_RECEIVER_TARGET,
         OFF_FORMATION, OFF_PERSONNEL_GROUP, EXPECTED_POINTS_ADDED,
         DEF_FRONT, DEF_PERSONNEL, PASS_COVERAGE_BASIC, BLITZ,
         DOWN, DISTANCE, FIELD_POSITION, QUARTER, TEMPO)

#exploratory analysis 

#CHANGING 'GAIN_LOSS_NET' TO A NUMERIC INSTEAD OF CHR
data_offense <- data_offense %>%
  mutate(GAIN_LOSS_NET = as.numeric(GAIN_LOSS_NET))


#running back yardage per carry
yards_per_carrier <- data_offense %>%
  filter(!is.na(BALL_CARRIER)) %>%
  group_by(BALL_CARRIER) %>%
  summarise(
    avg_yards = mean(GAIN_LOSS_NET,na.rm=TRUE),
    n_plays = n()
  ) %>%
  arrange(desc(avg_yards))
print(yards_per_carrier)

#____________________________________________________________________________________


#Formation analysis
yards_per_formation <- data_offense %>%
  filter(!is.na(OFF_FORMATION)) %>%            # remove any NA formations
  mutate(
    GAIN_LOSS_NET = as.numeric(GAIN_LOSS_NET),
    EXPECTED_POINTS_ADDED = as.numeric(EXPECTED_POINTS_ADDED)  # ensure numeric
  ) %>%
  group_by(OFF_FORMATION) %>%
  summarise(
    avg_yards = mean(GAIN_LOSS_NET, na.rm = TRUE),                # average yards
    avg_epa = mean(EXPECTED_POINTS_ADDED, na.rm = TRUE),          # average expected points added
    n_plays = n()                                                  # number of plays
  ) %>%
  filter(n_plays > 4) %>%
  arrange(desc(n_plays))                                            # sort by number of plays

print(yards_per_formation, n = 168)

ggplot(yards_per_formation, aes(x = avg_yards, y = avg_epa)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +   # points
  geom_text(aes(label = OFF_FORMATION), hjust = 0.5, vjust = -0.5, size = 3, check_overlap = TRUE) + # optional labels
  labs(
    title = "Average Yards vs Average EPA by Formation",
    x = "Average Yards",
    y = "Average Expected Points Added (EPA)"
  ) +
  theme_minimal()

#____________________________________________________________________________________
ggplot(
  yards_per_formation %>%
    filter(OFF_FORMATION != "TE-L^; HB-L; SRiWR; SRoWR; RWR^"),
  aes(
    x = reorder(OFF_FORMATION, avg_epa),
    y = avg_epa,
    fill = avg_epa > 0
  )
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("TRUE" = "green3", "FALSE" = "red3")) +
  labs(
    title = "All Formations by Average EPA (Excluded 1 Invalid Formation)",
    x = "Offensive Formation",
    y = "Average Expected Points Added (EPA)",
    fill = "EPA > 0?"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

top_formations <- yards_per_formation %>%
  arrange(desc(avg_yards)) %>%
  slice(1:25)

ggplot(top_formations, aes(x = reorder(OFF_FORMATION, -avg_yards), y = avg_yards)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(
    title = "Top 25 Formations by Average Yards",
    x = "Offensive Formation",
    y = "Average Yards"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






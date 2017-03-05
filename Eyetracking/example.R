# load *eyetrackingR* and set data options
library(eyetrackingR)
data("word_recognition")
dataset <- make_eyetrackingr_data(word_recognition, 
                                  participant_column = "ParticipantName",
                                  trial_column = "Trial",
                                  time_column = "TimeFromTrialOnset",
                                  trackloss_column = "TrackLoss",
                                  aoi_columns = c('Animate','Inanimate'),
                                  treat_non_aoi_looks_as_missing = TRUE
)
# remove trackloss-ridden trials
dataset_clean <- clean_by_trackloss(dataset, 
                                    participant_prop_thresh = 1, trial_prop_thresh = .25, 
                                    window_start_time = 15500, window_end_time = 21000)

# zoom in on response window
word_window <- subset_by_window(dataset_clean, rezero = FALSE,
                                window_start_time = 15500, window_end_time = 21000)

# create a column indicating what type of trial:
word_window$Target <- as.factor( ifelse(test = grepl('(Spoon|Bottle)', word_window$Trial), 
                                        yes = 'Inanimate', 
                                        no  = 'Animate') )

# convert data into a series of time-bins:
word_time <- make_time_sequence_data(word_window, time_bin_size = 100, 
                                     predictor_columns = "Target", aois = c("Animate"))
plot(word_time, predictor_column = "Target")




# An important step in performing regression analysis is to center predictors (in order to make parameter estimates more interpretable)
word_time$TargetC <- ifelse(word_time$Target == 'Animate', .5, -.5)
word_time$TargetC <- word_time$TargetC - mean(word_time$TargetC)

# perform a growth-curve analysis
library(lme4)
model <- lmer(Elog ~ TargetC*(ot1 + ot2 + ot3 + ot4 + ot5) + (1 | Trial) + (1 | ParticipantName), data = word_time, REML = FALSE)
broom::tidy(model, effects="fixed")
drop1(model,~.,test="Chi")


# load the data
load("NDSU_data_6_19")

# create new dataset that pulls student_id (2), student_or_la (3), pre_agree_to_participate(4)
# pre_duration(6), post_agree_to_participate(7), post_duration (9), pre_answered (10),
# post_answered(11), pre_score (12), post_score(13), gender_text (16), male (17), female (18), year_in_school (31)
# pre1-30 (38-67), post1-30 (85-114), course_id (138), institution_id (139), if it's the FCI (161), 
# and how the data were collected (199)
LASSO <- subset(NDSU_data_6_19, select=c(2, 3, 4, 6, 7, 9, 10, 11, 12, 13, 16, 17, 18, 31, 38:67, 85:114, 138, 139, 152, 199))

# Add two columns to figure out the fraction of students per course who participated in the pre and
# post tests
LASSO$pre_percent_participate <- NDSU_data_6_19$pre_tests_completed / NDSU_data_6_19$students_enrolled
LASSO$post_percent_participate <- NDSU_data_6_19$post_tests_completed / NDSU_data_6_19$students_enrolled

# Now we need to filter the data and record the results of the filtering.  We'll filter in the order
# that Jayson & Ben did in their paper.  First, let's find the total number of students in the data
totalstudents <- nrow(LASSO)

# Now we remove all students that have "NA" in the FCI spot.  For this particular dataset, this means
# that they did not respond to either the pretest or the postest
fcifilter <- subset(LASSO, !is.na(FCI))
fcifilterstudents <- nrow(fcifilter)

# We'll remove students that don't identify as a "Student" (LAs).  We don't *have* any LAs in the 
# NDSU_data_6_19 dataset, so this filter doesn't really do anything, but it's good to have
lafilter <- subset(fcifilter, student_or_la = "Student")
lafilterstudents <-nrow(lafilter)

# Before doing any more filtering, we need to deal with the "admin_upload" data, though this data
# was NOT collected online, whihc we might not want to include.  At any rate, for now we'll replace
# the "NA" in the pre_duration column and the post_duration column with "999999" for any that satisfy 
# "admin_upload" in "how_collected"
adminfilter <- lafilter
adminfilter$pre_duration[adminfilter$how_collected == "admin_upload"] <- 999999
adminfilter$post_duration[adminfilter$how_collected == "admin_upload"] <- 999999

# Next we'll only keep students that completed thet test online.  At this point, I'm more confident in
# the analysis if all of the data were collected the same way (online), given the differences in scores 
# that I've seen when using LASSO vs pencil/paper
onlinefilter <- subset(adminfilter, how_collected == "online")
onlinefilterstudents <- nrow(onlinefilter)

# Next we'll make sure that each student submitted either a pre or post test, via the pre_duraton and
# post_duration columns, making sure they're not NA.  This will also screen out students who do not
# agree to participate, or who are under 18 (for this dataset, check on others)
submissionfilter <- subset(onlinefilter, !is.na(pre_duration) | !is.na(post_duration))
submissionfilterstudents <- nrow(submissionfilter)

# Filter for students that took less than 300 seconds (5 minutes) on the pre/post test
# We'll replace the value in pre_duration or post_duration with NA, and then re-run the
# prior filter
submissionfilter$pre_duration[submissionfilter$pre_duration < 300] <- NA
submissionfilter$post_duration[submissionfilter$post_duration < 300] <- NA

timefilter <- subset(submissionfilter, !is.na(pre_duration) | !is.na(post_duration))
submissionfilterstudents <- nrow(timefilter)

# # Next we'll drop students that do not agree to participate, either on the pre or post, and see how
# # many we lose at each stage
# prefilter <- LASSO[LASSO$pre_agree_to_participate == "I agree to participate" | LASSO$pre_agree_to_participate == "I agree to share",]
# prefilterstudents <- nrow(prefilter)
# 
# postfilter <- prefilter[prefilter$post_agree_to_participate == "I agree to participate" | prefilter$post_agree_to_participate == "I agree to share",]
# postfilterstudents <- nrow(postfilter)

# # Filter out students that took less than 5 minutes on either the pretest or the posttest
# pre5filter <- adminfilter[adminfilter$pre_duration > 300 & !is.na(adminfilter$pre_duration),]
# pre5filterstudents <- nrow(pre5filter)
# 
# post5filter <- pre5filter[pre5filter$post_duration > 300 & !is.na(pre5filter$post_duration),]
# post5filterstudents <- nrow(post5filter)

# Filter out students who answered less than 80% of the questions on either the pretest or the post
pre80filter <- post5filter[post5filter$pre_answered > 80,]
pre80filterstudents <- nrow(pre80filter)

post80filter <- pre80filter[pre80filter$post_answered > 80,]
post80filterstudents <- nrow(post80filter)

# Drop students (and thus courses) where the percent participation was less than 40%
missing60filter <- post80filter[post80filter$percent_participate > 0.40,]
missing60filterstudents <- nrow(missing60filter)

# This is the dataset we'll actually run our analysis on.  If any additional filtering needs to be
# done, we should be able to add it above so that we don't need to change the "LASSOfiltered" name
LASSOfiltered <- missing60filter

LASSOfiltered_male <- LASSOfiltered[LASSOfiltered$male == 1,]
males_filtered <- nrow(LASSOfiltered_male)

LASSOfiltered_female <- LASSOfiltered[LASSOfiltered$female == 1,]
females_filtered <- nrow(LASSOfiltered_female)

# Create vectors of names that we'll reuse, making our new columns "pre_1_C", "pre_2_C", etc.
# which will be populated with 0s (incorrect), 1s (correct), or NA (no response)
qnames <- colnames(select(timefilter, pre_1:pre_30, post_1:post_30))
qcnames <- paste(qnames, "C", sep = "_")

# Add blank columns to our dataframe
timefilter[qcnames] <- NA

# Make subsets of just pretests and just posttests
pretests <- select(timefilter[!is.na(timefilter$pre_duration),], -(post_1:post_30),-(post_1_C:post_30_C))
posttests <- select(timefilter[!is.na(timefilter$post_duration),], -(pre_1:pre_30),-(pre_1_C:pre_30_C))

# Import the FCIkey, convert it to lowercase, then populate the responses in the _C columns with
# 1 for correct answers and 0 for incorrect answers in the "pretests" and "posttests" dataframes
key <- tolower(read.csv("FCIkey.csv", stringsAsFactors = FALSE, header = FALSE))
for (i in seq_along(key)){
  pretests[50+i][pretests[14+i] == key[[i]]] <- 1
  pretests[50+i][pretests[14+i] != key[[i]]] <- 0
  posttests[50+i][posttests[14+i] == key[[i]]] <- 1
  posttests[50+i][posttests[14+i] != key[[i]]] <- 0
}

# Create subset dataframes of men & women
pretests_males = subset(pretests, pretests$male == 1)
pretests_females = subset(pretests, pretests$female == 1)
posttests_males = subset(posttests, posttests$male == 1)
posttests_females = subset(posttests, posttests$female == 1)

# # Find means of these subsets
# library(tidyr)
# library(dplyr)
# ctt_male_post_diff = sapply(select(posttests_males, post_1_C:post_30_C), mean, na.rm = TRUE)
# ctt_female_post_diff = sapply(select(posttests_females, post_1_C:post_30_C), mean, na.rm = TRUE)
# ctt_male_pre_diff = sapply(select(pretests_males, pre_1_C:pre_30_C), mean, na.rm = TRUE)
# ctt_female_pre_diff = sapply(select(pretests_females, pre_1_C:pre_30_C), mean, na.rm = TRUE)

# # Function that generalizes "mean" so it's bootstrappable
# library(boot)
# bootstrapmean <- function(x, d) {
#   sampledmean <- mean(x[d], na.rm = TRUE)
#   return(sampledmean)
# }

# # Function that finds the error bars on our means, takes the array/matrix/tibble of "scores" (1s or 0s)
# # and returns a vector with the standard deviation of hte bootstrapped means as each element, 
# # with the names of the columns taken from the name of the "scores"
# bootstrap_errorbar <- function(scores){
#   error <- vector(length = length(scores), mode = "numeric")
#   names(error) <- colnames(scores)
#   bootstrapresults <- lapply(scores, boot, statistic = bootstrapmean, R = 1000)
#   for (i in seq_along(bootstrapresults)){
#     error[[i]] <- sd(bootstrapresults[[i]][["t"]])
#   }
#   return(error)
# }
# 
# ctt_male_post_error <- bootstrap_errorbar(select(posttests_males, post_1_C:post_30_C))
# ctt_male_pre_error <- bootstrap_errorbar(select(pretests_males, pre_1_C:pre_30_C))
# ctt_female_post_error <- bootstrap_errorbar(select(posttests_females, post_1_C:post_30_C))
# ctt_female_pre_error <- bootstrap_errorbar(select(pretests_females, pre_1_C:pre_30_C))
# 
# psycho = data.frame(ctt_male_pre_diff, ctt_male_pre_error, ctt_male_post_diff, ctt_male_post_error, ctt_female_pre_diff, ctt_female_pre_error, ctt_female_post_diff, ctt_female_post_error)
# row.names(psycho) <- paste("Q", 1:30, sep = "")
# 
# ctt_male_delta = ctt_male_post_diff - ctt_male_pre_diff
# ctt_female_delta = ctt_female_post_diff - ctt_female_pre_diff
# ctt_male_delta_se = sqrt(ctt_male_post_error**2 + ctt_male_pre_error**2)
# ctt_female_delta_se = sqrt(ctt_female_post_error**2 + ctt_female_pre_error**2)
# ctt_delta_delta = ctt_male_delta - ctt_female_delta
# ctt_delta_se = sqrt(ctt_male_delta_se**2 + ctt_female_delta_se**2)
# 
# male_delta <- data.frame("Q" = 1:30, ctt_delta = ctt_male_delta, "ctt_se" = ctt_male_delta_se, "gender" = "male")
# female_delta <- data.frame("Q" = 1:30, ctt_delta = ctt_female_delta, "ctt_se" = ctt_female_delta_se, "gender" = "female")
# delta_delta <- data.frame("Q" = 1:30, ctt_delta = ctt_delta_delta, "ctt_se" = ctt_delta_se, "gender" = "delta")
# 
# delta_frame = rbind(male_delta, female_delta, delta_delta)

# Calculate all of the Pearson's phi correlations (sqrt(chisq/N))
# This calculates one, we'll generalize in a bit

# For all posttest questions, create the 2x2 contingency tables ("tbt")
# calculate the chi-squared statistic, the total number of students,
# and the pearson correlation phi

# psycho$phi <- NA
# psycho$phi_p <- NA
# postscores <- select(posttests, post_1_C:post_30_C)
# for (i in seq_along(postscores)){
#   tbt <- with(posttests, xtabs(~ male + postscores[[i]]))
#   xsqtest <- chisq.test(tbt)
#   print(xsqtest)
#   xsq <- xsqtest$statistic[[1]]
#   pvalue <- xsqtest$p.value
#   N <- sum(tbt)
#   phi <- sqrt(xsq/N)
#   psycho$phi[[i]] <- phi
#   psycho$phi_p <- pvalue
# }

# # IRT Stuff
# library(ltm)
# 
# irt.post_male <- ltm(select(posttests_males, post_1_C:post_30_C) ~ z1, IRT.param = TRUE)
# irt.post_female <- ltm(select(posttests_females, post_1_C:post_30_C) ~ z1, IRT.param = TRUE)
# irt.pre_male <- ltm(select(pretests_males, pre_1_C:pre_30_C) ~ z1, IRT.param = TRUE)
# irt.pre_female <- ltm(select(pretests_females, pre_1_C:pre_30_C) ~ z1, IRT.param = TRUE)
# 
# irt_male_post_diff <- summary(irt.post_male)$coefficients[,1]
# irt_female_post_diff <- summary(irt.post_female)$coefficients[,1]
# irt_male_pre_diff <- summary(irt.pre_male)$coefficients[,1]
# irt_female_pre_diff <- summary(irt.pre_female)$coefficients[,1]
# 
# irt_male_post_se <- summary(irt.post_male)$coefficients[,2]
# irt_female_post_se <- summary(irt.post_female)$coefficients[,2]
# irt_male_pre_se <- summary(irt.pre_male)$coefficients[,2]
# irt_female_pre_se <- summary(irt.pre_female)$coefficients[,2]
# 
# psycho$irt_male_post_diff <- irt_male_post_diff[1:30]
# psycho$irt_female_post_diff <- irt_female_post_diff[1:30]
# psycho$irt_male_pre_diff <- irt_male_pre_diff[1:30]
# psycho$irt_female_pre_diff <- irt_female_pre_diff[1:30]
# 
# psycho$irt_male_post_se <- irt_male_post_se[1:30]
# psycho$irt_female_post_se <- irt_female_post_se[1:30]
# psycho$irt_male_pre_se <- irt_male_pre_se[1:30]
# psycho$irt_female_pre_se <- irt_female_pre_se[1:30]


library(dplyr)
# DIF stuff
# library(difR)
# difMH(select(posttests, post_1_C:post_30_C), posttests$male, 1, p.adjust.method = "bonferroni")


# Plots
library(ggplot2)

# ggplot(psycho, aes(x = ctt_female_post_diff, y = ctt_male_post_diff, label=rownames(psycho))) +  # This creates a graph object using our data and defining x and y axes
#   geom_point() +                           # This tells ggplot to take that object and make it a scatterplot
#   xlab("CTT Difficulty (Female)") +           # Adds a string specifying the label on the x-axis
#   ylab("CTT Difficulty (Male)") +          # Adds a string specifying the label on the y-axis
#   geom_abline()   +                         # Adds a line with slope of 1 and intercept of 0
#   xlim(0,1) +
#   ylim(0,1) +
#   geom_errorbar(ymax = ctt_male_post_diff + ctt_male_post_error, ymin = ctt_male_post_diff - ctt_male_post_error, width = 0.01) +
#   geom_errorbarh(xmax = ctt_female_post_diff + ctt_female_post_error, xmin = ctt_female_post_diff - ctt_female_post_error, height = 0.01) +
#   geom_text(nudge_x = -0.02, nudge_y = 0.02) +
#   geom_rect(aes(xmin = 0.2, xmax = 0.8, ymin = 0.2, ymax = 0.8), fill = "blue", alpha = 0.003)
# 
# ggplot(psycho, aes(x = ctt_female_pre_diff, y = ctt_male_pre_diff, label=rownames(psycho))) +  # This creates a graph object using our data and defining x and y axes
#   geom_point() +                           # This tells ggplot to take that object and make it a scatterplot
#   xlab("CTT Difficulty (Female)") +           # Adds a string specifying the label on the x-axis
#   ylab("CTT Difficulty (Male)") +          # Adds a string specifying the label on the y-axis
#   geom_abline()   +                         # Adds a line with slope of 1 and intercept of 0
#   xlim(0,1) +
#   ylim(0,1) +
#   geom_errorbar(ymax = ctt_male_pre_diff + ctt_male_pre_error, ymin = ctt_male_pre_diff - ctt_male_pre_error, width = 0.01) +
#   geom_errorbarh(xmax = ctt_female_pre_diff + ctt_female_pre_error, xmin = ctt_female_pre_diff - ctt_female_pre_error, height = 0.01) + 
#   geom_text(nudge_x = -0.02, nudge_y = 0.02) +
#   geom_rect(aes(xmin = 0.2, xmax = 0.8, ymin = 0.2, ymax = 0.8), fill = "blue", alpha = 0.003)
# 
# ggplot(delta_frame, aes(x = Q, y = ctt_delta, fill = gender)) + # creates a plot object with x axis and categories
#   geom_col(position = "dodge") +      # makes the histogram, transparency, and ensures bars don't overlap
#   geom_errorbar(data = delta_frame, aes(ymax = ctt_delta + ctt_se, ymin = ctt_delta - ctt_se), width = 0.5, position = position_dodge(0.8)) +
#   xlab("FCI Question #") +                                    # Sets horizontal axis title
#   ylab("Post - Pre (%)") +                           # Sets vertical axis title
#   theme(legend.title=element_blank()) +                  # Leaves the legend title blank
#   scale_fill_discrete(labels = c("Male","Female","M - F"))  # Changes the labels in the legend
# 
# ggplot(psycho, aes(x = irt_female_post_diff, y = irt_male_post_diff, label=rownames(psycho))) +  # This creates a graph object using our data and defining x and y axes
#   geom_point() +                           # This tells ggplot to take that object and make it a scatterplot
#   xlab("IRT Difficulty (Female)") +           # Adds a string specifying the label on the x-axis
#   ylab("IRT Difficulty (Male)") +          # Adds a string specifying the label on the y-axis
#   geom_abline()   +                         # Adds a line with slope of 1 and intercept of 0
#   xlim(-4, 4) +
#   ylim(-4, 4) +
#   geom_errorbar(aes(ymax = irt_male_post_diff + irt_male_post_se, ymin = irt_male_post_diff - irt_male_post_se), width = 0.1) +
#   geom_errorbarh(aes(xmax = irt_female_post_diff + irt_female_post_se, xmin = irt_female_post_diff - irt_female_post_se), height = 0.1) +
#   geom_text(nudge_x = -0.15, nudge_y = 0.15) 
# 
# ggplot(psycho, aes(x = irt_female_pre_diff, y = irt_male_pre_diff, label=rownames(psycho))) +  # This creates a graph object using our data and defining x and y axes
#   geom_point() +                           # This tells ggplot to take that object and make it a scatterplot
#   xlab("IRT Difficulty (Female)") +           # Adds a string specifying the label on the x-axis
#   ylab("IRT Difficulty (Male)") +          # Adds a string specifying the label on the y-axis
#   geom_abline()   +                         # Adds a line with slope of 1 and intercept of 0
#   xlim(-4, 4) +
#   ylim(-4, 4) +
#   geom_errorbar(aes(ymax = irt_male_pre_diff + irt_male_pre_se, ymin = irt_male_pre_diff - irt_male_pre_se), width = 0.1) +
#   geom_errorbarh(aes(xmax = irt_female_pre_diff + irt_female_pre_se, xmin = irt_female_pre_diff - irt_female_pre_se), height = 0.1) +
#   geom_text(nudge_x = -0.15, nudge_y = 0.15) 
#  geom_rect(aes(xmin = 0.2, xmax = 0.8, ymin = 0.2, ymax = 0.8), fill = "blue", alpha = 0.003)



## For now, commenting out the stuff I had started for CTT so I can work on building out the dataframe
## with spots for the correct answers
# # find the score that 27% of scores are below and store this lower cutoff
# lowcutoff <- quantile(NDSU_data_6_19$pre_score, 0.27, na.rm = TRUE)
# 
# # figure out which rows we actually need to look at for the lower 27%
# lowscores <- NDSU_data_6_19[NDSU_data_6_19$pre_score <= cutoff]


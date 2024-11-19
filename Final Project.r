#Final Project
#load data

survey = read.csv("survey_data.csv")
head(survey)
#what columns are in the data
names(survey)
#drop first 2 row and select neccesary columns
survey = survey[survey$DistributionChannel!="preview",]
my_cols = c("Duration..in.seconds.","ResponseId", "Grp18.timer.1_Page.Submit", "Grp18.timer2_Page.Submit",
  "Grp18.timer3_Page.Submit","Grp18.Q1.","Grp18.Q2","Grp18.Q3","FL_6_DO","PROLIFIC_PID")
survey.dropped = subset(survey[-c(1:2), ], select = my_cols)
#remove rows with missing values
survey.clean = survey.dropped[complete.cases(survey.dropped),]
#check how many rows were removed (0)
nrow(survey.clean) - nrow(survey.dropped)
#rename columns
names(survey.clean) = c("Duration", "ResponseId", "sad_pic.viewtime","cute_pic.viewtime","DV_viewtime", "DONATE", "PET", "GENDER", "PIC","PROLIFIC_PID")
#change pic's value to "sad" and "cute"
survey.clean$PIC = ifelse(survey.clean$PIC == "Grp18-Block2-Sad", "sad", "cute")
#check the data
head(survey.clean)

#check data type
num_cols = c("Duration", "sad_pic.viewtime", "cute_pic.viewtime", "DV_viewtime", "DONATE", "PET","GENDER")
survey.clean[num_cols] <- lapply(survey.clean[num_cols], as.numeric)
summary(survey.clean)
#remove survey that duration is less than 20
survey.clean = survey.clean[survey.clean$Duration > 20,]
nrow(survey.clean)
#save the clean data
write.csv(survey.clean, "clean_survey.csv", row.names = FALSE)

#load demographics data
demo = read.csv("Demographics-data-numeric.csv")
head(demo)
nrow(demo)

my_cols2 = c("Duration..in.seconds.","ResponseId", "Gender", "Gender_4_TEXT",
  "age_1","marital","Income","sex_or","Education", "PROLIFIC_PID")
demo.dropped = subset(demo[-c(1:2), ], select = my_cols2)
summary(demo.dropped)
num_cols2 = c("Duration..in.seconds.", "Gender",
  "age_1","marital","Income","sex_or","Education")
demo.dropped[num_cols2] <- lapply(demo.dropped[num_cols2], as.numeric)
#join the two data
library(dplyr)
survey.merge <- left_join(survey.clean, demo.dropped, by = "PROLIFIC_PID") 
# survey.merge
names(survey.merge)
#test every possible moderator in demographics data
summary(aov(DONATE ~ PIC*Education, data = survey.merge))

#summary statistics
library(dplyr)
group_by(survey.clean, PIC) %>%
summarise(
count = n(),
mean = mean(DONATE, na.rm = TRUE),
sd = sd(DONATE, na.rm = TRUE)
)

#examine the distribution of DV
ggplot(data= survey.clean, aes(x= DONATE)) +
geom_histogram(binwidth = 1, fill = "blue", color = "black") +
labs(title = "Distribution of DV", x = "Willingness to donate", y = "Frequency")

#inferential statistics
t.test(DONATE ~ PIC, data = survey.clean)
res.ano1 = aov(DONATE ~ PIC*GENDER, data = survey.clean)
res.ano2 = aov(DONATE ~ PIC*PET, data = survey.clean)
summary(res.ano2)
#two sided t-test
t.test(DONATE ~ PIC, data = survey.clean[survey.clean$PET == 3,])
t.test(DONATE ~ PIC, data = survey.clean[survey.clean$PET == 2,])
t.test(DONATE ~ PIC, data = survey.clean[survey.clean$PET == 1,])

t.test(DONATE ~ PIC, data = survey.clean[survey.clean$GENDER == 3,])
t.test(DONATE ~ PIC, data = survey.clean[survey.clean$GENDER == 2,])
t.test(DONATE ~ PIC, data = survey.clean[survey.clean$GENDER == 1,])

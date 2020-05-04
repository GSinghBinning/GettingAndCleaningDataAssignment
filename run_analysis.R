library(dplyr)
library(readr)
library(tidyr)
library(stringr)

#Please keep in mind to set the working directory to the correct filepath containing the assignment files

#reading in test-files
x_test <- read.table('./test/X_test.txt') 
y_test <- read.table('./test/y_test.txt', col.names = "activity_nr") #colnames hinzuf?gen zum mergen
subject_test <- read.table('./test/subject_test.txt', col.names ="subject")

#reading in train-files
x_train <- read.table('./train/X_train.txt')
y_train <- read.table('./train/y_train.txt', col.names = "activity_nr")
subject_train <- read.table('./train/subject_train.txt', col.names = "subject")

#reading in the column names(features) and activity-labels
features <- read.table('./features.txt')
activity_labels <- read.table('./activity_labels.txt', col.names = c("activity_nr","activity_name"))

#selecting the vector, which contains the column names
colnames(x_test) <- features[[2]]
colnames(x_train) <- features[[2]]

#Adding the column of Subjects and the column with the activites (saved as numbers) to the dataset
subjectyx_test <- cbind(subject_test, y_test, x_test)
subjectyx_train <- cbind(subject_train, y_train, x_train)

#Merging the dataset with the activity labels, so we get a tidy set, where the activites are written 
#as a string and not identified by a number
syx_activity_test <- merge(activity_labels, subjectyx_test, by="activity_nr")
syx_activity_train <- merge(activity_labels, subjectyx_train, by="activity_nr")

#excluding the column, which contained the number 1-6 for the activties, since we now got the description of 
#activites as string and filtering the columns to get only the columns, which contain mean and std values
filtered_syxa_test <- syx_activity_test[,c(3,2,grep('*mean\\(|*std\\(', names(syx_activity_test)))]
filtered_syxa_train <- syx_activity_train[,c(3,2,grep('*mean\\(|*std\\(', names(syx_activity_test)))]

#add the train dataset to the test dataset to get one big dataset
test_train_binded <- rbind(filtered_syxa_test,filtered_syxa_train)

#saving the dataset to a local file called "tidy_dataset.txt"
write.table(test_train_binded, './tidy_dataset.txt', row.name=FALSE)

#creating the second independent table as the assignment instructs us to do, by grouping the data by subjects 
#and activties and then calculating the Mean of every variable for each subject and activity and afterwards
#renaming the column names with adding AVG in the front
avg_table <- test_train_binded %>% 
    group_by(subject, activity_name) %>% 
    summarize_all(mean) %>% 
    rename_at(vars(-subject, -activity_name), ~ paste0('AVG: ',.)) 

#saving the second data table to a local file called "dataset_with_avg.txt"
write.table(avg_table, './dataset_with_avg.txt', row.name=FALSE)

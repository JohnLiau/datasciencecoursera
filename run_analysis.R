# read a file with given path and name
read.file<-function(path,name){
    root<-"~/Tools/R/Data Science/UCI HAR Dataset"
    file<-paste(root,"/",path,"/",name,sep="")
    data<-read.table(file)
}

features<-read.file(".","features.txt")
activity.labels<-read.file(".","activity_labels.txt")

# merge the training and the test sets to create one data set
read.both<-function(path,name){
    root<-"~/Tools/R/Data Science/UCI HAR Dataset"
    # file name of training data set
    file1<-paste(root,"/train/",path,"/",name,"_train.txt",sep="")
    # file name of test data set
    file2<-paste(root,"/test/",path,"/",name,"_test.txt",sep="")
    # target file name
    file<-paste(root,"/",name,".txt",sep="")
    # remove target file
    file.remove(file)
    # create a copy of the training data set
    file.copy(file1,file)
    # append test data set to the training data set
    if (file.append(file,file2)) {
        # read the merged data set
        data<-read.table(file)
    } else {
        # read training data set
        data<-read.table(file1)
    }
}

# merge the training and the test sets to create one data set
X<-read.both(".","X")
y<-read.both(".","y")
subject<-read.both(".","subject")
total.acc.x<-read.both("Inertial Signals","total_acc_x")
total.acc.y<-read.both("Inertial Signals","total_acc_y")
total.acc.z<-read.both("Inertial Signals","total_acc_z")
body.acc.x<-read.both("Inertial Signals","body_acc_x")
body.acc.y<-read.both("Inertial Signals","body_acc_y")
body.acc.z<-read.both("Inertial Signals","body_acc_z")
body.gyro.x<-read.both("Inertial Signals","body_gyro_x")
body.gyro.y<-read.both("Inertial Signals","body_gyro_y")
body.gyro.z<-read.both("Inertial Signals","body_gyro_z")

# label the data set with descriptive variable names
colnames(X)<-gsub("-",".",gsub("[()]","",features[,2]))

# extract only the measurements on the mean and standard deviation for each measurement
means<-grep("mean",features[,2])
stds<-grep("std",features[,2])
x<-X[,c(means,stds)]

# use descriptive activity names to name the activities in the data set
x$activity<-activity.labels[y[,1],2]
x$factor<-as.factor(paste(subject[,1],y[,1],sep="."))

# create a new data set with the average of each variable for each activity and each subject
mean<-matrix(nrow=length(levels(x$factor)),ncol=length(c(means,stds)))
rownames(mean)<-levels(x$factor)
colnames(mean)<-paste("mean.",colnames(x)[1:length(c(means,stds))],sep="")
for (i in 1:length(c(means,stds))){
    mean[,i]<-tapply(x[,i],x$factor,mean)
}

# write the new data set
write.table(mean,file="mean.txt")

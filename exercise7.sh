
# Set working directory
setwd("C:/Users/Michelle Wang/Desktop/BIOS 
60318/Intro_Biocomp_ND_317_Tutorial7")

# Load libraries
library(ggplot2)
library(grid)
library(gridExtra)

#-----------------
### Question 1 ###
#-----------------

setwd("~/Desktop/data-shell/Exercise7/Intro_Biocomp_ND_317_Tutorial7/")
fasta <- scan("C:Users/Michelle Wang/Desktop/BIOS 60318/Intro_Biocomp_ND_317_Tutorial7/Lecture11.fasta', what = character())

#####Below info obtained from answers_Lecture11.R
# pre-allocate matrix to store sequence information
summ=matrix(NA,length(fasta)/2,4)

# loop through lines of fasta and process each record
for(i in 1:length(fasta)){
  # if-else to figure out if we have a recordID line or sequence line
  if(grepl(">",fasta[i])){
    summ[((i+1)/2),1]=substr(fasta[i],2,nchar(fasta[i])) # could use counter instead of complicated row calculation
  }else{
    seqLength=nchar(fasta[i])
    
    numG=nchar(fasta[i])-nchar(gsub("G","",fasta[i]))
    numC=nchar(fasta[i])-nchar(gsub("C","",fasta[i]))
    if(seqLength<=14){
      Tm=2*(numG+numC)+2*seqLength
    }else{
      Tm=-9999
    }
    
    summ[i/2,2]=seqLength
    summ[i/2,3]=round((numG+numC)/seqLength*100,1)
    summ[i/2,4]=Tm
  }
}

seqSumm=data.frame(sequenceID=summ[,1],sequenceLength=as.numeric(summ[,2]),percentGC=as.numeric(summ[,3]),meltingTemp=as.numeric(summ[,4]))

# 2
# Report starting element and length of runs in "findRuns.txt"
# load data from text file
findRuns=read.table("findRuns.txt",header=FALSE,sep="\t")
# convert data from a single-column dataframe to a vector
findRuns=unlist(findRuns)

# create a variable out that is currently undefined
out=NULL
# I will use this variable cur to hold onto the previous number in the vector;
# this is analagous to using findRuns[i-1]
cur=findRuns[1]
# this is a counter that I use to keep track of how long a run of repeated values is;
# if there are not repeated values than this count equals 1
count=1

# loop through each entry of our vector (except the 1st one, which we set to cur above)
for(i in 2:length(findRuns)){
  # test if the ith value in the vector findRuns equals the previous (stored in cur)
  if(findRuns[i]==cur){
    # test whether count is 1 (we aren't in the middle of a run) or >1 (in the middle of a run)
    if(count==1){
      # if the ith value in the vector equals the previous (stored in cur) and count is 1, we
      # are at the beginning of a run and we want to store this value (we temporarily store it in 'start')
      start=(i-1)
    }
    # we add one to count because the run continued based on the ith value of findRuns being equal to
    # the previous (stored in cur)
    count=count+1
    # if the ith value in findRuns is not the same as the previous (stored in cur) we either are not in a run
    # or we are ending a run
  }else{
    # if count is greater than 1 it means we were in a run and must be exiting one
    if(count>1){
      # add a row to 'out' that will hold the starting positions in the first column and the length
      # of runs in the second column; this appends rows to out after finding and counting each run
      out=rbind(out,c(start,count))
      # reset count to 1 because we just exited a run
      count=1
    }
  }
  # remember cur holds the previous element in findRuns, so we need to update this after each time
  # we go through the for loop
  cur=findRuns[i]
}

# give out column names and print it
colnames(out)=c('start','length')
out



####New code starts here:
#Plot a histogram of the sequence length. Remove the default grey background with white lines. Set x and y axis labels.
L = ggplot(data = seqSumm)
L + geom_histogram(aes(x = sequenceLength), fill = "orange", color = "red") + theme_classic() + xlab("Sequence Length (bp)") + ylab("Count")

#Plot a histogram of the percentage GC content. Remove the default grey background with white lines and replace with blue fill with black border. Set x and y axis labels.
GC = ggplot(data = seqSumm)
GC + geom_histogram(aes(x = percentGC), fill = "blue", color = "black") + theme_classic() + xlab("GC Content (%)") + ylab("Count")


#-----------------
### Question 2 ###
#-----------------


###Exercise7_Question 2

#Set working directory
setwd("~/Desktop/")

#Read in data and call it "A"
A <- read.csv(file = "BCA_Sample.csv")

#Plot data points with Standard conentration on the x-axis and optical density on the y. Remove the default grey background and white lines. Add trendline.
ggplot(data=A, aes(x=StandardConcentration, y=OpticalDensity)) + geom_point() + theme_classic() + stat_smooth(method = "lm")


#-----------------
### Question 3 ###
#-----------------

# Create dataframe
data = read.table(file = "data.txt", header = TRUE, sep = "," , 
stringsAsFactors = FALSE)


# Create Mean Barplot
meanplot = ggplot(data = data)

mplot = meanplot + geom_bar(aes(x = as.factor(region), y = 
observations), stat = "summary",
        fun.y = "mean", fill = "lightsteelblue2", color = "black") +
        theme_classic() + xlab("Region") + ylab("Observation") +
        ggtitle("Mean Observation by Region")

# Create Scatterplot
scatterplot = ggplot(data = data, aes(x=region, y = observations))

splot = scatterplot + geom_point()+ coord_cartesian() + geom_jitter() + 
theme_classic() + 
        ggtitle("Scatterplot by Region")

# Analyze side-by-side
grid.arrange(mplot, splot, ncol = 2)


#The jitter plot looks quite different from the bar plot because the bar plot is depicting the 
mean observation of each region which are all roughly 15. The jitter plot shows individual data 
points, which although they have a mean of about 15, also depict the range and clustering of 
data points, giving a better representation of how the data is distributed.


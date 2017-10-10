
# Set working directory
setwd("C:/Users/Michelle Wang/Desktop/BIOS 
60318/Intro_Biocomp_ND_317_Tutorial7")

# Load libraries
library(ggplot2)
library(grid)
library(gridExtra)

#-----------------
### Question 2 ###
#-----------------

# Iceland Airbnb dataset
iceland = read.csv(file = "tomslee_airbnb_iceland_1370_2017-06-18.csv", 
header = TRUE)

# Extract only number of reviews and price
bnb = iceland[,c(9,14)]

# Create scatterplot
iceplot = ggplot(data = bnb, aes(x=price, y=reviews))
iceplot + geom_point()+ coord_cartesian() + geom_jitter() + 
theme_classic() + 
          scale_x_log10() + stat_smooth(method = "loess") +
          ggtitle("Number of Reviews versus Price in Iceland Airbnb") 


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



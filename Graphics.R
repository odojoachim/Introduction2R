# Sometimes pictures are better than text
# See some graphs done in R:
# Run the codes: R graphics system are extremely powerful. Of course doing this kind of graphics demand hours of practice. 

#1. 
moxbuller = function(n) {   
  u = runif(n)   
  v = runif(n)   
  x = cos(2*pi*u)*sqrt(-2*log(v))  
  y = sin(2*pi*v)*sqrt(-2*log(u))
  r = list(x=x, y=y)
  return(r) 
}
r = moxbuller(50000) 
par(bg="DarkGreen") 
par(mar=c(0,0,0,0)) 
plot(r$x,r$y, pch=".", col="yellow", cex=1.2)

#2.
install.packages("igraph")
install.packages("RColorBrewer")
library(igraph)
head(mtcars)
mat=cor(t(mtcars[,c(1,3:6)]))
mat[mat<0.995]=0
network=graph_from_adjacency_matrix( mat, weighted=T, mode="undirected", diag=F)
plot(network)
library(RColorBrewer)
coul = brewer.pal(nlevels(as.factor(mtcars$cyl)), "Set2")
my_color=coul[as.numeric(as.factor(mtcars$cyl))]
par(bg="grey13", mar=c(0,0,0,0))
set.seed(4)
plot(network, 
     vertex.size=12,
     vertex.color=my_color, 
     vertex.label.cex=0.7,
     vertex.label.color="white",
     vertex.frame.color="transparent"
)
text(0,0,"The network chart of the mtcars dataset",col="white")
text(0.2,-0.1," - by the R graph gallery",col="white")
legend(x=-0.6, y=-0.12, legend=paste( levels(as.factor(mtcars$cyl)), " cylinders", sep=""), col = coul , bty = "n", pch=20 , pt.cex = 2, cex = 1, text.col="white" , horiz = T)

# 3.
install.packages("wordcloud2")
library(wordcloud2)
letterCloud( demoFreq, word = "R", color='random-light' , backgroundColor="black")

# It's good to add William's Cleveland's principles for scientific visualisation
# A graphic should display as much information as it can, with the lowest possible cognitive strain to the viewer. 
# Strive for clarity. Make the data stand out. Specific tips for increasing clarity include:
# - Avoid too many superimposed elements, such as too many curves in the same graphing space. 
# - Find the right aspect ratio and scaling to properly bring out the details of the data.
# - Avoid having the data all skewed to one side or the other of your graphs. 
# Visualisation is an iterative process. Its purpose is to answer questions about the data. 

########################################################Ready. Steady. Go.###########################################
# https://www.kaggle.com/unsdsn/world-happiness/data download 2015 data set
download.file('https://github.com/ywchiu/rcookbook/raw/master/chapter7/superstore_sales.csv', 'superstore_sales.csv')
library(dplyr)
library(ggplot2)
HappyData <- read.csv('2015.csv')
str(HappyData)
head(HappyData)
summary(HappyData)
colnames(HappyData)
# I will cover basic plotting in R very quickly as most of the time in industry we use ggplot2 instead. 
###### Basic Graphics: 
# scatterplot
par(mfrow=c(1,1)) # create a layout of graphic wondow, useful if you want to put more than one graph in one graphic window 
plot(HappyData$Happiness.Score, HappyData$Region) # plot scatterplot for the variables
plot(HappyData$Happiness.Score) # plot one variable only

# distribution
hist(HappyData$Happiness.Score, binw = 0.5) # histogram
stem(HappyData$Family) # Stem and leaf plot
plot(density(HappyData$Happiness.Score)) # density plot
boxplot(HappyData$Happiness.Score) # box plot
boxplot(HappyData$Happiness.Score~ HappyData$Region) # box plot by group

# categorical variables
plot(HappyData$Region) # categorical variable will be presented in bar chart
plot(prop.table(table(HappyData$Region))) #p.m.f. of categorical variable

# add options to the main plotting functions
plot(HappyData$Happiness.Score, HappyData$Region,
     xlab = "Happy",  # x-axis label
     ylab = "Region",      # y-axis label
     pch = 6,          # plotting character
     las = 1,         # orientation of axis labels
     xlim = c(0, 8), # x axis limits
)

# overlay different elements
title("Happy Region") # add title to top of plot
abline(h = mean(HappyData$Happiness.Score), lty = 2, col = "red") # add straight line

?par # built-in help for graphics parameters
?plot.default # built-in help 

# Saving plot: Click on export in RStudio

########################## 7 layers of graphics: Data, Aesthetics, Geometries, Statistics, Facets, Coordinates, Theme

# 1. Data is the data that you used to create a graph
# Factors in R are categorical variables
# In our example we want to use Year as Factor rather than TS data as the purpose of analysis is different

# 2. Aesthetic are like axis or colour of bars that are on the graph
p1 <- ggplot(data = HappyData, # specify the data frame
       aes(x=Country, y=Happiness.Score, # specify the x and y axis
                             colour = Region, # colour will match to country
           size = Family)) + # size will tell us about how big is the family rate for given Country
  p2 <- ggplot(data = HappyData, aes(x=Region, y=Generosity, 
                                     colour = Dystopia.Residual, Size = Trust..Government.Corruption.))
# 2. Adding colour:
p1 + geom_point(aes(colour = Region)) # mapping to a variable in a data set
p1 + geom_point(colour = 'DarkGreen') # setting to a colour of your choice
p1 + geom_point(aes(colour='DarkGreen')) # Error as we don't have variable 'DarkGreen' in our data set
  # we need plot to see anything 
p1 + geom_point()


# 3. Geometry form of the graph like barchart or line chart or point as before
p1 + geom_point()
p1 + geom_dotplot()
p1 + geom_point() + geom_dotplot() # you can plot two graph in once

# Back to 2. 
# overwriting aesthetics for the specific plot
p2 + geom_point(aes(size = Family, colour = Freedom)) # size of the graph is distributed according to the size of Family variable
p2 + geom_point(aes(x=Happiness.Score  )) + xlab('Happy') # changing name of the x-axis
p1 + geom_point(size = 1) + geom_dotplot() # changing size of points

# 4. Statistics like in hstogram we run a statistics by grouping rows by some column
# 4 Geometry + Statistics: Histograms and density charts

############# Histogram: 
# A basic histogram bins a variable into fixed width buckets and returns the number of data points that falls into each bucket. 
# The primary disadvantge of histograms is that you must decide ahead of time how wide the buckets are. 
############# Density plots
# You can think of a 'density plot' as a "continuous histogram" of a variable, except the area under the density plot is equal to 1. 
# You should use a log scale to better visualise data that is heavily skewed. 
############ Bar charts
# A bar chart is a histogram for discrete data. 
############ Line plots
# Works best when the relationship between two variables is relatively clean: each x value has a unique y value. 
# When the data is not so cleanle related you'll want to use scatterplot instead
############ Hexbin plots
# A hexbin plot is like two dimensional histogram. The data is divided into bins, and the number of data ponits in each bin is represented by colour or shadin.

p3 <- ggplot(data=HappyData, aes(x=Economy..GDP.per.Capita.)) # check the range of variable in our data set
range(HappyData$Economy..GDP.per.Capita.) # tip: always check range of the variable to estimate binwidth
p3 + geom_histogram(binwidth =0.2) # set up the binwidth
p3 + geom_histogram(binwidth =0.2, aes(fill = Region)) # map second variable as a colour
p3 + geom_histogram(binwidth =0.2, aes(fill = Region), colour = 'Black') # set up border for different Regions in the histogram
p3 + geom_density() # create density plot
p3 + geom_density(aes(fill = Region)) # create density plot for different Region
p3 + geom_density(aes(fill = Region), position = 'stack') # so you can see better density distribution for different Regions
# Tip: You don't want to show your boss density plot because it will be very hard to explain to boss what density plot is. 
? geom_smooth # Seeing pattern in the presence of overploting. Allows you to see trends and patterns, relationships. If there is a lot of data points.
p4 <- ggplot(data=HappyData, aes(x=Freedom, y=Trust..Government.Corruption., colour = Region))
p4 + geom_point() + geom_smooth() # add trend to the graph
p4 + geom_point() + geom_smooth(fill=NA) # but do not add the shade
p5 <- ggplot(data = HappyData, aes(x = Region, y=Freedom, colour=Region))
p5 + geom_boxplot()
p5 + geom_boxplot(size = 1.3) # increase size of the borders
p5 + geom_boxplot(size = 1.3) + geom_point() # too pretty to be good
p5 + geom_boxplot(size = 1.3) + geom_jitter() # it's just randomly thrown points helping you too see the data better
p5 + geom_jitter() + geom_boxplot(size = 1.3, alpha = 0.5) # boxplot on top and set up transparency

# 5. Facets is like how many graphs do we see on one chart. 
p3 <- ggplot(data=HappyData, aes(x=Economy..GDP.per.Capita.))
p3 + geom_histogram(binwidth =0.2, aes(fill = Region), colour = 'Black') + facet_grid(Region~.) # to see histogram for different region seperately. 
p3 + geom_histogram(binwidth =0.2, aes(fill = Region), colour = 'Black') + facet_grid(Region~., scales = 'free') # each histogram got they own scale. 
p4 <- ggplot(data=HappyData, aes(x=Freedom, y=Trust..Government.Corruption., colour = Region))
p4 + geom_point() + geom_smooth() + facet_grid(Region~.) # to see grid on top of each other
p4 + geom_point() + facet_grid(.~Region) # to see grids on sides of each other
p4 + geom_point() + facet_grid(smth~smth) # to have top/sides together 

# 6. Coordinates the x and y axeis scale, cartesian coordinates, polar coordinates and so on. 
p2 <- ggplot(data = HappyData, aes(x=Generosity, y=Family, 
                                   colour = Dystopia.Residual, Size = Trust..Government.Corruption.))
p2 + geom_point() + xlim(0,0.4) + ylim(0.75, 1.25) # selecting fragment of a plot. 
p3 <- ggplot(data=HappyData, aes(x=Economy..GDP.per.Capita.))
p3 + geom_histogram(binwidth =0.2, aes(fill = Region), colour = 'Black') + coord_cartesian(ylim = c(0,20)) #zoom in

# 7. Theme: Things like title, legend, colour of x and y axis description, size of the text and so on. 
p3 <- ggplot(data=HappyData, aes(x=Economy..GDP.per.Capita.))
r <- p3 + geom_histogram(binwidth =0.2, aes(fill = Region), colour = 'Black')
r + 
  xlab('Economy') + # add the x axis title
  ylab('Total score for economy') + # add the y-axis title
  ggtitle('Economic growth for different regions') + # add the title
  theme(axis.title.x = element_text(colour = 'Blue', size = 20), # change colour and size of x-axis title
        axis.title.y = element_text(colour = 'Red', size = 20), # change colour and size of y-axis title
        legend.title = element_text(size = 20), # change the size of title in the legend
        legend.text = element_text(size=10), # change the size of content in the legend
        legend.position = c(1,1), # according to: 0,0 = origin, 0,1 = on the y-axis where x=0, 1,0 = on the x-axis where y=0
        legend.justification = c(1,1),
        plot.title = element_text(colour = 'DarkGreen', size = 30)) # which corner of the text box you want to put in which corner of the grid box.
?theme

# Producing a horizontal bar chart
ggplot(data = HappyData, aes(x=Region)) + # Plot bar chart as before
  geom_bar() +
  coord_flip() + # Flip the x and y axis
  theme(axis.text.y = element_text(size=rel(0.8))) #reduce the size of the y-axis tick labels 
RegionSums <- table(HappyData$Region) # aggregate the data by state of residence
Regionf <- as.data.frame(RegionSums) # convert to data frame
colnames(Regionf) <- c('Region', 'count') # change the column names
summary(Regionf) # default ordering is alphabetical
Regionf <- transform(Regionf, Region = reorder(Region, count)) # reorder
summary(Regionf) # The region is now count ordered
ggplot(Regionf) +
  geom_bar(aes(x=Region, y=count), 
           stat = 'identity',
           fill = 'blue') +
  coord_flip() +
  theme(axis.text.y = element_text(size = rel(0.8)))

# Producing a hexbin (work on it)
install.packages("hexbin")
library(hexbin) # load hexbin library
ggplot(Data = HappyData, aes(x=Happiness.Rank, y=Generosity)) + 
  geom_hex(binwidth = c(5,0.1)) + #binned the variables. 
  geom_smooth(colour = 'white', se=F) + # add smothing line and suppress standard error ribon
  ylim(0, 1)

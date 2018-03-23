# M&M Analysis Script work
# February, 2018

# packages and libraries used
library(ggplot2)
library(dplyr)


### MAKE THEME for background azure, and switch background as needed

# theme(panel.background = element_rect(fill="azure"),
#       panel.grid.major = element_line(color="grey90"),
#       panel.grid.minor = element_line(color="grey95")) 

# background image to plots?
# https://www.r-bloggers.com/how-to-add-a-background-image-to-ggplot2-graphs/

# import the data file
original_df <- read.csv("M&M Pareto Counts.csv")

# nrow(read.csv("M&M Pareto Counts.csv"))

# fix dates, make working df
df <- original_df 
df$Date <- as.Date(df$Date)

# add bag count, determine sizing ranges
df$BagCount <- rowSums(df[4:9])

# Do as histogram - shows a first glance
ggplot(df, aes(x=BagCount)) +
  geom_histogram(bins=20) +
  theme(panel.background = element_rect(fill="azure")) +
  ggtitle("Histogram: Count of M&Ms in Bags") +
  xlab("Count of M&Ms in a bag")

# play with xlim to zero in on sizes (maybe learn to do with plotly)
ggplot(df, aes(BagCount))+
  geom_bar(aes(fill=Kind)) +
  scale_fill_brewer(palette = "YlOrBr") +
  theme(panel.background = element_rect(fill="azure")) +
  ggtitle("Bar Chart: Count of M&Ms in Bags, by Kind") +
  xlab("Count of M&Ms in a bag")

# Zoom in on Plain:Regular and Sharing Size
ggplot(df, aes(BagCount))+
  geom_bar(aes(fill=Kind)) +
  xlim(45,110) +
  theme(panel.background = element_rect(fill="azure")) +
  ggtitle("Bar Chart: Count of M&Ms in Bags, Zoom in") +
  xlab("Count of M&Ms in a bag")


#explore that outlier on the low side of Plain:Regular
ggplot(df, aes(BagCount))+
  geom_bar(aes(fill=Kind)) +
  scale_fill_brewer(palette = "YlOrBr") +
  xlim(15,45) +
  theme(panel.background = element_rect(fill="azure")) +
  ggtitle("Bar Chart: Count of M&Ms in Bags, Zoom in") +
  xlab("Count of M&Ms in a bag")

# zoom in on the small count to find Peanut:Regular and FunSize: Plain
ggplot(df, aes(BagCount))+
  geom_bar(aes(fill=Kind)) +
  scale_fill_brewer(palette = "YlOrBr") +
  xlim(0,30) +
  theme(panel.background = element_rect(fill="azure"))  +
  ggtitle("Bar Chart: Count of M&Ms in Bags, Zoom in") +
  xlab("Count of M&Ms in a bag")

# facet wrap to look at that range seperately
ggplot(df, aes(BagCount))+
  geom_histogram() +
  xlim(0,30) + 
  facet_wrap(~Kind) +
  theme(panel.background = element_rect(fill="azure"))  +
  ggtitle("Bar Chart: Count of M&Ms in Bags, Zoom in") +
  xlab("Count of M&Ms in a bag")


# testing Peanut sizes
df %>% filter (Kind == "Peanut") %>%
  ggplot(aes(BagCount)) +
  geom_histogram() +
  theme(panel.background = element_rect(fill="cornsilk"))  +
  ggtitle("Bar Chart: Count of M&Ms in Peanut Bags") +
  xlab("Count of M&Ms in a bag")

# Assign Bag Sizes

# Peanut sizes
# fun size is less than 10 -- there was only one in the data set
# set break point between the ranges observed
pfun <- (10+15)/2
# regular is 15-25

# peanut butter bag is regular sized

# Plain sizes
# set break point between the ranges observed
# 10-20 is fun size
plfun <- (35+20)/2
# 35-75 is regular
plreg <- (90+75)/2
# 90-115 is big
plrb <- (150+115)/2
# >150 is really big

## set bag size factor

# initialize bag size
df$BagSize <- "NotSet"

# assign bag size factor
for (i in 1:nrow(df)) {
  # We have two sizes of Peanut so far, so identify Fun Size and class the rest as Regular
  if (df[i, "Kind"] == "Peanut"){
    if (df[i, "BagCount"] > pfun) {
      df[i, "BagSize"] <- "Regular"
    } else df[i, "BagSize"] <- "Fun Size"
  # We have four sizes of Plain so far, so knock them down from smallest to largest
        } else if (df[i, "Kind"] == "Plain"){
      if (df[i, "BagCount"] < plfun) {
        df[i, "BagSize"] <- "Fun Size"
      } else if (df[i,"BagCount"] < plreg) {
        df[i, "BagSize"] <- "Regular"
      } else if (df[i, "BagCount"] < plrb) {
        df[i, "BagSize"]  <- "Sharing Size"
      } else  df[i, "BagSize"] <- "Really Big"
  # if not Peanut, and not Plain, at present, with only one Peanut Butter, it's a Regular
    } else df[i, "BagSize"] <- "Regular"
}


# sample single bag plot code using dotplot.  Not very satisfying
library(tidyverse)
library(ggplot2)
library(forcats)
library(dplyr)
colorsm <- c("Blue","Blue","Blue","Blue","Orange","Orange","Orange","Orange","Red","Red","Red","Brown","Brown","Yellow","Yellow","Green")
dfcase<-data.frame(colorsm)
dfcase$vals <- 1
ggplot(dfcase, aes(fct_infreq(colorsm)))+
  geom_dotplot(color=colorsm, fill=colorsm) +
  xlab(NULL) + ylab(NULL) +
  theme(panel.background = element_rect(fill="azure")) +
  ggtitle("Dot Plot Single Bag Visualization")

# same chart as a bar chart. Couldn't get fct_infreq to work so used arrange and fct_inorder instead
dfcase %>% 
  group_by(colorsm) %>% 
  summarize(counts=sum(vals)) %>% 
  mutate(colorsm = as.character(colorsm)) %>%
  arrange(desc(counts)) %>%
  ggplot(aes(fct_inorder(colorsm),counts)) + 
  geom_col(fill=c("mediumblue","orange3","red4","chocolate4","yellow3","green3")) +
  xlab(NULL) +
  theme(panel.background = element_rect(fill="azure")) +
  ggtitle("Bar Plot Single Bag Visualization")

# Tidying the data set
# Note that doing this is important to do after certain bag stats are run, but before others are run (e.g. before proportions of each color)
library(tidyr)
df_tidy <- df %>% tibble::rownames_to_column("BagNum") %>% 
  gather(key= MMColor, value = CountInBag, -BagNum, -Date, -Notes, -BagCount, -Kind, -Submitter, -BagSize)

# Overall Proportion of Colors
df_tidy %>% group_by(MMColor) %>% summarize(count = sum(CountInBag)) %>% arrange(desc(count)) %>%
  ggplot(aes(fct_inorder(MMColor),count)) + 
  geom_col(fill=c("mediumblue","orange3","green3","yellow3","chocolate4","red4")) +
  ggtitle("Color Totals across all Bags in Sample") +
  xlab(NULL) +
  theme(panel.background = element_rect(fill="azure"))
  


# # not working
# df_tidy %>% filter(Kind != "Peanut Butter") %>% 
#   ggplot(df_tidy, aes(x=Kind, y=CountInBag)) +
#   geom_col(position = "dodge") +
#   facet_grid(BagSize~.) +
#   ggtitle("Count in Bag")


# color distribution of the data set,by proportion of a Bag
ggplot(df_tidy, aes(CountInBag/BagCount)) + 
  geom_histogram(aes(y=..density..)) +
  geom_density(color="lightgreen") + 
  facet_wrap(~MMColor) +
  ggtitle("Frequency of Proportion of a Color in a Bag, by Color") +
  xlab("Proportion of a Color in a Bag") +
  ylab("Proportion Frequency") +
  theme(panel.background = element_rect(fill="azure"),
        panel.grid.major = element_line(color="grey90"),
        panel.grid.minor = element_line(color="grey95")) 
  
#ridgeline
ggplot(df_tidy, aes(x= CountInBag/BagCount, y=reorder(MMColor,-CountInBag/BagCount, median)))+
  ggridges::geom_density_ridges(fill="lightgreen", alpha=.5) +
  ggtitle("Density Curve of Proportion of a Color in a Bag, by Color") +
  xlab("Proportion of a Color in a Bag") +
  ylab(NULL) +
  theme(panel.background = element_rect(fill="azure"))

# mosaic? keep playing with it. Review the basics so you get this right
library(grid)
library(vcd)

df_m<- df_tidy %>%
  group_by(MMColor,Kind,BagSize) %>% 
  summarize(Freq=sum(CountInBag))
# Meh
mosaic(MMColor~Kind, df_m,direction= c("v","h"),labeling = labeling_border(rot_labels = c(20, 45)))




# color distribution of the data set, trimming out the Really Big Bag -- does the same thing as geom_histogram!!
ggplot(df_tidy, aes(CountInBag)) + 
  geom_bar(aes(y=..density..), binwidth = 2.5) +
  geom_density(color="lightgreen") + 
  xlim(c(0,30)) +
  facet_wrap(~MMColor) +
  ggtitle("Frequency of Counts of a Color in a Bag, by Color") +
  xlab("Count of a Color in a Bag") +
  ylab("Proportion Frequency") +
  theme(panel.background = element_rect(fill="azure"),
        panel.grid.major = element_line(color="grey90"),
        panel.grid.minor = element_line(color="grey95")) 



# More Bag Stats
# proportion of each color within bag
df$yellowp <- df$Yellow/df$BagCount
df$bluep <- df$Blue/df$BagCount
df$greenp <- df$Green/df$BagCount
df$brownp <- df$Brown/df$BagCount
df$orange <- df$Orange/df$BagCount
df$redp <- df$Red/df$BagCount

# biggest proportion and smallest proportion in each bag (columns 13 to 18)
df[, "maxp"] <- apply(df[, 13:18], 1, max)
df[, "minp"] <- apply(df[, 13:18], 1, min)

# Max Analysis
# plot Kind and BagSize
ggplot(df, aes(maxp)) +
  geom_histogram(aes(fill=Kind)) +
  facet_wrap(~BagSize) +
  theme(panel.background = element_rect(fill="azure")) +
  ggtitle("Maximum Single Color Proportion by Size and Kind") +
  xlab("Maximum Single Color Proportion")

df %>% filter(Kind=="Plain") %>%
  ggplot(aes(maxp)) +
  geom_histogram() +
  facet_wrap(~BagSize) +
  theme(panel.background = element_rect(fill="antiquewhite2")) +
  ggtitle("Maximum Single Color Proportion by Size, Plain") +
  xlab("Maximum Single Color Proportion")

df %>% filter(Kind=="Peanut") %>%
  ggplot(aes(maxp)) +
  geom_histogram() +
  facet_wrap(~BagSize) +
  theme(panel.background = element_rect(fill="cornsilk")) +
  ggtitle("Maximum Single Color Proportion by Size, Peanut") +
  xlab("Maximum Single Color Proportion")

# Min Analysis
# plot kind and BagSize
ggplot(df, aes(minp)) +
  geom_histogram(aes(fill=Kind)) +
  facet_wrap(~BagSize) +
  theme(panel.background = element_rect(fill="azure")) +
  ggtitle("Minimum Single Color Proportion by Size and Kind") +
  xlab("Minimum Single Color Proportion")


df %>% filter(Kind=="Plain") %>%
  ggplot(aes(minp)) +
  geom_histogram() +
  facet_wrap(~BagSize) +
  theme(panel.background = element_rect(fill="antiquewhite2")) +
  ggtitle("Minimum Single Color Proportion by Size, Plain") + 
  xlab("Minimum Single Color Proportion")

df %>% filter(Kind=="Peanut") %>%
  ggplot(aes(minp)) +
  geom_histogram() +
  facet_wrap(~BagSize) +
  theme(panel.background = element_rect(fill="cornsilk")) +
  ggtitle("Minimum Single Color Proportion by Size, Peanut") +
  xlab("Minimum Single Color Proportion")


# count the zeroes (missing colors) (this code summarizes it, but I want to add to each row)
foo <- colnames(df[,13:18])
count00 <- sapply(foo,FUN=function(x,df){sum(df[,x]==0, ra.rm=TRUE)},df)
# fix this...
# df[,"zeroes"] <- apply(df(,13:18, 1, )))
# initialize a column
df$zeroes <- 0
# assign 1 to any row that has a zero count
for (i in 1:nrow(df)) {
  if (df[i,"minp"] == 0) {
    df[i,"zeroes"] <- 1
  } 
}
# graph it
# sumamrize
dfzero <- df %>%
  group_by(Kind, BagSize) %>%
  summarize(bagswithmissingp = mean(zeroes), bagswithmissing = sum(zeroes), countofbags = n())

#order BasSize, which is ordinal
dfzero$BagSize <- factor(dfzero$BagSize, levels = c("Fun Size", "Regular", "Sharing Size", "Really Big"))

# filter out the ones with low bag counts

dfzero <- dfzero %>% filter(countofbags > 3)

# combine tags

dfzero$KindSize <- c(paste0(dfzero$Kind,":",dfzero$BagSize))

# graph
ggplot(dfzero, aes(x=reorder(KindSize,-bagswithmissingp), y=bagswithmissingp)) +
  geom_bar(stat="identity")+
  theme(panel.background = element_rect(fill="azure")) +
  annotate("text", x=3, y=0.12, label=c(paste0(as.character(100*round(dfzero[3,3],digits=3)),"%"))) +
  ggtitle("Proportion of Bags with a Missing Color, by Kind and Size") +
  xlab("Kind and Size of Bag") +
  ylab("Proportion of Bags with Missing Color")


# scatter plot that!
ggplot(df, aes(x=BagCount, y=minp)) +
  geom_point(alpha=.3) +
  annotate("text", x=25, y=.06, label="odd curve")




# answer the question about frequency of only one of one color

# count the solo counts (one only) 
foo2 <- colnames(df[,4:9])
count01 <- sapply(foo2,FUN=function(x,df){sum(df[,x]==1, ra.rm=TRUE)},df)
# initialize a column
df$ones <- 0
# assign 1 to any row that has a single count
# for loop within a for loop, iterating for every color-column
for (j in 4:9) {
  for (i in 1:nrow(df)) {
    if (df[i,j] == 1) {
      df[i,"ones"] <- 1
    } 
  }
}

# graph it
# sumamrize
dfone <- df %>%
  group_by(Kind, BagSize) %>%
  summarize(bagswithsinglep = mean(ones), bagswithsingle = sum(ones), countofbags = n())

#order BasSize, which is ordinal
dfone$BagSize <- factor(dfone$BagSize, levels = c("Fun Size", "Regular", "Sharing Size", "Really Big"))

# filter out the ones with low bag counts

dfone <- dfone %>% filter(countofbags > 3)

# combine tags

dfone$KindSize <- c(paste0(dfone$Kind,":",dfone$BagSize))

# graph
ggplot(dfone, aes(x=reorder(KindSize,-bagswithsinglep), y=bagswithsinglep)) +
  geom_bar(stat="identity")+
  theme(panel.background = element_rect(fill="azure")) +
  annotate("text", x=3, y=0.06, label=c(paste0(as.character(100*round(dfone[3,3],digits=3)),"%"))) +
  ggtitle("Proportion of Bags with a Single M&M of a Color, by Kind and Size") +
  xlab("Kind and Size of Bag") +
  ylab("Proportion of Bags with Single M&M of a Color")

# note that both graphs have 1 out of the Plain:Regular with zero and one, which is 3.8% at the time I ran this data set

df %>% filter(Kind=="Plain") %>% filter(BagSize=="Regular") %>% nrow() 

xblue <- df_tidy %>% filter(Kind=="Plain",MMColor=="Blue")
yred <-  df_tidy %>% filter(Kind=="Plain",MMColor=="Red")
yorange <-  df_tidy %>% filter(Kind=="Plain",MMColor=="Orange")
t.test(xblue$CountInBag , yred$CountInBag)
t.test(xblue$CountInBag , yorange$CountInBag)

xblue <- df_tidy %>% filter(Kind=="Peanut",MMColor=="Blue")
yred <-  df_tidy %>% filter(Kind=="Peanut",MMColor=="Red")
yorange <-  df_tidy %>% filter(Kind=="Peanut",MMColor=="Orange")
t.test(xblue$CountInBag , yred$CountInBag)
t.test(xblue$CountInBag , yorange$CountInBag)

xblue <- df_tidy %>% filter(MMColor=="Blue")
yorange <-  df_tidy %>% filter(MMColor=="Orange")
t.test(xblue$CountInBag , yorange$CountInBag)




# consider plotting scatterplots of each color against the other
library(GGally)
lattice::splom(~df[,4:9], aes(z=Kind))

df %>% filter(Kind != "Peanut Butter") %>%
  ggpairs(columns = 13:18,
          mapping=aes(color = Kind),
          title="Scatterplot Matrix of Color Count in Bags")


# parallel coordinate plots?

df %>% filter(Kind != "Peanut Butter") %>%
  ggparcoord(columns = 13:18, scale = "globalminmax",groupColumn="Kind", 
             title="Count of M&Ms by Color in a Bag", 
             order = c(14,17,15,13,16,18)) +
      scale_color_manual(values = c("yellow","brown"))




# then start writing the R-Markdown Doc and get it finished.



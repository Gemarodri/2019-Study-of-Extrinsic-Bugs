#################################################################
############## ADDING LIBRARIES #################################
#################################################################
library(ggplot2)
library (rlist)
library(dplyr)
library(purrr)

#################################################################
################ LOADING DATASETS ###############################
#################################################################

df_Openstack_links <- read.csv("data/openstack_links.csv", header = TRUE)
df_BUG_BIC <- read.csv("data/Openstack_1880.csv", header = TRUE)
df_Openstack_Issues <- read.csv("data/openstack.csv", header = TRUE)

#################################################################
########### IDENTIFYING INTRINSIC AND EXTRINSIC BUGS ############
#################################################################

# Function myFunctionForApply: With this fuction we convert the integer(0) into NaN 
# Thus we will not drop values from the list (ret) when we unlist it.
myFunctionForApply <- function(x,column) {
  
  ret<-column[df_Openstack_Issues$commit_id==x]
  if (length(ret) == 0)
    return(NA)
  return(ret)
}

# From factor to character
df_Openstack_links$HashId <- sapply(df_Openstack_links$HashId, function(x) as.character(x))
df_Openstack_links$FixHashId <- sapply(df_Openstack_links$FixHashId, function(x) as.character(x))
df_Openstack_Issues$commit_id <- sapply(df_Openstack_Issues$commit_id, function(x) as.character(x))

#Adding Type of Bugs
#df_Openstack_links$TTF <- (df_Openstack_links$FixDate - df_Openstack_links$BugReportDate)/86400
df_Openstack_links$isBUG <- sapply(df_Openstack_links$BugId, function(i) df_BUG_BIC$BUG[df_BUG_BIC$BugId==i] )
df_Openstack_links$isBIC <- sapply(df_Openstack_links$BugId, function(i) df_BUG_BIC$BIC[df_BUG_BIC$BugId==i] )

df_Openstack_links$type[(df_Openstack_links$isBUG==1 & df_Openstack_links$isBIC==1)] <- "Intrinsic" 
df_Openstack_links$type[(df_Openstack_links$isBUG==1 & df_Openstack_links$isBIC==0)] <- "Extrinsic" 
df_Openstack_links$type[(df_Openstack_links$isBUG==0 & df_Openstack_links$isBIC==0)] <- "NoBug" 

# Aggregating data per type of bug
df_Openstack_links$BICdate <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$author_date))

# Size
df_Openstack_links$la <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$la)) #lines added
df_Openstack_links$ld <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$ld)) #Lines deleted

# Diffusion
df_Openstack_links$nf <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$nf)) #Number of files
df_Openstack_links$nd <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$nd)) #Number of components 
df_Openstack_links$ns <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$ns)) #Number of subsystems
df_Openstack_links$ent <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$ent)) #Entropy

# Review
df_Openstack_links$hcmt <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$hcmt)) #"Review comments"
df_Openstack_links$rtime <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$rtime)) #"Review timespan"
df_Openstack_links$nrev <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$nrev)) #"Review revisions"
df_Openstack_links$app <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$app)) #"Reviewers"

# History
df_Openstack_links$age <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$age)) #"Time since last modification"
df_Openstack_links$ndev <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$ndev)) #"Number of developers"
df_Openstack_links$nuc <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$nuc)) #"Number of past changes"

# Experience
df_Openstack_links$aexp <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$aexp)) #Author experience
df_Openstack_links$rexp <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$rexp)) #Reviewer experience
df_Openstack_links$oexp <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$oexp)) #Overall experience
df_Openstack_links$arexp <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$arexp)) #Relative author experience
df_Openstack_links$rrexp <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$rrexp)) #Relative reviewer experience
df_Openstack_links$orexp <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$orexp)) #Relative overall experience
df_Openstack_links$asexp <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$asexp)) #Subsystem author experience
df_Openstack_links$rsexp <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$rsexp)) #Subsystem reviewer experience
df_Openstack_links$osexp <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$osexp)) #Subsystem overall experience

# Awareness
df_Openstack_links$tcmt <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$tcmt)) 
df_Openstack_links$asawr <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$asawr)) #Author awareness
df_Openstack_links$rsawr <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$rsawr)) #Reviewer awareness
df_Openstack_links$osawr <- sapply(df_Openstack_links$HashId, function(x) myFunctionForApply(x,df_Openstack_Issues$osawr)) #Overall awareness

#Dropping rows with NaN values to obtain the summary of the dataframe.
#NaN values were set because those changes were non-reviewed 
completerecords <- na.omit(df_Openstack_links)
bugs <- completerecords %>% split(.$type) %>% map(summary)
bugs

write.csv(df_Openstack_links, file="Openstack_links_aggregating.csv" )

#################################################################
######################## PLOTS ##################################
#################################################################
counts <- table(completerecords$type)
barplot(counts, main="Type of issues",xlab="Number of issues")

#p <- ggplot(completerecords, aes(x=type, y=TTF)) + 
#  geom_violin()
#p + geom_boxplot(width=0.1) 
#geom_jitter(height = 0, width = 0.1)

p <- ggplot(completerecords, aes(x=type, y=BICdate)) + 
  geom_violin()
p + geom_boxplot(width=0.1)

p <- ggplot(completerecords, aes(x=type, y=BICage)) + 
  geom_violin()
p + geom_boxplot(width=0.1)

####### REMOVING OUTLIERS ##############
# # You can get the actual values of the outliers with this
outliers <- boxplot(df_Openstack_links$BICage~df_Openstack_links$type)$out
# 
# # First you need find in which rows the outliers are
df_Openstack_NoOutliers <- df_Openstack_links[which(df_Openstack_links$BICage %in% outliers),]
# 
p <- ggplot(df_Openstack_NoOutliers, aes(x=type, y=BICage)) + 
  geom_violin()
p + geom_boxplot(width=0.1)


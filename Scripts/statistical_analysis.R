###############################################################
## Statistical analisys to compare different groups of bugs ###
###############################################################
library(ggplot2)
library(dplyr)
library("ggpubr")
library(reshape2)

######################
## Load dataframes
######################
df_FixHashId <- read.csv("Grouping_by_FixHashID.csv", header = TRUE)
#df_FixHashId$type[df_FixHashId$type=="NoBug"] <- "Mislabeled"
df_BicHashId <- read.csv("Grouping_by_HashID.csv", header = TRUE)
df_BugId <- read.csv("Grouping_by_bugId.csv", header = TRUE)
df_BugIdShane <-df_BugId
df_BugIdShane$type <- "All"
df_BugId <- rbind(df_BugId, df_BugIdShane)
#################################################################
######################## PLOTS ##################################
#################################################################

m.dfFix <- melt(df_FixHashId, id.var="type")
m.dfBIC <- melt(df_BicHashId, id.var="type")
m.dfBug <- melt(df_BugId, id.var="type")

for (family in c("Size","Author","Diffusion","Review","Reviewer", "History")){
  
  figure_fname<-paste0(family, sep = ".", "pdf")
  m.df <-  m.dfFix[which(m.dfFix$variable ==family),]
  label<- paste0(family, sep = " ", "Family of BFC (log10)")
  ggplot(m.df, aes(x=type, y=value)) + geom_violin(trim = TRUE,scale = "area") + geom_boxplot(width=0.07)+ scale_y_log10() + ylab(label) + xlab("Classification of bugs") 
  ggsave(paste("PDF", "FIX", figure_fname, sep="/"), width=10, height=5)
 
}

for (family in c("Size","Author","Diffusion","Review","Reviewer", "History")){
  
  figure_fname<-paste0(family, sep = ".", "pdf")
  m.df <-  m.dfBIC[which(m.dfBIC$variable ==family),]
  label<- paste0(family, sep = " ", "Family of BIC (log10)")
  ggplot(m.df, aes(x=type, y=value)) + geom_violin(trim = TRUE,scale = "area") + geom_boxplot(width=0.07)+ scale_y_log10() + ylab(label) + xlab("Classification of bugs") 
  ggsave(paste("PDF", "BIC", figure_fname, sep="/"), width=10, height=5)
}

for (family in c("BugId", "FixHashId")){
  figure_fname<-paste0(family, sep = ".", "pdf")
  df_BugId <- df_BugId[which(df_BugId$BIC <= 20),]
  # Basic violin plot
  ggplot(df_BugId, aes(x=type, y=BIC)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(x="Type bug", y = "# BIC per Bug Report")+
    geom_boxplot(width=0.1)+
    theme_classic()  
  ggsave(paste("PDF", "BUG", figure_fname, sep="/"), width=10, height=5)
  
}

############################################
## Statistical Test     ####################
##  Kruskal-Wallis test ####################
############################################
# p-value fewer than the significance level 0.05, there are significant differences between the treatment groups.
 
# BIC
kruskal.test(History ~ type, data = df_BicHashId)
kruskal.test(Size ~ type, data = df_BicHashId)
kruskal.test(Review ~ type, data = df_BicHashId)
kruskal.test(Reviewer ~ type, data = df_BicHashId)
kruskal.test(Author ~ type, data = df_BicHashId)
kruskal.test(Diffusion ~ type, data = df_BicHashId)

# Multiple pairwise-comparison between groups
# To know which pairs of groups are different.
# pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.

Author_BIC_wilcox <- pairwise.wilcox.test(df_BicHashId$Author, df_BicHashId$type,
                                          p.adjust.method = "BH")
Author_BIC_wilcox[3]
History_BIC_wilcox <- pairwise.wilcox.test(df_BicHashId$History, df_BicHashId$type,
                                           p.adjust.method = "BH")
History_BIC_wilcox[3]
Size_BIC_wilcox <- pairwise.wilcox.test(df_BicHashId$Size, df_BicHashId$type,
                                        p.adjust.method = "BH")
Size_BIC_wilcox[3]
Review_BIC_wilcox <- pairwise.wilcox.test(df_BicHashId$Review, df_BicHashId$type,
                                          p.adjust.method = "BH")
Review_BIC_wilcox[3]
Reviewer_BIC_wilcox <- pairwise.wilcox.test(df_BicHashId$Reviewer, df_BicHashId$type,
                                            p.adjust.method = "BH")
Reviewer_BIC_wilcox[3]
Diffusion_BIC_wilcox <- pairwise.wilcox.test(df_BicHashId$Diffusion, df_BicHashId$type,
                                             p.adjust.method = "BH")
Diffusion_BIC_wilcox[3]

# FIX
kruskal.test(History ~ type, data = df_FixHashId)
kruskal.test(Size ~ type, data = df_FixHashId)
kruskal.test(Review ~ type, data = df_FixHashId)
kruskal.test(Reviewer ~ type, data = df_FixHashId)
kruskal.test(Author ~ type, data = df_FixHashId)
kruskal.test(Diffusion ~ type, data = df_FixHashId)

# Multiple pairwise-comparison between groups
Author_FIX_wilcox <- pairwise.wilcox.test(df_FixHashId$Author, df_FixHashId$type,
                                          p.adjust.method = "BH")
Author_FIX_wilcox[3]

History_FIX_wilcox <- pairwise.wilcox.test(df_FixHashId$History, df_FixHashId$type,
                                           p.adjust.method = "BH")
History_FIX_wilcox[3]

Size_FIX_wilcox <- pairwise.wilcox.test(df_FixHashId$Size, df_FixHashId$type,
                                        p.adjust.method = "BH")
Size_FIX_wilcox[3]

Review_FIX_wilcox <- pairwise.wilcox.test(df_FixHashId$Review, df_FixHashId$type,
                                          p.adjust.method = "BH")
Review_FIX_wilcox[3]

Reviewer_FIX_wilcox <- pairwise.wilcox.test(df_FixHashId$Reviewer, df_FixHashId$type,
                                            p.adjust.method = "BH")
Reviewer_FIX_wilcox[3]

Diffusion_FIX_wilcox <- pairwise.wilcox.test(df_FixHashId$Diffusion, df_FixHashId$type,
                                             p.adjust.method = "BH")
Diffusion_FIX_wilcox[3]


hist(normal,probability=T, main="Histogram of normal data",xlab="Approximately normally distributed data") lines(density(normal),col=2)

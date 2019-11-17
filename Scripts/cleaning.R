suppressMessages(library(futile.logger))
library(dplyr)
library(naniar)

ds_1880_issues <- read.csv("data/OpenStack_1880.csv")
valid_bugs = ds_1880_issues[ds_1880_issues[, "BUG"] == 1 | ds_1880_issues[, "BUG"] == '?' | ds_1880_issues[, "BUG"] == ' ',]
ds_bugs_reports = valid_bugs
ds_bugs_reports <- apply(ds_bugs_reports,2,as.character)

valid_NO_bugs= ds_1880_issues[ds_1880_issues[, "BUG"] == 0, ]
ds_NO_bugs_reports = valid_NO_bugs
ds_NO_bugs_reports <- apply(ds_NO_bugs_reports,2,as.character)

valid_intrinsics = valid_bugs[valid_bugs[, "BIC"] == 1 | valid_bugs[, "BIC"] == '?' | valid_bugs[, "BIC"] == '',]
ds_intrinsic_bugs = valid_intrinsics
ds_intrinsic_bugs <- apply(ds_intrinsic_bugs,2,as.character)

valid_extrinsics= valid_bugs[valid_bugs[, "BIC"] == 0,]
ds_extrinsic_bugs = valid_extrinsics
ds_extrinsic_bugs <- apply(ds_extrinsic_bugs,2,as.character)

# ====================== Selecting Shane's work
ds_openstack_issues <- read.csv("data/openstack.csv")
ds_openstack_links <- read.csv("data/openstack_links.csv")

#get the unique BICs and FIXs of each bugID in BugsIDs
get_BIC_Fix_id = function(bugIDs, ds_links){
  n = length(bugIDs)
  bics <- vector()
  fixs <- vector()
  for(idx in 1:n){
    #getting BICS and FIX from ds_links
    hashFix.factor = ds_links$FixHashId[ds_links[, "BugId"] == bugIDs[idx]]
    hashBIC.factor = ds_links$HashId[ds_links[, "BugId"] == bugIDs[idx]]

    #getting unique BICs and FIXs
    hashBIC = unique ((as.character(hashBIC.factor)),incomparable=FALSE)
    hashFix = unique ((as.character(hashFix.factor)),incomparable=FALSE)

    #Adding unique data to vectors
    bics <- c(bics, hashBIC)
    fixs <- c(fixs, hashFix)

  }
  return(list(bics=bics,fixs=fixs))
}

#Unmarking the commits that are not BICs or not FIX
removingMark = function(commit_id,hashs){
  ifelse(commit_id %in% hashs,return(-length(which(commit_id==hashs))),return(0))
}
addingMark = function(commit_id,hashs){
  ifelse(commit_id %in% hashs,return(length(which(commit_id==hashs))),return(0))
}


BIC_FIX_nobugs <- get_BIC_Fix_id(valid_NO_bugs$BugId,ds_openstack_links)
BIC_FIX_extrinsicbugs <- get_BIC_Fix_id(valid_extrinsics$BugId,ds_openstack_links)
BIC_FIX_intrinsicsbugs <- get_BIC_Fix_id(valid_intrinsics$BugId,ds_openstack_links)

issues <- ds_openstack_issues
issues$nobics = lapply(issues$commit_id, removingMark, hashs=unlist(BIC_FIX_nobugs[1]))
issues$nofix = lapply(issues$commit_id, removingMark, hashs=unlist(BIC_FIX_nobugs[2]))
issues$nobicsextrinsic = lapply(issues$commit_id, removingMark, hashs=unlist(BIC_FIX_extrinsicbugs[1]))
issues$fixsextrinsic = lapply(issues$commit_id, removingMark, hashs=unlist(BIC_FIX_extrinsicbugs[2]))
issues$bicintrinsics = lapply(issues$commit_id, addingMark, hashs=unlist(BIC_FIX_intrinsicsbugs[1]))
issues$fixsintrinsic = lapply(issues$commit_id, addingMark, hashs=unlist(BIC_FIX_intrinsicsbugs[2]))

issues$cleanfixcount <- rowSums(sapply(issues[, c(4, 34, 36)],as.numeric))
issues$cleanfixcount=ifelse(issues$cleanfixcount<=0,0,issues$cleanfixcount)
issues$cleanbugcount <- rowSums(sapply(issues[, c(3, 33, 35)],as.numeric))
issues$cleanbugcount=ifelse(issues$cleanbugcount<=0,0,issues$cleanbugcount)

cleanissues <- ds_openstack_issues
cleanissues$bugcount = issues$bicintrinsics
cleanissues$fixcount = issues$fixsintrinsic

cleanissues <- apply(cleanissues,2,as.character)
write.csv(cleanissues, file="data/openstack_clean.csv" )


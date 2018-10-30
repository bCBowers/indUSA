library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(lattice)

# DFP plot theme 
theme_dfp <- function() {
  get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
  }
  if(get_os() == "osx") {
    theme_bw() +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.caption=element_text(hjust=1, size=9,
                                      margin=margin(t=10),
                                      face="italic"),
            plot.title=element_text(hjust=0, size=18,
                                    margin=margin(b=10),
                                    face="bold", family='FuturaBT-Heavy'),
            plot.subtitle=element_text(hjust=0,
                                       family='Montserrat-Regular'),
            axis.title.y=element_text(size=10,hjust=1,
                                      face="italic", family="Montserrat-Thin"),
            axis.title.x=element_text(hjust=1, size=10, face="italic", family="Montserrat-Thin",
                                      margin = margin(t = 10)), # , margin = margin(t = 10)
            legend.position = "bottom",
            legend.title = element_text(face="bold", family="Montserrat-Regular"),
            text=element_text(family="Montserrat-Regular"))
  }
  else {
    theme_bw() +
      theme(panel.border = element_blank(),
            plot.caption=element_text(hjust=1, size=9,
                                      margin=margin(t=10),
                                      face="italic"),
            plot.title=element_text(hjust=0, size=18,
                                    margin=margin(b=10),
                                    face="bold", family='FuturaBT-Heavy'),
            plot.subtitle=element_text(hjust=0,
                                       family='Montserrat-Regular'),
            axis.title.y=element_text(size=10,hjust=1,
                                      face="italic", family="Montserrat-Thin"),
            axis.title.x=element_text(hjust=1, size=10, face="italic", family="Montserrat-Thin",
                                      margin = margin(t = 10)), # , margin = margin(t = 10)
            legend.position = "bottom",
            legend.title = element_text(face="bold", family="Montserrat-Regular"),
            text=element_text(family="Montserrat-Regular"))
  }
}

# Compile all polls for their csv files
if (exists('dataset')){
  rm('dataset')
}
  
folder <- "../DFPPolls/Polls"
file_list <- list.files(folder)

for (file in file_list){
  if (!exists("dataset")){
    dataset <- read.csv(file.path(folder,file))

    dataset$distr <- file
  }
  
  if (exists("dataset")){
    temp_dataset <-read.csv(file.path(folder,file))
    temp_dataset$distr <- file
    dataset<-merge(dataset, temp_dataset, all=TRUE)
    rm(temp_dataset)
  }
}

# Subset polls by Independent voters
allInd <- dataset[dataset$partyid=='Independent (No party)',]
allDem <- allInd[allInd$response=='Dem',]
allRep <- allInd[allInd$response=='Rep',]

# Build bar charts to display demographic information
demo <- allInd %>% 
  select(ager, educ, file_race, gender, w_LV)

ager <- aggregate(demo$w_LV, by=list(Age=demo$ager), FUN=sum)
ager <- ager[ager$Age!='[DO NOT READ] Refused', ]
ager$x <- ager$x/sum(ager$x)
ager$Age <- as.character(ager$Age)
ager$perc <- paste(round(ager$x*100,1), "%", sep="")

plot1 <- ggplot(ager, aes(y=x, x=Age, fill=Age)) +
  geom_bar(stat="identity", width=0.8) +
  xlab('') + ylab('') + guides(fill=FALSE) +
  geom_text(aes( label =perc, hjust = -.1)) +
  theme_dfp() + theme(axis.text.x = element_blank()) + coord_flip(ylim=c(0,1)) + 
  scale_x_discrete(name ="", limits=c("65 and older", "50 to 64", "35 to 49","18 to 34")) +
  scale_fill_manual(values=c("#124073", "#A8BF14", "#B71D1A", '#BF7800')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank())

educ <- aggregate(demo$w_LV, by=list(Edu=demo$educ), FUN=sum)
educ <- educ[educ$Edu!='[DO NOT READ] Refused', ]
educ$Edu <- as.character(educ$Edu)
educ$Edu[educ$Edu == "Grade school"] <- 'High school or below'
educ$x[educ$Edu == "High school or below"] <- educ$x[educ$Edu == "High school or below"] + educ$x[educ$Edu == "High school"]
educ <- educ[educ$Edu!='High school', ]
educ$order[educ$Edu=='High school or below'] <- 4
educ$order[educ$Edu=='Some college or trade school'] <- 3
educ$order[educ$Edu=="Bachelors' degree"] <- 2
educ$order[educ$Edu=='Graduate or Professional Degree'] <- 1
educ$Edu <- factor(educ$Edu,levels = educ$Edu[order(educ$order)])
educ$x <- educ$x/sum(educ$x)
educ$perc <- paste(round(educ$x*100,1), "%", sep="")
plot2 <- ggplot(educ, aes(y=x, x=Edu, fill=Edu)) +
  geom_bar(stat="identity", width=0.8) +
  xlab('') + ylab('') + guides(fill=FALSE) + 
  scale_x_discrete(name ="", labels=c("Graduate or\nProfessional\nDegree", "Bachelors'\ndegree", "Some college\nor trade school","High school\nor below")) +
  geom_text(aes( label =perc, hjust = -.1)) +
  theme_dfp() + theme(axis.text.x = element_blank()) + coord_flip(ylim=c(0,1)) +
  scale_fill_manual(values=c("#124073", "#A8BF14", "#B71D1A", '#BF7800')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank())

race <- aggregate(demo$w_LV, by=list(Race=demo$file_race), FUN=sum)
race <- race[race$Race!='[DO NOT READ] Refused', ]
race$x <- race$x/sum(race$x)
race$perc <- paste(round(race$x*100,1), "%", sep="")
plot3 <- ggplot(race, aes(y=x, x=Race, fill=Race)) +
  geom_bar(stat="identity") +
  xlab('') + ylab('') + guides(fill=FALSE) +
  scale_x_discrete(name ="", limits=c("Unknown", "Other", "Asian", "Black", "Hispanic", "White")) +
  geom_text(aes( label=perc, hjust = -.1)) + 
  theme_dfp()  + theme(axis.text.x = element_blank()) + coord_flip(ylim=c(0,1)) +
  scale_fill_manual(values=c("#124073", "#A8BF14", "#B71D1A", '#BF7800', '#b3b3b3', '#0A2645')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank())

gender <- aggregate(demo$w_LV, by=list(Gender=demo$gender), FUN=sum)
gender <- gender[gender$Gender!='[DO NOT READ] Refused', ]
gender$x <- gender$x/sum(gender$x)
gender$perc <- paste(round(gender$x*100,1), "%", sep="")
plot4 <- ggplot(gender, aes(y=x, x=Gender, fill=Gender)) +
  geom_bar(stat="identity", width=0.70) +
  geom_text(aes( label=perc, hjust = -.1)) + 
  xlab('') + ylab('') + guides(fill=FALSE) +
  theme_dfp() + theme(axis.text.x = element_blank()) + coord_flip(ylim=c(0,1)) +
  scale_fill_manual(values=c("#124073", "#A8BF14", '#BF7800')) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank())

g <- grid.arrange(plot1, plot2, plot3, plot4, nrow=6, layout_matrix = cbind(c(4, 4, 3, 3, 3, 3), c(1, 1, 1, 2, 2, 2)),
                  top="Demographic Breakdown for Self-Identified Independents\n")
ggsave('Demo.png',g)

########################################################################################################################################## All Independents

# Subset relevant questions
issues <- allInd %>%
  select(genballot, approve, CRIMES, NFLA, NFLRIGHT, CHECK, REVERSERACIS, SWAMP, COLLUDE, MUELLER, IMPEACH, APPKAV, SINGLEPAY, FEMINISM, 
         WOMEN, TARIFF, TAXREFORM, TRUMPECON, KAVASST, ASSAULTW, ENGLISH, ABOLOISHICE, CONIMMIG, IMMCRIME, w_LV)

issueList <- issues %>%
  select(genballot, approve, CRIMES, NFLA, NFLRIGHT, CHECK, REVERSERACIS, SWAMP, COLLUDE, MUELLER, IMPEACH, APPKAV, SINGLEPAY, FEMINISM, 
                WOMEN, TARIFF, TAXREFORM, TRUMPECON, KAVASST, ASSAULTW, ENGLISH, ABOLOISHICE, CONIMMIG, IMMCRIME)

# Aggregate all of the issues
rm(sumIssue)
for (issue in names(issueList)){
  if (!exists("sumIssue")){
    issueData <- issues %>%
      select(issue, w_LV)
    issueData <- na.omit(issueData)
    sumIssue <- aggregate(issueData[2], by=issueData[1], FUN=sum)
    sumIssue <- data.frame(t(sumIssue))
    names(sumIssue) <- as.character(unlist(sumIssue[1,]))
    sumIssue <- sumIssue[-1,]
    sumIssue$issue <- issue
  }

  if (exists("sumIssue")){
    issueData <- issues %>%
      select(issue, w_LV)
    issueData <- na.omit(issueData)
    temp_issue <- aggregate(issueData[2], by=issueData[1], FUN=sum)
    temp_issue <- data.frame(t(temp_issue))
    names(temp_issue) <- as.character(unlist(temp_issue[1,]))
    temp_issue <- temp_issue[-1,]
    temp_issue$issue <- issue
    sumIssue<-merge(sumIssue, temp_issue, all=TRUE)
  }
}
rownames(sumIssue) <- sumIssue$issue
sumIssue <- subset(sumIssue,select=-c(issue))

# Standardize naming convention (make everything Yes or)
names(sumIssue)[names(sumIssue) == 'agree'] <- 'Dem'
sumIssue$Dem <- as.character(sumIssue$Dem)
sumIssue$Support <- as.character(sumIssue$Support)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Support[is.na(sumIssue$Dem)]


sumIssue$support <- as.character(sumIssue$support)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$support[is.na(sumIssue$Dem)]

sumIssue$'Reps. keep House' <- as.character(sumIssue$'Reps. keep House')
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$'Reps. keep House'[is.na(sumIssue$Dem)]

sumIssue$Believe <- as.character(sumIssue$Believe)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Believe[is.na(sumIssue$Dem)]

sumIssue$Approve <- as.character(sumIssue$Approve)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Approve[is.na(sumIssue$Dem)]

names(sumIssue)[names(sumIssue) == 'disagree'] <- 'Rep'
sumIssue$Rep <- as.character(sumIssue$Rep)
sumIssue['CHECK', 'Rep'] <- sumIssue['CHECK', 'Dem']
sumIssue['CHECK', 'Dem'] <- NA

sumIssue$Oppose <- as.character(sumIssue$Oppose)
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$Oppose[is.na(sumIssue$Rep)]

sumIssue$oppose <- as.character(sumIssue$oppose)
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$oppose[is.na(sumIssue$Rep)]

sumIssue$Check <- as.character(sumIssue$Check)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Check[is.na(sumIssue$Dem)]

sumIssue$'Dems. take House' <- as.character(sumIssue$'Dems. take House')
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$'Dems. take House'[is.na(sumIssue$Rep)]

sumIssue$'Do not believe' <- as.character(sumIssue$'Do not believe')
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$'Do not believe'[is.na(sumIssue$Rep)]

sumIssue$Disapp. <- as.character(sumIssue$Disapp.)
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$Disapp.[is.na(sumIssue$Rep)]

sumIssue <- sumIssue %>% 
  select("Don't know", 'Dem', 'Rep')

sumIssue$"Don't know" <- as.numeric(as.character(sumIssue$"Don't know"))
sumIssue$Dem <- as.numeric(sumIssue$Dem)
sumIssue$Rep <- as.numeric(sumIssue$Rep)
normIssue <- sumIssue/rowSums(sumIssue)
sumIssue$polled <- sumIssue$"Don't know" +sumIssue$Dem + sumIssue$Rep

noFlip<- normIssue[rownames(normIssue) == 'CHECK' | rownames(normIssue) == 'CRIMES' | rownames(normIssue) == 'NFLA' | rownames(normIssue) == 'NFLRIGHT' |
                      rownames(normIssue) == 'MUELLER' | rownames(normIssue) == 'IMPEACH' |
                      rownames(normIssue) == 'SINGLEPAY' | rownames(normIssue) == 'FEMINISM' | rownames(normIssue) == 'WOMEN' | rownames(normIssue) == 'ASSAULTW' | 
                      rownames(normIssue) == 'ABOLOISHICE' | rownames(normIssue) == 'KAVASST',]



toFlip <- normIssue[rownames(normIssue) == 'COLLUDE' | rownames(normIssue) == 'APPKAV' | rownames(normIssue) == 'TAXREFORM' | rownames(normIssue) == 'TRUMPECON' |
                      rownames(normIssue) == 'REVERSERACIS' | rownames(normIssue) == 'ENGLISH' | rownames(normIssue) == 'CONIMMIG' | rownames(normIssue) == 'IMMCRIME' | 
                      rownames(normIssue) == 'approve' | rownames(normIssue) == 'TARIFF' | rownames(normIssue) == 'genballot'| rownames(normIssue) == 'SWAMP',]
toFlip$hold <- toFlip$Rep
toFlip$Rep <- toFlip$Dem
toFlip$Dem <- toFlip$hold
flipped <- toFlip %>% 
  select("Don't know", 'Dem', 'Rep')

normIssue <- noFlip

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)
#base1 <- (merge(lookup, largetable, by = 'HouseType'))

labels <- read.csv('nytMap.csv')
rownames(labels) <- labels[,1]
normIssue <- (merge(labels, normIssue, by='issue'))
normIssue <- subset(normIssue, select = -c(issue, Question))
rownames(normIssue) <- normIssue[,1]
normIssue <- subset(normIssue, select = -c(Label))
colnames(normIssue) <- c("Don't know", 'Yes', 'No')

norm2 <- normIssue %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
norm2$colname <- factor(norm2$colname, c('Yes', 'No', "Don't know"))
norm2$rowname <- factor(norm2$rowname, c('Elect more women?', 'Bothered by hearing foreign language?', 'Do immigrants commit more crime?', 'Reduce immigration and build the wall?', 
                                         'Support assault weapons ban?', 'Support rights of kneeling NFL players?', 'Support single payer?', "Has Trump drained the swamp?", 
                                         "Support Trump tariffs?", 'Do you want Congress to check Trump?', "Do you support Mueller investigation?", 'Is reverse racism bigger issue than racism?', 
                                         'Want Republicans to keep the House?','Approve of Trump?', 'Elect more feminists?',   
                                         'Support 2017 tax reform?', 'Has Trump committed crimes in office?', 'Should a Democratic House impeach Trump?', 'Do you support Kavanaugh confirmation?',
                                         'Has Trump helped you economically?', 'Is Russia investigation a witch hunt?',
                                         'Support actions of kneeling NFL players?',  'Abolish ICE?', 'Believe Kavanaugh accusations?'))

norm2 <- norm2[with(norm2, order(norm2$value)),]

plot1 <- ggplot(norm2, aes(x = colname, y = fct_rev(as_factor(rowname)), fill = value)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = paste0(round(value * 100), "%"))) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient('value', low='white', high='#A8BF14') +
  xlab("") + ylab('') + theme_dfp() +
  theme(legend.position="none")
#ggsave('indYes.png')

normIssue <- flipped

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)
#base1 <- (merge(lookup, largetable, by = 'HouseType'))

labels <- read.csv('nytMap.csv')
rownames(labels) <- labels[,1]
normIssue <- (merge(labels, normIssue, by='issue'))
normIssue <- subset(normIssue, select = -c(issue, Question))
rownames(normIssue) <- normIssue[,1]
normIssue <- subset(normIssue, select = -c(Label))
colnames(normIssue) <- c("Don't know", 'No', 'Yes')

norm2 <- normIssue %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
norm2$colname <- factor(norm2$colname, c('Yes', 'No', "Don't know"))
norm2$rowname <- factor(norm2$rowname, c('Elect more women?', 'Bothered by hearing foreign language?', 'Do immigrants commit more crime?', 'Reduce immigration and build the wall?', 
                                         'Support assault weapons ban?', 'Support rights of kneeling NFL players?', 'Support single payer?', "Has Trump drained the swamp?", 
                                         "Support Trump tariffs?", 'Do you want Congress to check Trump?', "Do you support Mueller investigation?", 'Is reverse racism bigger issue than racism?', 
                                         'Want Republicans to keep the House?','Approve of Trump?', 'Elect more feminists?',   
                                         'Support 2017 tax reform?', 'Has Trump committed crimes in office?', 'Should a Democratic House impeach Trump?', 'Do you support Kavanaugh confirmation?',
                                         'Has Trump helped you economically?', 'Is Russia investigation a witch hunt?',
                                         'Support actions of kneeling NFL players?',  'Abolish ICE?', 'Believe Kavanaugh accusations?'))

norm2 <- norm2[with(norm2, order(norm2$value)),]

plot2 <- ggplot(norm2, aes(x = colname, y = fct_rev(as_factor(rowname)), fill = value)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = paste0(round(value * 100), "%"))) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient('value', low='white', high='#A8BF14') +
  xlab("") + ylab('') + theme_dfp() +
  theme(legend.position="none")
#ggsave('indNo.png',units=c('cm'))

g <- grid.arrange(plot1, plot2, nrow=1, top="Independent Voters")

ggsave('AllInd.png',g)

############################ Dem Voters

issues <- allDem %>%
  select(genballot, approve, CRIMES, NFLA, NFLRIGHT, CHECK, REVERSERACIS, SWAMP, COLLUDE, MUELLER, IMPEACH, APPKAV, SINGLEPAY, FEMINISM, 
         WOMEN, TARIFF, TAXREFORM, TRUMPECON, KAVASST, ASSAULTW, ENGLISH, ABOLOISHICE, CONIMMIG, IMMCRIME, w_LV)

issueList <- issues %>%
  select(genballot, approve, CRIMES, NFLA, NFLRIGHT, CHECK, REVERSERACIS, SWAMP, COLLUDE, MUELLER, IMPEACH, APPKAV, SINGLEPAY, FEMINISM, 
         WOMEN, TARIFF, TAXREFORM, TRUMPECON, KAVASST, ASSAULTW, ENGLISH, ABOLOISHICE, CONIMMIG, IMMCRIME)


rm(sumIssue)
for (issue in names(issueList)){
  if (!exists("sumIssue")){
    issueData <- issues %>%
      select(issue, w_LV)
    issueData <- na.omit(issueData)
    sumIssue <- aggregate(issueData[2], by=issueData[1], FUN=sum)
    sumIssue <- data.frame(t(sumIssue))
    names(sumIssue) <- as.character(unlist(sumIssue[1,]))
    sumIssue <- sumIssue[-1,]
    sumIssue$issue <- issue
    
  }
  
  if (exists("sumIssue")){
    issueData <- issues %>%
      select(issue, w_LV)
    issueData <- na.omit(issueData)
    temp_issue <- aggregate(issueData[2], by=issueData[1], FUN=sum)
    temp_issue <- data.frame(t(temp_issue))
    names(temp_issue) <- as.character(unlist(temp_issue[1,]))
    temp_issue <- temp_issue[-1,]
    temp_issue$issue <- issue
    sumIssue<-merge(sumIssue, temp_issue, all=TRUE)
  }
}

rownames(sumIssue) <- sumIssue$issue
sumIssue <- subset(sumIssue,select=-c(issue))


names(sumIssue)[names(sumIssue) == 'agree'] <- 'Dem'
sumIssue$Dem <- as.character(sumIssue$Dem)
sumIssue$Support <- as.character(sumIssue$Support)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Support[is.na(sumIssue$Dem)]


sumIssue$support <- as.character(sumIssue$support)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$support[is.na(sumIssue$Dem)]

sumIssue$'Reps. keep House' <- as.character(sumIssue$'Reps. keep House')
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$'Reps. keep House'[is.na(sumIssue$Dem)]

sumIssue$Believe <- as.character(sumIssue$Believe)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Believe[is.na(sumIssue$Dem)]

sumIssue$Approve <- as.character(sumIssue$Approve)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Approve[is.na(sumIssue$Dem)]

names(sumIssue)[names(sumIssue) == 'disagree'] <- 'Rep'
sumIssue$Rep <- as.character(sumIssue$Rep)
sumIssue['CHECK', 'Rep'] <- sumIssue['CHECK', 'Dem']
sumIssue['CHECK', 'Dem'] <- NA

sumIssue$Oppose <- as.character(sumIssue$Oppose)
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$Oppose[is.na(sumIssue$Rep)]

sumIssue$oppose <- as.character(sumIssue$oppose)
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$oppose[is.na(sumIssue$Rep)]

sumIssue$Check <- as.character(sumIssue$Check)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Check[is.na(sumIssue$Dem)]

sumIssue$'Dems. take House' <- as.character(sumIssue$'Dems. take House')
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$'Dems. take House'[is.na(sumIssue$Rep)]

sumIssue$'Do not believe' <- as.character(sumIssue$'Do not believe')
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$'Do not believe'[is.na(sumIssue$Rep)]

sumIssue$Disapp. <- as.character(sumIssue$Disapp.)
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$Disapp.[is.na(sumIssue$Rep)]

sumIssue <- sumIssue %>% 
  select("Don't know", 'Dem', 'Rep')

sumIssue$"Don't know" <- as.numeric(as.character(sumIssue$"Don't know"))
sumIssue$Dem <- as.numeric(sumIssue$Dem)
sumIssue$Rep <- as.numeric(sumIssue$Rep)
normIssue <- sumIssue/rowSums(sumIssue)
sumIssue$polled <- sumIssue$"Don't know" +sumIssue$Dem + sumIssue$Rep

noFlip<- normIssue[rownames(normIssue) == 'CHECK' | rownames(normIssue) == 'CRIMES' | rownames(normIssue) == 'NFLA' | rownames(normIssue) == 'NFLRIGHT' |
                     rownames(normIssue) == 'MUELLER' | rownames(normIssue) == 'IMPEACH' |
                     rownames(normIssue) == 'SINGLEPAY' | rownames(normIssue) == 'FEMINISM' | rownames(normIssue) == 'WOMEN' | rownames(normIssue) == 'ASSAULTW' | 
                     rownames(normIssue) == 'ABOLOISHICE' | rownames(normIssue) == 'KAVASST',]



toFlip <- normIssue[rownames(normIssue) == 'COLLUDE' | rownames(normIssue) == 'APPKAV' | rownames(normIssue) == 'TAXREFORM' | rownames(normIssue) == 'TRUMPECON' |
                      rownames(normIssue) == 'REVERSERACIS' | rownames(normIssue) == 'ENGLISH' | rownames(normIssue) == 'CONIMMIG' | rownames(normIssue) == 'IMMCRIME' | 
                      rownames(normIssue) == 'approve' | rownames(normIssue) == 'TARIFF' | rownames(normIssue) == 'genballot'| rownames(normIssue) == 'SWAMP',]
toFlip$hold <- toFlip$Rep
toFlip$Rep <- toFlip$Dem
toFlip$Dem <- toFlip$hold
flipped <- toFlip %>% 
  select("Don't know", 'Dem', 'Rep')

normIssue <- noFlip

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)
#base1 <- (merge(lookup, largetable, by = 'HouseType'))

labels <- read.csv('nytMap.csv')
rownames(labels) <- labels[,1]
normIssue <- (merge(labels, normIssue, by='issue'))
normIssue <- subset(normIssue, select = -c(issue, Question))
rownames(normIssue) <- normIssue[,1]
normIssue <- subset(normIssue, select = -c(Label))
colnames(normIssue) <- c("Don't know", 'Yes', 'No')

norm2 <- normIssue %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
norm2$colname <- factor(norm2$colname, c('Yes', 'No', "Don't know"))
norm2$rowname <- factor(norm2$rowname, c('Elect more women?', 'Bothered by hearing foreign language?', 'Do immigrants commit more crime?', 'Reduce immigration and build the wall?', 
                                         'Support assault weapons ban?', 'Support rights of kneeling NFL players?', 'Support single payer?', "Has Trump drained the swamp?", 
                                         "Support Trump tariffs?", 'Do you want Congress to check Trump?', "Do you support Mueller investigation?", 'Is reverse racism bigger issue than racism?', 
                                         'Want Republicans to keep the House?','Approve of Trump?', 'Elect more feminists?',   
                                         'Support 2017 tax reform?', 'Has Trump committed crimes in office?', 'Should a Democratic House impeach Trump?', 'Do you support Kavanaugh confirmation?',
                                         'Has Trump helped you economically?', 'Is Russia investigation a witch hunt?',
                                         'Support actions of kneeling NFL players?',  'Abolish ICE?', 'Believe Kavanaugh accusations?'))

norm2 <- norm2[with(norm2, order(norm2$value)),]

plot1 <- ggplot(norm2, aes(x = colname, y = fct_rev(as_factor(rowname)), fill = value)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = paste0(round(value * 100), "%"))) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient('value', low='white', high='#124073') +
  xlab("") + ylab('') + theme_dfp() +
  theme(legend.position="none")
#ggsave('demYes.png')

normIssue <- flipped

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)
#base1 <- (merge(lookup, largetable, by = 'HouseType'))

labels <- read.csv('nytMap.csv')
rownames(labels) <- labels[,1]
normIssue <- (merge(labels, normIssue, by='issue'))
normIssue <- subset(normIssue, select = -c(issue, Question))
rownames(normIssue) <- normIssue[,1]
normIssue <- subset(normIssue, select = -c(Label))
colnames(normIssue) <- c("Don't know", 'No', 'Yes')

norm2 <- normIssue %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
norm2$colname <- factor(norm2$colname, c('Yes', 'No', "Don't know"))
norm2$rowname <- factor(norm2$rowname, c('Elect more women?', 'Bothered by hearing foreign language?', 'Do immigrants commit more crime?', 'Reduce immigration and build the wall?', 
                                         'Support assault weapons ban?', 'Support rights of kneeling NFL players?', 'Support single payer?', "Has Trump drained the swamp?", 
                                         "Support Trump tariffs?", 'Do you want Congress to check Trump?', "Do you support Mueller investigation?", 'Is reverse racism bigger issue than racism?', 
                                         'Want Republicans to keep the House?','Approve of Trump?', 'Elect more feminists?',   
                                         'Support 2017 tax reform?', 'Has Trump committed crimes in office?', 'Should a Democratic House impeach Trump?', 'Do you support Kavanaugh confirmation?',
                                         'Has Trump helped you economically?', 'Is Russia investigation a witch hunt?',
                                         'Support actions of kneeling NFL players?',  'Abolish ICE?', 'Believe Kavanaugh accusations?'))

norm2 <- norm2[with(norm2, order(norm2$value)),]

plot2 <- ggplot(norm2, aes(x = colname, y = fct_rev(as_factor(rowname)), fill = value)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = paste0(round(value * 100), "%"))) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient('value', low='white', high='#124073') +
  xlab("") + ylab('') + theme_dfp() +
  theme(legend.position="none")
#ggsave('demNo.png')

g <- grid.arrange(plot1, plot2, nrow=1, top="Independent Voters who Support Democratic Representatives")
ggsave('DemInd.png',g)
################## Rep Voters

issues <- allRep %>%
  select(genballot, approve, CRIMES, NFLA, NFLRIGHT, CHECK, REVERSERACIS, SWAMP, COLLUDE, MUELLER, IMPEACH, APPKAV, SINGLEPAY, FEMINISM, 
         WOMEN, TARIFF, TAXREFORM, TRUMPECON, KAVASST, ASSAULTW, ENGLISH, ABOLOISHICE, CONIMMIG, IMMCRIME, w_LV)

issueList <- issues %>%
  select(genballot, approve, CRIMES, NFLA, NFLRIGHT, CHECK, REVERSERACIS, SWAMP, COLLUDE, MUELLER, IMPEACH, APPKAV, SINGLEPAY, FEMINISM, 
         WOMEN, TARIFF, TAXREFORM, TRUMPECON, KAVASST, ASSAULTW, ENGLISH, ABOLOISHICE, CONIMMIG, IMMCRIME)


rm(sumIssue)
for (issue in names(issueList)){
  if (!exists("sumIssue")){
    issueData <- issues %>%
      select(issue, w_LV)
    issueData <- na.omit(issueData)
    sumIssue <- aggregate(issueData[2], by=issueData[1], FUN=sum)
    sumIssue <- data.frame(t(sumIssue))
    names(sumIssue) <- as.character(unlist(sumIssue[1,]))
    sumIssue <- sumIssue[-1,]
    sumIssue$issue <- issue
    
  }
  
  if (exists("sumIssue")){
    issueData <- issues %>%
      select(issue, w_LV)
    issueData <- na.omit(issueData)
    temp_issue <- aggregate(issueData[2], by=issueData[1], FUN=sum)
    temp_issue <- data.frame(t(temp_issue))
    names(temp_issue) <- as.character(unlist(temp_issue[1,]))
    temp_issue <- temp_issue[-1,]
    temp_issue$issue <- issue
    sumIssue<-merge(sumIssue, temp_issue, all=TRUE)
  }
}

rownames(sumIssue) <- sumIssue$issue
sumIssue <- subset(sumIssue,select=-c(issue))

names(sumIssue)[names(sumIssue) == 'agree'] <- 'Dem'
sumIssue$Dem <- as.character(sumIssue$Dem)
sumIssue$Support <- as.character(sumIssue$Support)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Support[is.na(sumIssue$Dem)]


sumIssue$support <- as.character(sumIssue$support)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$support[is.na(sumIssue$Dem)]

sumIssue$'Reps. keep House' <- as.character(sumIssue$'Reps. keep House')
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$'Reps. keep House'[is.na(sumIssue$Dem)]

sumIssue$Believe <- as.character(sumIssue$Believe)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Believe[is.na(sumIssue$Dem)]

sumIssue$Approve <- as.character(sumIssue$Approve)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Approve[is.na(sumIssue$Dem)]

names(sumIssue)[names(sumIssue) == 'disagree'] <- 'Rep'
sumIssue$Rep <- as.character(sumIssue$Rep)
sumIssue['CHECK', 'Rep'] <- sumIssue['CHECK', 'Dem']
sumIssue['CHECK', 'Dem'] <- NA

sumIssue$Oppose <- as.character(sumIssue$Oppose)
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$Oppose[is.na(sumIssue$Rep)]

sumIssue$oppose <- as.character(sumIssue$oppose)
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$oppose[is.na(sumIssue$Rep)]

sumIssue$Check <- as.character(sumIssue$Check)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Check[is.na(sumIssue$Dem)]

sumIssue$'Dems. take House' <- as.character(sumIssue$'Dems. take House')
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$'Dems. take House'[is.na(sumIssue$Rep)]

sumIssue$'Do not believe' <- as.character(sumIssue$'Do not believe')
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$'Do not believe'[is.na(sumIssue$Rep)]

sumIssue$Disapp. <- as.character(sumIssue$Disapp.)
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$Disapp.[is.na(sumIssue$Rep)]

sumIssue <- sumIssue %>% 
  select("Don't know", 'Dem', 'Rep')

sumIssue$"Don't know" <- as.numeric(as.character(sumIssue$"Don't know"))
sumIssue$Dem <- as.numeric(sumIssue$Dem)
sumIssue$Rep <- as.numeric(sumIssue$Rep)
normIssue <- sumIssue/rowSums(sumIssue)
sumIssue$polled <- sumIssue$"Don't know" +sumIssue$Dem + sumIssue$Rep

noFlip<- normIssue[rownames(normIssue) == 'CHECK' | rownames(normIssue) == 'CRIMES' | rownames(normIssue) == 'NFLA' | rownames(normIssue) == 'NFLRIGHT' |
                     rownames(normIssue) == 'MUELLER' | rownames(normIssue) == 'IMPEACH' |
                     rownames(normIssue) == 'SINGLEPAY' | rownames(normIssue) == 'FEMINISM' | rownames(normIssue) == 'WOMEN' | rownames(normIssue) == 'ASSAULTW' | 
                     rownames(normIssue) == 'ABOLOISHICE' | rownames(normIssue) == 'KAVASST',]



toFlip <- normIssue[rownames(normIssue) == 'COLLUDE' | rownames(normIssue) == 'APPKAV' | rownames(normIssue) == 'TAXREFORM' | rownames(normIssue) == 'TRUMPECON' |
                      rownames(normIssue) == 'REVERSERACIS' | rownames(normIssue) == 'ENGLISH' | rownames(normIssue) == 'CONIMMIG' | rownames(normIssue) == 'IMMCRIME' | 
                      rownames(normIssue) == 'approve' | rownames(normIssue) == 'TARIFF' | rownames(normIssue) == 'genballot'| rownames(normIssue) == 'SWAMP',]
toFlip$hold <- toFlip$Rep
toFlip$Rep <- toFlip$Dem
toFlip$Dem <- toFlip$hold
flipped <- toFlip %>% 
  select("Don't know", 'Dem', 'Rep')

normIssue <- noFlip

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)
#base1 <- (merge(lookup, largetable, by = 'HouseType'))

labels <- read.csv('nytMap.csv')
rownames(labels) <- labels[,1]
normIssue <- (merge(labels, normIssue, by='issue'))
normIssue <- subset(normIssue, select = -c(issue, Question))
rownames(normIssue) <- normIssue[,1]
normIssue <- subset(normIssue, select = -c(Label))
colnames(normIssue) <- c("Don't know", 'Yes', 'No')

norm2 <- normIssue %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
norm2$colname <- factor(norm2$colname, c('Yes', 'No', "Don't know"))
norm2$rowname <- factor(norm2$rowname, c('Elect more women?', 'Bothered by hearing foreign language?', 'Do immigrants commit more crime?', 'Reduce immigration and build the wall?', 
                                         'Support assault weapons ban?', 'Support rights of kneeling NFL players?', 'Support single payer?', "Has Trump drained the swamp?", 
                                         "Support Trump tariffs?", 'Do you want Congress to check Trump?', "Do you support Mueller investigation?", 'Is reverse racism bigger issue than racism?', 
                                         'Want Republicans to keep the House?','Approve of Trump?', 'Elect more feminists?',   
                                         'Support 2017 tax reform?', 'Has Trump committed crimes in office?', 'Should a Democratic House impeach Trump?', 'Do you support Kavanaugh confirmation?',
                                         'Has Trump helped you economically?', 'Is Russia investigation a witch hunt?',
                                         'Support actions of kneeling NFL players?',  'Abolish ICE?', 'Believe Kavanaugh accusations?'))

norm2 <- norm2[with(norm2, order(norm2$value)),]

plot1 <- ggplot(norm2, aes(x = colname, y = fct_rev(as_factor(rowname)), fill = value)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = paste0(round(value * 100), "%"))) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient('value', low='white', high='#B71D1A') +
  xlab("") + ylab('') + theme_dfp() +
  theme(legend.position="none")
#ggsave('repYes.png')

normIssue <- flipped

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)
#base1 <- (merge(lookup, largetable, by = 'HouseType'))

labels <- read.csv('nytMap.csv')
rownames(labels) <- labels[,1]
normIssue <- (merge(labels, normIssue, by='issue'))
normIssue <- subset(normIssue, select = -c(issue, Question))
rownames(normIssue) <- normIssue[,1]
normIssue <- subset(normIssue, select = -c(Label))
colnames(normIssue) <- c("Don't know", 'No', 'Yes')

norm2 <- normIssue %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
norm2$colname <- factor(norm2$colname, c('Yes', 'No', "Don't know"))
norm2$rowname <- factor(norm2$rowname, c('Elect more women?', 'Bothered by hearing foreign language?', 'Do immigrants commit more crime?', 'Reduce immigration and build the wall?', 
                                         'Support assault weapons ban?', 'Support rights of kneeling NFL players?', 'Support single payer?', "Has Trump drained the swamp?", 
                                         "Support Trump tariffs?", 'Do you want Congress to check Trump?', "Do you support Mueller investigation?", 'Is reverse racism bigger issue than racism?', 
                                         'Want Republicans to keep the House?','Approve of Trump?', 'Elect more feminists?',   
                                         'Support 2017 tax reform?', 'Has Trump committed crimes in office?', 'Should a Democratic House impeach Trump?', 'Do you support Kavanaugh confirmation?',
                                         'Has Trump helped you economically?', 'Is Russia investigation a witch hunt?',
                                         'Support actions of kneeling NFL players?',  'Abolish ICE?', 'Believe Kavanaugh accusations?'))

norm2 <- norm2[with(norm2, order(norm2$value)),]

plot2 <- ggplot(norm2, aes(x = colname, y = fct_rev(as_factor(rowname)), fill = value)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = paste0(round(value * 100), "%"))) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient('value', low='white', high='#B71D1A') +
  xlab("") + ylab('') + theme_dfp() +
  theme(legend.position="none")
#ggsave('repNo.png')

g <- grid.arrange(plot1, plot2, nrow=1, top="Independent Voters who Support Republican Representatives")
ggsave('RepInd.png',g)
############################################################################################ PARTISANS

Dems <- dataset[dataset$partyid=='Democrat',]
Dems <- Dems[Dems$response=='Dem',]

Rep <- dataset[dataset$partyid=='Republican',]
Rep <- Rep[Rep$response=='Rep',]

############################ Dem Voters

issues <- Dems %>%
  select(genballot, approve, CRIMES, NFLA, NFLRIGHT, CHECK, REVERSERACIS, SWAMP, COLLUDE, MUELLER, IMPEACH, APPKAV, SINGLEPAY, FEMINISM, 
         WOMEN, TARIFF, TAXREFORM, TRUMPECON, KAVASST, ASSAULTW, ENGLISH, ABOLOISHICE, CONIMMIG, IMMCRIME, w_LV)

issueList <- issues %>%
  select(genballot, approve, CRIMES, NFLA, NFLRIGHT, CHECK, REVERSERACIS, SWAMP, COLLUDE, MUELLER, IMPEACH, APPKAV, SINGLEPAY, FEMINISM, 
         WOMEN, TARIFF, TAXREFORM, TRUMPECON, KAVASST, ASSAULTW, ENGLISH, ABOLOISHICE, CONIMMIG, IMMCRIME)


rm(sumIssue)
for (issue in names(issueList)){
  if (!exists("sumIssue")){
    issueData <- issues %>%
      select(issue, w_LV)
    issueData <- na.omit(issueData)
    sumIssue <- aggregate(issueData[2], by=issueData[1], FUN=sum)
    sumIssue <- data.frame(t(sumIssue))
    names(sumIssue) <- as.character(unlist(sumIssue[1,]))
    sumIssue <- sumIssue[-1,]
    sumIssue$issue <- issue
    
  }
  
  if (exists("sumIssue")){
    issueData <- issues %>%
      select(issue, w_LV)
    issueData <- na.omit(issueData)
    temp_issue <- aggregate(issueData[2], by=issueData[1], FUN=sum)
    temp_issue <- data.frame(t(temp_issue))
    names(temp_issue) <- as.character(unlist(temp_issue[1,]))
    temp_issue <- temp_issue[-1,]
    temp_issue$issue <- issue
    sumIssue<-merge(sumIssue, temp_issue, all=TRUE)
  }
}

rownames(sumIssue) <- sumIssue$issue
sumIssue <- subset(sumIssue,select=-c(issue))

names(sumIssue)[names(sumIssue) == 'agree'] <- 'Dem'
sumIssue$Dem <- as.character(sumIssue$Dem)
sumIssue$Support <- as.character(sumIssue$Support)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Support[is.na(sumIssue$Dem)]


sumIssue$support <- as.character(sumIssue$support)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$support[is.na(sumIssue$Dem)]

sumIssue$'Reps. keep House' <- as.character(sumIssue$'Reps. keep House')
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$'Reps. keep House'[is.na(sumIssue$Dem)]

sumIssue$Believe <- as.character(sumIssue$Believe)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Believe[is.na(sumIssue$Dem)]

sumIssue$Approve <- as.character(sumIssue$Approve)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Approve[is.na(sumIssue$Dem)]

names(sumIssue)[names(sumIssue) == 'disagree'] <- 'Rep'
sumIssue$Rep <- as.character(sumIssue$Rep)
sumIssue['CHECK', 'Rep'] <- sumIssue['CHECK', 'Dem']
sumIssue['CHECK', 'Dem'] <- NA

sumIssue$Oppose <- as.character(sumIssue$Oppose)
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$Oppose[is.na(sumIssue$Rep)]

sumIssue$oppose <- as.character(sumIssue$oppose)
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$oppose[is.na(sumIssue$Rep)]

sumIssue$Check <- as.character(sumIssue$Check)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Check[is.na(sumIssue$Dem)]

sumIssue$'Dems. take House' <- as.character(sumIssue$'Dems. take House')
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$'Dems. take House'[is.na(sumIssue$Rep)]

sumIssue$'Do not believe' <- as.character(sumIssue$'Do not believe')
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$'Do not believe'[is.na(sumIssue$Rep)]

sumIssue$Disapp. <- as.character(sumIssue$Disapp.)
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$Disapp.[is.na(sumIssue$Rep)]

sumIssue <- sumIssue %>% 
  select("Don't know", 'Dem', 'Rep')

sumIssue$"Don't know" <- as.numeric(as.character(sumIssue$"Don't know"))
sumIssue$Dem <- as.numeric(sumIssue$Dem)
sumIssue$Rep <- as.numeric(sumIssue$Rep)
normIssue <- sumIssue/rowSums(sumIssue)
sumIssue$polled <- sumIssue$"Don't know" +sumIssue$Dem + sumIssue$Rep

noFlip<- normIssue[rownames(normIssue) == 'CHECK' | rownames(normIssue) == 'CRIMES' | rownames(normIssue) == 'NFLA' | rownames(normIssue) == 'NFLRIGHT' |
                     rownames(normIssue) == 'MUELLER' | rownames(normIssue) == 'IMPEACH' |
                     rownames(normIssue) == 'SINGLEPAY' | rownames(normIssue) == 'FEMINISM' | rownames(normIssue) == 'WOMEN' | rownames(normIssue) == 'ASSAULTW' | 
                     rownames(normIssue) == 'ABOLOISHICE' | rownames(normIssue) == 'KAVASST',]



toFlip <- normIssue[rownames(normIssue) == 'COLLUDE' | rownames(normIssue) == 'APPKAV' | rownames(normIssue) == 'TAXREFORM' | rownames(normIssue) == 'TRUMPECON' |
                      rownames(normIssue) == 'REVERSERACIS' | rownames(normIssue) == 'ENGLISH' | rownames(normIssue) == 'CONIMMIG' | rownames(normIssue) == 'IMMCRIME' | 
                      rownames(normIssue) == 'approve' | rownames(normIssue) == 'TARIFF' | rownames(normIssue) == 'genballot'| rownames(normIssue) == 'SWAMP',]
toFlip$hold <- toFlip$Rep
toFlip$Rep <- toFlip$Dem
toFlip$Dem <- toFlip$hold
flipped <- toFlip %>% 
  select("Don't know", 'Dem', 'Rep')

normIssue <- noFlip

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)
#base1 <- (merge(lookup, largetable, by = 'HouseType'))

labels <- read.csv('nytMap.csv')
rownames(labels) <- labels[,1]
normIssue <- (merge(labels, normIssue, by='issue'))
normIssue <- subset(normIssue, select = -c(issue, Question))
rownames(normIssue) <- normIssue[,1]
normIssue <- subset(normIssue, select = -c(Label))
colnames(normIssue) <- c("Don't know", 'Yes', 'No')

norm2 <- normIssue %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
norm2$colname <- factor(norm2$colname, c('Yes', 'No', "Don't know"))
norm2$rowname <- factor(norm2$rowname, c('Elect more women?', 'Bothered by hearing foreign language?', 'Do immigrants commit more crime?', 'Reduce immigration and build the wall?', 
                                         'Support assault weapons ban?', 'Support rights of kneeling NFL players?', 'Support single payer?', "Has Trump drained the swamp?", 
                                         "Support Trump tariffs?", 'Do you want Congress to check Trump?', "Do you support Mueller investigation?", 'Is reverse racism bigger issue than racism?', 
                                         'Want Republicans to keep the House?','Approve of Trump?', 'Elect more feminists?',   
                                         'Support 2017 tax reform?', 'Has Trump committed crimes in office?', 'Should a Democratic House impeach Trump?', 'Do you support Kavanaugh confirmation?',
                                         'Has Trump helped you economically?', 'Is Russia investigation a witch hunt?',
                                         'Support actions of kneeling NFL players?',  'Abolish ICE?', 'Believe Kavanaugh accusations?'))

norm2 <- norm2[with(norm2, order(norm2$value)),]

plot1 <- ggplot(norm2, aes(x = colname, y = fct_rev(as_factor(rowname)), fill = value)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = paste0(round(value * 100), "%"))) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient('value', low='white', high='#124073') +
  xlab("") + ylab('') + theme_dfp() +
  theme(legend.position="none")
#ggsave('DemsYes.png')

normIssue <- flipped

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)
#base1 <- (merge(lookup, largetable, by = 'HouseType'))

labels <- read.csv('nytMap.csv')
rownames(labels) <- labels[,1]
normIssue <- (merge(labels, normIssue, by='issue'))
normIssue <- subset(normIssue, select = -c(issue, Question))
rownames(normIssue) <- normIssue[,1]
normIssue <- subset(normIssue, select = -c(Label))
colnames(normIssue) <- c("Don't know", 'No', 'Yes')

norm2 <- normIssue %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
norm2$colname <- factor(norm2$colname, c('Yes', 'No', "Don't know"))
norm2$rowname <- factor(norm2$rowname, c('Elect more women?', 'Bothered by hearing foreign language?', 'Do immigrants commit more crime?', 'Reduce immigration and build the wall?', 
                                         'Support assault weapons ban?', 'Support rights of kneeling NFL players?', 'Support single payer?', "Has Trump drained the swamp?", 
                                         "Support Trump tariffs?", 'Do you want Congress to check Trump?', "Do you support Mueller investigation?", 'Is reverse racism bigger issue than racism?', 
                                         'Want Republicans to keep the House?','Approve of Trump?', 'Elect more feminists?',   
                                         'Support 2017 tax reform?', 'Has Trump committed crimes in office?', 'Should a Democratic House impeach Trump?', 'Do you support Kavanaugh confirmation?',
                                         'Has Trump helped you economically?', 'Is Russia investigation a witch hunt?',
                                         'Support actions of kneeling NFL players?',  'Abolish ICE?', 'Believe Kavanaugh accusations?'))

norm2 <- norm2[with(norm2, order(norm2$value)),]

plot2 <- ggplot(norm2, aes(x = colname, y = fct_rev(as_factor(rowname)), fill = value)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = paste0(round(value * 100), "%"))) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient('value', low='white', high='#124073') +
  xlab("") + ylab('') + theme_dfp() +
  theme(legend.position="none")
#ggsave('DemsNo.png')

g <- grid.arrange(plot1, plot2, nrow=1, top="Democratic Voters")
ggsave('Dems.png',g)

################## Rep Voters

issues <- Rep %>%
  select(genballot, approve, CRIMES, NFLA, NFLRIGHT, CHECK, REVERSERACIS, SWAMP, COLLUDE, MUELLER, IMPEACH, APPKAV, SINGLEPAY, FEMINISM, 
         WOMEN, TARIFF, TAXREFORM, TRUMPECON, KAVASST, ASSAULTW, ENGLISH, ABOLOISHICE, CONIMMIG, IMMCRIME, w_LV)

issueList <- issues %>%
  select(genballot, approve, CRIMES, NFLA, NFLRIGHT, CHECK, REVERSERACIS, SWAMP, COLLUDE, MUELLER, IMPEACH, APPKAV, SINGLEPAY, FEMINISM, 
         WOMEN, TARIFF, TAXREFORM, TRUMPECON, KAVASST, ASSAULTW, ENGLISH, ABOLOISHICE, CONIMMIG, IMMCRIME)


rm(sumIssue)
for (issue in names(issueList)){
  if (!exists("sumIssue")){
    issueData <- issues %>%
      select(issue, w_LV)
    issueData <- na.omit(issueData)
    sumIssue <- aggregate(issueData[2], by=issueData[1], FUN=sum)
    sumIssue <- data.frame(t(sumIssue))
    names(sumIssue) <- as.character(unlist(sumIssue[1,]))
    sumIssue <- sumIssue[-1,]
    sumIssue$issue <- issue
    
  }
  
  if (exists("sumIssue")){
    issueData <- issues %>%
      select(issue, w_LV)
    issueData <- na.omit(issueData)
    temp_issue <- aggregate(issueData[2], by=issueData[1], FUN=sum)
    temp_issue <- data.frame(t(temp_issue))
    names(temp_issue) <- as.character(unlist(temp_issue[1,]))
    temp_issue <- temp_issue[-1,]
    temp_issue$issue <- issue
    sumIssue<-merge(sumIssue, temp_issue, all=TRUE)
  }
}

rownames(sumIssue) <- sumIssue$issue
sumIssue <- subset(sumIssue,select=-c(issue))

names(sumIssue)[names(sumIssue) == 'agree'] <- 'Dem'
sumIssue$Dem <- as.character(sumIssue$Dem)
sumIssue$Support <- as.character(sumIssue$Support)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Support[is.na(sumIssue$Dem)]


sumIssue$support <- as.character(sumIssue$support)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$support[is.na(sumIssue$Dem)]

sumIssue$'Reps. keep House' <- as.character(sumIssue$'Reps. keep House')
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$'Reps. keep House'[is.na(sumIssue$Dem)]

sumIssue$Believe <- as.character(sumIssue$Believe)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Believe[is.na(sumIssue$Dem)]

sumIssue$Approve <- as.character(sumIssue$Approve)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Approve[is.na(sumIssue$Dem)]

names(sumIssue)[names(sumIssue) == 'disagree'] <- 'Rep'
sumIssue$Rep <- as.character(sumIssue$Rep)
sumIssue['CHECK', 'Rep'] <- sumIssue['CHECK', 'Dem']
sumIssue['CHECK', 'Dem'] <- NA

sumIssue$Oppose <- as.character(sumIssue$Oppose)
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$Oppose[is.na(sumIssue$Rep)]

sumIssue$oppose <- as.character(sumIssue$oppose)
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$oppose[is.na(sumIssue$Rep)]

sumIssue$Check <- as.character(sumIssue$Check)
sumIssue$Dem[is.na(sumIssue$Dem)] <- sumIssue$Check[is.na(sumIssue$Dem)]

sumIssue$'Dems. take House' <- as.character(sumIssue$'Dems. take House')
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$'Dems. take House'[is.na(sumIssue$Rep)]

sumIssue$'Do not believe' <- as.character(sumIssue$'Do not believe')
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$'Do not believe'[is.na(sumIssue$Rep)]

sumIssue$Disapp. <- as.character(sumIssue$Disapp.)
sumIssue$Rep[is.na(sumIssue$Rep)] <- sumIssue$Disapp.[is.na(sumIssue$Rep)]

sumIssue <- sumIssue %>% 
  select("Don't know", 'Dem', 'Rep')

sumIssue$"Don't know" <- as.numeric(as.character(sumIssue$"Don't know"))
sumIssue$Dem <- as.numeric(sumIssue$Dem)
sumIssue$Rep <- as.numeric(sumIssue$Rep)
normIssue <- sumIssue/rowSums(sumIssue)
sumIssue$polled <- sumIssue$"Don't know" +sumIssue$Dem + sumIssue$Rep

noFlip<- normIssue[rownames(normIssue) == 'CHECK' | rownames(normIssue) == 'CRIMES' | rownames(normIssue) == 'NFLA' | rownames(normIssue) == 'NFLRIGHT' |
                     rownames(normIssue) == 'MUELLER' | rownames(normIssue) == 'IMPEACH' |
                     rownames(normIssue) == 'SINGLEPAY' | rownames(normIssue) == 'FEMINISM' | rownames(normIssue) == 'WOMEN' | rownames(normIssue) == 'ASSAULTW' | 
                     rownames(normIssue) == 'ABOLOISHICE' | rownames(normIssue) == 'KAVASST',]



toFlip <- normIssue[rownames(normIssue) == 'COLLUDE' | rownames(normIssue) == 'APPKAV' | rownames(normIssue) == 'TAXREFORM' | rownames(normIssue) == 'TRUMPECON' |
                      rownames(normIssue) == 'REVERSERACIS' | rownames(normIssue) == 'ENGLISH' | rownames(normIssue) == 'CONIMMIG' | rownames(normIssue) == 'IMMCRIME' | 
                      rownames(normIssue) == 'approve' | rownames(normIssue) == 'TARIFF' | rownames(normIssue) == 'genballot'| rownames(normIssue) == 'SWAMP',]
toFlip$hold <- toFlip$Rep
toFlip$Rep <- toFlip$Dem
toFlip$Dem <- toFlip$hold
flipped <- toFlip %>% 
  select("Don't know", 'Dem', 'Rep')

normIssue <- noFlip

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)
#base1 <- (merge(lookup, largetable, by = 'HouseType'))

labels <- read.csv('nytMap.csv')
rownames(labels) <- labels[,1]
normIssue <- (merge(labels, normIssue, by='issue'))
normIssue <- subset(normIssue, select = -c(issue, Question))
rownames(normIssue) <- normIssue[,1]
normIssue <- subset(normIssue, select = -c(Label))
colnames(normIssue) <- c("Don't know", 'Yes', 'No')

norm2 <- normIssue %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
norm2$colname <- factor(norm2$colname, c('Yes', 'No', "Don't know"))
norm2$rowname <- factor(norm2$rowname, c('Elect more women?', 'Bothered by hearing foreign language?', 'Do immigrants commit more crime?', 'Reduce immigration and build the wall?', 
                                         'Support assault weapons ban?', 'Support rights of kneeling NFL players?', 'Support single payer?', "Has Trump drained the swamp?", 
                                         "Support Trump tariffs?", 'Do you want Congress to check Trump?', "Do you support Mueller investigation?", 'Is reverse racism bigger issue than racism?', 
                                         'Want Republicans to keep the House?','Approve of Trump?', 'Elect more feminists?',   
                                         'Support 2017 tax reform?', 'Has Trump committed crimes in office?', 'Should a Democratic House impeach Trump?', 'Do you support Kavanaugh confirmation?',
                                         'Has Trump helped you economically?', 'Is Russia investigation a witch hunt?',
                                         'Support actions of kneeling NFL players?',  'Abolish ICE?', 'Believe Kavanaugh accusations?'))

norm2 <- norm2[with(norm2, order(norm2$value)),]

plot1 <- ggplot(norm2, aes(x = colname, y = fct_rev(as_factor(rowname)), fill = value)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = paste0(round(value * 100), "%"))) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient('value', low='white', high='#B71D1A') +
  xlab("") + ylab('') + theme_dfp() +
  theme(legend.position="none")
#ggsave('RepsYes.png',units=c('cm'))

normIssue <- flipped

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)

normIssue <- na.omit(normIssue)
normIssue$issue <- rownames(normIssue)
#base1 <- (merge(lookup, largetable, by = 'HouseType'))

labels <- read.csv('nytMap.csv')
rownames(labels) <- labels[,1]
normIssue <- (merge(labels, normIssue, by='issue'))
normIssue <- subset(normIssue, select = -c(issue, Question))
rownames(normIssue) <- normIssue[,1]
normIssue <- subset(normIssue, select = -c(Label))
colnames(normIssue) <- c("Don't know", 'No', 'Yes')

norm2 <- normIssue %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
norm2$colname <- factor(norm2$colname, c('Yes', 'No', "Don't know"))
norm2$rowname <- factor(norm2$rowname, c('Elect more women?', 'Bothered by hearing foreign language?', 'Do immigrants commit more crime?', 'Reduce immigration and build the wall?', 
                                         'Support assault weapons ban?', 'Support rights of kneeling NFL players?', 'Support single payer?', "Has Trump drained the swamp?", 
                                         "Support Trump tariffs?", 'Do you want Congress to check Trump?', "Do you support Mueller investigation?", 'Is reverse racism bigger issue than racism?', 
                                         'Want Republicans to keep the House?','Approve of Trump?', 'Elect more feminists?',   
                                         'Support 2017 tax reform?', 'Has Trump committed crimes in office?', 'Should a Democratic House impeach Trump?', 'Do you support Kavanaugh confirmation?',
                                         'Has Trump helped you economically?', 'Is Russia investigation a witch hunt?',
                                         'Support actions of kneeling NFL players?',  'Abolish ICE?', 'Believe Kavanaugh accusations?'))

norm2 <- norm2[with(norm2, order(norm2$value)),]

plot2 <- ggplot(norm2, aes(x = colname, y = fct_rev(as_factor(rowname)), fill = value)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = paste0(round(value * 100), "%"))) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient('value', low='white', high='#B71D1A') +
  xlab("") + ylab('') + theme_dfp() +
  theme(legend.position="none")
#ggsave('RepsNo.png',units=c('cm'))

g <- grid.arrange(plot1, plot2, nrow=1, top="Republican Voters")
ggsave('Reps.png',g)


# Build plot of voter likelihood
like <- allInd %>% 
  group_by(likely) %>%
  tally()
like$likely <- as.character(like$likely)
like <- like[like$likely != "[DO NOT READ] Don't know/Refused", ]
like$n <- like$n/sum(like$n)
like$perc <- paste(round(like$n*100,1), "%", sep="")
like$order[like$likely == 'Not at all likely'] <- 1
like$order[like$likely == 'Not very likely'] <- 2
like$order[like$likely == 'Somewhat likely'] <- 3
like$order[like$likely == 'Very likely'] <- 4
like$order[like$likely == 'Almost certain'] <- 5
like$order[like$likely == 'Already voted'] <- 6
like$likely <- factor(like$likely,levels = like$likely[order(like$order)])

ggplot(like, aes(y=n, x=likely, fill=likely)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=perc, hjust = -.1)) + 
  xlab('') + ylab('') + guides(fill=FALSE) +
  theme_dfp() + theme(axis.text.x = element_blank()) + coord_flip() +
  scale_fill_manual(values=c("#124073", "#A8BF14", "#B71D1A", '#BF7800', '#b3b3b3', '#0A2645')) + 
  ggtitle('Voter Likelihood for Self-Indentified Independents ') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank())
ggsave('VoterLikelihood.png')
#PPT with TBI
TBI <- read.csv('~/OneDrive - SickKids/ABCD_TBI/ABCD_Data/TBI_FORQUALANALYSIS.csv')
colnames(TBI)[2] <- ("subjectkey")
colnames(TBI)[3] <- ("Group")
TBI <- TBI[,-1]
levels(TBI$Group) <- sub("yes", "TBI", levels(TBI$Group))

#acspsw03.txt - ACS_PostStratification_Weight
## TWINS = A combination of having the same rel_family_id as another person & rel_relationship (twin, triplet) 
ACS <- read.csv('~/OneDrive - SickKids/ABCD_TBI/ABCD_Data/ACS_PostStratification_Weight/acspsw03.txt', sep='\t', header=T)
#data dictionary
dataDic_ACS <- ACS[1,]
dataDic_ACS <- t(dataDic_ACS)
ACS <- ACS[-1,]
ACS <- subset(ACS, eventname=="baseline_year_1_arm_1")

ACS_ALL <- merge(ACS, TBI, by='subjectkey', all.x = T)
levels <- levels(ACS_ALL$Group)
levels[length(levels) + 1] <- "CTRL"
ACS_ALL$Group <- factor(ACS_ALL$Group, levels = levels)
ACS_ALL$Group[is.na(ACS_ALL$Group)] <- "CTRL"
ACS_ALL <- ACS_ALL[,c(1,6,7,8,10,11,12,13,14,15,18)]

#only TBI
ACS_TBI <- ACS_ALL[ACS_ALL$Group=="TBI",]

### Number of twin pairs who both have TBI
#### 2 = Twins
TBItwins <- ACS_TBI[c('subjectkey', 'rel_family_id')] %>% 
  group_by(rel_family_id) %>% 
  #adding new column n
  mutate(n=n()) %>% 
  ungroup() %>% 
  arrange(rel_family_id) %>% 
  arrange(-n)

count(subset(TBItwins, TBItwins$n==2))
print(c("There are 4 pairs of twins where both siblings have had a TBI"))

### Number of relative pairs where one has TBI
#### 0 = No sibling; Only want participant with siblings
temp <- subset(ACS_ALL, rel_relationship!=0)
#### 2 = Twin; 1 = Sibling; 3 = Triplet
NoTBIsibling <- as.data.frame(table(temp$rel_relationship[temp$Group=="TBI"])) %>% 
  arrange(-Freq)
NoTBIsibling
print(c("There are 74 pairs of twins (including 4 where both have TBI, and 70 where only one sibling has a TBI, 
        48 (non-twin) siblings where one has a TBI, and 1 set of triplets where one has a TBI"))

### Siblings
#### Non-TBI Twin Sibling
twinSiblings <- subset(ACS_ALL, rel_relationship==2)

#not including the 4 pairs that are all twins with TBI
twinSiblings <- twinSiblings[!(duplicated(twinSiblings[c(6,11)])|duplicated(twinSiblings[c(6,11)], fromLast=TRUE)),] %>% 
  arrange(rel_family_id) %>%  
  #rel_family_id: 11635 does not have TBI twin 
  filter(rel_family_id != "11635")

#TBI twins
twinSiblingsTBI <- twinSiblings[twinSiblings$Group=="TBI",]   #66 twin siblings with TBI

#twin siblings of TBI 
twinSiblings_noTBI <- twinSiblings[twinSiblings$Group=="CTRL",] %>% 
  arrange(rel_family_id)  #66 twin siblings without TBI

#### Non-twin Siblings
siblings <- subset(ACS_ALL, rel_relationship==1)
names(siblings)

#keep only those with TBI  
siblingsTBI <- siblings[siblings$Group=="TBI",]   #48 non-twin siblings where one has TBI
TBIid <- as.data.frame(siblingsTBI[,6]) 
colnames(TBIid) <- "rel_family_id"

#find CTRL siblings of TBI
siblings_noTBI <- merge(TBIid, siblings, by= c("rel_family_id"), all.x =T) %>% 
  arrange(rel_family_id) %>%   #10594 has two CTRL siblings
  filter(Group=="CTRL")

write.csv(twinSiblingsTBI, file = "~/OneDrive - SickKids/ABCD_TBI/TBI_SiblingIDs/twinSiblingsTBI.csv")
write.csv(twinSiblings_noTBI, file = "~/OneDrive - SickKids/ABCD_TBI/TBI_SiblingIDs/twinSiblings_noTBI.csv")
write.csv(siblingsTBI, file = "~/OneDrive - SickKids/ABCD_TBI/TBI_SiblingIDs/NonTwinSiblingsTBI.csv")
write.csv(siblings_noTBI, file = "~/OneDrive - SickKids/ABCD_TBI/TBI_SiblingIDs/NonTwinSiblings_noTBI.csv")




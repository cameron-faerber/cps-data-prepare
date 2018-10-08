library(readr)
library(data.table)
source("functions.R")

# import raw data / dictionary data #
# note original data is split into 4 separate files due to github file restriction size. you may also download the raw data from the website and read that directly in
files = list.files("data/", pattern="\\.dat$", full.names=T)
raw = list()
for(f in files) raw[[f]] = readLines(f)
raw = unlist(raw)
#
dict = readLines("data/08ASEC2018_Data_Dict_Full.txt", encoding="UTF-8")

### prepare dictionary object(s) for household, family, and person variables ###
ind_family = grep("FAMILY RECORD", dict)
ind_person = grep("PERSON RECORD", dict)
hdict = dictionary_clean(dict[1:(ind_family-1)]) # household 
fdict = dictionary_clean(dict[ind_family:(ind_person-1)]) # family
pdict = dictionary_clean(dict[-c(1:ind_person)]) # person
### ###

### parse raw data file ###
# determine indices of household, family, and person records; note the heirarchical structure on p.2-6 of the reference documentation
pos = as.numeric(substr(raw, 7, 8))
i_household = which(pos==0)
i_family = which(pos>0&pos<40)
i_person = which(pos>=41)

# create family identifier for person data
currentFamily = numeric(length(raw))
for(i in 1:length(raw)){
  if(pos[i]==0){
    # reset family
    family_holder = NA
  }
  if(pos[i]>0&pos[i]<40){
    family_holder = substr(raw[i], start=7, stop=8)
  }
  currentFamily[i] = family_holder
}

# household (127 vars)
household = list()
for(i in 1:nrow(hdict)){
  household[[hdict$label[i]]] = substr(raw[i_household], hdict$begin[i], hdict$begin[i]+(hdict$size[i]-1))
  print(i)
}
household = data.frame(do.call(cbind, household), stringsAsFactors=F)

# family (74 vars)
family = list()
for(i in 1:nrow(fdict)){
  family[[fdict$label[i]]] = substr(raw[i_family], fdict$begin[i], fdict$begin[i]+(fdict$size[i]-1))
  print(i)
}
family = data.frame(do.call(cbind, family), stringsAsFactors=F)


# person (496 vars)
person = list()
for(i in 1:nrow(pdict)){
  person[[pdict$label[i]]] = substr(raw[i_person], pdict$begin[i], pdict$begin[i]+(pdict$size[i]-1))
  print(i)
}
person = data.frame(do.call(cbind, person), stringsAsFactors=F)
person$id_family = currentFamily[i_person]
# ensure proper order
person$person_order = 1:nrow(person)

## merge household, family, and person, retaining all data on the person level
# merge person household
dat = merge(person, household, by.x="PH_SEQ", by.y="H_SEQ", all.x=T)
# merge this with family data
dat = merge(dat, family, by.x=c("PH_SEQ","id_family"), by.y=c("FH_SEQ","FFPOS"), all.x=T)
# order by household, family, person
dat = dat[base::order(dat$PH_SEQ, dat$id_family, dat$person_order),]
dat$person_order = NULL
### ###

# clear memory #
rm(list=setdiff(ls(), "dat"))
# # 

# make names lower case (because I like them that way) #
names(dat) = tolower(names(dat))
# # 

# store as a .csv and test performance #
fwrite(dat, "data/march2018.csv")
# #

if(T == F){ 
library(data.table)
library(readxl)
library(tidyverse)
options(stringsAsFactors = F)
m = fread("markov_clustered_orgs_interpretable_full_201904182237.csv") 
setkey(m,alias_id)
set(m,which(is.na(m$alias_name)),"alias_name",'')
m[,alias_name:=trimws(alias_name)]
sup = m[,max(nchar(alias_name))]
m[,alias_length := nchar(alias_name)]
t = m[data.table(start_pos=1:100),
      .(alias_id,alias_name,start_pos,alias_length=nchar(alias_name)),
      on="alias_length>=start_pos",
      nomatch=0,allow.cartesian=T][
        order(alias_id)
        ]
t[,end_pos := pmin(start_pos+2,alias_length)]
directory_trigrams= t[start_pos==1 | start_pos+2 == end_pos, 
                      .(
                        trigram=substr(alias_name,start_pos,end_pos),
                        alias_id)]


setkey(directory_trigrams,trigram)

library(xml2)
result = read_xml("Data/Banking Research/20171231_ATTRIBUTES_ACTIVE/20171231_ATTRIBUTES_ACTIVE.XML")
children = xml_children(result)
first_child = children[[1000]]
xml_children(first_child)
rssd = xml_find_all(result,"//ATTRIBUTES//ID_RSSD")
NM_LGL = xml_find_all(result,"//ATTRIBUTES//NM_LGL")
NM_SHORT = xml_find_all(result,"//ATTRIBUTES//NM_SHORT")

x = data.table(
  rssd=trimws(xml_text(rssd)),
  NM_LGL=trimws(xml_text(NM_LGL)),
  NM_SHORT=trimws(xml_text(NM_SHORT))
)


compustat = fread("Data/Banking Research/compustat_data.csv")
w = compustat[,.I[.N],by=permco]
y = compustat[w$V1,.(permco,comnam)]


bank_research <- new.env()
assign("x",x,envir=bank_research)
assign("y",y,envir=bank_research)
assign('directory_trigrams',directory_trigrams,envir=bank_research)
assign('directory',m[,.(alias_id,alias_name,canonical_id,canonical_name)],envir=bank_research)
save(file="LinkIt-software/data/bank_research.data",list=ls(envir=bank_research),envir=bank_research)


ikiisup=max(nchar(m$alias_name),nairm=T)
total = data.frame(i=1:sup)

x2 = sqldf("select alias_id,
           alias_name,
           case when i is null then 1
           else i
           end as start_pos from m
           left join total
           on i<length(alias_name)-1")

ptm=proc.time()
trigrams = sqldf("with example as (select alias_id,
                 alias_name,
                 case when i is null then 1
                 else i
                 end as start_pos from m
                 left join total
                 on i<length(alias_name)-1)
                 select 
                 substr(alias_name,start_pos,3) as trigram,
                 alias_id from example
                 order by trigram") %>%
  as_tibble()
ptm2 = proc.time()
ptm2-ptm



set.seed(1)
examples = sample(m$alias_name,100)
examples
total_matches = NULL
for (example in examples){
  starter = data.frame(example,start_pos=1:(nchar(example)-2))
  yo = sqldf(
    "
    with eg_trigram as (
    select 
    substr(example,start_pos,3) as trigram 
    from 
    starter)
    select * from eg_trigram as A
    left join trigrams as B
    on A.trigram=B.trigram
    "
  )
  trigrams[]
  
  
  total_matches=c(total_matches,length(unique(yo$alias_id)))
}


t2 = data.table(trigrams)

setkey(t2,"trigram")


trigramify <- function(x){
  if (nchar(x)<3){
    return(x)
  } else {
    dtable = data.frame(start_pos=seq(1,nchar(x)-2,1))
    return(apply(dtable,1,function(start_pos) substr(x,start_pos,start_pos+2)))
  }
}



t3 = m[,list(trigram=trigramify(alias_name)),alias_id]
setkey(t3,trigram)


set.seed(1)
examples = sample(m$alias_name,100)
ptm=proc.time()
counts = NULL
time.full = NULL
time.small = NULL
for (example in examples){
  responses = unique(t3[trigramify(example),alias_id],by='alias_id')
  counts = c(counts,length(responses))
  #check the full time
  process_start = proc.time()
  dists = stringdist(example,m$alias_name,method='lv')
  infimum =min(dists,na.rm=T)
  match = m[which(dists==infimum),"alias_id"]
  #record
  time.full = c(time.full,(proc.time()-process_start)['elapsed'])
  #check the short time
  process_start = proc.time()
  responses = unique(t3[trigramify(example),alias_id])
  match = m[alias_id %in% responses,
            list(alias_id,lv=stringdist(example,alias_name,method='lv'))
            ][order(lv)[1],alias_id]
  time.small = c(time.small,(proc.time()-process_start)['elapsed'])
}

ggplot(data.frame(small=time.small,full=time.full),
       aes(x=small,y=full)) + 
  geom_point() + 
  geom_abline(slope=1,intercept=0)


qplot(time.full/time.small)  +
  scale_x_log10()

ptm2 = proc.time()
ptm2-ptm
library(ggplot2)
qplot(x=counts/nrow(m),geom = 'density')

stringdist(example,m$alias_name)

# Libgober and Carpenter --------------------------------------------------
ground_truth = read_excel("Data/Meetings/MeetingsData.xlsx") %>%
  separate(Symbol,into=c("Symbol","Exchange"),sep=":",fill='left') %>%
  mutate(Exchange=ifelse(Exchange=='NA',NA,Exchange)) 

matchable = ground_truth$Exchange %in% c("NASDAQ","NYSE","US","AMEX","NASDAQ")
ground_truth$Symbol[!matchable]  = NA
ground_truth$Exchange[!matchable] = NA


ground_truth

X = read_excel("Data/Meetings/MeetingsData.xlsx",2)

read.csv("Data/Meetings/nasdaq.csv")



# Bank Redux --------------------------------------------------------------
fr_y_path = 'Data/Banking Research/fr-y'
for (fname in dir(fr_y_path)){
  fpath=file.path(fr_y_path,fname)
  archive_fname = unzip(fpath,list=T)[1,"Name"]
  cmd = paste("unzip -p",paste0('"',fpath,'"'),archive_fname,sep=" ")
  fry.data= fread(cmd,
        sep="^",
        quote="",
        na.strings="--------",header=T)
}
} 
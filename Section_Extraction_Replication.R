


library(tidyverse)
library(stringr)

setwd('C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD IKMB/Distant Political News Classification/Damian Data/relevant_urls/')

nl = read.csv('nl.csv')

pl = read.csv('pl.csv')

it = read.csv('it.csv')

de = read.csv('de.csv')



nl.sum = nl %>% group_by(full_domain) %>% dplyr::summarise(n = n())

pl.sum = pl %>% group_by(full_domain) %>% dplyr::summarise(n = n())

it.sum = it %>% group_by(full_domain) %>% dplyr::summarise(n = n())

de.sum = de %>% group_by(full_domain) %>% dplyr::summarise(n = n())


#### Extracting Sections NL ###############
nl.sec =str_split(nl$clean_url, "/")
nl.sec2 = unlist(nl.sec, recursive = FALSE)
nl.sec2 = tolower(nl.sec2)

nl.sec2 = data.frame(sections = c(t(nl.sec2)), stringsAsFactors=T)
nl.sections = nl.sec2 %>% group_by(sections) %>% summarize(n=n())


nl.sections2 = nl.sections %>% dplyr::filter(!grepl('http', sections, fixed=T)) %>% 
  dplyr::filter(!grepl('.', sections, fixed=T)) %>% 
  dplyr::filter(!grepl('[[:digit:]]', sections)) %>% dplyr::filter(!sections == "")


nl.sections3 = slice_max(nl.sections2, prop=0.01,order_by= n) #Get top 0.5% of sections
nl.sections3 = slice_max(nl.sections2, n=250,order_by= n) #Get top 250 sections

write.csv(nl.sections3,'nl_sections.csv')


sum(nl.sections3$n)/sum(nl.sections2$n)





#### Extracting Sections DE ###############
de.sec =str_split(de$clean_url, "/")
de.sec2 = unlist(de.sec, recursive = FALSE)
de.sec2 = tolower(de.sec2)
de.sec2 = data.frame(sections = c(t(de.sec2)), stringsAsFactors=T)
de.sections = de.sec2 %>% group_by(sections) %>% summarize(n=n())


de.sections2 = de.sections %>% dplyr::filter(!grepl('http', sections, fixed=T)) %>% 
  dplyr::filter(!grepl('.', sections, fixed=T)) %>% 
  dplyr::filter(!grepl('[[:digit:]]', sections)) %>% dplyr::filter(!sections == "")


de.sections3 = slice_max(de.sections2, prop=0.005,order_by= n) #Get top 0.5% of sections
de.sections3 = slice_max(de.sections2, n=250,order_by= n) #Get top 250 of sections


write.csv(de.sections3,'de_sections.csv')


sum(de.sections3$n)/sum(de.sections2$n)



#### Extracting Sections IT ###############
it.sec =str_split(it$clean_url, "/")
it.sec2 = unlist(it.sec, recursive = FALSE)
it.sec2 = tolower(it.sec2)
it.sec2 = data.frame(sections = c(t(it.sec2)), stringsAsFactors=T)
it.sections = it.sec2 %>% group_by(sections) %>% summarize(n=n())


it.sections2 = it.sections %>% dplyr::filter(!grepl('http', sections, fixed=T)) %>% 
  dplyr::filter(!grepl('.', sections, fixed=T)) %>% 
  dplyr::filter(!grepl('[[:digit:]]', sections)) %>% dplyr::filter(!sections == "")


it.sections3 = slice_max(it.sections2, prop=0.005,order_by= n) #Get top 0.5% of sections
it.sections3 = slice_max(it.sections2, n=250,order_by= n) #Get top 250 of sections

write.csv(it.sections3,'it_sections.csv')


sum(it.sections3$n)/sum(it.sections2$n)

#### Extracting Sections PL ###############
pl.sec =str_split(pl$clean_url, "/")
pl.sec2 = unlist(pl.sec, recursive = FALSE)
pl.sec2 = tolower(pl.sec2)
pl.sec2 = data.frame(sections = c(t(pl.sec2)), stringsAsFactors=T)
pl.sections = pl.sec2 %>% group_by(sections) %>% summarize(n=n())


pl.sections2 = pl.sections %>% dplyr::filter(!grepl('http', sections, fixed=T)) %>% 
  dplyr::filter(!grepl('.', sections, fixed=T)) %>% 
  dplyr::filter(!grepl('[[:digit:]]', sections)) %>% dplyr::filter(!sections == "")


pl.sections3 = slice_max(pl.sections2, prop=0.005,order_by= n) #Get top 0.5% of sections
pl.sections3 = slice_max(pl.sections2, n=250,order_by= n) #Get top 0.5% of sections

write.csv(pl.sections3,'pl_sections.csv')


sum(pl.sections3$n)/sum(pl.sections2$n)













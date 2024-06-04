library(tidyverse)
library(here)

load("G:/Shared drives/EL Research Team Drive/07_Data Projects/3DSA Dashboard/4_Formatted Data Results/24_ALLDimensionsData.RData")

survey <- bind_rows(SY22_23surveys,SY23_24surveys) 

form235 <- survey |> 
  filter(formId=="235" & identifier != "16")

form235answers <- form235 |> 
  distinct(identifier, .keep_all=TRUE) |> 
  select(identifier, options) |> 
  unnest(options) |> 
  mutate(name = fct_inorder(name))

answerlevels <- levels(form235answers$name)

CrewSurvey <- form235 |> 
  select(eventend,gender,race,focusGroup,identifier,subtext,value) |> 
  mutate(value=factor(value, levels=answerlevels)) |> 
  mutate(month=month(eventend),
         time=case_when(month %in% c("8","9","10") ~ "Beginning of Year\n(Aug-Oct)",
                        month %in% c("11","12","1","2") ~ "Middle of Year\n(Nov-Feb)",
                        month %in% c("3","4","5","6") ~ "End of Year\n(March-June)"),
         time=factor(time,levels=c("Beginning of Year\n(Aug-Oct)","Middle of Year\n(Nov-Feb)","End of Year\n(March-June)"))) |>
  filter(month!="7") |> 
  select(-month,-eventend)

save(CrewSurvey, file=here("data","CrewSurvey.RData"))

CrewSurvey |> 
  ggplot(aes(x=subtext,fill=value)) +
  geom_bar(position="dodge") +facet_wrap(~time, scales="free") + theme_minimal()

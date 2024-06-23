library(tidyverse)
library(here)
library(cowplot)

load("G:/Shared drives/EL Research Team Drive/07_Data Projects/3DSA Dashboard/4_Formatted Data Results/24_ALLDimensionsData.RData")

walkthroughs <- bind_rows(SY22_23walkthroughs,SY23_24walkthroughs)

Crew <- walkthroughs |> 
  filter(formId %in% c("248","545"))

Overall <- Crew |> 
  select(eventend,focusGroup,identifier,subtext,value) |> 
  mutate(value=factor(value, levels=c("Not Evident","Somewhat Evident","Evident"))) |> 
  filter(!is.na(value)) |> 
  mutate(month=month(eventend),
         time=case_when(month %in% c("8","9","10") ~ "Beginning of Year\n(Aug-Oct)",
                        month %in% c("11","12","1","2") ~ "Middle of Year\n(Nov-Feb)",
                        month %in% c("3","4","5","6") ~ "End of Year\n(March-June)"),
         time=factor(time,levels=c("Beginning of Year\n(Aug-Oct)","Middle of Year\n(Nov-Feb)","End of Year\n(March-June)")))

Overall <- Overall |> 
  mutate(question=str_wrap(subtext, width=25),
         question=case_when(question=="Students across social identities have meaningful opportunities to demonstrate or access the learning opportunities named in the walkthrough indicators above.\n" ~
                              "Students across social identities have meaningful opportunities to demonstrate or access the above learning opportunities.\n",
                            question=="Students reflect on Habits of Scholarship/Habits of Learning within instructional routines and lessons." ~
                              "Students reflect on Habits of Scholarship/\nHabits of Learning within instructional\nroutines and lessons.",
                            .default=question)) |> 
  filter(!question %in% c("Teachers / Crew Leaders view and respond to student behavior through a trauma-responsive lens (for example, seeing resistant behavior as a possible sign of an underlying anxiety or fear, and helping students unpack and acknowledge their concerns). ",
                          "Teachers / Crew Leaders and peers use protocols to ensure respectful, inclusive interactions. If/when appropriate the crew breaks protocol in service of inclusive or responsive interactions (e.g., when the subject is emotionally heavy, choosing to give people more paired processing time in a think-pair-share). ",
                          "Teachers / Crew Leaders give students opportunities to learn/share about one’s own and others’ historical / cultural / linguistic / social identity, with an emphasis on resilience, pride, and perspective-taking.",
                          "Students demonstrate empathy, integrity, respect, and compassion as part of Crew activities."))

crewgraph <- function(x) {
  Overall |> 
    filter(identifier==x) |> 
    ggplot(mapping=aes(x=question, fill=value)) +
    geom_bar(position="dodge") +
    facet_wrap(~time, scales="free") +
    theme(legend.position="none",
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.title=element_blank())
}

Q1 <- Overall |> 
  filter(identifier=="1A") |> 
  ggplot(mapping=aes(x=question, fill=value)) +
  geom_bar(position="dodge") +
  facet_wrap(~time, scales="free") +
  theme(legend.position="none",
        axis.title=element_blank())
Q2 <- crewgraph("1B")
Q3 <- crewgraph("1C")
Q4 <- crewgraph("1D")
Q5 <- crewgraph("1E")
Q6 <- crewgraph("1F")
Q7 <- crewgraph("1G")
Q8 <- crewgraph("1H")
Q9 <- crewgraph("2A")
Q10 <- crewgraph("2B")
Q11 <- crewgraph("2C")
Q12 <- crewgraph("2D")
Q13 <- crewgraph("2E")
Q14 <- crewgraph("2F")
Q15 <- crewgraph("2G")
Q17 <- crewgraph("3A")
Q18 <- crewgraph("3C")
Q19 <- crewgraph("4A")


plot_grid(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,
          Q14,Q15,Q17,Q18, ncol=1, rel_heights=c(1.2,1,1,1.1,1,1,1,1,1,1,1,1,1,1,1,1,1.1,1.1))

ggsave("crewwalkthroughs.png",height=35,width=6)

library(tidyverse)
library(cowplot)

load(url("https://github.com/ELJRussell/DataVisualizationWorkgroup/raw/main/data/CrewSurvey.RData"))

crewgraph <- function(x) {
  CrewSurvey |> 
    filter(identifier==x) |> 
    ggplot(mapping=aes(x=question, fill=value)) +
    geom_bar(position="dodge") +
    facet_wrap(~time, scales="free") +
    theme(legend.position="none",
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.title=element_blank())
}

Q1 <- CrewSurvey |> 
    filter(identifier=="1") |> 
    ggplot(mapping=aes(x=question, fill=value)) +
    geom_bar(position="dodge") +
    facet_wrap(~time, scales="free") +
    theme(legend.position="none",
          axis.title=element_blank())
Q2 <- crewgraph("2")
Q3 <- crewgraph("3")
Q4 <- crewgraph("4")
Q5 <- crewgraph("5")
Q6 <- crewgraph("6")
Q7 <- crewgraph("7")
Q8 <- crewgraph("8")
Q9 <- crewgraph("9")
Q10 <- crewgraph("10")
Q11 <- crewgraph("11")
Q12 <- crewgraph("12")
Q13 <- crewgraph("13")
Q14 <- crewgraph("14")
Q15 <- crewgraph("15")
Q17 <- crewgraph("17")
Q18 <- crewgraph("18")
Q19 <- crewgraph("19")


plot_grid(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,
          Q14,Q15,Q17,Q18,Q19, ncol=1, rel_heights=c(1.2,1,1,1.1,1,1,1,1,1,1,1,1,1,1,1,1,1.1,1.1))

ggsave("crewsurvey.png",height=25,width=6)

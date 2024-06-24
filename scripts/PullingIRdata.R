library(tidyverse)
library(here)

load("G:/Shared drives/EL Research Team Drive/07_Data Projects/3DSA Dashboard/4_Formatted Data Results/24_3DSAData.RData")

IRs <- IRlongACTIVE |> 
  filter(rubric!="overall" &
           year %in% c("2022-2023","2023-2024")) |> 
  left_join(ActiveSchools |> select(`Dimensions schoolId`,`Official Year 1 of Partnership (coach-provided)`),
            by=c("schoolid"="Dimensions schoolId")) |> 
  select(-Partnership,-schoolname,-coach) |> 
  rename(`Partnership Year`=`Official Year 1 of Partnership (coach-provided)`) |> 
  mutate(category=case_when(str_detect(rubric,"Assessment") ~ "Assessment",
                            str_detect(rubric,"Culture") ~ "Culture and Character",
                            str_detect(rubric,"Curriculum") ~ "Curriculum",
                            str_detect(rubric,"Instruction") ~ "Instruction",
                            str_detect(rubric,"Leadership") ~ "Leadership")) |> 
  mutate(rubric=case_when(rubric=="Assessment1" ~ "18. Crafting and using Learning Targets",
                          rubric=="Assessment2" ~ "19. Checking for Understanding in Daily Instruction",
                          rubric=="Assessment3" ~ "20. Using Assessments to Boost Student Achievement",
                          rubric=="Assessment4" ~ "21. Communicating Student Achievement",
                          rubric=="Culture_Character1" ~ "13. Creating a Community of Learning",
                          rubric=="Culture_Character2" ~ "14. Fostering Habits of Character",
                          rubric=="Culture_Character3" ~ "15. Bulding the Culture and Structure of Crew",
                          rubric=="Culture_Character4" ~ "16. Engaging Families and the Community in the Life of the School",
                          rubric=="Culture_Character5" ~ "17. Creating Beautiful Spaces that Promote Learning",
                          rubric=="Curriculum1" ~ "1. Choosing, Adapting and Enhancing Curricula",
                          rubric=="Curriculum2" ~ "2. Mapping Knowledge, Skills, and Habits of Character",
                          rubric=="Curriculum3" ~ "3. Designing Case Studies",
                          rubric=="Curriculum4" ~ "4. Designing Projects and Products",
                          rubric=="Curriculum5" ~ "5. Designing Learning Expeditions",
                          rubric=="Instruction1" ~ "6. Planning Effective Lessons",
                          rubric=="Instruction2" ~ "7. Delivering Effective Lessons",
                          rubric=="Instruction3" ~ "8. Planning for and Supporting High-Quality Work",
                          rubric=="Instruction4" ~ "9. Teaching Reading across the Disciplines",
                          rubric=="Instruction5" ~ "10. Teaching Writing across the Disciplines",
                          rubric=="Instruction6" ~ "11. Teaching Mathematics",
                          rubric=="Instruction7" ~ "12. Differentiating Instruction and Supporting All Students",
                          rubric=="Leadership1" ~ "22. Fostering a Cohesive School Vision",
                          rubric=="Leadership2" ~ "23. Leading Evidence-Based Strategic Improvement",
                          rubric=="Leadership3" ~ "24. Cultivating a Positive Professional Culture",
                          rubric=="Leadership4" ~ "25. Promoting Shared Leadership",
                          rubric=="Leadership5" ~ "26. Leading Professional Learning")) |> 
  mutate(category=factor(category,levels=c("Curriculum","Instruction","Culture and Character","Assessment","Leadership"))) |> 
  arrange(year,schoolid,category) |> 
  mutate(Credentialed=case_when(Credentialed %in% c("Credentialed", "Overdue for Renewal",
                                                    "Pathway School","Provisional Credential") ~ "Credentialed",
                                Credentialed %in% c("Not Credentialed","No Longer Credentialed") ~ "Not Credentialed")) |> 
  mutate(year=case_when(year=="2022-2023" ~ "2023",
                        year=="2023-2024" ~ "2024"),
         year=as.numeric(year),
         `Partnership Year`=as.numeric(`Partnership Year`))
         
 save(IRs,file=here("data","SY22_23andSY23_24IRs.RData"))          
      